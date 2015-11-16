#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

#define DBG_LEVEL 1

#if DBG_LEVEL > 0
  #define MSG(msg) for(int i=0; i<depth; i++) Rprintf( "  " ) ; Rprintf(msg) ;
  #define DBG(msg, obj) MSG(msg); Rprintf(" [%s] : ", type2name(obj) ) ; Rf_PrintValue(obj)
#else
  #define MSG(msg)
  #define DBG(msg,obj)
#endif

namespace dplyr {

  GlobalSubstitute::GlobalSubstitute( SEXP call_, const DataFrame& data, const Environment& env_ ) :
    call(call_),
    variable_names(data.names()),
    env(env_),
    match("match")
  {
    cache_indata_map() ;
    TraverseResult res = traverse(call, 0);
    call = res.result ;
  }

  void GlobalSubstitute::cache_indata_map(){
    int n = variable_names.size() ;
    for(int i=0; i<n; i++){
      indata_map[ Rf_install(variable_names[i]) ] = true ;
    }
  }

  bool GlobalSubstitute::in_data( SEXP s){
    bool res = false ;
    dplyr_hash_map<SEXP,bool>::const_iterator it = indata_map.find(s) ;
    if( it == indata_map.end() ){
        indata_map[s] = res = as<int>(match(CharacterVector::create(PRINTNAME(s)), variable_names)) != NA_INTEGER ;
    } else {
        res = it->second ;
    }
    return res ;
  }

  TraverseResult GlobalSubstitute::traverse_function( SEXP obj, int depth ){
    return TraverseResult( obj, false ) ;
  }

  TraverseResult GlobalSubstitute::traverse(SEXP obj, int depth){
    DBG("traverse. obj", obj) ;

    if(TYPEOF(obj) == LANGSXP){
      TraverseResult res = traverse_call(obj, depth + 1) ;
      if( !res.needs_data && TYPEOF(res.result) == LANGSXP && CAR(res.result) != Rf_install("~") ){
        res.result = Rcpp_eval(res.result, R_GlobalEnv) ;
      }
      return res ;
    } else if( TYPEOF(obj) == SYMSXP){
      bool needs_data = in_data(obj) ;
      if( needs_data ){
        return TraverseResult(obj, true) ;
      } else {
        return TraverseResult( Rf_findVar(obj, env), false ) ;
      }
    }
    return TraverseResult(obj, false) ;
  }

  TraverseResult GlobalSubstitute::substitute_order_by( SEXP obj, int depth){
    bool needs_data = false ;
    SEXP order_by = CADR(obj) ;
    TraverseResult order_by_processed = traverse(order_by, depth + 1) ;
    if( order_by_processed.needs_data )
      needs_data = true ;

    SEXP call = CADDR(obj) ;
    SEXP fun = CAR(call) ;
    SEXP args = CDR(call) ;
    RObject new_call = Rf_lang3( Rf_install("with_order"), order_by_processed.result, fun ) ;
    SETCDR( CDDR(new_call), args ) ;

    SEXP p = CDDDR(new_call) ;
    while( !Rf_isNull(p)){
      TraverseResult res = traverse( CAR(p), depth + 1 ) ;
      if( res.needs_data )
        needs_data = true ;
      SETCAR(p, res.result) ;

      p = CDR(p) ;
    }

    return TraverseResult( new_call, needs_data ) ;
  }

  TraverseResult GlobalSubstitute::substitute_dollar(SEXP obj, int depth){
    RObject lhs = CADR(obj) ;
    RObject rhs = CADDR(obj) ;
    DBG( "substitute_dollar", obj ) ;
    if( TYPEOF(lhs) == LANGSXP ){
      lhs = traverse_call(lhs, depth+1).result ;
    }
    if( TYPEOF(lhs) == SYMSXP ){
      if( TYPEOF(rhs) == SYMSXP ){
        if( !in_data(lhs) ){
          Language expr = Rf_lang3( R_DollarSymbol, lhs, rhs) ;
          return TraverseResult( expr.eval(env), false ) ;
        } else {
          return TraverseResult( obj, true) ;
        }
      }
    }
    Language expr = Rf_lang3( R_DollarSymbol, lhs, rhs) ;
    return TraverseResult( expr.eval(env), false ) ;
  }

  TraverseResult GlobalSubstitute::traverse_call(SEXP obj, int depth){
    DBG("traverse_call", obj) ;

    if( CAR(obj) == Rf_install("order_by") ){
      return substitute_order_by(obj, depth + 1) ;
    }

    if( CAR(obj) == Rf_install("function")) {
      return traverse_function(obj, depth) ;
    }

    if( CAR(obj) == Rf_install("global") ){
      return substitute_global(obj, depth+1) ;
    }

    if( CAR(obj) == Rf_install("column")){
      return substitute_column(obj, depth+1) ;
    }

    if( CAR(obj) == Rf_install("~") ){
      return TraverseResult(obj, false) ;
    }

    if( CAR(obj) == Rf_install("order_by") ){
      return TraverseResult( obj, true ) ;
    }

    if( CAR(obj) == R_DollarSymbol ){
      return substitute_dollar(obj, depth + 1)  ;
    }

    bool needs_data = false ;
    if( TYPEOF(CAR(obj)) == LANGSXP ){
      TraverseResult res = traverse(CAR(obj), depth + 1) ;
      if( res.needs_data ) needs_data = true ;
      SETCAR(obj, res.result) ;
    }

    SEXP p = obj ;
    if( TYPEOF(CAR(p)) == SYMSXP ) p = CDR(p) ;
    while( !Rf_isNull(p) ){
      TraverseResult res = traverse(CAR(p), depth + 1) ;
      if( res.needs_data ) needs_data = true ;
      SETCAR(p, res.result) ;
      p = CDR(p) ;
    }

    return TraverseResult(obj,needs_data) ;
  }

  TraverseResult GlobalSubstitute::substitute_global( SEXP obj, int depth ){
    if(Rf_length(obj) != 2 || TYPEOF(CADR(obj)) != SYMSXP)
      stop("global only handles symbols") ;
    SEXP symb = CADR(obj) ;
    SEXP res  = Rf_findVar( symb, env ) ;
    return TraverseResult(res, false) ;
  }

  TraverseResult GlobalSubstitute::substitute_column( SEXP obj, int depth ){
    if( Rf_length(obj) != 2)
      stop("unsupported form for column") ;

    SEXP arg = CADR(obj) ;
    RObject value ;

    // formula
    if( TYPEOF(arg) == LANGSXP && CAR(arg) == Rf_install("~") ){
      if( Rf_length(arg) != 2 || TYPEOF(CADR(arg)) != SYMSXP )
        stop("unhandled formula in column") ;
      value = CharacterVector::create( PRINTNAME(CADR(arg))) ;
    } else {
      value = Rcpp_eval(arg, env) ;
    }
    if( is<Symbol>(value) ){
      value = CharacterVector::create( PRINTNAME(value) ) ;
    }
    if( !is<String>(value)){
      stop("column must return a single string") ;
    }
    Symbol res(STRING_ELT(value, 0)) ;

    return TraverseResult(res, true) ;

  }

}


// [[Rcpp::export]]
SEXP global_subtitute(SEXP call, DataFrame df, Environment env){
    GlobalSubstitute gs(call, df, env) ;
    return gs.get() ;
}
