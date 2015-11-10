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
    call = traverse(call, 0);
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
    Rprintf( "in_data( %s ) = ", CHAR(PRINTNAME(s))) ;
    if( it == indata_map.end() ){
        Rprintf(" (new  ) ") ;
        indata_map[s] = res = as<int>(match(CharacterVector::create(PRINTNAME(s)), variable_names)) != NA_INTEGER ;
    } else {
        Rprintf(" (cached) ") ;
        res = it->second ;
    }

    Rprintf("%s\n", res ? "true" : "false") ;
    return res ;
  }

  SEXP GlobalSubstitute::traverse(SEXP obj, int depth){
    DBG("traverse. obj", obj) ;

    if(TYPEOF(obj) == LANGSXP){
      return traverse_call(obj, depth + 1) ;
    }
    return obj ;
  }

  SEXP GlobalSubstitute::substitute_dollar(SEXP obj, int depth){
    RObject lhs = CADR(obj) ;
    RObject rhs = CADDR(obj) ;
    DBG( "substitute_dollar", obj ) ;
    if( TYPEOF(lhs) == LANGSXP ){
      lhs = traverse_call(lhs, depth+1) ;
    }
    if( TYPEOF(lhs) == SYMSXP ){
      if( TYPEOF(rhs) == SYMSXP ){
        if( !in_data(lhs) ){
          Language expr = Rf_lang3( R_DollarSymbol, lhs, rhs) ;
          return expr.eval(env) ;
        }
      }
    } else {
      Language expr = Rf_lang3( R_DollarSymbol, lhs, rhs) ;
      return expr.eval(env);
    }
    Rf_PrintValue(obj) ;
    return obj ;
  }

  SEXP GlobalSubstitute::traverse_call(SEXP obj, int depth){
    DBG("traverse_call", obj) ;
    if( CAR(obj) == Rf_install("global") ){
      return substitute_global(obj, depth+1) ;
    }

    if( CAR(obj) == Rf_install("column")){
      return substitute_column(obj, depth+1) ;
    }

    if( CAR(obj) == Rf_install("order_by") ){
      return obj ;
    }

    if( CAR(obj) == R_DollarSymbol ){
      RObject out = substitute_dollar(obj, depth + 1)  ;
      MSG("length(out)="); Rprintf("%d, [%s]", Rf_length(out), type2name(out)) ;
      MSG( "substitute_dollar.out") ;
      Rf_PrintValue(out) ;
      return out ;
    }

    if( TYPEOF(CAR(obj)) == LANGSXP ){
      SETCAR(obj, traverse(CAR(obj), depth + 1)) ;
    }

    SEXP p = obj ;
    if( TYPEOF(CAR(p)) == SYMSXP ) p = CDR(p) ;
    while( !Rf_isNull(p) ){
      SETCAR(p, traverse(CAR(p), depth + 1)) ;
      p = CDR(p) ;
    }

    return obj ;
  }

  SEXP GlobalSubstitute::substitute_global( SEXP obj, int depth ){
    if(Rf_length(obj) != 2 || TYPEOF(CADR(obj)) != SYMSXP)
      stop("global only handles symbols") ;
    SEXP symb = CADR(obj) ;
    SEXP res  = env.find(CHAR(PRINTNAME(symb))) ;
    return res ;
  }

  SEXP GlobalSubstitute::substitute_column( SEXP obj, int depth ){
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

    return res ;

  }

}


// [[Rcpp::export]]
SEXP global_subtitute(SEXP call, DataFrame df, Environment env){
    GlobalSubstitute gs(call, df, env) ;
    return gs.get() ;
}
