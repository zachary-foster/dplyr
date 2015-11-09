#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

namespace dplyr {

  GlobalSubstitute::GlobalSubstitute( SEXP call_, const DataFrame& data, const Environment& env_ ) :
    call(call_),
    names(data.names()),
    env(env_),
    match("match")
  {
    call = traverse(call);
  }

  bool GlobalSubstitute::in_data( Symbol s){
    return as<int>(match(CharacterVector::create(PRINTNAME(s)), names)) != NA_INTEGER ;
  }

  SEXP GlobalSubstitute::traverse(SEXP obj){
    if(TYPEOF(obj) == LANGSXP){
      return traverse_call(obj) ;
    }
    return obj ;
  }

  SEXP GlobalSubstitute::substitute_dollar(SEXP obj){
    SEXP lhs = CADR(obj) ;
    SEXP rhs = CADDR(obj) ;

    if( TYPEOF(lhs) == LANGSXP ){
      SETCAR(CDR(obj), traverse_call(lhs)) ;
    }
    return obj ;
  }

  SEXP GlobalSubstitute::traverse_call(SEXP obj){
    if( CAR(obj) == Rf_install("global") ){
      return substitute_global(obj) ;
    }

    if( CAR(obj) == Rf_install("column")){
      return substitute_column(obj) ;
    }

    if( CAR(obj) == Rf_install("order_by") ){
      return obj ;
    }

    // if( CAR(obj) == R_DollarSymbol && TYPEOF(CADDR(obj)) == SYMSXP ){
    //   return substitute_dollar(obj) ;
    // }

    if( TYPEOF(CAR(obj)) == LANGSXP ){
      SETCAR(obj, traverse(CAR(obj))) ;
    }

    SEXP p = obj ;
    if( TYPEOF(CAR(p)) == SYMSXP ) p = CDR(p) ;
    while( !Rf_isNull(p) ){
      SETCAR(p, traverse(CAR(p))) ;
      p = CDR(p) ;
    }

    return obj ;
  }

  SEXP GlobalSubstitute::substitute_global( SEXP obj ){
    if(Rf_length(obj) != 2 || TYPEOF(CADR(obj)) != SYMSXP)
      stop("global only handles symbols") ;
    SEXP symb = CADR(obj) ;
    SEXP res  = env.find(CHAR(PRINTNAME(symb))) ;
    return res ;
  }

  SEXP GlobalSubstitute::substitute_column( SEXP obj ){
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
