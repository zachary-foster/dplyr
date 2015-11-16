#ifndef DPLYR_GLOBALSUBSTITUTE_H
#define DPLYR_GLOBALSUBSTITUTE_H

namespace dplyr {

  struct TraverseResult {
      SEXP result ;
      bool needs_data ;

      TraverseResult( SEXP result_, bool needs_data_ ) :
        result(result_), needs_data(needs_data_)
      {} ;
  } ;

  class GlobalSubstitute {
  public:

    GlobalSubstitute(
      SEXP call_,
      const DataFrame& data,
      const Environment& env_
    )  ;

    inline SEXP get() const {
      return call ;
    }

  private:
    RObject call ;
    CharacterVector variable_names ;
    Environment env ;
    Function match ;
    dplyr_hash_map<SEXP, bool> indata_map ;

    // substitute global(foo) with value of foo in the env
    TraverseResult substitute_global(SEXP obj, int depth) ;

    // substutute column(x)
    TraverseResult substitute_column(SEXP obj, int depth) ;

    // substitute x$y
    TraverseResult substitute_dollar(SEXP obj, int depth) ;

    // traverse a LANGSXP
    TraverseResult traverse_call(SEXP obj, int depth) ;

    TraverseResult traverse(SEXP obj, int depth) ;

    // traverse a function
    TraverseResult traverse_function( SEXP obj, int depth) ;

    // deal with order_by( ... )
    TraverseResult substitute_order_by( SEXP obj, int depth ) ;

    bool in_data( SEXP s) ;

    void cache_indata_map() ;

  } ;

}

#endif
