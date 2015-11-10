#ifndef DPLYR_GLOBALSUBSTITUTE_H
#define DPLYR_GLOBALSUBSTITUTE_H

namespace dplyr {

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
    SEXP substitute_global(SEXP obj, int depth) ;

    // substutute column(x)
    SEXP substitute_column(SEXP obj, int depth) ;

    // substitute x$y
    SEXP substitute_dollar(SEXP obj, int depth) ;

    // traverse a LANGSXP
    SEXP traverse_call(SEXP obj, int depth) ;

    SEXP traverse(SEXP obj, int depth) ;

    bool in_data( SEXP s) ;

    void cache_indata_map() ;

  } ;

}

#endif
