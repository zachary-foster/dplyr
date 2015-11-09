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
    CharacterVector names ;
    Environment env ;
    Function match ;

    // substitute global(foo) with value of foo in the env
    SEXP substitute_global(SEXP obj) ;

    // substutute column(x)
    SEXP substitute_column(SEXP obj) ;

    // substitute x$y
    SEXP substitute_dollar(SEXP obj) ;

    // traverse a LANGSXP
    SEXP traverse_call(SEXP obj) ;

    SEXP traverse(SEXP obj) ;

    bool in_data( Symbol s) ;

  } ;

}

#endif
