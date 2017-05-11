//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix design_matrix(CharacterVector x,bool ones_c) {
  int i=0;
  const int n=x.size();
  CharacterVector tmp=sort_unique(x);
  CharacterVector::iterator xx=x.begin(),leksi_bg,leksi_en;
  IntegerMatrix Final(n,tmp.size());
  for(leksi_bg=tmp.begin(),leksi_en=tmp.end(),i=0;xx!=x.end();++xx,++i)
    Final(i,lower_bound(leksi_bg,leksi_en,*xx)-leksi_bg)=1;
  if(ones_c){
    IntegerVector ones(n,true);
    Final.column(0)=ones;
  }
  return Final;
}

//the model.matrix form R but by collumn
RcppExport SEXP Rfast_design_matrix(SEXP xSEXP,SEXP onesSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< CharacterVector >::type x(xSEXP);
    traits::input_parameter< bool >::type ones(onesSEXP);
    __result = wrap(design_matrix(x,ones));
    return __result;
END_RCPP
}
