//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
umat design_matrix_big(DataFrame x) {
  unsigned int i,n=x.length(),last=1,j,nrw;
  umat dm,F;
  dm=design_matrix_helper_big(x(0));
  nrw=dm.n_rows;
  F.reshape(nrw,dm.n_cols);
  for(j=1;j<dm.n_cols;++j)
    F.col(last++)=dm.col(j);
  for(i=1;i<n;++i){
    dm=design_matrix_helper_big(x(i));
    last=F.n_cols;
    F.reshape(nrw,last+dm.n_cols-1); //-1 giati afairo mia stili
    for(j=1;j<dm.n_cols;++j)
      F.col(last++)=dm.col(j);
  }
  F.col(0)=uvec(nrw,fill::ones);
  return F;
}

//the model.matrix form R for many columns
RcppExport SEXP Rfast_design_matrix_big(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< DataFrame >::type x(xSEXP);
    __result = wrap(design_matrix_big(x));
    return __result;
END_RCPP
}
