//Author: Manos Papadakis


#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
NumericVector lower_tri(NumericMatrix x,const bool dg){
  const int ncl=x.ncol(),nrw=x.nrow();
  int i,j;
  NumericVector f = (ncl<nrw) ? NumericVector(ncl*(nrw-1)*0.5 + (dg ? nrw : 0)) : 
                                NumericVector(nrw*(nrw-1)*0.5 + (dg ? nrw : 0));
  NumericVector::iterator ff=f.begin();
  if(dg){
    for(i=0;i<ncl;++i)
      for(j=i;j<nrw;++j,++ff)
        *ff=x(j,i);
  }else{
    for(i=0;i<ncl;++i)
      for(j=i+1;j<nrw;++j,++ff)
        *ff=x(j,i);
  }
  return f;
}


double sum_lower_tri(NumericMatrix x,const bool dg){
  const int ncl=x.ncol(),nrw=x.nrow();
  int i,j;
  double s=0.0;
  if(dg){
    for(i=0;i<ncl;++i)
      for(j=i;j<nrw;++j)
        s+=x(j,i);
  }else{
    for(i=0;i<ncl;++i)
      for(j=i+1;j<nrw;++j)
        s+=x(j,i);
  }
  return s;
}

LogicalMatrix lower_tri_b(const int nrw,const int ncl,const bool dg){
  int i,j;
  LogicalMatrix f(nrw,ncl);
  if(dg){
    for(i=0;i<ncl;++i)
      for(j=i;j<nrw;++j)
        f(j,i)=true;
  }else{
    for(i=0;i<ncl;++i)
      for(j=i+1;j<nrw;++j)
        f(j,i)=true;
  }
  return f;
}

RcppExport SEXP Rfast_lower_tri(SEXP xSEXP,SEXP dgSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type dg(dgSEXP);
    __result = wrap(lower_tri(x,dg));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_lower_tri_b(SEXP nclSEXP, SEXP nrwSEXP,SEXP dgSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type ncl(nclSEXP);
    traits::input_parameter< const int >::type nrw(nrwSEXP);
    traits::input_parameter< const bool >::type dg(dgSEXP);
    __result = wrap(lower_tri_b(nrw,ncl,dg));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_sum_lower_tri(SEXP xSEXP,SEXP dgSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type dg(dgSEXP);
    __result = wrap(sum_lower_tri(x,dg));
    return __result;
END_RCPP
}
