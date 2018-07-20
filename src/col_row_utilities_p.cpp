
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include "templates.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

SEXP col_meds_helper_1_p(SEXP X){
  NumericMatrix x(X);
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP col_meds_helper_2_p(SEXP X){
  NumericMatrix x(X);
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0 * 1.482602;
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1] * 1.482602;
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP row_meds_helper_1_p(SEXP X){
  NumericMatrix x(X);
  const int p=x.nrow(),step=x.ncol(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP row_meds_helper_2_p(SEXP X){
  NumericMatrix x(X);
  const int p=x.nrow(),step=x.ncol(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0 * 1.482602;
    }
  }else{
#ifdef _OPENMP
#pragma omp parallel for
#endif
    for(int i=0;i<p;++i){
      rowvec tmp=xx.row(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1] * 1.482602;
    }
  }
  UNPROTECT(1);
  return F;
}

//[[Rcpp::export]]
SEXP col_mads_p(SEXP x){
  SEXP y=PROTECT(col_meds_helper_1_p(x));
  y=eachrow_min_abs(x,y);
  y=col_meds_helper_2_p(y);
  UNPROTECT(1);
  return y;
}

//[[Rcpp::export]]
SEXP row_mads_p(SEXP x){
  SEXP y=PROTECT(row_meds_helper_1_p(x));
  y=eachcol_min_abs(x,y);
  y=row_meds_helper_2_p(y);
  UNPROTECT(1);
  return y;
}

RcppExport SEXP Rfast_row_mads_p(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_mads_p(x);
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_col_mads_p(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_mads_p(x);
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////


SEXP col_meds_simple_p(NumericMatrix x){
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

SEXP col_meds_na_rm_p(NumericMatrix& x){
  const int p=x.ncol();
  int i;
  mat xx(x.begin(),x.nrow(),p,false);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif
  for(i=0;i<p;++i){
    colvec tmp=xx.col(i);
    FF[i]=med_helper<colvec>(tmp.begin(),tmp.begin()+(int)(std::remove_if(tmp.begin(),tmp.end(),R_IsNA)-tmp.begin()));
  }
  return F;
}

SEXP col_meds_p(NumericMatrix x,const bool na_rm){
  return na_rm ? col_meds_na_rm_p(x) : col_meds_simple_p(x);
}

// colMedians
RcppExport SEXP Rfast_col_meds_p(SEXP xSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = col_meds_p(x,na_rm);
    return __result;
END_RCPP
}

SEXP row_meds_p(NumericMatrix x){
  const int sz=x.ncol(),p=x.nrow(),middle=sz/2-1;
  mat xx(x.begin(),sz,p,false);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  if(sz%2==0){
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      rowvec rowi=xx.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      FF[i]=(rowi[middle]+*(min_element(rowi.begin()+middle+1,rowi.end())))/2.0;
    }
  }else{
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      rowvec rowi=xx.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      FF[i]=rowi[middle+1]/2.0;
    }
  }
  return F;
}

// rowMedians
RcppExport SEXP Rfast_row_meds_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = row_meds_p(x);
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////


using std::nth_element;

SEXP col_nth_p(NumericMatrix x,IntegerVector elems,const bool descend,const bool na_rm,const bool index){
  const int n=elems.size();
  mat xx(x.begin(),x.nrow(),n,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,n));
  double *FF=REAL(F);
  if(index){
    #ifdef _OPENMP
      #pragma omp parallel for
    #endif
    for(int i=0;i<n;++i){
      colvec y=xx.col(i);
      int elem=elems[i]-1;
      FF[i]=nth_helper_index<colvec>(y,elem,descend,na_rm);
    }
  }else{
    #ifdef _OPENMP
      #pragma omp parallel for
    #endif
    for(int i=0;i<n;++i){
      colvec y=xx.col(i);
      int elem=elems[i]-1;
      FF[i]=nth_helper<colvec>(y,elem,descend,na_rm);
    }
  }
  UNPROTECT(1);
  return F;
}

// nth_element
RcppExport SEXP Rfast_col_nth_p(SEXP xSEXP,SEXP ySEXP,SEXP descendSEXP,SEXP na_rmSEXP,SEXP indexSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type y(ySEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    traits::input_parameter< const bool >::type index(indexSEXP);
    __result = col_nth_p(x,y,descend,na_rm,index);
    return __result;
END_RCPP
}

SEXP row_nth_p(NumericMatrix x,IntegerVector elems,const bool descend,const bool na_rm,const bool index){
  const int n=elems.size();
  mat xx(x.begin(),n,x.ncol(),false);
  SEXP F;
  if(index){
    F=PROTECT(Rf_allocVector(INTSXP,n));
    int *FF=INTEGER(F);
    #ifdef _OPENMP
      #pragma omp parallel for
    #endif
    for(int i=0;i<n;++i){
      rowvec y=xx.row(i);
      int elem=elems[i]-1;
      FF[i]=nth_helper_index<rowvec>(y,elem,descend,na_rm);
    }
  }else{
    F=PROTECT(Rf_allocVector(REALSXP,n));
    double *FF=REAL(F);
    #ifdef _OPENMP
      #pragma omp parallel for
    #endif
    for(int i=0;i<n;++i){
      rowvec y=xx.row(i);
      int elem=elems[i]-1;
      FF[i]=nth_helper<rowvec>(y,elem,descend,na_rm);
    }
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_row_nth_p(SEXP xSEXP,SEXP ySEXP,SEXP descendSEXP,SEXP na_rmSEXP,SEXP indexSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type y(ySEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    traits::input_parameter< const bool >::type index(indexSEXP);
    __result = row_nth_p(x,y,descend,na_rm,index);
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////


IntegerMatrix col_order_p(NumericMatrix x,const bool stable,const bool descending){
  const int ncl=x.ncol(),nrw=x.nrow();
  IntegerMatrix f(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false);
  imat ff(f.begin(),nrw,ncl,false);
  #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<ncl;++i){
      ff.col(i)=Order<icolvec,colvec>(xx.col(i),stable,descending,1);
    }
    return f;
}

RcppExport SEXP Rfast_col_order_p(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    traits::input_parameter< const bool >::type descending(descendingSEXP);
    __result = wrap(col_order_p(x,stable,descending));
    return __result;
END_RCPP
}

IntegerMatrix row_order_p(NumericMatrix x,const bool stable,const bool descending){
  const int ncl=x.ncol(),nrw=x.nrow();
  IntegerMatrix f(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false);
  imat ff(f.begin(),nrw,ncl,false);
  #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<nrw;++i){
      ff.row(i)=Order<irowvec,rowvec>(xx.row(i),stable,descending,1);
    }
    return f;
}

RcppExport SEXP Rfast_row_order_p(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    traits::input_parameter< const bool >::type descending(descendingSEXP);
    __result = wrap(row_order_p(x,stable,descending));
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////////////////////

NumericMatrix col_ranks_p(NumericMatrix x,string method,const bool descend,const bool stable){
  const int ncl=x.ncol(),nrw=x.nrow();
  NumericMatrix f(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false);
  mat ff(f.begin(),nrw,ncl,false);
  if(method == "average"){
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<ncl;++i){
      ff.col(i)=rank_mean<colvec,colvec,ivec>(xx.col(i),descend);
  }
  }else if(method == "min"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif
    for(int i=0;i<ncl;++i){
      ff.col(i)=rank_min<colvec,colvec,ivec>(xx.col(i),descend);
  }
  }else if(method == "max"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif    
    for(int i=0;i<ncl;++i){
      ff.col(i)=rank_max<colvec,colvec,ivec>(xx.col(i),descend);
    }
  }else if(method == "first"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif    
    for(int i=0;i<ncl;++i){
      ff.col(i)=rank_first<colvec,colvec,ivec>(xx.col(i),descend,stable);
    }
  }else
    stop("Error. Wrong method.");  
  return f;
}


RcppExport SEXP Rfast_col_ranks_p(SEXP xSEXP,SEXP methodSEXP,SEXP descendSEXP,SEXP stableSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(col_ranks_p(x,method,descend,stable));
    return __result;
END_RCPP
}

NumericMatrix row_ranks_p(NumericMatrix x,string method,const bool descend,const bool stable){
  const int ncl=x.ncol(),nrw=x.nrow();
  NumericMatrix f(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false);
  mat ff(f.begin(),nrw,ncl,false);
  if(method == "average"){
    #ifdef _OPENMP
  #pragma omp parallel for
  #endif
    for(int i=0;i<nrw;++i){
      ff.row(i)=rank_mean<rowvec,rowvec,ivec>(xx.row(i),descend);
  }
  }else if(method == "min"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif
    for(int i=0;i<nrw;++i){
      ff.row(i)=rank_min<rowvec,rowvec,ivec>(xx.row(i),descend);
  }
  }else if(method == "max"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif    
    for(int i=0;i<nrw;++i){
      ff.row(i)=rank_max<rowvec,rowvec,ivec>(xx.row(i),descend);
    }
  }else if(method == "first"){
  #ifdef _OPENMP
  #pragma omp parallel for
  #endif    
    for(int i=0;i<nrw;++i){
      ff.row(i)=rank_first<rowvec,rowvec,ivec>(xx.row(i),descend,stable);
    }
  }else
    stop("Error. Wrong method.");  
  return f;
}

RcppExport SEXP Rfast_row_ranks_p(SEXP xSEXP,SEXP methodSEXP,SEXP descendSEXP,SEXP stableSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(row_ranks_p(x,method,descend,stable));
    return __result;
END_RCPP
}

///////////////////////////////////////////////////////////////////////////////////


SEXP col_sums_p(NumericMatrix x){
  const int n=x.ncol();
  SEXP F=PROTECT(Rf_allocVector(REALSXP,n));
  double *FF=REAL(F);
  mat xx(x.begin(),x.nrow(),n,false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;i++){
    FF[i]=accu(xx.col(i));
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_col_sums_p(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = col_sums_p(x);
    return __result;
END_RCPP
}

SEXP row_sums_p(NumericMatrix x){
  const int n=x.nrow();
  SEXP F=PROTECT(Rf_allocVector(REALSXP,n));
  double *FF=REAL(F);
  mat xx(x.begin(),n,x.ncol(),false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;i++){
    FF[i]=accu(xx.row(i));
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_row_sums_p(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = row_sums_p(x);
    return __result;
END_RCPP
}

///////////////////////////////////////////////////////////////////////////


NumericVector col_max_p(NumericMatrix x){
  const int n=x.ncol();
  mat xx(x.begin(),x.nrow(),n,false);
  NumericVector f(n);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
    f[i]=*max_element(xx.begin_col(i),xx.end_col(i));
  }
  return f;
}

RcppExport SEXP Rfast_col_max_p(SEXP nSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type n(nSEXP);
    __result = wrap(col_max_p(n));
    return __result;
END_RCPP
}

///////////////////////////////////////////////////////////////////////////


SEXP col_mean_p(NumericMatrix x){
  const int n=x.ncol();
  SEXP F=PROTECT(Rf_allocVector(REALSXP,n));
  double *FF=REAL(F);
  mat xx(x.begin(),x.nrow(),n,false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;i++){
    FF[i]=mean(xx.col(i));
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_col_mean_p(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(col_mean_p(x));
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////

using std::min_element;

NumericVector col_min_p(NumericMatrix x){
  const int n=x.ncol();
  mat xx(x.begin(),x.nrow(),n,false);
  NumericVector f(n);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
    f[i]=*min_element(xx.begin_col(i),xx.end_col(i));
  }
  return f;
}

RcppExport SEXP Rfast_col_min_p(SEXP nSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type n(nSEXP);
    __result = wrap(col_min_p(n));
    return __result;
END_RCPP
}

/////////////////////////////////////////////////////////////////////


SEXP col_all_p(LogicalMatrix x){
  const int n=x.ncol();
  SEXP f=PROTECT(Rf_allocVector(LGLSXP,n));
  imat xx(x.begin(),x.nrow(),n,false);
  int *ff=LOGICAL(x);
  #ifdef _OPENMP
    #pragma omp parallel for
    #endif
  for(int i=0;i<n;++i){
    ff[i]=all(xx.col(i));
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_col_all_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalMatrix >::type x(xSEXP);
    __result = col_all_p(x);
    return __result;
END_RCPP
}

SEXP row_all_p(LogicalMatrix x){
  const int n=x.nrow();
  SEXP f=PROTECT(Rf_allocVector(LGLSXP,n));
  imat xx(x.begin(),n,x.ncol(),false);
  int *ff=LOGICAL(x);
  #ifdef _OPENMP
    #pragma omp parallel for
    #endif
  for(int i=0;i<n;++i){
    ff[i]=all(xx.row(i));
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_row_all_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalMatrix >::type x(xSEXP);
    __result = row_all_p(x);
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////


IntegerVector col_count_values_p(NumericMatrix x,NumericVector values){
  const int n=values.size(),p=x.nrow();
  IntegerVector f(n);
  mat xx(x.begin(),p,n,false);
  ivec ff(f.begin(),n,false);
  colvec vv(values.begin(),n,false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
    ff[i]=count_value_helper<colvec,double>(xx.col(i),vv[i]);
  }
  return f;
}

RcppExport SEXP Rfast_col_count_values_p(SEXP xSEXP,SEXP valuesSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type values(valuesSEXP);
    __result = wrap(col_count_values_p(x,values));
    return __result;
END_RCPP
}


IntegerVector row_count_values_p(NumericMatrix x,NumericVector values){
  const int n=values.size(),p=x.nrow();
  IntegerVector f(n);
  mat xx(x.begin(),p,n,false);
  ivec ff(f.begin(),n,false);
  colvec vv(values.begin(),n,false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
    ff[i]=count_value_helper<rowvec,double>(xx.row(i),vv[i]);
  }
  return f;
}

RcppExport SEXP Rfast_row_count_values_p(SEXP xSEXP,SEXP valuesSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type values(valuesSEXP);
    __result = wrap(row_count_values_p(x,values));
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////////
