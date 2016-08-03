// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include <string>
#include <vector>
#include <iostream>
#include <cstring>
#include <cmath>
#include "g2t.h"
#include "mn.h"
using namespace std;
using namespace arma;
using namespace Rcpp;

//[[Rcpp::export]]
mat sort_mat_p(mat x,int ncores){
  int p=x.n_cols;
  #ifdef SUPPORT_OPENMP
  omp_set_dynamic(0); 
  omp_set_num_threads(ncores);
  #pragma omp parallel for
  #endif
  for(int i=0;i<p;++i)
    sort(x.begin_col(i),x.end_col(i));
  return x;
}

//[[Rcpp::export]]
vector<double> colmeds_p(mat x,int ncores){
  unsigned int i,p=x.n_cols,sz=x.n_rows,middle=sz/2-1;
  vector<double> F(p);
  #ifdef SUPPORT_OPENMP
  omp_set_dynamic(0); 
  omp_set_num_threads(ncores);
  #endif
  if(sz%2==0){
  	#ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(i=0;i<p;++i){
      nth_element(x.begin_col(i),x.begin_col(i)+middle,x.end_col(i));
      F[i]=(x(middle,i)+*(min_element(x.begin_col(i)+middle+1,x.end_col(i))))/2.0;
    }
  }else{
    #ifdef SUPPORT_OPENMP
    #pragma omp parallel for
    #endif 
    for(i=0;i<p;++i){
      nth_element(x.begin_col(i),x.begin_col(i)+middle+1,x.end_col(i));
      F[i]=x(middle+1,i);
    }
  }
  return F;
}

//[[Rcpp::export]]
vector<double> colmax_p(NumericMatrix x,int ncores,bool value=false){
  unsigned int i,p=x.ncol();
  mat X = mat(x.begin(), x.nrow(), p, false); 
  vector<double> F(p);
  #ifdef SUPPORT_OPENMP
  omp_set_dynamic(0); 
  omp_set_num_threads(ncores);
  #endif
  if(value){
    rowvec f=min(X,0);
    #ifdef SUPPORT_OPENMP
    #pragma omp parallel for
    #endif
    for(i=0;i<p;i++)
      F[i]=f(i);
  }else{
    #ifdef SUPPORT_OPENMP
    #pragma omp parallel for
    #endif
    for(i=0;i<p;++i)
      F[i]=max_element(X.begin_col(i),X.end_col(i))-X.begin_col(i)+1;
  }
  return F;
}

//[[Rcpp::export]]
vector<double> colmin_p(NumericMatrix x,int ncores,bool value=false){
  unsigned int i,p=x.ncol();
  mat X = mat(x.begin(), x.nrow(), p, false); 
  vector<double> F(p);
  #ifdef SUPPORT_OPENMP
  omp_set_dynamic(0); 
  omp_set_num_threads(ncores);
  #endif
  if(value){
    rowvec f=min(X,0);
    #ifdef SUPPORT_OPENMP
    #pragma omp parallel for
    #endif
    for(i=0;i<p;i++)
      F[i]=f(i);
  }else{
    #ifdef SUPPORT_OPENMP
    #pragma omp parallel for
    #endif
    for(i=0;i<p;++i)
      F[i]=min_element(X.begin_col(i),X.end_col(i))-X.begin_col(i)+1;
  }
  return F;
}
