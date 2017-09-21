//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "reg_lib.h"
#include <iostream>
#include <algorithm>
#include "mn.h"

using namespace std;
using namespace arma;
using namespace Rcpp;

//[[Rcpp::plugins(openmp)]]

int increment_maybe(int value, double vec_i){
  return vec_i == 0 ? value : ( value +1 ) ;  
}

int get_max(NumericVector id){
  int max = 0, size = id.size();
  
  for(int i = 0; i < size;i++)
    if(id(i)>max)
      max = id(i);
    return max;
}

double vector_sum(NumericVector y){
  double sum = 0.0;
  int n = y.size();

  for(int i = 0; i < n; i++)
    sum+= y[i];
  return sum;
}

double getDeviance(int xRowSz, Rcpp::NumericVector y){
  double p = vector_sum(y)/xRowSz;
  
  return  -2 * vector_sum(y * log(p) + (1 - y) * log(1 - p));
}

double my_lchoose(int n, int k){
  return lgamma( n + 1 ) - lgamma(k+1) - lgamma( n - k + 1 );
}

mat bindColsToMat(vec a, vec *vecs, int vecsz, mat ret){
  
  for(int i = 0; i < vecsz; i++){
    ret.col(i) = vecs[i];
  }
  
  ret.col(vecsz) = a;
  
  return ret;
}

mat bindColsToMat2(int exept, vec *vecs, int vecsz, mat ret){
  for(int i = 0; i < vecsz; i++){
    if(i < exept)
      ret.col(i) = vecs[i];
    else if(i > exept)
      ret.col(i-1) = vecs[i];
  }
  return ret;
}

void initXcols(double* xidxs, int size, int parallel=1){
  #ifdef _OPENMP
    #pragma omp simd
  #endif
  for(int i = 0; i < size; i++) xidxs[i] = i;
}

double* removeIdx(int start, double *array, int size){
  if(start >= size/2 ){
    for(int i = start; i < size - 1; i++)
      array[i] = array[i+1];
    return array;
  }else{
    for(int i = start; i > 0; i--)
      array[i] = array[i-1];
    array = &array[1];
    return array;
  }
}

double* removeIdx_p(int start, double *array, int size){
  if(start >= size/2 ){
    vec tmpvec(size-start);
    double *tmp = &tmpvec[0];
    #ifdef _OPENMP
        #pragma omp parallel
    	{
    #endif
        #ifdef _OPENMP
            #pragma omp for
        #endif
        for(int i = start; i<size;i++)
          tmp[i-start] = array[i];
        #ifdef _OPENMP
            #pragma omp for
        #endif
        for(int i = start; i < size - 1; i++)
          array[i] = tmp[i+1-start];
    #ifdef _OPENMP
        }
    #endif
    
    return array;
  }else{
    vec tmpvec(start+1);
    double *tmp = &tmpvec[0];
 
    #ifdef _OPENMP
        #pragma omp parallel
    	{
    #endif
        #ifdef _OPENMP
            #pragma omp for
        #endif
        for(int i = 0; i<=start;i++)
          tmp[i] = array[i];
        #ifdef _OPENMP
            #pragma omp for
        #endif
        for(int i = start; i > 0; i--)
          array[i] = tmp[i-1];
    #ifdef _OPENMP
        }
    #endif
    
    array = &array[1];
    return array;
  }
}

double* removeXColumn(int idx, double *xidxs, int size, int parallel){
  // the vector x.column(idx) will always be located at xcols at an i such that i <= idx where xcols[i] = x.column(idx) 
  int start;
  if(idx > size - 1)
    start = size-1;
  else
    start = idx;
  
  for(int i = start; i > 0; i--)
    if(xidxs[i] == idx){
      start = i;
      break;
    }
    if(parallel)
      return removeIdx_p(start, xidxs, size);
    return removeIdx(start, xidxs, size);
}

double* removeDIdx(int start, double *array, int size){
  if(start >= size/2 ){
    for(int i = start; i < size - 1; i++)
      array[i] = array[i+1];
    return array;
  }else{
    for(int i = start; i > 0; i--)
      array[i] = array[i-1];
    array = &array[1];
    return array;
  }
}

vec* removeVecIdx(int start, vec *array, int size){
  if(start >= size/2 ){
    for(int i = start; i < size - 1; i++)
      array[i] = array[i+1];
    return array;
  }else{
    for(int i = start; i > 0; i--)
      array[i] = array[i-1];
    array = &array[1];
    return array;
  }
}

double calcylogy(vec y, int parallel){
  double ret = 0.0;
  int sz = y.size();
  if(parallel){
    #ifdef _OPENMP
        #pragma omp simd reduction(+:ret)
    #endif
    for(int i = 0; i< sz; i++)
      if(y[i]>0)
        ret+=y[i]*log(y[i]);
  }
  else{
    for(int i = 0; i< sz; i++)
      if(y[i]>0)
        ret+=y[i]*log(y[i]);
  }
  return ret;
}

double calc_f(vec nix, double n, vec ni2hi2, double S, double x, int size){
  double sum1 = 0.0, sum2 = 0.0;

  for(int i = 0; i < size; i++){
    sum1+=log1p(nix[i]);
    sum2+=ni2hi2[i]/(1+nix[i]);
  }
  
  return sum1+n*log(S-x*sum2);
}

colvec log1pColvec(colvec vec, int parallel = 1){
  if(parallel){
    #ifdef _OPENMP
        #pragma omp simd
    #endif
    for(unsigned int i = 0; i < vec.size(); i++)
      vec[i] = log1p(vec[i]);
  }else{
    for(unsigned int i = 0; i < vec.size(); i++)
      vec[i] = log1p(vec[i]);
  }
  return vec;
}

vec colsumsVec(mat x, int parallel){
  const int n=x.n_cols;
  vec f(n);
  #ifdef _OPENMP
        #pragma omp parallel for if(parallel)
  #endif
  for(int i=0;i<n;i++){
    f[i]=sum(x.col(i));
  }
  return f;
}

vec rowsumsVec(mat x){
  const int n=x.n_rows;
  vec f(n);

  for(int i=0;i<n;i++){
    f[i]=sum(x.row(i));
  }
  return f;
}

vec indexesOfNum(mat m, int num){
  int sz = m.n_rows * m.n_cols;
  vec tmp(sz);
  int i,j = 0;
  for(i=0; i<sz;i++)
    if(m(i)==num)
      tmp(j++)=i;
  
  vec ret(tmp.begin(),j,true,true);
  
  return ret;
}

double sumabsmat(mat b2,mat b1, int parallel){
  double sum = 0.0;
  int n = b2.n_rows,d = b2.n_cols;
  #ifdef _OPENMP
        #pragma omp parallel if(parallel)
        {
  #endif
          double subtr,privsum = 0.0;
    #ifdef _OPENMP
        #pragma omp for
    #endif
    for(int i = 0;i<d;i++){
      for(int j = 0; j < n; j++){
        subtr = b1(j,i)-b2(j,i);
        if(subtr < 0)
          privsum+=-subtr;
        else
          privsum+=subtr;
      }
    }
    #ifdef _OPENMP
        #pragma omp critical
    	{
    #endif
      sum+=privsum;
    #ifdef _OPENMP
        }
        }
    #endif
  return sum;
}

vec colmeansVec(mat x, int parallel){
  const int n=x.n_cols, divider = x.n_rows;
  vec f(n);
  #ifdef _OPENMP
        #pragma omp parallel for if(parallel)
  #endif
  for(int i=0;i<n;i++){
    f[i]=sum(x.col(i))/divider;
  }
  return f;
}

vec colsumsVecminVec(mat a, mat b, int parallel){
  //equal to f = colsums(a)-colsums(b)
  const int n=a.n_cols;
  vec f(n);
  #ifdef _OPENMP
        #pragma omp parallel for if(parallel)
  #endif
  for(int i=0;i<n;i++){
    f[i]=sum(a.col(i)-b.col(i));
  }
  return f;
}

mat create_id_mat(int d){
  mat ret(2,d);
  ret(0,0) = 0;
  ret(1,0) = 1;
  
  for(int i=1;i<d;i++){
    ret(0,i) = ret(0,i-1)+2;
    ret(1,i) = ret(1,i-1)+2;
  }
  return ret;
}

double calc_multinom_ini(mat Y1,vec m0){
  double ret = 0.0;
  int n = Y1.n_rows;
  rowvec logm0(m0.begin(),m0.size(),false);
  logm0 = log(logm0);
  for(int i = 0;i<n;i++){
    ret+=sum(Y1.row(i)%logm0);
  }
 
 return 2*ret;
}

mat removefirstcol(mat x){
  int n = x.n_rows,d = x.n_cols;
  mat ret(n,d-1);
  for(int i = 0; i < d-1;i++)
    ret.col(i) = x.col(i+1);
  return ret;
}

double calcSumLog(mat ma, vec poia){
  double ret = 0.0;
  int sz = poia.size();
  for(int i=0; i < sz;i++){
    ret+=sum(log(ma(poia(i))));
  }
  return ret;
}

mat safe_exp_m1(mat m, double expLimit, int limit){
  int sz = m.n_cols*m.n_rows;
  for(int i = 0; i < sz; i++)
    if(m(i)>limit)
      m(i) = expLimit;
    else
      m(i) = exp(m(i));
  return m;
}

mat colvec_mat_cbind(vec v, mat m){
  int n = m.n_rows, d = m.n_cols;
  mat ret(n,d+1);
  ret.col(0)= v;
  for(int i =1; i<d+1;i++)
    ret.col(i) = m.col(i-1);
  
  return ret;
}

double calcDevRes(mat p,vec y,mat est){
  int psize = p.n_rows;
  double summ =0.0;
  
  for(int i=0;i<psize;i++){
    if(y(i)==1){
      if(p(i,0) == 0){
        summ += est(i,0);
      }
      else{
        summ+=log(p(i,0));
      }
    }
    else{
      if(p(i,0) == 1){
        summ+= est(i,0);
      }
      else{
        summ+=log(1-p(i,0));
      }
    }
  }
  
  return summ;
}

mat design_matrix2(CharacterVector x,bool ones_c) {
  //Author: Manos Papadakis
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
  NumericMatrix final2 = as<NumericMatrix>(wrap(Final));
  mat ret(final2.begin(),final2.nrow(),final2.ncol(),false);
  return ret;
}

mat cross_x_y_2(mat x,mat y){
  //Author: Manos Papadakis
  const int ncl=x.n_cols,nrw=x.n_rows,p=y.n_cols;
  
  mat f(ncl,p);
  colvec yv(nrw);
  int i,j;
  for(i=0;i<p;++i){
    yv=y.col(i);
    for(j=0;j<ncl;++j){
      f(j,i)=sum(x.col(j)%yv);
    }
  }
  return f;
}

mat cross_x_2(mat xx){
  //Author: Manos Papadakis
  const int ncl=xx.n_cols,nrw=xx.n_rows;

  mat f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl;++i){
    xv=xx.col(i);
    for(j=i;j<ncl;++j){
      a=sum(xx.col(j)%xv);
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

vec group_sum2(vec x, NumericVector f){
  //Author: Manos Papadakis
  std::vector<double> tmpvec(10) ;
  tmpvec.reserve(1000); 
  const int n=x.size();
  unsigned int index,vecsz=tmpvec.size();
  for( int i=0; i<n; i++){
    index=f[i]; 
    while( index >= vecsz ){
      tmpvec.resize( vecsz * 2 ) ; 
      vecsz=tmpvec.size();
    }
    tmpvec[ index ] += x[i] ;
  }
  // count the number of non zeros
  int s = std::accumulate( tmpvec.begin(), tmpvec.end(), 0, increment_maybe) ; 
  vec result(s);
  
  std::vector<double>::iterator it = tmpvec.begin() ;
  for( int i=0, j=0 ; j<s; j++ ,++it, ++i ){
    // move until the next non zero value
    while( ! *it ){ 
      ++i ; 
      ++it ;
    }
    result(j) = *it;
  }
  
  return result;
}

vec group_sum3(vec x, vec f){
  //Author: Manos Papadakis
  std::vector<double> tmpvec(10) ;
  tmpvec.reserve(1000); 
  const int n=x.size();
  
  double index,vecsz=tmpvec.size();
  for( int i=0; i<n; i++){
    index=f[i]; 
    while( index >= vecsz ){
      tmpvec.resize( vecsz * 2 ) ; 
      vecsz=tmpvec.size();
    }
    tmpvec[ index ] += x[i] ;
  }
  // count the number of non zeros
  int s = std::accumulate( tmpvec.begin(), tmpvec.end(), 0, increment_maybe) ; 
  vec result(s);
  
  std::vector<double>::iterator it = tmpvec.begin() ;
  for( int i=0, j=0 ; j<s; j++ ,++it, ++i ){
    // move until the next non zero value
    while( ! *it ){ 
      ++i ; 
      ++it ;
    }
    result(j) = *it;
  }
  
  return result;
}
