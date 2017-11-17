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

double vmf_mle2(double nR, const int n, const double tol, const double maxiters){
  double apk, k2, R = nR/n, R2 = R*R, k1 = R * (2 - R2)/(1 - R2);

  int i = 2;
  if(k1 < 1e+05){
    apk = R::bessel_i(k1,1, 1)/R::bessel_i(k1,0, 1);
    k2 = k1 - (apk - R)/(1 - apk*apk - 1/k1 * apk);
    while (i++<maxiters && abs(k2 - k1) > tol) {
      k1 = k2;
      apk = R::bessel_i(k1,1,1)/R::bessel_i(k1,0,1);
      k2 = k1 - (apk - R)/(1 - apk*apk - 1/k1 * apk);
    }
    return k2;
  }

  return k1;
}

vec my_pow2(vec inp,vec out,const double power,const int sz){
  for(double *startx=&inp[0],*starty=&out[0],*end=startx+sz;startx!=end;++startx,++starty)
    *starty=std::pow(*startx,power);

  return out;
}

double getDeviance(int xRowSz, vec y){
  double p = sum(y)/xRowSz;

  return  -2 * sum(y * log(p) + (1 - y) * log(1 - p));
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

void initXcols(double* xidxs, int size){
#ifdef _OPENMP
#pragma omp simd
#endif
  for(int i = 0; i < size; i++)
    xidxs[i] = i;
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

double* removeXColumn(int idx, double *xidxs, int size){
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

double calcylogy(vec y, int sz){
  double ret = 0.0;
  vec::iterator it, end = y.end();
  for(it = y.begin(); it != end; it++)
    if(*it>0)
      ret+=(*it)*log(*it);
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

double log1pColvecSum(colvec input, int sz){
  double ret = 0.0;
  colvec::iterator iter1 = input.begin(), end = input.end();

  for(; iter1 != end; ++iter1)
    ret+= log1p(*iter1);

  return ret;
}

colvec log1pColvec(colvec input, int sz){
  colvec ret(sz);
  colvec::iterator iter1 = input.begin(),iter2 = ret.begin(), end = input.end();

  for(; iter1 != end; ++iter1,++iter2)
    *iter2 = log1p(*iter1);

  return ret;
}

vec indexesOfNum(mat m, int num){
  int sz = m.n_cols*m.n_rows;
  vec tmp(sz);
  int i,j = 0;

  for(i=0; i<sz;i++)
    if(m(i)==num)
      tmp(j++)=i;

    tmp.resize(j);

    return tmp;
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
#endif
#ifdef _OPENMP
}
#endif
return sum;
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
  int n=Y1.n_rows;
  rowvec logm0 = conv_to<rowvec>::from(log(m0));
  for(int i = 0;i<n;i++){
    ret+=sum(Y1.row(i)%logm0);
  }

  return 2*ret;
}

double calcSumLog(mat ma, vec poia, int sz){
  double ret = 0.0;
  for(int i=0; i < sz;i++){
    ret+=sum(log(ma(poia(i))));
  }
  return ret;
}

mat colvec_mat_cbind(vec v, mat m){
  int n = m.n_rows,d=m.n_cols;
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

vec subvec(vec v, int start, int size){
  vec ret(size);
  for(int i=start,j = 0;i<size+start;i++,j++)
    ret(j) = v(i);
  return ret;
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
  const int ncl=x.n_cols,p=y.n_cols;

  mat f(ncl,p);
  int i,j;
  for(i=0;i<p;++i)
    for(j=0;j<ncl;++j){
      f(j,i)=sum(x.col(j)%y.col(i));
    }

    return f;
}

mat cross_x_2(mat xx){
  //Author: Manos Papadakis
  const int ncl=xx.n_cols;

  mat f(ncl,ncl);
  double a;
  int i,j;
  for(i=0;i<ncl;++i)
    for(j=i;j<ncl;++j){
      a=sum(xx.col(j)%xx.col(i));
      f(i,j)=a;
      f(j,i)=a;
    }

    return f;
}

vec group_sum2(vec x,NumericVector key,const int n){
  NumericVector::iterator kk=key.begin();
  vec f(n,fill::zeros);
  vec::iterator xx=x.begin(),ff=f.begin(),rr;
  for(;xx!=x.end();++xx,++kk){
    f[*kk-1]+=*xx;
  }
  int count_not_zero=0;
  for(;ff!=f.end();++ff){
    if(*ff!=0)
      ++count_not_zero;
  }
  vec res(count_not_zero);
  for(rr=res.begin(),ff=f.begin();ff!=f.end();++ff){
    if(*ff!=0)
      *rr++=*ff;
  }
  return res;
}

vec group_sum3(vec x,vec key,const int n){
  vec::iterator kk=key.begin();
  vec f(n,fill::zeros);
  vec::iterator xx=x.begin(),ff=f.begin(),rr;
  for(;xx!=x.end();++xx,++kk){
    f[*kk-1]+=*xx;
  }
  int count_not_zero=0;
  for(;ff!=f.end();++ff){
    if(*ff!=0)
      ++count_not_zero;
  }
  vec res(count_not_zero);
  for(rr=res.begin(),ff=f.begin();ff!=f.end();++ff){
    if(*ff!=0)
      *rr++=*ff;
  }
  return res;
}

double varcomps_mle2(vec x, vec ina,const int n,const double tol) {
  //Author: Manos Papadakis
  const int N=x.size(),d=N/n;
  vec y=abs(x-mean(x)),syina=group_sum3(y,ina,n);
  double sy2=sum(sqrt(syina)), a=0,ratio=2.0/(sqrt(5) + 1),sy=sum(sqrt(y)),b=sy/N,s=b;
  double x1=b-ratio*b,x2=ratio*b;
  double se=s-x1;
  double f1=N*log(se)+n*log1p(d*x1/se)+sy/se-x1/(se*se+d*x1*se)*sy2;
  se=s-x2;
  double f2=N*log(se)+n*log1p(d*x2/se)+sy/se-x2/(se*se+d*x2*se)*sy2;

  while (abs(b-a)>tol){
    if(f2>f1){
      b=x2;
      x2=x1;
      f2=f1;
      x1=b - ratio * (b - a);
      se=s - x1;
      f1=N * log(se) + n * log1p(d * x1 / se) + sy/se - x1 / (se*se + d * x1 * se) * sy2 ;
    } else {
      a=x1;
      x1=x2;
      f1=f2;
      x2=a + ratio * (b - a);
      se=s - x2;
      f2=N * log(se) + n * log1p(d * x2 / se) + sy/se - x2 / (se*se + d * x2 * se) * sy2;
    }
  }

  return -0.5 * f2 - N*0.5 * 1.837877;
}

mat varcomps_mle3(vec x, vec ina,const int N, const int n,const bool ranef, const double tol,const int maxiters) {
  const int d=N/n;
  int i = 2;
  vec y=abs(x-mean(x)),syina=group_sum3(y,ina,n);

  double sy2=sum(sqrt(syina)), a=0,ratio=2.0/(sqrt(5) + 1),sy=sum(sqrt(y)),b=sy/N,s=b;
  double x1=b-ratio*b,x2=ratio*b;
  double se=s-x1;
  double f1=N*log(se)+n*log1p(d*x1/se)+sy/se-x1/(se*se+d*x1*se)*sy2;
  se=s-x2;
  double f2=N*log(se)+n*log1p(d*x2/se)+sy/se-x2/(se*se+d*x2*se)*sy2;

  while (i++ < maxiters && abs(b-a)>tol){
    if(f2>f1){
      b=x2;
      x2=x1;
      f2=f1;
      x1=b - ratio * (b - a);
      se=s - x1;
      f1=N * log(se) + n * log1p(d * x1 / se) + sy/se - x1 / (se*se + d * x1 * se) * sy2 ;
    } else {
      a=x1;
      x1=x2;
      f1=f2;
      x2=a + ratio * (b - a);
      se=s - x2;
      f2=N * log(se) + n * log1p(d * x2 / se) + sy/se - x2 / (se*se + d * x2 * se) * sy2;
    }
  }

  mat ret;
  if(ranef){
    if(n>4)
      ret = mat(2,n);
    else
      ret = mat(2,4);
  }
  else
    ret = mat(1,4);

  const double tau=(a+b)/2.0;

  ret(0,0) = i-1;
  ret(0,2) = tau;
  ret(0,1) = s - tau;
  ret(0,3) = -0.5 * (f2 + N * 1.837877);


  if(ranef){
    ret.row(1) = conv_to<rowvec>::from(ret(0,2)/(ret(0,2) + ret(0,1)/d) * syina/d);
  }

  return ret;
}

double spml_mle2(mat u, vec ci2, vec cisi, vec si2, const int n, const double tol, const int maxiters){
  vec su(2);
  su(0) = sum(u.col(0)), su(1) = sum(u.col(1));

  double nR = sqrt(sum(su%su)), kappa = vmf_mle2(nR, n, tol, maxiters);

  vec mu = su*(1/nR);

  vec mu1 = mu*kappa;
  double f = -0.5, con = 2.506628274631;
  vec tau = u*mu1, ptau = pnormc(tau);

  vec rat = ptau/(exp(f * tau%tau)/con + tau % ptau);

  vec psit = tau + rat;
  vec psit2 = 2 - rat%(tau + rat);

  vec der(2);
  der[0] = sum(u.col(0)%psit) - n * mu1[0];
  der[1] = sum(u.col(1)%psit) - n * mu1[1];

  double dera = der[0],derb = der[1],dera2 = sum(psit2%ci2)-n,derab = sum(psit2%cisi),derb2 = sum(psit2%si2)-n;

  double down = dera2 * derb2 - derab*derab;
  vec mu2(2);
  mu2[0] = mu1[0] - (derb2 * dera - derab * derb)/down;
  mu2[1] = mu1[1] - (-derab * dera + dera2 * derb)/down;

  int i = 2;
  while (i++<maxiters && sum(abs(mu2 - mu1)) > tol) {
    mu1 = mu2;
    tau = u*mu1;
    ptau = pnormc(tau);
    rat = ptau/(exp(f * (tau%tau))/con + tau % ptau);
    psit = tau + rat;
    psit2 = 2 - rat%(tau + rat);
    der[0] = sum(u.col(0)%psit) - n * mu1[0];
    der[1] = sum(u.col(1)%psit) - n * mu1[1];
    dera = der[0],derb = der[1],dera2 = sum(psit2%ci2)-n,derab = sum(psit2%cisi),derb2 = sum(psit2%si2)-n;
    down = dera2 * derb2 - derab*derab;
    mu2[0] = mu1[0] - (derb2 * dera - derab * derb)/down;
    mu2[1] = mu1[1] - (-derab * dera + dera2 * derb)/down;

  }

  return -0.5 * n * (mu2[0]*mu2[0]+mu2[1]*mu2[1]) + log1pColvecSum(((tau % ptau) * con)/exp(f*tau%tau),n) - n * 1.83787706640935;
}

vec weibull_mle2(vec x, int n, const double tol, const int maxiters){
  int i=2;
  vec lx = log(x),lx2 = lx%lx, y = x;
  double mlx = sum(lx)/n, co = sum(y%lx),sy = sum(y), fb = 1+mlx-co/sy,fb2 = -1 -(sum(y%lx2)*sy-co*co)/(sy*sy);
  double b1 = 1,b2 = 1 - fb/fb2;

  while (++i<maxiters && sum(abs(b2 - b1)) > tol) {
    b1 = b2;
    y = my_pow2(x,y,b1,n);
    co = sum(y % lx);
    sy = sum(y);
    fb = 1/b1 + mlx - co/sy;
    fb2 = -1/(b1*b1) - (sum(y % lx2) * sy - co*co)/(sy*sy);
    b2 = b1 - fb/fb2;
  }

  vec param(2);
  param[0] = b2;
  param[1] = pow(sy/n,1/b2);

  return param;
}
