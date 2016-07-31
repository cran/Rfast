// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include <string>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

bool my_compare(const pair<string,int>& a,const pair<string,int>& b){
  return a.first<b.first;
}

mat operator+(colvec &y,mat &x){
  int i,j,d=x.n_cols,n=x.n_rows,a;
  mat Y(n,d);
  for(i=0;i<n;i++){
    a=y(i);
    for(j=0;j<d;j++)
      Y(i,j)=x(i,j)+a;
  }
  return Y;
}

colvec operator^(colvec x,const int y){
  int i,n=x.n_rows;
  double t;
  colvec Y(n);
  for(i=0;i<n;i++){
    t=x(i);
    Y(i)=t*t;
  }
  return Y;
}

colvec operator-(colvec &x,colvec &y){
  int i,n=x.n_rows;
  colvec F(n);
  for(i=0;i<n;i+=2){
    F(i)=x(i)-y(i);F(i+1)=x(i+1)-y(i+1);
  }
  return F;
}

double operator*(vec x,vec y){
  int i,n=x.n_rows;
  double s=0.0;
  for(i=0;i<n;i++){
    s+=x(i)*y(i);
  }
  return s;
}

colvec operator+(colvec &x,colvec &y){
  int i,n=x.n_rows;
  colvec F(n);
  for(i=0;i<n;i+=2){
    F(i)=x(i)+y(i);F(i+1)=x(i+1)+y(i+1);
  }
  return F;
}

colvec operator^(const char a,const colvec y){
  int i,yrow=y.n_rows;
  colvec Y(yrow);
  for(i=0;i<yrow;i++)
      Y(i)=exp(y(i));
  return Y;
}

vec loga(vec &x){
  int n=x.size();
  vec F(n);
  for(int i=0;i<n;i++)
    F(i)=log(x(i));
  return F;
}

//[[Rcpp::export]]
vec regression_only(mat x, colvec y) {
  int n=x.n_rows,p=x.n_cols,i,j;
  double SSO=var(y)*(double)(n-1),SS1=0.0;
  mat b(2,1),z(n,2),tr_z(2,n);
  vec F1(p),res(n);
  for(j=0;j<n;j++){
    z(j,0)=1;
  }
  for(i=0;i<p;i++){
    for(j=0;j<n;j++){
      z(j,1)=x(j,i);
    }
    tr_z=trans(z);
    b=inv(tr_z*z)*tr_z*y;
    res=y-z*b;
    SS1=var(res)*(n-1);
    F1(i)=(SSO/SS1-1)*(n-2);
  }
  return F1;
}

long double regression_only_col(colvec x, colvec y) {
  unsigned int n=x.size(),j;
  double SSO=var(y)*(double)(n-1),SS1=0.0;
  long double F1=0.0;
  mat z(n,2),tr_z(2,n);
  colvec b(2);
  vec res(n);
  for(j=0;j<n;j++){
    z(j,0)=1;
    z(j,1)=x(j);
  }
  tr_z=z.t();
  b=inv(tr_z*z)*tr_z*y;
  res=y-z*b;
  SS1=var(res)*(n-1);
  F1=(SSO/SS1-1)*(n-2);
  return F1;
}

unsigned int generatekey2(const string &s,unsigned int seed = 0){
  unsigned int hash = seed;
  const char *ss=s.c_str();
  while (*ss)
    hash = hash * 101 + *ss++;
  return hash;
}

/* Fowler / Noll / Vo (FNV) Hash */
size_t generatekey1(const string &s)
{
  size_t hash = InitialFNV;
  for(size_t i = 0; i < s.length(); i++){
    hash = hash ^ (s[i]);       /* xor  the low 8 bits */
    hash = hash * FNVMultiple;  /* multiply by the magic number */
  }
  return hash;
}
