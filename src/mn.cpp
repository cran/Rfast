// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include <string>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

bool my_compare1(const pair<string,int>& a,const pair<string,int>& b){
  return a.first<b.first;
}

bool my_compare_order(const pair<double,int>& a,const pair<double,int>& b){
  return a.first<b.first;
}

bool descending(const double& a,const double& b){
  return a>b;
}

bool cor_vecs(const pair<double,double>& a,const pair<double,double>& b){
  return a.second<b.second;
}

bool s_indx_asc(const pair<int,double>& a,const pair<int,double>& b){
  return a.second<b.second;
}

bool s_indx_des(const pair<int,double>& a,const pair<int,double>& b){
  return a.second>b.second;
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

long double regression_only_col(colvec x, colvec y) {
  int n=x.size();
  double SSO=var(y)*(double)(n-1),SS1=0.0,F1=0.0;
  mat z(n,2),tr_z(2,n);
  colvec b(2);
  vec res(n);
  z.col(0)=ones(n);
  z.col(1)=x;
  tr_z=z.t();
  b=inv(tr_z*z)*tr_z*y;
  res=y-z*b;
  SS1=var(res)*(n-1);
  F1=(SSO/SS1-1)*(n-2);
  return F1;
}

double trigamma ( double x)
{
  using namespace std;
  double a = 0.0001;
  double b = 5.0;
  double b2 =  0.1666666667;
  double b4 = -0.03333333333;
  double b6 =  0.02380952381; 
  double b8 = -0.03333333333;
  double value;
  double y;
  double z;
  
  z = x;
  //
  //  Use small value approximation if X <= A.
  //
  if ( x <= a )
  {
    value = 1.0 / x / x;
    return value;
  }
  //
  //  Increase argument to ( X + I ) >= B.
  //
  value = 0.0;
  
  while ( z < b )
  {
    value = value + 1.0 / z / z;
    z = z + 1.0;
  }
  //
  //  Apply asymptotic formula if argument is B or greater.
  //
  y = 1.0 / z / z;
  
  value = value + 0.5 *
    y + ( 1.0
            + y * ( b2
                      + y * ( b4
                      + y * ( b6
                      + y *   b8 )))) / z;
                      
                      return value;
}


double digamma(double x) {
  double result = 0, xx, xx2, xx4;
  for ( ; x < 7; ++x)
    result -= 1/x;
  x -= 1.0/2.0;
  xx = 1.0/x;
  xx2 = xx*xx;
  xx4 = xx2*xx2;
  result += log(x)+(1./24.)*xx2-(7.0/960.0)*xx4+(31.0/8064.0)*xx4*xx2-(127.0/30720.0)*xx4*xx4;
  return result;
}

int i4_min ( double i1, double i2 ){ 
  if ( i1 < i2 )
    return i1;
  return i2;
}
  
void i4mat_floyd ( int n, vector<double> &a ){
  int i,j,k;
  const double i4_huge = 2147483647;
  for ( k = 0; k < n; k++ )
    for ( j = 0; j < n; j++ )
      if ( a[k+j*n] < i4_huge )
        for ( i = 0; i < n; i++ )
          if ( a[i+k*n] < i4_huge )
            a[i+j*n] = i4_min ( a[i+j*n], a[i+k*n] + a[k+j*n] );
}
