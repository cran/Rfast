// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

bool my_compare_order(const pr& a,const pr& b){
  return a.first<b.first;
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
  int i,n=x.n_elem;
  double t;
  colvec Y(n);
  for(i=0;i<n;i++){
    t=x(i);
    Y(i)=t*t;
  }
  return Y;
}

colvec operator-(colvec &x,colvec &y){
  int i,n=x.n_elem;
  colvec F(n);
  for(i=0;i<n;++i){
    F(i)=x(i)-y(i);
  }
  return F;
}

double operator*(vec x,vec y){
  int i,n=x.n_elem;
  double s=0.0;
  for(i=0;i<n;i++){
    s+=x(i)*y(i);
  }
  return s;
}

colvec operator+(colvec &x,colvec &y){
  int i,n=x.n_elem;
  colvec F(n);
  for(i=0;i<n;++i){
    F(i)=x(i)+y(i);
  }
  return F;
}

colvec operator^(const char a,const colvec y){
  int i,yrow=y.n_elem;
  colvec Y(yrow);
  for(i=0;i<yrow;i++)
      Y(i)=exp(y(i));
  return Y;
}

rowvec operator/(colvec x,double s){
  rowvec f(x.n_elem);
  for(rowvec::iterator ff=f.begin(),xx=x.begin();ff!=f.end();++ff,++xx)
    *ff=*xx/s;
  return f;
}

mat operator^(mat x,const int y){
  int n=x.n_rows,p=x.n_cols;
  mat Y(n,p);
  mat::iterator YY=Y.begin(),xx=x.begin();
  for(;YY!=Y.end();++YY,++xx)
    *YY=*xx**xx;
  return Y;
}

double regression_only_col(colvec x, colvec &y) {
  int n=x.size();
  double SSO=var(y)*(double)(n-1),SS1=0.0,F1=0.0;
  mat z(n,2,fill::ones),tr_z(2,n);
  colvec b(2);
  vec res(n);
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
	return i1 < i2 ? i1 : i2;
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

void min_max_d(double *start,double *end,double &min, double &max){
  double xxx;
  min=max=*start;
  start++;
  for(;start!=end;++start){
    xxx=*start;
    if(xxx>max)
      max=xxx;
    else if(xxx<min)
      min=xxx;
  }
}

void min_max_i(int *start,int *end,int &min, int &max){
  int xxx;
  min=max=*start;
  start++;
  for(;start!=end;++start){
    xxx=*start;
    if(xxx>max)
      max=xxx;
    else if(xxx<min)
      min=xxx;
  }
}

void max_d(double *start,double *end, double &mx){
  double xxx;
  mx=*start;
  start++;
  for(;start!=end;++start){
    xxx=*start;
    if(xxx>mx)
      mx=xxx;
  }
}

void max_i(int *start,int *end, int &mx){
  int xxx;
  mx=*start;
  start++;
  for(;start!=end;++start){
    xxx=*start;
    if(xxx>mx)
      mx=xxx;
  }
}

void min_d(double *start,double *end, double &mn){
  double xxx;
  mn=*start;
  start++;
  for(;start!=end;++start){
    xxx=*start;
    if(xxx<mn)
      mn=xxx;
  }
}

void min_i(int *start,int *end, int &mn){
  int xxx;
  mn=*start;
  start++;
  for(;start!=end;++start){
    xxx=*start;
    if(xxx<mn)
      mn=xxx;
  }
}

colvec Digamma_v(colvec x,int &p){
  double *start=x.memptr(),*end=start+p;
  for(;start!=end;++start)
    *start=digamma(*start);
  return x;
}

colvec Trigamma_v(colvec x,int &p){
  double *start=x.memptr(),*end=start+p;
  for(;start!=end;++start)
    *start=trigamma(*start);
  return x;
}

void fill_m(double *start,double *end,double v){
  for(;start!=end;++start)
    *start=v;
}

rowvec colMedians(mat x){
  int i,p=x.n_cols,sz=x.n_rows,middle=sz/2-1,step=sz;
  mat::iterator first=x.begin(),last=first+step;
  rowvec F(p);
  rowvec::iterator FF=F.begin();
  if(sz%2==0)
    for(i=0;i<p;++i,++FF,first=last,last+=step){
      nth_element(first,first+middle,last);
      *FF=(x(middle,i)+*(min_element(first+middle+1,last)))/2.0;
    }
  else 
    for(i=0;i<p;++i,++FF,first=last,last+=step){
      nth_element(first,first+middle+1,last);
      *FF=x(middle+1,i);
    }
  return F;
}

void combn(Rcpp::NumericVector& data, const int n,
       const int start_idx, std::vector<double>& combn_data,
       Rcpp::NumericMatrix& combn_dataset, int& combn_col) {
  if (!n) {
    for (size_t i = 0; i < combn_data.size(); ++i) {
      combn_dataset(i, combn_col) = combn_data[i];
    }
    combn_col++;
    return;
  }
  for (int i = start_idx; i <= (data.size() - n); ++i) {
    combn_data[combn_data.size() - n] = data[i];
    combn(data, n - 1, i + 1, combn_data, combn_dataset, combn_col);
  }
}
