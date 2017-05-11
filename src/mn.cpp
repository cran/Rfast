// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <vector>
#include <string>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

bool my_compare_order(const pr& a,const pr& b){
  return a.first<b.first;
}

bool descending_int(const int& a,const int& b){
  return a<b;
}

bool descending_double(const double& a,const double& b){
  return a<b;
}

bool descending_string(const string& a,const string& b){
  return a<b;
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

mat sqrt_mat(mat x){
  colvec f(x.n_elem);
  for(double *start=&x[0],*startf=&f[0],*end=&(*x.end());start!=end;++start,++startf){
    *startf=std::sqrt(*start);    
  }
  return f;
}

void fill_with_log(double *start,double *end,double *startf){
  for(;start!=end;++start,++startf)
    *startf=std::log(*start);
}

void max_neg_pos(int* start, int *end,int &mx,int &mn,int &pos){
  mn=mx=*start;
  int x;
  for(;start!=end;++start){
    x=*start;
    if(x<0){
      if(mn>x)
        mn=x;
    }else{ 
      pos++;
      if(mx<x)
        mx=x;
    }
  }
}

uvec Order_rmdp(colvec& x){
  const int n=x.size();
  int i=0;
  pr *y=new pr[n];
  uvec f(n);
  uvec::iterator b=f.begin();
  colvec::iterator xx=x.begin();
  for(;i!=n;++i,++xx){
    y[i].first=*xx;
    y[i].second=i;
  }
  stable_sort(y,y+n,my_compare_order);
  for(i=0;i!=n;++i,++b)
    *b=y[i].second;
  return f;
}

rowvec colvar_rmdp(mat& x){
  rowvec nyr1=x.row(0),nyr2=x.row(1);
  return 0.5*(square(nyr1) + square(nyr2)) - nyr1%nyr2;
}

double sum_pow(colvec x,const double p){
  const int sz=x.size();
  double s=0;
  for(double *startx=&x[0],*end=startx+sz;startx!=end;++startx)
    s+=std::pow(*startx,p);
  return s;
}

NumericVector Tabulate(NumericVector x,int &nroww){
  int aa;
  NumericVector f(nroww);
  NumericVector::iterator F=f.begin();
  NumericVector::iterator a=x.begin();
  for(;a!=x.end();++a){
    aa=*a;
    F[aa-1]++;
  }
  return f;
}

NumericMatrix design_matrix_regr(CharacterVector x) {
  int i=0;
  const int n=x.size();
  CharacterVector tmp=sort_unique(x);
  CharacterVector::iterator xx=x.begin(),leksi_bg,leksi_en;
  NumericMatrix Final(n,tmp.size());
  for(leksi_bg=tmp.begin(),leksi_en=tmp.end(),i=0;xx!=x.end();++xx,++i)
    Final(i,lower_bound(leksi_bg,leksi_en,*xx)-leksi_bg)=1;
  return Final;
}

umat design_matrix_helper_big(CharacterVector x) {
  int i=0;
  const int n=x.size();
  CharacterVector tmp=sort_unique(x);
  CharacterVector::iterator xx=x.begin(),leksi_bg,leksi_en;
  umat Final(n,tmp.size());
  for(leksi_bg=tmp.begin(),leksi_en=tmp.end(),i=0;xx!=x.end();++xx,++i)
    Final(i,lower_bound(leksi_bg,leksi_en,*xx)-leksi_bg)=1;
  return Final;
}

colvec my_pow(colvec x,const double p){
  const int sz=x.size();
  for(double *startx=&x[0],*end=startx+sz;startx!=end;++startx)
    *startx=std::pow(*startx,p);
  return x;
}

NumericVector minus_mean(NumericVector& x,const double k){
  NumericVector y(x.size());
  double v;
  for(NumericVector::iterator xx=x.begin(),yy=y.begin();x.end()-xx;++xx,++yy){
    v=*xx;
    *yy=v-k;
  }
  return y;
}

NumericVector sqr(NumericVector& x){
  NumericVector y(x.size());
  double v;
  for(NumericVector::iterator xx=x.begin(),yy=y.begin();x.end()-xx;++xx,++yy){
    v=*xx;
    *yy=v*v;
  }
  return y;
}

int increment_maybe(int value, double vec_i){
  return vec_i == 0 ? value : ( value +1 ) ;  
}

void minus_c(double f[],double &x,double *y,int offset,int &len){
  double *ff=f;
  for(int i=0;i<len;++i,ff+=offset,++y)
    *ff=abs(x-*y);
}

int my_round(const double x){
  const int y=x*10;
  return y%10>4 ? int(x)+1 : x;
}

double my_round_gen(double x,const int dg){
  if(x==Rcpp::NA)
    return x;
  int t=10;
  for(int i=0;i<dg;++i)
    t= (t<<3) + (t<<1);
  const bool nx=x<0;
  int y= nx ? -x*t : x*t;
  const int m=y%10;
  y= m>4 ? y+10-m : y-m ;
  x=y;
  return nx ? -x/t : x/t ; 
}

int len_sort_unique_int(IntegerVector x){
  int aa,mx,mn,count_not_zero=0;
  int count_pos=0;
  max_neg_pos(&x[0],&(*x.end()),mx,mn,count_pos);
  const int count_neg=x.size()-count_pos;
  vector<int> pos,f,neg;
  vector<int>::iterator pp,nn,F;
  IntegerVector::iterator a=x.begin();
  if(count_pos>0)
    pos.resize(mx+1,INT_MAX);
  if(count_neg>0)
    neg.resize(1-mn,INT_MAX);
  if(count_pos && count_neg){
    for(nn=neg.begin(),pp=pos.begin();a!=x.end();++a){
      aa=*a;
      if(aa<0)
        *(nn-aa)=aa;
      else
        *(pp+aa)=aa;
    }
  }else if(count_pos){
    for(pp=pos.begin();a!=x.end();++a){
      aa=*a;
      *(pp+aa)=aa;
    }
    
  }else{ 
    for(nn=neg.begin();a!=x.end();++a){
      aa=*a;
      *(nn-aa)=aa;
    }
  }
  if(count_neg)
    for(nn=neg.begin();nn!=neg.end();++nn)
      if(*nn!=INT_MAX)
        count_not_zero++;
  if(count_pos)
    for(pp=pos.begin();pp!=pos.end();++pp)
      if(*pp!=INT_MAX)
        count_not_zero++;
  return count_not_zero;
}
