// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <vector>
#include <string>
#include <algorithm>
#include "mn.h"
#include "templates.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

bool my_compare_order2(const pr<double,int>& a,const pr<double,int>& b){
  return a.first<b.first;
}

bool my_compare_order_second(const pr<double,int>& a,const pr<double,int>& b){
  return a.second<b.second;
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

bool cor_vecs(const pr<double,double>& a,const pr<double,double>& b){
  return a.second<b.second;
}

bool s_indx_asc(const pr<int,double>& a,const pr<int,double>& b){
  return a.second<b.second;
}

bool s_indx_des(const pr<int,double>& a,const pr<int,double>& b){
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

rowvec operator/(colvec x,double s){
  rowvec f(x.n_elem);
  for(rowvec::iterator ff=f.begin(),xx=x.begin();ff!=f.end();++ff,++xx)
    *ff=*xx/s;
  return f;
}


//regression
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

//diri_nr_type2,gamma
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

//diri_nr_type2,gamma
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

//floyd
void i4mat_floyd ( int n, vector<double> &a ){
  int i,j,k;
  const double i4_huge = 2147483647;
  for ( k = 0; k < n; k++ )
    for ( j = 0; j < n; j++ )
      if ( a[k+j*n] < i4_huge )
        for ( i = 0; i < n; i++ )
          if ( a[i+k*n] < i4_huge ){
            a[i+j*n] = std::min( a[i+j*n], a[i+k*n] + a[k+j*n] );
          }
}

//col_row_min_max
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

//col_row_min_max
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

//col_row_max
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

//col_row_max
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
//col_row_min
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

//col_row_min
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

//diri_nr_type2
colvec Digamma_v(colvec x,int &p){
  double *start=x.memptr(),*end=start+p;
  for(;start!=end;++start)
    *start=digamma(*start);
  return x;
}

//diri_nr_type2
colvec Trigamma_v(colvec x,int &p){
  double *start=x.memptr(),*end=start+p;
  for(;start!=end;++start)
    *start=trigamma(*start);
  return x;
}

//diri_nr_type2
void fill_with_value(double *start,double *end,double v){
  for(;start!=end;++start)
    *start=v;
}

//spat_med
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

//comb_n
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

//total_dista,dista
mat sqrt_mat(mat x){
  colvec f(x.n_elem);
  for(double *start=&x[0],*startf=&f[0],*end=&(*x.end());start!=end;++start,++startf){
    *startf=std::sqrt(*start);    
  }
  return f;
}

//sort_unique,len_sort_unique
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
//[[Rcpp::plugins(cpp11)]]

//rmdp
uvec Order_rmdp(colvec& x){
  uvec ind=linspace<uvec>(0,x.n_elem-1,x.n_elem);
  stable_sort(ind.begin(),ind.end(),[&](int i,int j){return x[i]<x[j];});
  return ind;
}

//rmdp
rowvec colvar_rmdp(mat& x){
  rowvec nyr1=x.row(0),nyr2=x.row(1);
  return 0.5*(square(nyr1) + square(nyr2)) - nyr1%nyr2;
}

//dists
double sum_pow(colvec x,const double p){
  const int sz=x.size();
  double s=0;
  for(double *startx=&x[0],*end=startx+sz;startx!=end;++startx)
    s+=std::pow(*startx,p);
  return s;
}

//colTabulate
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

//regression
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

//Design_matrix
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

//varcomps_mle
NumericVector minus_mean(NumericVector& x,const double k){
  NumericVector y(x.size());
  double v;
  for(NumericVector::iterator xx=x.begin(),yy=y.begin();x.end()-xx;++xx,++yy){
    v=*xx;
    *yy=v-k;
  }
  return y;
}

//varcomps_mle
NumericVector sqr(NumericVector& x){
  NumericVector y(x.size());
  double v;
  for(NumericVector::iterator xx=x.begin(),yy=y.begin();x.end()-xx;++xx,++yy){
    v=*xx;
    *yy=v*v;
  }
  return y;
}

//vecdist
void minus_c(double f[],double &x,double *y,int offset,int &len){
  double *ff=f;
  for(int i=0;i<len;++i,ff+=offset,++y)
    *ff=abs(x-*y);
}

//squareform,Round
int my_round(const double x){
  const int y=x*10;
  return y%10>4 ? int(x)+1 : x;
}

//Round
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

//Norm
double sumsqr(NumericMatrix &x){
  double s=0,v;
  for(double *start=&x[0],*end=&(*x.end());start!=end;++start){
    v=*start;
    s+=v*v;
  }
  return std::sqrt(s);    
}

//col/row True
int True(int *start,int *end){
  int t=0;
  for(;start!=end;++start)
    if(*start) 
      ++t;
    return t;
}

//all
bool my_all(int* start,int *end){
  for(;start!=end;++start){
    if(!(*start)){
      return false;
    }
  }
  return true;
}

//any
bool my_any(int* start,int *end){
  for(;start!=end;++start){
    if(*start){
      return true;
    }
  }
  return false;
}

//total_dista
double sum_sqrt_mat(mat x){
  double *xx=&x[0],a=0,*endx=&(*x.end());
  for(;xx!=endx;++xx)
    a+=std::sqrt(*xx);
  return a;
}

//spml_mle
colvec pnormc(colvec x){
  for(double *xx=&x[0],*endx=&x[x.n_elem];xx!=endx;++xx){
    *xx=R::pnorm5(*xx,0,1,1,0);
  }
  return x;
}

//spml_mle
double sum_abs(mat x,mat y){
  double s=0;
  for(unsigned int i=0;i<x.n_elem;++i){
    s+=abs(x[i]-y[i]);
  }
  return s;
}

//hash2lists
NumericVector toNumbers(string x,char spliter){
  NumericVector f;
  x+=spliter;
  const char *split=&spliter;
  char *token = strtok(&x[0], split);
  while (token != NULL) {
    f.push_back(atof(token));
    token = strtok(NULL, split);
  }
  return f;
}

//bincomb
IntegerVector combine(IntegerVector x,IntegerVector y){
  const int n=x.size(),p=y.size(),z=n+p;
  IntegerVector f(z);
  f[Range(0,n-1)]=x;
  f[Range(n,z-1)]=y;
  return f;
}

//group_mad,mad2,group_med,mad2
double med_helper(NumericVector::iterator first,NumericVector::iterator last){
  double F;
  const int sz=last-first,middle=sz/2-1;
  if(sz%2==0){
    nth_element(first,first+middle,last);
    F=(*(first+middle)+*(min_element(first+middle+1,last)))/2.0;
  }else{
    nth_element(first,first+middle+1,last);
    F=*(first+middle+1);
  }
  return F;
}
