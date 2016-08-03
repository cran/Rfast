// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include <string>
#include <vector>
#include <iostream>
#include <cstring>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <Rinternals.h>
#include <map>
#include "g2t.h"
#include "mn.h"

using namespace std;
using namespace arma;
using namespace Rcpp;

//[[Rcpp::export]]
mat design_matrix(CharacterVector x) {
  int i,n=x.size();
  vector< pair<string,int> > indices_copy_x(n);
  vector< pair<string,int> >::iterator a,b;
  vector<string> leksi;
  mat Final;
  for(i=0;i<n;i++)
    indices_copy_x[i]=pair<string,int>(as<string>(x(i)),i);
  sort(indices_copy_x.begin(), indices_copy_x.end(),my_compare);
  leksi=as< vector<string> >(sort_unique(x));
  Final.resize(n,leksi.size());
  a=indices_copy_x.begin();
  for(;a!=indices_copy_x.end();a++)
    Final((*a).second,lower_bound(leksi.begin(),leksi.end(),(*a).first)-leksi.begin())=1;
  Final.shed_col(leksi.size()-1);
  Final.insert_cols(0,ones(n));
  return Final;
}

//[[Rcpp::export]]
vec logistic_only(mat x, colvec y){
  unsigned int j,d=2,n=x.n_rows,i,pcols=x.n_cols;
  char e='e';
  mat z(n,2),inv_L2(d,d),tmp,expyhat,z_tr(2,n),ytr=y.t(),W=zeros(1,n);
  vec p(n),L(pcols);
  colvec b_old(d),b_new(d),L1(d),yhat(n),L2_L1(d);
  double dif,s,t,sw=0.0,szw=0.0,sz2w=0.0,my = mean(y);
  b_old(0)=log(my/(1-my));
  b_old(1)=0;
  for(j=0;j<n;j++){
    z(j,0)=1;
  }
  for(i=0;i<pcols;i++){
    for(j=0;j<n;j++){
      z(j,1)=x(j,i);
    }
    dif=1.0;
    s=0.0;
    while(dif>0.000000001){
      sw=szw=sz2w=0.0;
      yhat = z*b_old;
      expyhat=(e^yhat);
      p = expyhat / ( 1 + expyhat );
      for(j=0;j<n;j++){
        t=p(j);
        W(j)=t*(1-t);
        sw+=W(j);
      }
      szw=sum(W*z.col(1));
      sz2w=sum(W*(z.col(1)^2));
      z_tr=z.t();
      L1=z_tr*(y-p);
      t=1.0/(sw*sz2w-szw*szw);
      inv_L2(0,0)=sz2w*t;
      inv_L2(0,1)=inv_L2(1,0)=-szw*t;
      inv_L2(1,1)=sw*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }  
    tmp=ytr*yhat;
    for(j=0;j<n;j++)
      s+=log(1+expyhat(j));
    L(i)=2.0*(s-tmp(0));
  }
  return L;
}

//[[Rcpp::export]]
vec poisson_only(mat x, colvec y){
  unsigned int i,j,d=2,n=x.n_rows,pcols=x.n_cols;
  char e='e';
  mat z(n,2),inv_L2(d,d),tmp,ytr=y.t();
  vec m(n),L(pcols),z_col_1(n);
  double dif,sm=0.0,szm=0.0,sz2m=0.0,t,ylogy=y*loga(y);
  colvec b_old(d),b_new(d),L1(d),yhat(n),L2_L1(d);
  b_old(0)=log(mean(y));
  b_old(1)=0;
  for(j=0;j<n;j++){
    z(j,0)=1;
  }
  for(i=0;i<pcols;i++){
    for(j=0;j<n;j++){
      z(j,1)=x(j,i);
    }
    z_col_1=z.col(1);
    dif=1.0;
    while(dif>0.000000001){
      sm=szm=sz2m=0.0;
      yhat=z*b_old;
      m=(e^yhat);
      L1=z.t()*(y-m);
      sm=sum(m);
      szm=sum(m*z_col_1);
      sz2m=sum(m*(z_col_1^2));
      t=1.0/(sm*sz2m-szm*szm);
      inv_L2(0,0)=sz2m*t;
      inv_L2(0,1)=inv_L2(1,0)=-szm*t;
      inv_L2(1,1)=sm*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }
    L(i)=2.0*(ylogy-y*yhat);
  }
  return L;
}

//[[Rcpp::export]]
mat regression(DataFrame x, colvec y){
  int pos_f=0,ptf=0, n=x.nrows(),p,size_F=x.length(),i=0;
  vec res;
  CharacterVector leksi;
  NumericVector x_i;
  colvec asses(y.n_rows);
  mat strtonum,tr_strtonum,b,F(2,size_F);
  double SSO=var(y)*(double)(n-1),SS1;
  vector<int> pos=which_isFactor(x);
  asses.fill(1);
  if(!pos.size()){
    for(;i<size_F;i++){
      x_i=x(i);
      F(0,i)=regression_only_col(as<colvec>(x_i),y);
      F(1,i)=1;
    }
    return F;
  }
  pos_f=pos[ptf]-1;
  for(;i<size_F;i++){
    if(pos_f==i){
      leksi=x(i);
      strtonum=design_matrix(leksi);
      tr_strtonum=strtonum.t();
      b=inv(tr_strtonum*strtonum)*tr_strtonum*y; 
      res=y-strtonum*b; 
      SS1=var(res)*(n-1);
      p=strtonum.n_cols;
      F(0,i)=(SSO-SS1)*(n-p)/((p-1)*SS1);
      F(1,i)=p-1;
      ptf++;
      pos_f=pos[ptf]-1;
    }else{
      x_i=x(i);
      F(0,i)=regression_only_col(as<colvec>(x_i),y);
      F(1,i)=1;
    }
  }
  return F;
}

//[[Rcpp::export]]
double nth(vector<int> x,unsigned int elem){
  nth_element(x.begin(),x.begin()+elem-1,x.end());
  return x[elem-1];
}

//[[Rcpp::export]]
vector<double> colmeds(mat x){
  unsigned int i,p=x.n_cols,sz=x.n_rows,middle=sz/2-1;
  vector<double> F(p);
  if(sz%2==0)
    for(i=0;i<p;i++){
      nth_element(x.begin_col(i),x.begin_col(i)+middle,x.end_col(i));
      F[i]=(x(middle,i)+*(min_element(x.begin_col(i)+middle+1,x.end_col(i))))/2.0;
    }
  else 
	for(i=0;i<p;i++){
	  nth_element(x.begin_col(i),x.begin_col(i)+middle+1,x.end_col(i));
	  F[i]=x(middle+1,i);
	}
   return F;
}

//[[Rcpp::export]]
mat sort_mat(mat x){
  unsigned int p=x.n_cols,i;
  for(i=0;i<p;i++)
    sort(x.begin_col(i),x.end_col(i));
  return x;
}

//[[Rcpp::export]]
vector<double> colmax(NumericMatrix x,bool value=false){
  unsigned int i,p=x.ncol();
  mat X = mat(x.begin(), x.nrow(), p, false); 
  vector<double> F(p);
  if(value){
  	rowvec f=max(X,0);
  	for(i=0;i<p;i++)
      F[i]=f(i);
  }else
    for(i=0;i<p;i++)
      F[i]=max_element(X.begin_col(i),X.end_col(i))-X.begin_col(i)+1;
  return F;
}

//[[Rcpp::export]]
vector<double> colmin(NumericMatrix x,bool value=false){
  unsigned int i,p=x.ncol();
  mat X = mat(x.begin(), x.nrow(), p, false); 
  vector<double> F(p);
  if(value){
  	rowvec f=min(X,0);
  	for(i=0;i<p;i++)
      F[i]=f(i);
  }else
    for(i=0;i<p;i++)
      F[i]=min_element(X.begin_col(i),X.end_col(i))-X.begin_col(i)+1;
  return F;
}

//[[Rcpp::export]]
vector<int> which_isFactor(DataFrame x){
  vector<int> P;
  for(int i=0;i<x.length();++i)
    if(Rf_isFactor(x(i)))
      P.push_back(i+1);
   return P;
}

//[[Rcpp::export]]
vec diri_nr_type2(vec a1, vec a2, vec ma,double tol){
  int n=a1.size(),i;
  vec f,tr1(n),dg1(n),der;
  double sa,trsa,dgsa;
  while (sum(abs(a2-a1))>tol){
    a1=a2;
    sa=sum(a1);
    dgsa=digamma(sa);
    trsa=trigamma(sa);
    for(i=0;i<n;++i){
      tr1(i)=-trigamma(a1(i))+trsa;
      dg1(i)=ma(i)-digamma(a1(i))+dgsa;
    }
    a2=a1-dg1/tr1;
  }
  return a2;
}

//[[Rcpp::export]]
long double med(vec x){
  long double F;
  int sz=x.n_rows,middle=sz/2-1;
  if(sz%2==0){
    nth_element(x.begin(),x.begin()+middle,x.end());
    F=(x(middle)+*(min_element(x.begin()+middle+1,x.end())))/2.0;
  }else{
    nth_element(x.begin(),x.begin()+middle+1,x.end());
    F=x(middle+1);
  }
  return F;
}

//[[Rcpp::export]]
List Hash_list(CharacterVector key,NumericVector x){
  int n=x.size(),i;
  List H_M;
  string m;
  for(i=0;i<n;++i){
    m=as<string>(key(i));
    H_M[m]=x(i);
  }
  return H_M;
}

//[[Rcpp::export]]
vector<int> generate_key_1(CharacterVector x){
	int i,n=x.size();
	vector<int> F(n);
	for(i=0;i<n;++i)
		F[i]=generatekey1(as<string>(x(i)));
	return F;
}

//[[Rcpp::export]]
vector<int> generate_key_2(CharacterVector x){
	int i,n=x.size();
	vector<int> F(n);
	for(i=0;i<n;++i)
		F[i]=generatekey2(as<string>(x(i)),0);
	return F;
}

//[[Rcpp::export]]
long double hash_find(List x,string value){
  vector<string> nam=x.names();
  if(std::find(nam.begin(),nam.end(),value)!=nam.end())
    return as<long double>(x[value]);
  return 0.0;
}

//[[Rcpp::export]]
int Match(colvec x,double key){
  int i,n=x.size();
  for(i=0;i<n;++i)
    if(x(i)==key)
      return i+1;
  return 0;
}

//[[Rcpp::export]]
mat Chol(mat x){
  return chol(x);
}

// [[Rcpp::export]]
rowvec colmeans(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return mean(X, 0); 
}

// [[Rcpp::export]]
rowvec colsums(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return sum(X, 0); 
}

// [[Rcpp::export]]
colvec rowsums(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return sum(X, 1); 
}

// [[Rcpp::export]]
colvec rowmeans(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return mean(X, 1); 
}

// [[Rcpp::export]]
colvec rowMaxs(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return max(X, 1); 
}

// [[Rcpp::export]]
colvec rowMins(NumericMatrix x){
  mat X = mat(x.begin(), x.nrow(), x.ncol(), false); 
  return min(X, 1); 
}


/////////////// GEORGE ///////////////////////////

//[[Rcpp::export]]
List g2Test(NumericMatrix data, int x, int y, NumericVector cs, NumericVector dc){
  int *ics = new int[cs.size()];
  int *idc = new int[dc.size()];
  for (int i = 0; i < cs.size(); ++i) {
    ics[i] = (int) cs[i] - 1;
  }
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  TestResult result = g2Test(data, x-1, y-1, ics, cs.size(), idc);

  delete[] ics;
  delete[] idc;  
  
  List out;
  out["statistic"] = result.stat;
  out["df"] = result.df;
  return out;
}

// [[Rcpp::export]]
List g2Test_univariate_perm(NumericMatrix data, NumericVector dc, int nperm) {
  int nvars = data.ncol();
  int *ics = new int[0];
  int *idc = new int[dc.size()];
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  int nout = nvars * (nvars - 1) / 2;
  NumericVector xout(nout);
  NumericVector yout(nout);
  NumericVector pvalues(nout);
  NumericVector statistics(nout);
  
  int idx = 0;
  for(int i = 0; i < nvars; ++i) {
    for(int j = i+1; j < nvars; ++j) {
      TestResult result = permG2Test(data, i, j, ics, 0, idc, nperm);
      xout[idx] = i + 1;
      yout[idx] = j + 1;
      pvalues[idx] = result.pvalue;
      statistics[idx] = result.stat;
      ++idx;
    }
  }
  
  delete[] ics;
  delete[] idc;  
  
  List out;
  out["statistic"] = statistics;
  out["pvalue"] = pvalues;
  out["x"] = xout;
  out["y"] = yout;
  return out;
}

// [[Rcpp::export]]
List g2Test_univariate(NumericMatrix data, NumericVector dc) {
  int nvars = data.ncol();
  int *idc = new int[dc.size()];
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  int nout = nvars * (nvars - 1) / 2;
  NumericVector xout(nout);
  NumericVector yout(nout);
  NumericVector statistics(nout);
  NumericVector df(nout);
  
  int idx = 0;
  for(int i = 0; i < nvars; ++i) {
    for(int j = i+1; j < nvars; ++j) {
      TestResult result = g2Test(data, i, j, idc);
        xout[idx] = i + 1;
        yout[idx] = j + 1;
        statistics[idx] = result.stat;
        df[idx] = (idc[i] - 1) * (idc[j] - 1);
        ++idx;
    }
  }
  
  delete[] idc;  
  
  List out;
  out["statistic"] = statistics;
  out["x"] = xout;
  out["y"] = yout;
  out["df"] = df;
  return out;
}

// [[Rcpp::export]]
List g2Test_perm(NumericMatrix data, int x, int y, NumericVector cs, NumericVector dc, int nperm) {
  int *ics = new int[cs.size()];
  int *idc = new int[dc.size()];
  for (int i = 0; i < cs.size(); ++i) {
    ics[i] = (int) cs[i] - 1;
  }
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  TestResult result = permG2Test(data, x-1, y-1, ics, cs.size(), idc, nperm);
  
  delete[] ics;
  delete[] idc;  
  
  List out;
  out["statistic"] = result.stat;
  out["pvalue"] = result.pvalue;
  out["x"] = x;
  out["y"] = y;
  out["df"] = result.df;
  return out;
}

////////////////// GEORGE //////////////////////////////////////////////////
