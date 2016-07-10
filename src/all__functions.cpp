// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include <string>
#include <vector>
using namespace std;
using namespace arma;
using namespace Rcpp;

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

float operator*(vec x,vec y){
  int i,n=x.n_rows;
  float s=0.0;
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

colvec operator^(const char &a,const colvec &y){
  int i,yrow=y.n_rows;
  colvec Y(yrow);
  for(i=0;i<yrow;i++)
      Y(i)=exp(y(i));
  return Y;
}

mat operator+(colvec y,mat x){
  int i,j,d=x.n_cols,n=x.n_rows,a;
  mat Y(n,d);
  for(i=0;i<n;i++){
    a=y(i);
    for(j=0;j<d;j++)
      Y(i,j)=x(i,j)+a;
  }
  return Y;
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

vec loga(vec x){
  int n=x.size();
  vec F(n);
  for(int i=0;i<n;i++)
    F(i)=log(x(i));
  return F;
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
double nth(vector<int> x,unsigned int elem){
  nth_element(x.begin(),x.begin()+elem-1,x.end());
  return x[elem-1];
}

//[[Rcpp::export]]
vector<double> colmeds(mat x){
  int i,p=x.n_cols,sz=x.n_rows,middle=sz/2-1;
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
