//Author: Manos Papadakis

#include "lgstc_pssn.h"
#include "mn.h"


//[[Rcpp::export]]
NumericVector logistic_only_h(NumericMatrix X, NumericVector Y, const double my){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=2;
  unsigned int j,i;
  char e='e';
  colvec b_old(d),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros),p(n),z_tr_i(n),x2_col;
  mat z(n,2,fill::ones),inv_L2(d,d),z_tr(2,n,fill::ones),x(X.begin(),n,pcols,false);
  NumericVector F(pcols);
  colvec::iterator expyhatiter=expyhat.begin();
  double dif,s,t,sw=0.0,szw=0.0,sz2w=0.0,lgmy=log(my/(1-my));
  for(i=0;i<pcols;++i){
    b_old(0)=lgmy;
    b_old(1)=0;
    z_tr_i=x.col(i);
    z.col(1)=z_tr_i;
    z_tr.row(1)=mat(z_tr_i.begin(),1,n,false);
    x2_col=square(z_tr_i);
    for(dif=1.0,s=0.0;dif>0.000000001;){
      sw=szw=sz2w=0.0;
      yhat = z*b_old;
      expyhat=(e^yhat);
      p = expyhat / ( 1 + expyhat );
      for(j=0;j<n;j++){
        t=p.at(j);
        W.at(j)=t*(1-t);
        sw+=W.at(j);
      }
      szw=sum(W%z_tr_i);
      sz2w=sum(W%x2_col);
      L1=z_tr*(y-p);
      t=1.0/(sw*sz2w-szw*szw);
      inv_L2.at(0,0)=sz2w*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szw*t;
      inv_L2.at(1,1)=sw*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }  
    for(expyhatiter=expyhat.begin();expyhatiter!=expyhat.end();++expyhatiter)
      s+=log1p(*expyhatiter);
    F[i]=2.0*(s-sum(y%yhat));
  }
  return F;
}

//[[Rcpp::export]]
NumericVector poisson_only_h(NumericMatrix X, NumericVector Y, const double ylogy, const double my){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=2;
  unsigned int i;
  char e='e';
  colvec b_old(d),b_new(d),L1(d),yhat(n),y(Y.begin(),n,false);
  mat z(n,2,fill::ones),inv_L2(d,d),ytr=y.t(),z_tr(2,n,fill::ones),x(X.begin(),n,pcols,false);
  vec m(n),z_col_1(n);
  NumericVector F(pcols);
  double dif,sm=0.0,szm=0.0,sz2m=0.0,t,lgmeany=log(my);
  for(i=0;i<pcols;++i){
    b_old(0)=lgmeany;
    b_old(1)=0;
    z_col_1=x.col(i);
    z.col(1)=z_col_1;
    z_tr.row(1)=mat(z_col_1.begin(),1,n,false);
    for(dif=1.0;dif>0.000000001;){
      sm=szm=sz2m=0.0;
      yhat=z*b_old;
      m=(e^yhat);
      L1=z_tr*(y-m);
      sm=sum(m);
      szm=sum(m%z_col_1);
      sz2m=sum(m%square(z_col_1));
      t=1.0/(sm*sz2m-szm*szm);
      inv_L2.at(0,0)=sz2m*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szm*t;
      inv_L2.at(1,1)=sm*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }
    F[i]=2.0*(ylogy-sum(y%yhat));
  }
  return F;
}

//[[Rcpp::export]]
double glm_logistic_h(NumericMatrix X, NumericVector Y, const double my){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols+1;
  unsigned int j;
  const char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros);
  mat L2,x(X.begin(),n,pcols,false),x_tr(n,pcols+1);
  vec p(n);
  colvec::iterator expyhatiter=expyhat.begin();
  double dif,s=0.0,t;
  x.insert_cols(0,ones(n));
  x_tr=x.t();
  b_old(0)=log(my)-log(1-my);
  for(dif=1.0;dif>0.000000001;){
    yhat = x*b_old;
    expyhat=(e^yhat);
    p = expyhat / ( 1 + expyhat );
    for(j=0;j<n;++j){
      t=p.at(j);
      W.at(j)=t-t*t;
    }
    L1=x_tr*(y-p);
    L2=x.each_col()%W;
    L2=x_tr*L2;
    b_new=b_old+solve(L2,L1);
    dif=sum(abs(b_new-b_old));
    b_old=b_new;
  }
  t=sum(y%yhat);
  for(expyhatiter=expyhat.begin();expyhatiter!=expyhat.end();++expyhatiter)
    s+=log1p(*expyhatiter);
  return 2.0*(s-t);
}

//[[Rcpp::export]]
double glm_poisson_h(NumericMatrix X, NumericVector Y, const double ylogy, const double my){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols+1;
  char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),y(Y.begin(),n,false),m(n);
  mat L2,x(X.begin(),n,pcols,false),x_tr(n,pcols+1);
  double dif;
  x.insert_cols(0,ones(n));
  b_old(0)=log(my);
  x_tr=x.t();
  for(dif=1.0;dif>0.000000001;){
    yhat=x*b_old;
    m=(e^yhat);
    L1=x_tr*(y-m);
    L2=x.each_col()%m;
    L2=x_tr*L2;
    b_new=b_old+solve(L2,L1);
    dif=sum(abs(b_new-b_old));
    b_old=b_new;
  }
  return 2.0*(ylogy-sum(y%yhat));
}

