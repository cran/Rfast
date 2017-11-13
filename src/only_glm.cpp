//Author: Manos Papadakis

#include "only_glm.h"

static colvec operator^(const char a,const colvec y){
  int i,yrow=y.n_elem;
  colvec Y(yrow);
  for(i=0;i<yrow;i++)
      Y(i)=exp(y(i));
  return Y;
}

NumericVector logistic_only(NumericMatrix& X,NumericVector& Y,const double my){
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

NumericVector poisson_only(NumericMatrix& X,NumericVector& Y,const double ylogy,const double my){
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

double glm_logistic(NumericMatrix& X,NumericVector& Y,const double my){
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

double arma_glm_logistic(mat x,vec y,const double my) {
  const unsigned int n=x.n_rows,pcols=x.n_cols,d=pcols+1;
  unsigned int j;
  const char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),expyhat,W(n,fill::zeros);
  mat L2,x_tr(n,pcols+1);
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

double glm_poisson(NumericMatrix& X,NumericVector& Y,const double ylogy,const double my){
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

double arma_glm_poisson(mat x,vec y,const double ylogy,const double my){
  const unsigned int n=x.n_rows,pcols=x.n_cols,d=pcols+1;
  char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),m(n);
  mat L2,x_tr(n,pcols+1);
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

static double calc_stat(arma::mat& l2, arma::colvec& u, arma::colvec& p, arma::colvec& b, 
		const double dof, const unsigned int nrows, const unsigned int ncols) {
	arma::mat l2_inv = arma::inv(l2);
	double sum = 0;
	for (unsigned int i = 0; i < nrows; ++i) {
		sum += std::pow(u[i], 2) / p[i] / (1 - p[i]);
	}
	const unsigned int last_ind = ncols - 1;
	return std::pow(b[last_ind], 2) / (sum / dof * l2_inv.at(last_ind, last_ind));
}

static Rcpp::NumericVector finalize(arma::colvec& f, arma::colvec& b, 
		const unsigned int n, const unsigned int size) {
	double fmax = 0;
	double findex = -1;
	for (unsigned int i = 0; i < size; ++i) {
	  if (f[i] > fmax) {
		  fmax = f[i];
		  findex = i;
	  }
	}
	const double bic = b[findex] + 2 * std::log(n);
	return Rcpp::NumericVector::create(bic, fmax, findex);
}

static double calc_bic(arma::colvec& y, arma::colvec& p, const unsigned int n) {
	double sum_a = 0;
	double sum_b = 0;
	for (unsigned int j = 0; j < n; ++j) {
		if (y[j] && p[j]) {
			sum_a += y[j] * std::log(y[j] / p[j]);
		}
		if (y[j] != 1 && p[j] != 1) {
			sum_b += (1 - y[j]) * std::log((1 - y[j]) / (1 - p[j]));
		}
	}
	return 2 * sum_a + 2 *sum_b;
}

NumericVector qs_binom_only(NumericMatrix& X, NumericVector& Y, const double my){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=2;
  unsigned int j,i;
  char e='e';
  colvec b_old(d),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros),p(n),z_tr_i(n),x2_col,u(n,fill::zeros),u2(n);
  mat z(n,2,fill::ones),inv_L2(d,d),z_tr(2,n,fill::ones),x(X.begin(),n,pcols,false);
  arma::colvec f(pcols);
  arma::colvec b(pcols);
  double dif,s,t,sw=0.0,szw=0.0,sz2w=0.0,lgmy=log(my/(1-my)),vb;
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
      u=y-p;
      L1=z_tr*u;
      t=1.0/(sw*sz2w-szw*szw);
      inv_L2.at(0,0)=sz2w*t;
      inv_L2.at(0,1)=inv_L2.at(1,0)=-szw*t;
      inv_L2.at(1,1)=sw*t;
      b_new=b_old+inv_L2*L1;
      dif=sum(abs(b_new-b_old));
      b_old=b_new;
    }  
    u2=square(u);
    s=sum(u2/W)/(n-2);
    vb=s*sw/(sw*sz2w-szw*szw);
    f[i]=b_new[1]*b_new[1]/vb;
	b[i] = calc_bic(y,p,n);
  }
  return finalize(f,b,n,pcols);
}

NumericVector qs_poisson_only(NumericMatrix& X, NumericVector& Y, const double ylogy, const double my){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=2;
  unsigned int i;
  char e='e';
  colvec b_old(d),b_new(d),L1(d),yhat(n),y(Y.begin(),n,false);
  mat z(n,2,fill::ones),inv_L2(d,d),ytr=y.t(),z_tr(2,n,fill::ones),x(X.begin(),n,pcols,false);
  vec m(n),z_col_1(n);
  arma::colvec f(pcols);
  arma::colvec b(pcols);
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
  	const double phi=arma::sum(arma::pow(y-m,2)/m)/(n-d); 
	f[i]=b_new[1]*b_new[1]/phi/inv_L2.at(1,1);
    b[i]=2.0*(ylogy-sum(y%yhat));
  }
  return finalize(f,b,n,pcols);
}

NumericVector glm_qs_binom(NumericMatrix& X,NumericVector& Y,const double my){
  const unsigned int n=X.nrow(),pcols=X.ncol(),d=pcols+1;
  unsigned int j,itters=0;
  const char e='e';
  colvec b_old(d,fill::zeros),b_new(d),L1(d),yhat(n),expyhat,y(Y.begin(),n,false),W(n,fill::zeros);
  mat L2,x(X.begin(),n,pcols,false),x_tr(n,pcols+1);
  vec p(n);
  double dif,t;
  x.insert_cols(0,ones(n));
  x_tr=x.t();
  b_old(0)=log(my)-log(1-my);
  for(dif=1.0;dif>0.000000001;++itters){
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
  arma::colvec u=y-p;
  const double dof=n-pcols-1;
  const double stat=calc_stat(L2,u,p,b_new,dof,n,pcols+1);
  const double bic=calc_bic(y,p,n);
  return Rcpp::NumericVector::create(bic,stat);
}

NumericVector glm_qs_poisson(NumericMatrix& X,NumericVector& Y,const double ylogy,const double my){
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
  arma::mat l2_inv=arma::inv(L2);
  const double phi=arma::sum(arma::pow(y-m,2)/m)/(n-d); 
  const double stat=b_new[b_new.size()-1]*b_new[b_new.size()-1]/phi/l2_inv.at(l2_inv.n_rows-1,l2_inv.n_cols-1);
  const double bic=2.0*(ylogy-sum(y%yhat))+d*std::log(n);
  return Rcpp::NumericVector::create(bic, stat);
}
