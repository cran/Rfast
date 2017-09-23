
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include "templates.h"
#include <string>

using namespace Rcpp;
using namespace arma;
using namespace std;

NumericMatrix euclidean_dist(NumericMatrix x,const bool sqr){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  if(sqr)
    for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=sum(square(xx.col(j)-xv));
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  else
    for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=std::sqrt(sum(square(xv-xx.col(j))));
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  return f;
}

NumericMatrix manhattan_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      a=sum(abs(xv-xx.col(j)));
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

NumericMatrix hellinger_dist(NumericMatrix x,const bool sqr){
  const int ncl=x.ncol(),nrw=x.nrow();
  const double p=1.0/std::sqrt(2.0);
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  if(sqr)
    for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=sum(square(xv-xx.col(j)))*0.5;
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  else
    for(i=0;i<ncl-1;++i){
      xv=xx.col(i);
      for(j=i+1;j<ncl;++j){
        a=p*std::sqrt(sum(square(xv-xx.col(j))));
        f(i,j)=a;
        f(j,i)=a;
      }
    }
  return f;
}

NumericMatrix max_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw),tmp(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      tmp=abs(xv-xx.col(j));
      a=tmp.at(tmp.index_max());
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

NumericMatrix min_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      xv=abs(xx.col(j)-xv);
      a=xv.at(xv.index_min());
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

NumericMatrix minkowski_dist(NumericMatrix x,const double p){
  const int ncl=x.ncol(),nrw=x.nrow();
  const double p_1=1.0/p;
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){      
      a=pow(sum_pow(abs(xv-xx.col(j)),p),p_1);
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

NumericMatrix canberra1_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw),yv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      yv=xx.col(j);
      a=sum(abs((xv-yv)/(xv+yv)));
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

NumericMatrix canberra2_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw),yv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      yv=xx.col(j);
      a=sum(abs(xv-yv)/(abs(xv)-abs(yv)));
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}



NumericMatrix total_variation_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      a=0.5*sum(abs(xv-xx.col(j)));
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

//[[Rcpp::export]]
NumericMatrix kullback_leibler_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  NumericMatrix f(ncl,ncl),log_x(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false),log_xx(log_x.begin(),nrw,ncl,false);
  colvec xv(nrw),log_xv(nrw);
  double a;
  int i,j;
  fill_with<std::log>(x.begin(),x.end(),log_xx.begin());
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    log_xv=log_xx.col(i);
    for(j=i+1;j<ncl;++j){
      a=sum((xv-xx.col(j))%(log_xv-log_xx.col(j)));
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

//[[Rcpp::export]]
NumericMatrix bhattacharyya_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  mat xx(x.begin(),nrw,ncl,false);
  NumericMatrix f(ncl,ncl);
  colvec xv(nrw);
  double a;
  int i,j;
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    for(j=i+1;j<ncl;++j){
      a=sum(sqrt(abs(xv%xx.col(j))));
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}

//[[Rcpp::export]]
NumericMatrix itakura_saito_dist(NumericMatrix x){
  const int ncl=x.ncol(),nrw=x.nrow();
  NumericMatrix f(ncl,ncl),log_x(nrw,ncl);
  mat xx(x.begin(),nrw,ncl,false),log_xx(log_x.begin(),nrw,ncl,false);
  colvec xv(nrw),log_xv(nrw);
  double a;
  int i,j;
  fill_with<std::log>(x.begin(),x.end(),log_xx.begin());
  for(i=0;i<ncl-1;++i){
    xv=xx.col(i);
    log_xv=log_xx.col(i);
    for(j=i+1;j<ncl;++j){
      a=sum((xv-xx.col(j)-log_xv-log_xx.col(j))-1);
      f(i,j)=a;
      f(j,i)=a;
    }
  }
  return f;
}


//[[Rcpp::export]]
NumericMatrix dist(NumericMatrix x,const string method,const bool sqr,const int p){
  if(method == "euclidean" || p==2){
    return euclidean_dist(x,sqr);
  }else if(method == "manhattan" || p==1){
    return manhattan_dist(x);
  }else if(method == "maximum"){
    return max_dist(x);
  }else if(method == "minimum"){
    return min_dist(x);
  }else if(method == "canberra1"){
    return canberra1_dist(x);
  }else if(method == "canberra2"){
    return canberra2_dist(x);
  }else if(method == "minkowski"){
    return minkowski_dist(x,p);
  }else if(method == "bhattacharyya"){
    return bhattacharyya_dist(x);
  }else if(method == "hellinger"){
    return hellinger_dist(x,sqr);
  }else if(method == "total_variation"){
    return total_variation_dist(x);
  }else if(method == "kullback_leibler" || method == "jensen_shannon"){
    return kullback_leibler_dist(x);
  }else if(method == "itakura_saito"){
    return itakura_saito_dist(x);
  }
  stop("Unsupported Method: %s",method);
}

RcppExport SEXP Rfast_dist(SEXP xSEXP,SEXP methodSEXP,SEXP sqrSEXP,SEXP pSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type sqr(sqrSEXP);
    traits::input_parameter< const int >::type p(pSEXP);
    __result = wrap(dist(x,method,sqr,p));
    return __result;
END_RCPP
}