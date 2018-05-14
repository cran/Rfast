#include <RcppArmadillo.h>
#include "templates.h"


using namespace arma;
using namespace Rcpp;
using namespace std;

static NumericVector eachcol_med_sum(NumericMatrix& x,NumericVector& y,SEXP ind){
  const bool is_ind_null = Rf_isNull(ind);
  const int n = is_ind_null ? x.ncol() : LENGTH(ind);
  NumericVector f(n),tmp(x.nrow());
  if(is_ind_null){
    for(int i=0;i<n;++i){
      tmp = x.column(i)+y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }else{
    IntegerVector indd(ind);
    for(int i=0;i<n;++i){
      tmp = x.column(indd[i]-1)+y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }
  return f;
}

static NumericVector eachcol_med_mult(NumericMatrix& x,NumericVector& y,SEXP ind){
  const bool is_ind_null = Rf_isNull(ind);
  const int n = is_ind_null ? x.ncol() : LENGTH(ind);
  NumericVector f(n),tmp(x.nrow());
  if(is_ind_null){
    for(int i=0;i<n;++i){
      tmp = x.column(i)*y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }else{
    IntegerVector indd(ind);
    for(int i=0;i<n;++i){
      tmp = x.column(indd[i]-1)*y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }
  return f;
}

static NumericVector eachcol_med_min(NumericMatrix& x,NumericVector& y,SEXP ind){
  const bool is_ind_null = Rf_isNull(ind);
  const int n = is_ind_null ? x.ncol() : LENGTH(ind);
  NumericVector f(n),tmp(x.nrow());
  if(is_ind_null){
    for(int i=0;i<n;++i){
      tmp = x.column(i)-y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }else{
    IntegerVector indd(ind);
    for(int i=0;i<n;++i){
      tmp = x.column(indd[i]-1)-y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }
  return f;
}

static NumericVector eachcol_med_div(NumericMatrix& x,NumericVector& y,SEXP ind){
  const bool is_ind_null = Rf_isNull(ind);
  const int n = is_ind_null ? x.ncol() : LENGTH(ind);
  NumericVector f(n),tmp(x.nrow());
  if(is_ind_null){
    for(int i=0;i<n;++i){
      tmp = x.column(i)/y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }else{
    IntegerVector indd(ind);
    for(int i=0;i<n;++i){
      tmp = x.column(indd[i]-1)/y;
      f[i]=med_helper<NumericVector>(tmp.begin(),tmp.end());
    }
  }
  return f;
}

//[[Rcpp::export]]
NumericVector eachcol_apply(NumericMatrix x,NumericVector y,SEXP ind,const char oper,const string method){
  if(method == "sum"){
    switch(oper){
    case '*': return eachcol_apply_helper<mmult<double>,madd<double> >(x,y,ind);
    case '/': return eachcol_apply_helper<mdiv<double>,madd<double> >(x,y,ind);
    case '+': return eachcol_apply_helper<madd <double>,madd<double> >(x,y,ind);
    case '-': return eachcol_apply_helper<mdiff<double>,madd<double> >(x,y,ind);
	case '^': return eachcol_apply_helper<std::pow,madd<double> >(x,y,ind);
    }
  }else if(method == "median"){
    switch(oper){
    case '*': return eachcol_med_mult(x,y,ind);
    case '/': return eachcol_med_div(x,y,ind);
    case '+': return eachcol_med_sum(x,y,ind);
    case '-': return eachcol_med_min(x,y,ind);
    case '^': stop("Error: Median does not support operator \'^\'.\n");
    }
  }else if(method == "max"){
    switch(oper){
    case '*': return eachcol_apply_helper<mmult<double>,mmax<double> >(x,y,ind);
    case '/': return eachcol_apply_helper<mdiv<double>,mmax<double> >(x,y,ind);
    case '+': return eachcol_apply_helper<madd <double>,mmax<double> >(x,y,ind);
    case '-': return eachcol_apply_helper<mdiff<double>,mmax<double> >(x,y,ind);
	case '^': return eachcol_apply_helper<std::pow,mmax<double> >(x,y,ind);
    }
  }else if(method == "min"){
    switch(oper){
    case '*': return eachcol_apply_helper<mmult<double>,mmin<double> >(x,y,ind);
    case '/': return eachcol_apply_helper<mdiv<double>,mmin<double> >(x,y,ind);
    case '+': return eachcol_apply_helper<madd <double>,mmin<double> >(x,y,ind);
    case '-': return eachcol_apply_helper<mdiff<double>,mmin<double> >(x,y,ind);
	case '^': return eachcol_apply_helper<std::pow,mmin<double> >(x,y,ind);
    }
  }
  stop("Error: wrong operation type.\n");
  return {};
}

RcppExport SEXP Rfast_eachcol_apply(SEXP xSEXP,SEXP ySEXP,SEXP ind,SEXP operSEXP,SEXP methodSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< const char >::type oper(operSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    __result = wrap(eachcol_apply(x,y,ind,oper,method));
    return __result;
END_RCPP
}
