//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>

using namespace Rcpp;

SEXP min_max(SEXP x,bool index=false){
  SEXP F;
  double *xx=REAL(x),*end=xx+LENGTH(x);
  double xxx;
  if(index){
    F = PROTECT(Rf_allocVector(INTSXP, 2));
    int *f=INTEGER(F),min_i=0,max_i=0;
    double *bg=xx;
    for(xx++;xx!=end;++xx){
      xxx=*xx;
      if(xxx>bg[max_i]){
        max_i=xx-bg;
      }else if(xxx<bg[min_i]){
        min_i=xx-bg;
      }
    }
    *f=min_i+1;
    f[1]=max_i+1;
    UNPROTECT(1);
    return F;
  }
  F= PROTECT(Rf_allocVector(REALSXP, 2));
  double *f=REAL(F),min=*xx,max=min;
  for(xx++;xx!=end;++xx){
    xxx=*xx;
    if(xxx>max)
      max=xxx;
    else if(xxx<min)
      min=xxx;
  }
  *f=min;
  f[1]=max;
  UNPROTECT(1);
  return F;
}


RcppExport SEXP Rfast_min_max(SEXP x,SEXP indexSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< bool >::type index(indexSEXP);
    __result = min_max(x,index);
    return __result;
END_RCPP
}


SEXP min_max_perc(SEXP x){
  const int n=LENGTH(x);
  SEXP f=Rf_allocVector(REALSXP,4);
  double *start=REAL(x),*end=start+n,mx,mn,pos=0,xx,*FF=REAL(f);
  mn=mx=*start;
  for(;start!=end;++start){
    xx=*start;
    if(xx>0) pos++;
    if(mn>xx)
      mn=xx;
    else if(mx<xx)
      mx=xx;
  }
  *FF=mn;
  FF[1]=mx;
  FF[3]=(pos/n)*100.0;
  FF[2]=100.0-FF[3];
  return f;
}

RcppExport SEXP Rfast_min_max_perc(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = min_max_perc(x);
    return __result;
END_RCPP
}


////////////////////////////////////////////////////////////////////

SEXP pmax_simple(SEXP x,SEXP y){
  SEXP f = (Rf_isMatrix(x) && Rf_isMatrix(y)) ?
      PROTECT(Rf_allocMatrix(REALSXP,Rf_nrows(x),Rf_ncols(x))) :
      PROTECT(Rf_allocVector(REALSXP,LENGTH(x)));
  double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f);
  for(;startx!=end;++startx,++starty,++startf)
    *startf=std::max(*startx,*starty);
  UNPROTECT(1);
  return f;
}


SEXP pmax_na_rm(SEXP x,SEXP y){
  SEXP f = (Rf_isMatrix(x) && Rf_isMatrix(y)) ?
      PROTECT(Rf_allocMatrix(REALSXP,Rf_nrows(x),Rf_ncols(x))) :
      PROTECT(Rf_allocVector(REALSXP,LENGTH(x)));
    double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f);
    for(;startx!=end;++startx,++starty,++startf){
      if(!(R_IsNA(*startx) || R_IsNA(*starty)))
        *startf=std::max(*startx,*starty);
    }
    UNPROTECT(1);
    return f;
}

SEXP pmax(SEXP x,SEXP y,const bool na_rm){
  return na_rm ? pmax_na_rm(x,y) : pmax_simple(x,y);
}

RcppExport SEXP Rfast_pmax(SEXP x,SEXP y,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = pmax(x,y,na_rm);
    return __result;
END_RCPP
}

SEXP pmin_simple(SEXP x,SEXP y){
  SEXP f = (Rf_isMatrix(x) && Rf_isMatrix(y)) ?
      PROTECT(Rf_allocMatrix(REALSXP,Rf_nrows(x),Rf_ncols(x))) :
      PROTECT(Rf_allocVector(REALSXP,LENGTH(x)));
  double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f);
  for(;startx!=end;++startx,++starty,++startf)
    *startf=std::min(*startx,*starty);
  UNPROTECT(1);
  return f;
}


SEXP pmin_na_rm(SEXP x,SEXP y){
  SEXP f = (Rf_isMatrix(x) && Rf_isMatrix(y)) ?
      PROTECT(Rf_allocMatrix(REALSXP,Rf_nrows(x),Rf_ncols(x))) :
      PROTECT(Rf_allocVector(REALSXP,LENGTH(x)));
    double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f);
    for(;startx!=end;++startx,++starty,++startf){
      if(!(R_IsNA(*startx) || R_IsNA(*starty)))
        *startf=std::min(*startx,*starty);
    }
    UNPROTECT(1);
    return f;
}

SEXP pmin(SEXP x,SEXP y,const bool na_rm){
  return na_rm ? pmin_na_rm(x,y) : pmin_simple(x,y);
}

RcppExport SEXP Rfast_pmin(SEXP x,SEXP y,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = pmin(x,y,na_rm);
    return __result;
END_RCPP
}

//[[Rcpp::export]]
SEXP pmin_pmax_simple(SEXP x,SEXP y){
  SEXP f=PROTECT(Rf_allocMatrix(REALSXP,2,LENGTH(x)));
  double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f);
  for(;startx!=end;++startx,++starty,startf+=2)
    if(*startx < *starty){
      *startf=*startx;
      startf[1]=*starty;
    }else{
      *startf=*starty;
      startf[1]=*startx;
    }
  UNPROTECT(1);
  return f;
}

//[[Rcpp::export]]
SEXP pmin_pmax_na_rm(SEXP x,SEXP y){
  SEXP f=PROTECT(Rf_allocMatrix(REALSXP,2,LENGTH(x)));
  double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f),vx,vy;
  for(;startx!=end;++startx,++starty,startf+=2){
    vx=*startx;
    vy=*starty;
    if(!(R_IsNA(vx) || (R_IsNA(vy)))){
      if(vx < vy){
        *startf=vx;
        startf[1]=vy;
      }else{
        *startf=vy;
        startf[1]=vx;
      }
  }
  }
  UNPROTECT(1);
  return f;
}

SEXP pmin_pmax(SEXP x,SEXP y,const bool na_rm){
  return na_rm ? pmin_pmax_na_rm(x,y) : pmin_pmax_simple(x,y);
}

RcppExport SEXP Rfast_pmin_pmax(SEXP x,SEXP y,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = pmin_pmax(x,y,na_rm);
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////
