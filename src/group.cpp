// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "templates.h"
#include "mn.h"

using namespace Rcpp;


LogicalVector group_all(LogicalVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  pr<int,int> *y=new pr<int,int>[n];
  int i,c=0,k;
  LogicalVector::iterator xx=x.begin();
  for(;xx!=x.end();++xx,++kk){
    k=*kk-1;
    y[k].first+=*xx;
    y[k].second++;
    y[k].is_good=true;
  }
  for(i=0;i<n;++i){
    if(y[i].is_good){
    	++c;
    }
  }
  LogicalVector F(c);
  for(i=0,k=0;i<n;++i){
    if(y[i].is_good){
      F[k++]= y[i].first==y[i].second;
    }
  }
  delete[] y;
  return F;
}


RcppExport SEXP Rfast_group_all(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type n(nSEXP);
    __result = wrap(group_all(x,group,n));
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////////////////

NumericMatrix group_min_max(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  const double int_max=INT_MAX;
  NumericVector mn(n,int_max),mx(n,(double)(INT_MIN));
  NumericVector::iterator xx=x.begin(),minv=mn.begin(),maxv=mx.begin();
  int k;
  for(;xx!=x.end();++xx,++kk){
    k=*kk-1;
    mx[k]=std::max(mx[k],*xx);
    mn[k]=std::min(mn[k],*xx);
  }
  int count_not_zero=0;
  for(;minv!=mn.end();++minv){
    if(*minv!=int_max)
      ++count_not_zero;
  }
  NumericMatrix res(2,count_not_zero);
  int i=0;
  for(minv=mn.begin(),maxv=mx.begin();minv!=mn.end();++minv,++maxv){
    if(*minv!=int_max){
      res(0,i)=*minv;
      res(1,i)=*maxv;
      ++i;
    }
  }
  return res;
}

RcppExport SEXP Rfast_group_min_max(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int  >::type max_n(max_nSEXP);
    __result = wrap(group_min_max(x,group,max_n));
    return __result;
END_RCPP
}


//////////////////////////////////////////////////////////////////////////////


//[[Rcpp::export]]
NumericVector group_sum(NumericVector x,IntegerVector key,SEXP minn,SEXP maxx){
  int *mn=nullptr,*mx=nullptr;
    if(!Rf_isNull(minn))
        *mn=Rf_asInteger(minn);
    if(!Rf_isNull(maxx))
        *mn=Rf_asInteger(maxx);
  
  return group_sum_helper<NumericVector,NumericVector,IntegerVector>(x,key,mn,mx);
}

RcppExport SEXP Rfast_group_sum(SEXP xSEXP,SEXP groupSEXP,SEXP minn,SEXP maxx) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    __result = wrap(group_sum(x,group,minn,maxx));
    return __result;
END_RCPP
}



/////////////////////////////////////////////////////////////////////////


NumericVector group_min(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  const double int_max=INT_MAX;
  NumericVector f(n,int_max);
  NumericVector::iterator xx=x.begin(),ff=f.begin(),rr;
  for(;xx!=x.end();++xx,++kk){
    f[*kk-1]=std::min(f[*kk-1],*xx);
  }
  int count_not_zero=0;
  for(;ff!=f.end();++ff){
    if(*ff!=int_max)
      ++count_not_zero;
  }
  NumericVector res(count_not_zero);
  for(rr=res.begin(),ff=f.begin();ff!=f.end();++ff){
    if(*ff!=int_max)
      *rr++=*ff;
  }
  return res;
}

RcppExport SEXP Rfast_group_min(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type max_n(max_nSEXP);
    __result = wrap(group_min(x,group,max_n));
    return __result;
END_RCPP
}


////////////////////////////////////////////////////////////////////////


NumericVector group_med(NumericVector x,IntegerVector group){
  const int n=x.size(),n_1=n+1;
  IntegerVector::iterator kk=group.begin();
  NumericVector f(n);
  pr<double,int> *y=new pr<double,int>[n_1];
  pr<int,int> *ind=new pr<int,int>[n];
  int i,j=0,c=0;
  NumericVector::iterator xx=x.begin();
  for(i=0;xx!=x.end();++xx,++kk,++i){
    y[i].first=*xx;
    y[i].second=*kk-1;
  }
  y[n]=pr<double,int>();
  sort(y,y+n,my_compare_order_second);
  for(i=1;i<n_1;++i){
    if(y[j].second!=y[i].second){
      ind[y[j].second].first=j;
      ind[y[j].second].second=i;
      ind[y[j].second].is_good=true;
      ++c;
      j=i;
    }
    f[i-1]=y[i-1].first;
  }
  NumericVector F(c);
  for(int i=0,k=0;i<n;++i){
    if(ind[i].is_good){
      F[k++]=med_helper<NumericVector>(f.begin()+ind[i].first,f.begin()+ind[i].second);
    }
  }
  delete[] y;
  delete[] ind;
  return F;
}


RcppExport SEXP Rfast_group_med(SEXP xSEXP,SEXP groupSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    __result = wrap(group_med(x,group));
    return __result;
END_RCPP
}

///////////////////////////////////////////////////////////////////


NumericVector group_mean(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  pr<double,int> *f=new pr<double,int>[n];
  NumericVector::iterator xx=x.begin(),rr;
  int i;
  for(;xx!=x.end();++xx,++kk){
    f[*kk-1].first+=*xx;
    f[*kk-1].second++;
  }
  int count_not_zero=0;
  for(i=0;i<n;++i){
    if(f[i].second!=0)
      ++count_not_zero;
  }
  NumericVector res(count_not_zero);
  for(i=0,rr=res.begin();i<n;++i){
    if(f[i].second!=0)
      *rr++=f[i].first/f[i].second;
  }
  delete[] f;
  return res;
}

RcppExport SEXP Rfast_group_mean(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type max_n(max_nSEXP);
    __result = wrap(group_mean(x,group,max_n));
    return __result;
END_RCPP
}


/////////////////////////////////////////////////////////////////////////////


NumericVector group_max(NumericVector x,IntegerVector key,SEXP minn,SEXP maxx){
  int mn,mx;
  const bool is_mn_null=Rf_isNull(minn),is_mx_null=Rf_isNull(minn);
  if(is_mx_null && is_mn_null){
    min_max<int>(key.begin(),key.end(),mn,mx);
  }else if(is_mx_null){
    mn=Rf_asInteger(minn);
    maximum<int>(key.begin(),key.end(),mx);
  }else if(is_mn_null){
    mx=Rf_asInteger(maxx);
    minimum<int>(key.begin(),key.end(),mn);
  }else{
    mn=Rf_asInteger(minn);
    mx=Rf_asInteger(maxx);
  }
  IntegerVector::iterator kk=key.begin();
  NumericVector f(mx-mn+1,double(INT_MIN));
  NumericVector::iterator xx=x.begin(),ff=f.begin(),rr;
  vector<bool>::iterator ok;
  int index;
  for(;xx!=x.end();++xx,++kk){
    index = *kk-mn;
    f[index] = std::max(f[index],*xx);
  }
  int number_of_values=0;
  for(auto v : f){
    if(v!=INT_MIN)
      ++number_of_values;
  }
  NumericVector res(number_of_values);
  for(rr=res.begin(),ff=f.begin();ff!=f.end();++ff){
    if(*ff!=INT_MIN)
      *rr++=*ff;
  }
  return res;
}

RcppExport SEXP Rfast_group_max(SEXP xSEXP,SEXP groupSEXP,SEXP minn,SEXP maxx) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    __result = wrap(group_max(x,group,minn,maxx));
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////////////


static double mean_ad(NumericVector::iterator first,NumericVector::iterator last){
  const int n=first-last+1;
  const double mn=std::accumulate(first,last,0.0)/n;
  double s=0;
  NumericVector::iterator ff=first;
  for(;ff!=last;++ff)
    s+=abs(*ff-mn);
  return s/n;
}

static double med_ad(NumericVector::iterator first,NumericVector::iterator last){
  const double md=med_helper<NumericVector>(first,last);
  const double center = 1.4826;
  NumericVector::iterator ff=first;
  for(;ff!=last;++ff)
    *ff=abs(*ff-md);
  return med_helper<NumericVector>(first,last)*center;
}

//[[Rcpp::export]]
NumericVector group_mad(NumericVector x,IntegerVector group,const string method){
  const int n=x.size(),n_1=n+1;
  IntegerVector::iterator kk=group.begin();
  NumericVector f(n);
  pr<double,int> *y=new pr<double,int>[n_1];
  pr<int,int> *ind=new pr<int,int>[n];
  int i,j=0,c=0;
  NumericVector::iterator xx=x.begin();
  for(i=0;xx!=x.end();++xx,++kk,++i){
    y[i].first=*xx;
    y[i].second=*kk-1;
  }
  y[n]=pr<double,int>();
  sort(y,y+n,my_compare_order_second);
  for(i=1;i<n_1;++i){
    if(y[j].second!=y[i].second){
      ind[y[j].second].first=j;
      ind[y[j].second].second=i;
      ind[y[j].second].is_good=true;
      ++c;
      j=i;
    }
    f[i-1]=y[i-1].first;
  }
  NumericVector F(c);
  if(method=="median"){
  for(int i=0,k=0;i<n;++i){
    if(ind[i].is_good){
      F[k++]=med_ad(f.begin()+ind[i].first,f.begin()+ind[i].second);
    }
  }
  }else if(method=="mean"){
  for(int i=0,k=0;i<n;++i){
    if(ind[i].is_good){
      F[k++]=mean_ad(f.begin()+ind[i].first,f.begin()+ind[i].second);
    }
  }
  }
  delete[] y;
  delete[] ind;
  return F;
}
 


RcppExport SEXP Rfast_group_mad(SEXP xSEXP,SEXP groupSEXP,SEXP methodSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    __result = wrap(group_mad(x,group,method));
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////////////


LogicalVector group_any(LogicalVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  pr<int,int> *y=new pr<int,int>[n];
  int i,c=0,k;
  LogicalVector::iterator xx=x.begin();
  for(;xx!=x.end();++xx,++kk){
    k=*kk-1;
    y[k].first+=*xx;
    y[k].is_good=true;
  }
  for(i=0;i<n;++i){
    if(y[i].is_good){
      ++c;
    }
  }
  LogicalVector F(c);
  for(i=0,k=0;i<n;++i){
    if(y[i].is_good){
      F[k++]= y[i].first>0;
    }
  }
  delete[] y;
  return F;
}


RcppExport SEXP Rfast_group_any(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalVector >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type group(groupSEXP);
    traits::input_parameter< const int >::type n(nSEXP);
    __result = wrap(group_any(x,group,n));
    return __result;
END_RCPP
}

//////////////////////////////////////////////////////////////////////////


struct var_h{
  double x2;
  double x;
  int n;
  bool is_good;
  var_h() : x2(0), x(0), n(0), is_good(false) {}
};

NumericVector group_var(NumericVector x,IntegerVector group,const int n){
  IntegerVector::iterator kk=group.begin();
  var_h *y=new var_h[n];
  int i,c=0,k;
  double xxx;
  NumericVector::iterator xx=x.begin();
  for(i=0;xx!=x.end();++xx,++kk,++i){
    k=*kk-1;
    xxx=*xx;
    y[k].x2+=xxx * xxx;
    y[k].x+=xxx;
    y[k].n++;
    y[k].is_good=true;
  }
  for(i=0;i<n;++i){
    if(y[i].is_good){
      y[i].x*=y[i].x;
      ++c;
    }
  }
  NumericVector F(c);
  double x2,x1;
  int ni;
  for(i=0,k=0;i<n;++i){
    if(y[i].is_good){
      x2=y[i].x2;
      x1=y[i].x;
      ni=y[i].n;
      F[k++]=(x2-x1/ni)/(ni-1);
    }
  }
  delete[] y;
  return F;
}


RcppExport SEXP Rfast_group_var(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP) {
  BEGIN_RCPP
  RObject __result;
  RNGScope __rngScope;
  traits::input_parameter< NumericVector >::type x(xSEXP);
  traits::input_parameter< IntegerVector >::type group(groupSEXP);
  traits::input_parameter< const int  >::type n(nSEXP);
  __result = wrap(group_var(x,group,n));
  return __result;
  END_RCPP
}

///////////////////////////////////////////////////////////////////////////
