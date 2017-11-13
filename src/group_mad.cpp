// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace Rcpp;


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
  const double md=med_helper(first,last);
  NumericVector::iterator ff=first;
  for(;ff!=last;++ff)
    *ff=abs(*ff-md);
  return med_helper(first,last)*center;
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
