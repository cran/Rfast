//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "reg_lib2.h"
#include "reg_lib.h"
#include "my_k_sorted_array.h"
#include <vector>

using namespace std;
using namespace arma;
using namespace Rcpp;

//[[Rcpp::plugins(openmp)]]
//[[Rcpp::plugins(cpp11)]]

double most_frequent_value(vec y, a_node* my_ar, const int size){
  std::map<int,int> counts;
  a_node* tmp = my_ar;
  for(int i=0;i<size;i++,tmp++)
    counts[(int)y(tmp->index)]++;

  map<int, int>::iterator tmpit;
  int mostFrequent = -1;
  int maxCount = 0;
  for(tmpit = counts.begin(); tmpit!= counts.end(); tmpit++) {
    if(tmpit->second > maxCount) {
      mostFrequent = tmpit->first;
      maxCount = tmpit->second;
    }
  }
  return mostFrequent;
}

double average_value(vec y, a_node* my_ar, const int size){
  double sum=0.0;
  a_node* it = my_ar;
  for(int i=0;i<size;i++,it++)
    sum+= y(it->index);

  return sum/size;
}

double weighted_average_value(vec y, a_node* my_ar, const int size){
  double sum=0.0, divider = 0.0;
  a_node* it = my_ar;
  for(int i=0;i<size;i++,it++){
      sum+= y(it->index)/exp(it->value);
      divider+=1/exp(it->value);
  }

  return sum/divider;
}

double weighted_most_frequent_value(vec y, a_node* my_ar, const int size){
  std::map<int,double> counts;
  a_node* tmp = my_ar;
  for(int i=0;i<size;i++,tmp++){
      counts[(int)y(tmp->index)]+=1/exp(tmp->value);

  }

  map<int, double>::iterator tmpit;
  int mostFrequent = -1;
  double maxCount = 0;
  for(tmpit = counts.begin(); tmpit!= counts.end(); tmpit++) {
    if(tmpit->second > maxCount) {
      mostFrequent = tmpit->first;
      maxCount = tmpit->second;
    }
  }
  return mostFrequent;
}


double calc_neg_ll(vec wx, vec expwx, vec y, int size){
  double sum = 0.0;
  vec::iterator wit = wx.begin(), yit = y.begin();
  for(int i=0;i<size;i++,wit++,yit++){
    if(*wit<=30)
      sum+=(*yit-1)*(*wit)+log(expwx[i]);
    else
      sum+=(*yit)*(*wit);
  }
  return sum;
}

double bc2helper(double lambda, vec x, vec tmp, double vlx, double slx, double n2, double size) {
  double s;
  if ( abs(lambda) < 1e-12 )
    s = vlx;
  else
    s = var(my_pow2(x,tmp,lambda,size)) / (lambda*lambda);

  return n2 * log(s) + lambda * slx;
}

vec removeIthCol(vec selectedg, const int which, const int selectedColumnSize){
  for(int i=which+1;i<selectedColumnSize;i++)
    selectedg[i-1] = selectedg[i];
  selectedg.resize(selectedColumnSize-1);
  return selectedg;
}

double calc_manhatan(double *ait, double *bit, const int size){
  double tmpval, ret = 0.0;
  for(int i = 0; i< size; i++,bit++,ait++){
    tmpval = *bit - *ait;
    if(tmpval > 0)
      ret+=tmpval;
    else
      ret-=tmpval;
  }
  return ret;
}

double calc_eucl(double *ait, double *bit, const int size){
  double tmpval, ret = 0.0;
  for(int i = 0; i< size; i++,bit++,ait++){
    tmpval = *bit - *ait;
    ret+=tmpval*tmpval;
  }
  return sqrt(ret);
}

mat vecdist_arma(vec &x, const int p){
  mat f(p,p);

  int i,j;
  double res;
  for(i = 0; i< p; i++)
    for(j = i+1; j<p;j++){
      res = x[i]-x[j];
      if(res>0){
        f(i,j) = res;
        f(j,i) = res;
      }
      else{
        f(i,j) = -res;
        f(j,i) = -res;
      }
    }
  f.diag().zeros();
  return f;
}

vec vec_vecdist_arma(vec &x, const int p){
  vec f(p*(p-1)/2);
  vec::iterator iter = f.begin();
  int i,j;
  double res;
  for(i = 0; i< p; i++)
    for(j = i+1; j<p;j++){
      res = x[i]-x[j];
      if(res>0){
        *iter = res;
      }
      else{
        *iter = -res;
      }
      iter++;
    }

  return f;
}
