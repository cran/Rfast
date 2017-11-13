//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "reg_lib2.h"
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
