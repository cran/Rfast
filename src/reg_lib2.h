//Author: Stefanos Fafalios

#ifndef _reg_lib_2_
#define _reg_lib_2_

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "my_k_sorted_array.h"

using namespace std;
using namespace arma;
using namespace Rcpp;

double most_frequent_value(vec, a_node*, const int);
//uvec k_smallest_values_indexes(vec, const int);
double average_value(vec, a_node*, const int);


#endif
