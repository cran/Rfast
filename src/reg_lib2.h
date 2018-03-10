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
double weighted_average_value(vec, a_node*, const int);
double weighted_most_frequent_value(vec, a_node*, const int);
double calc_neg_ll(vec, vec, vec, int);
double lrfit2(mat &, vec &, vec &, vec &, vec &, const double,const double, const int);
double bc2helper(double, vec, vec, double, double, double, double);
vec removeIthCol(vec, const int, const int);
double calc_manhatan(double *, double *, const int);
double calc_eucl(double *, double *, const int);
mat vecdist_arma(vec &, const int);
vec vec_vecdist_arma(vec &, const int);


#endif
