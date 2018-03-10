#ifndef _only_glm_h_
#define _only_glm_h_

#include <vector>
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends("RcppArmadillo")]]

using namespace std;
using namespace arma;
using namespace Rcpp;


NumericVector logistic_only(NumericMatrix& X, NumericVector& Y, const double my);

NumericVector poisson_only(NumericMatrix& X, NumericVector& Y, const double ylogy, const double my);

double glm_logistic(NumericMatrix& X, NumericVector& Y, const double my);

double arma_glm_logistic(mat x, vec y, const double my);

double glm_poisson(NumericMatrix& X, NumericVector& Y, const double ylogy, const double my);

double arma_glm_poisson(mat x, vec y, const double ylogy, const double my);

NumericVector qs_binom_only(NumericMatrix& X, NumericVector& Y, const double my);

NumericVector glm_qs_binom(NumericMatrix& X, NumericVector& Y, const double my);

NumericVector qs_poisson_only(NumericMatrix& X, NumericVector& Y, const double ylogy, const double my);

NumericVector glm_qs_poisson(NumericMatrix& X, NumericVector& Y, const double ylogy, const double my);

#endif
