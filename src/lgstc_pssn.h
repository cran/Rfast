#ifndef _lgstc_pssn_h_
#define _lgstc_pssn_h_

#include <RcppArmadillo.h>
#include <vector>

// [[Rcpp::depends("RcppArmadillo")]]

using namespace std;
using namespace arma;
using namespace Rcpp;

Rcpp::NumericVector logistic_only_h(Rcpp::NumericMatrix X, Rcpp::NumericVector Y, const double my);

Rcpp::NumericVector poisson_only_h(Rcpp::NumericMatrix X, Rcpp::NumericVector Y, const double ylogy, const double my);

double glm_logistic_h(Rcpp::NumericMatrix X, Rcpp::NumericVector Y, const double my);

double glm_poisson_h(Rcpp::NumericMatrix X, Rcpp::NumericVector Y, const double ylogy, const double my);

#endif
