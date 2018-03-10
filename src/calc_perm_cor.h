#ifndef CALC_PERM_COR_H
#define CALC_PERM_COR_H

#include <iostream>
#include <cmath>
#include <omp.h>
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends("RcppArmadillo")]]

arma::vec calc_perm_cor(arma::vec& x, arma::vec& y, const unsigned int r);

#endif
