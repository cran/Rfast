#ifndef _calc_bs_reg_h_
#define _calc_bs_reg_h_

#include <RcppArmadillo.h>
#include "only_glm.h"

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends("RcppArmadillo")]]

Rcpp::List calc_bs_reg(arma::vec& y, arma::mat& ds, const double sig, const std::string type);

#endif
