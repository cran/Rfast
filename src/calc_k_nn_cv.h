#ifndef _calc_k_nn_cv_h_
#define _calc_k_nn_cv_h_

#include <iostream>
#include <RcppArmadillo.h>
#include "calc_k_nn.h"

// [[Rcpp::depends("RcppArmadillo")]]

Rcpp::List calc_k_nn_cv(Rcpp::List& folds, arma::vec& y, arma::mat& ds, arma::uvec& idxs, 
		const std::string dist_type, const std::string type, const std::string method,
		const unsigned int freq_option, const bool pred_ret);

#endif
