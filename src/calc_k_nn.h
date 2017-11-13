#ifndef _calc_k_nn_h_
#define _calc_k_nn_h_

#include <algorithm>
#include <vector>
#include <unordered_map>
#include <random>
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends("RcppArmadillo")]]

arma::mat calc_k_nn(arma::mat& ds_extra, arma::vec& y, arma::mat& ds, arma::uvec& idxs,
		const std::string dist_type, const std::string type, const std::string method,
		const unsigned int freq_option, const bool mem_eff);

#endif
