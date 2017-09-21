#ifndef _calc_pc_skel_h_
#define _calc_pc_skel_h_

#include <iostream>
#include <string>
#include <ctime>
#include <RcppArmadillo.h>
#include "cts.h"

// [[Rcpp::depends("RcppArmadillo")]]

Rcpp::List calc_pc_skel(arma::mat& ds, const std::string method, const double sig, const unsigned int r);

#endif
