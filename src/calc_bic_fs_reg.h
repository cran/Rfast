#ifndef _calc_bic_fs_reg_h_
#define _calc_bic_fs_reg_h_

#include "only_glm.h"
#include <RcppArmadillo.h>

// [[Rcpp::depends("RcppArmadillo")]]

Rcpp::NumericMatrix calc_bic_fs_reg(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds, 
		const double tol, const std::string type);

#endif
