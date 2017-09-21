#ifndef _fs_reg_st_h_
#define _fs_reg_st_h_

#include <RcppArmadillo.h>

// [[Rcpp::depends("RcppArmadillo")]]

Rcpp::NumericMatrix fs_reg_st(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds, 
		const double sig, const double tol, const std::string type);

#endif
