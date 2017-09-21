// Author: 	Marios Dimitriadis
// Contact: kmdimitriadis@gmail.com

#include "calc_k_nn_cv.h"

#define DEBUG 0
#define db_print(...) \
	do { if (DEBUG) Rprintf(__VA_ARGS__); } while (0)

arma::vec calc_sqrt_sum(arma::mat& ds, arma::vec& vals) {
	arma::vec sums(ds.n_cols, arma::fill::zeros);
	for (unsigned int j = 0; j < ds.n_cols; ++j) {
		for (unsigned int i = 0; i < ds.n_rows; ++i) {
			sums[j] += std::pow((ds.at(i, j) - vals[i]), 2);
		}
	}
	return sums;
}

arma::vec calc_eq_sum(arma::mat& ds, arma::vec& vals) {
	arma::vec sums(ds.n_cols, arma::fill::zeros);
	for (unsigned int j = 0; j < ds.n_cols; ++j) {
		for (unsigned int i = 0; i < ds.n_rows; ++i) {
			if (ds.at(i, j) == vals[i]) {
				++sums[j];
			}
		}
	}
	return sums;
}

// Alters idxs, idxs_inc, idxs_exc
void store_idxs(arma::uvec& idxs, arma::uvec& idxs_exc, const unsigned int total_size) {
	for (unsigned int i = 0, m = 0; i < total_size; ++i) {
		if (arma::all(idxs != i)) {
			idxs_exc[m++] = i;
		}
	}
}

Rcpp::List calc_k_nn_cv(Rcpp::List& folds, arma::vec& y, arma::mat& ds, arma::uvec& idxs, 
		const std::string dist_type, const std::string type, const std::string method,
		const unsigned int freq_option, const bool pred_ret) { 
	Rcpp::List preds;
	unsigned int curr_fold = 1;
	arma::vec crit(idxs.size(), arma::fill::zeros);
	idxs -= 1;
	db_print("Forming prediction list, calculating column summaries.\n");
	for (Rcpp::List::iterator it = folds.begin(); it != folds.end(); ++it) {
		db_print("Altering base indexes.\n");
		arma::uvec curr_idxs = *it; curr_idxs -= 1;
		db_print("Storing indexes.\n");
		arma::uvec curr_idxs_exc(ds.n_rows - curr_idxs.size());
		store_idxs(curr_idxs, curr_idxs_exc, ds.n_rows);
		db_print("Calculating knn.\n");
		arma::mat fd_ds_extra = ds.rows(curr_idxs);
		arma::vec fd_y = y.elem(curr_idxs_exc);
		arma::mat fd_ds = ds.rows(curr_idxs_exc);
		arma::mat pred = calc_k_nn(fd_ds_extra, fd_y, fd_ds, idxs, dist_type, type, method, freq_option);
		db_print("Calculating column summaries.\n");
		arma::vec fd_y_inc = y.elem(curr_idxs);
		if (!type.compare("C")) {
			crit += calc_eq_sum(pred, fd_y_inc);
		}
		else {
			crit += calc_sqrt_sum(pred, fd_y_inc);
		}
		db_print("Updating list.\n");
		std::ostringstream oss;
		oss << curr_fold++;
		preds[oss.str()] = pred;
	}
	db_print("Constructing return list.\n");
	Rcpp::List ret;
	ret["crit"] = crit / y.size();
	if (pred_ret) {
		ret["pred"] = preds;
	}
	return ret;
}
