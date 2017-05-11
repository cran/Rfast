// Author: 	Marios Dimitriadis
// Contact: kmdimitriadis@gmail.com

#include <iostream>
#include <RcppArmadillo.h>
#include "lgstc_pssn.h"

#define DEBUG 0
#define db_print(...) \
		do { if (DEBUG) Rprintf(__VA_ARGS__); } while (0)

// [[Rcpp::depends("RcppArmadillo")]]

struct ms_t {
	double sum;
	double gt_one_lsum;
	double gt_one_gsum;
	double mean;
	double add;
};

Rcpp::NumericMatrix finalize(Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics,
		Rcpp::IntegerVector& y, const double add);

double calc_glm_type(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds,  
		const double gt_one_lsum, const double mean, const bool is_logistic);

Rcpp::NumericMatrix form_cmat(Rcpp::NumericMatrix& ds,
		Rcpp::IntegerVector& rows, Rcpp::IntegerVector& cols);

Rcpp::IntegerVector app_val(Rcpp::IntegerVector& data, const int elem);

// Alters prev_dist, idxs, stats, pvalues, bics
bool update_vals_end(Rcpp::NumericVector& min_min_col, std::vector<bool>& used_cols,  
		double& prev_dist, const double log_sig, 
		const double tol,  const double log_n, const int step,
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics);	

Rcpp::NumericVector calc_min(Rcpp::NumericVector& data);

Rcpp::NumericVector calc_type_only(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds, 
		const double gt_one_lsum, const double mean, const bool is_logistic);

// Alters ms
double calc_base_dist(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds, 
		const bool is_logistic, ms_t& ms); 

// Alters prev_dist, ms
bool calc_base(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds, const bool is_logistic, 
		std::vector<bool>& used_cols, double& prev_dist, 
		const double log_sig, const double tol, const double log_n, 
		const int step, ms_t& ms, 
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats,
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics);

// [[Rcpp::export]]
Rcpp::NumericMatrix fs_reg(Rcpp::IntegerVector y, Rcpp::NumericMatrix ds, 
		const double sig, const double tol, 
		const std::string type) {
	const bool is_logistic = !type.compare("logistic");							   
	const double log_sig = std::log(sig);
	const int adj_min_dim = std::min(ds.nrow(), ds.ncol()) - 15;
	std::vector<bool> used_cols(ds.ncol());
	Rcpp::IntegerVector idxs;
	Rcpp::NumericVector stats;
 	Rcpp::NumericVector pvalues;
	Rcpp::NumericVector bics;
	double log_n = std::log(ds.nrow());
	int step = 1;
	bool end = false;
	double prev_dist = 0;
	ms_t ms;
	Rcpp::IntegerVector rows = Rcpp::Range(0, ds.nrow() - 1);

	db_print("Calculating base.\n");
	end = calc_base(y, ds, is_logistic, used_cols, prev_dist, log_sig, tol, log_n, step, ms, idxs, stats, pvalues, bics);
	step++;
	if (end) {
		db_print("end\n");
		return finalize(idxs, stats, pvalues, bics, y, ms.add);
	}
	db_print("Entering main loop.\n");
	while (step < adj_min_dim) {
		db_print("step < adj_min_dim\n");
		db_print("step == %d\n", step);
		Rcpp::NumericVector min_min_col(2);
		min_min_col[0] = -1;
		for (int i = 0; i < ds.ncol(); i++) {
			if (used_cols[i]) {
				continue;
			}
			Rcpp::IntegerVector cols = app_val(idxs, i); 
			Rcpp::NumericMatrix rh = form_cmat(ds, rows, cols);
			const double dist_i = calc_glm_type(y, rh, ms.gt_one_lsum, ms.mean, is_logistic);
			if (min_min_col[0] == -1 || dist_i < min_min_col[1]) {
				min_min_col[0] = i;
				min_min_col[1] = dist_i;
			}
		}
		db_print("Updating values.\nChecking end conditions.\n");
		end = update_vals_end(min_min_col, used_cols, 
				prev_dist, log_sig, tol, log_n, step, 
				idxs, stats, pvalues, bics);
		if (end) {
			db_print("end\n");
			return finalize(idxs, stats, pvalues, bics, y, ms.add);
		}
		step++;
	}
	db_print("Exiting main loop.\n");
	return finalize(idxs, stats, pvalues, bics, y, ms.add);
}

bool calc_base(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds, const bool is_logistic, 
		std::vector<bool>& used_cols, double& prev_dist, 
		const double log_sig, const double tol, const double log_n, 
		const int step, ms_t& ms, 
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats,
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics) {
	db_print("Calculating base dist.\n");
	prev_dist = calc_base_dist(y, ds, is_logistic, ms);
	db_print("Calculating dist.\n");
	Rcpp::NumericVector dist = calc_type_only(y, ds, ms.gt_one_lsum, ms.mean, is_logistic);
	db_print("Finding min of dist.\n");
	Rcpp::NumericVector min_min_col = calc_min(dist);
	db_print("Updating values.\nChecking end conditions.\n");
	return update_vals_end(min_min_col, used_cols, prev_dist, log_sig, tol, log_n, step, 
			idxs, stats, pvalues, bics);
}

double calc_base_dist(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds, 
		const bool is_logistic, ms_t& ms) {
	double base_dist = 0;
	ms.sum = 0;
	ms.gt_one_lsum = 0;
	ms.gt_one_gsum = 0;
	ms.mean = 0;
	ms.add = 0;
	if (is_logistic) {
		for (int i = 0; i < y.size(); i++) {
			ms.sum += y[i];
		}
		ms.mean = ms.sum / ds.nrow();
		base_dist = -2  * (ms.sum * std::log(ms.mean) + (ds.nrow() - ms.sum) * std::log(1 - ms.mean));
	}
	else {
		for (int i = 0; i < y.size(); i++) {
			ms.sum += y[i];
			if (y[i] > 0) {
				ms.gt_one_lsum += (y[i] * std::log(y[i]));
				ms.gt_one_gsum += std::lgamma(y[i] + 1); }
		}
		ms.mean = ms.sum / y.size();
		double lsum = 0;
		for (int i = 0; i < y.size(); i++) {
			if (y[i] > 0) {
				lsum += y[i] * std::log(y[i] / ms.mean);
			}
		}
		ms.add = -2 * (ms.gt_one_lsum - ms.sum - ms.gt_one_gsum);
		base_dist = 2 * lsum; 
	}
	return base_dist;
}

Rcpp::NumericVector calc_type_only(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds, 
		const double gt_one_lsum, const double mean, const bool is_logistic) {
	Rcpp::NumericVector ynum = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(y));
	if (is_logistic) {
		return logistic_only_h(ds, ynum, mean);
	}
	return poisson_only_h(ds, ynum, gt_one_lsum, mean);
}

Rcpp::NumericVector calc_min(Rcpp::NumericVector& data) {
	if (!data.size()) {
		db_print("!data.size()\n");
		db_print("Exiting...\n");
		stop("");
	}
	Rcpp::NumericVector min_min_col(2);
	min_min_col[0] = -1;
	for (int i = 0; i < data.size(); i++) {
		if (min_min_col[0] == -1 || data[i] < min_min_col[1]) {
			min_min_col[0] = i;
			min_min_col[1] = data[i];
		}
	}
	return min_min_col;
}

bool update_vals_end(Rcpp::NumericVector& min_min_col, std::vector<bool>& used_cols,  
		double& prev_dist, const double log_sig, 
		const double tol,  const double log_n, const int step,
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics) {	
	if (min_min_col[0] == -1) {
	db_print("min_min_col[0] == -1\n");
	return true;
	}
	db_print("Calculating statistic.\n");
	const double stat = prev_dist - min_min_col[1];
	db_print("Calculating pvalue.\n");
	const double pvalue = R::pchisq(stat, 1, false, true);
	if (pvalue >= log_sig) {
	db_print("pvalue >= log_sig\n");
	return true;
	}
	db_print("Calculating BIC.\n");
	const double bic = min_min_col[1] + (step + 1) * log_n;
	if (bics.size() && (bics[bics.size() - 1] - bic) <= tol) {
	db_print("bics.size() && (bics[bics.size() - 1] - bic) <= tol\n");
	return true;
	}
	db_print("Updating.\n");
	used_cols[min_min_col[0]] = true;
	idxs.push_back(min_min_col[0]);
	bics.push_back(bic);
	stats.push_back(stat);
	pvalues.push_back(pvalue);
	prev_dist = min_min_col[1];
	return false;
}					 

Rcpp::IntegerVector app_val(Rcpp::IntegerVector& data, const int elem) {
	Rcpp::IntegerVector app_data(data.size() + 1);
	int i;
	for (i = 0; i < data.size(); i++) {
		app_data[i] = data[i];
	}
	app_data[i] = elem;
	return app_data;
}

Rcpp::NumericMatrix form_cmat(Rcpp::NumericMatrix& ds,
		Rcpp::IntegerVector& rows, Rcpp::IntegerVector& cols) {
	Rcpp::NumericMatrix formed_ds(rows.size(), cols.size());
	for (int i = 0; i < rows.size(); i++) {
		for (int j = 0; j < cols.size(); j++) {
			formed_ds(i, j) = ds(rows[i], cols[j]);
		}
	}
	return formed_ds;
}

double calc_glm_type(Rcpp::IntegerVector& y, Rcpp::NumericMatrix& ds,  
		const double gt_one_lsum, const double mean, const bool is_logistic) {
	Rcpp::NumericVector y_adj = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(y));
	if (is_logistic) {
		return glm_logistic_h(ds, y_adj, mean);
	}
	return glm_poisson_h(ds, y_adj, gt_one_lsum, mean);
}

Rcpp::NumericMatrix finalize(Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics, 
		Rcpp::IntegerVector& y, const double add) {
	Rcpp::NumericMatrix ret(idxs.size(), 4);
	for (int i = 0; i < idxs.size(); i++) {
		ret(i, 0) = idxs[i] + 1;
		ret(i, 1) = pvalues[i];
		ret(i, 2) = stats[i];
		ret(i, 3) = bics[i] + add;
	}
	return ret;
}
								

RcppExport SEXP Rfast_fs_reg(SEXP ySEXP,SEXP dsSEXP,SEXP sigSEXP,SEXP tolSEXP,SEXP typeSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< IntegerVector >::type y(ySEXP);
    traits::input_parameter< NumericMatrix>::type ds(dsSEXP);
    traits::input_parameter< const double >::type sig(sigSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const string >::type type(typeSEXP);
    __result = wrap(fs_reg(y,ds,sig,tol,type));
    return __result;
END_RCPP
}
