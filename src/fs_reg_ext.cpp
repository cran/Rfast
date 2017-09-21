// Author: 	Marios Dimitriadis
// Contact: kmdimitriadis@gmail.com

#include "fs_reg_ext.h"
#include "only_glm.h"

#define DEBUG 0
#define db_print(...) \
	do { if (DEBUG) Rprintf(__VA_ARGS__); } while (0)

static const int g_BIC_POS = 0;
static const int g_STAT_POS = 1;
static const int g_INDEX_POS = 2;

struct ms_t {
	double sum;
	double gt_one_lsum;
	double gt_one_gsum;
	double mean;
	double add;
};

static Rcpp::NumericMatrix finalize(Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics,
		Rcpp::NumericVector& y, const double add);

static Rcpp::NumericVector calc_glm_type(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds,  
		const double gt_one_lsum, const double mean, const bool is_quasi_logistic);

static Rcpp::NumericMatrix form_cmat(Rcpp::NumericMatrix& ds,
		Rcpp::IntegerVector& rows, Rcpp::IntegerVector& cols);

static Rcpp::IntegerVector app_val(Rcpp::IntegerVector& data, const int elem);

// Alters used_cols, idxs, stats, pvalues, bics
static bool update_vals_end(Rcpp::NumericVector& vals, std::vector<bool>& used_cols,  
		const double log_sig, const double tol,  const int step,
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics);

static Rcpp::NumericVector calc_type_only(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds, 
		const double gt_one_lsum, const double mean, const bool is_quasi_logistic);

// Alters ms
static void setup_ms(Rcpp::NumericVector& y, const double log_n, const bool is_quasi_logistic, ms_t& ms);

static bool calc_base(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds, const bool is_quasi_logistic, 
		std::vector<bool>& used_cols, 
		const double log_sig, const double tol, const double log_n, 
		const int step, ms_t& ms, 
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats,
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics);

Rcpp::NumericMatrix fs_reg_ext(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds, 
		const double sig, const double tol, const std::string type) {
	const bool is_quasi_logistic = !type.compare("quasilogistic");							   
	const double log_sig = std::log(sig);
	const int adj_nrows = ds.nrow() - 15;
	std::vector<bool> used_cols(ds.ncol());
	Rcpp::IntegerVector idxs;
	Rcpp::NumericVector stats;
 	Rcpp::NumericVector pvalues;
	Rcpp::NumericVector bics;
	double log_n = std::log(ds.nrow());
	int step = 1;
	bool end = false;
	ms_t ms;
	Rcpp::IntegerVector rows = Rcpp::Range(0, ds.nrow() - 1);

	db_print("Calculating base.\n");
	end = calc_base(y, ds, is_quasi_logistic, used_cols, 
			log_sig, tol, log_n, step, ms, idxs, stats, pvalues, bics);
	++step;
	if (end) {
		db_print("end\n");
		return finalize(idxs, stats, pvalues, bics, y, ms.add);
	}
	db_print("Entering main loop.\n");
	while (step < adj_nrows) {
		db_print("step < adj_nrows\n");
		db_print("step == %d\n", step);
		Rcpp::NumericVector vals = Rcpp::NumericVector::create(0, 0, -1);
		for (int i = 0; i < ds.ncol(); ++i) {
			if (used_cols[i]) {
				continue;
			}
			Rcpp::IntegerVector cols = app_val(idxs, i); 
			Rcpp::NumericMatrix rh = form_cmat(ds, rows, cols);
			Rcpp::NumericVector tmp_vals = calc_glm_type(y, rh, ms.gt_one_lsum, ms.mean, is_quasi_logistic);
			if (vals[g_INDEX_POS] == -1 || tmp_vals[g_STAT_POS] > vals[g_STAT_POS]) {
				vals[g_BIC_POS] = tmp_vals[g_BIC_POS];
				vals[g_STAT_POS] = tmp_vals[g_STAT_POS];
				vals[g_INDEX_POS] = i;
			}
		}
		db_print("Updating values.\nChecking end conditions.\n");
		end = update_vals_end(vals, used_cols, log_sig, tol, step, 
				idxs, stats, pvalues, bics);
		if (end) {
			db_print("end\n");
			return finalize(idxs, stats, pvalues, bics, y, ms.add);
		}
		++step;
	}
	db_print("Exiting main loop.\n");
	return finalize(idxs, stats, pvalues, bics, y, ms.add);
}

static bool calc_base(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds, const bool is_quasi_logistic, 
		std::vector<bool>& used_cols, 
		const double log_sig, const double tol, const double log_n, const int step, ms_t& ms, 
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics) {
	db_print("Setting up ms.\n");
	setup_ms(y, log_n, is_quasi_logistic, ms);
	db_print("Calculating type only.\n");
	Rcpp::NumericVector vals = calc_type_only(y, ds, ms.gt_one_lsum, ms.mean, is_quasi_logistic);
	db_print("Updating values.\nChecking end conditions.\n");
	return update_vals_end(vals, used_cols, log_sig, tol, step, 
			idxs, stats, pvalues, bics);
}

static void setup_ms(Rcpp::NumericVector& y, const double log_n, const bool is_quasi_logistic, ms_t& ms) {
	ms.sum = 0;
	ms.gt_one_lsum = 0;
	ms.gt_one_gsum = 0;
	ms.mean = 0;
	ms.add = log_n;
	if (is_quasi_logistic) {
		for (int i = 0; i < y.size(); ++i) {
			ms.sum += y[i];
			ms.mean = ms.sum / y.size();
		}
	}
	else {
		for (int i = 0; i < y.size(); ++i) {
			ms.sum += y[i];
			if (y[i] > 0) {
				ms.gt_one_lsum += (y[i] * std::log(y[i]));
				ms.gt_one_gsum += std::lgamma(y[i] + 1); }
		}
		ms.mean = ms.sum / y.size();
		ms.add = -2 * (ms.gt_one_lsum - ms.sum - ms.gt_one_gsum);
	}
}

static Rcpp::NumericVector calc_type_only(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds, 
		const double gt_one_lsum, const double mean, const bool is_quasi_logistic) {
	if (is_quasi_logistic) {
		return qs_binom_only(ds, y, mean);
	}
	return qs_poisson_only(ds, y, gt_one_lsum, mean);
}

static bool update_vals_end(Rcpp::NumericVector& vals, std::vector<bool>& used_cols,  
		const double log_sig, const double tol, const int step,
		Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics) {	
	const double pvalue = R::pchisq(vals[g_STAT_POS], 1, false, true);
	if (pvalue >= log_sig) {
		db_print("pvalue < log_sig\n");
		return true;
	}
	if (bics.size() && (bics[bics.size() - 1] - vals[g_BIC_POS]) <= tol) {
		db_print("bics.size() && (bics[bics.size() - 1] - bic) <= tol\n");
		return true;
	}
	db_print("Updating.\n");
	used_cols[vals[g_INDEX_POS]] = true;
	idxs.push_back(vals[g_INDEX_POS]);
	bics.push_back(vals[g_BIC_POS]);
	stats.push_back(vals[g_STAT_POS]);
	pvalues.push_back(pvalue);
	return false;
}					 

static Rcpp::IntegerVector app_val(Rcpp::IntegerVector& data, const int elem) {
	Rcpp::IntegerVector app_data(data.size() + 1);
	int i;
	for (i = 0; i < data.size(); ++i) {
		app_data[i] = data[i];
	}
	app_data[i] = elem;
	return app_data;
}

static Rcpp::NumericMatrix form_cmat(Rcpp::NumericMatrix& ds,
		Rcpp::IntegerVector& rows, Rcpp::IntegerVector& cols) {
	Rcpp::NumericMatrix formed_ds(rows.size(), cols.size());
	for (int i = 0; i < rows.size(); ++i) {
		for (int j = 0; j < cols.size(); ++j) {
			formed_ds(i, j) = ds(rows[i], cols[j]);
		}
	}
	return formed_ds;
}

static Rcpp::NumericVector calc_glm_type(Rcpp::NumericVector& y, Rcpp::NumericMatrix& ds,  
		const double gt_one_lsum, const double mean, const bool is_quasi_logistic) {
	if (is_quasi_logistic) {
		return glm_qs_binom(ds, y, mean);
	}
	return glm_qs_poisson(ds, y, gt_one_lsum, mean);
}

static Rcpp::NumericMatrix finalize(Rcpp::IntegerVector& idxs, Rcpp::NumericVector& stats, 
		Rcpp::NumericVector& pvalues, Rcpp::NumericVector& bics, 
		Rcpp::NumericVector& y, const double add) {
	Rcpp::NumericMatrix ret(idxs.size(), 4);
	for (int i = 0; i < idxs.size(); ++i) {
		ret(i, 0) = idxs[i] + 1;
		ret(i, 1) = pvalues[i];
		ret(i, 2) = stats[i];
		ret(i, 3) = bics[i] + add;
	}
	return ret;
}
