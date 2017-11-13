// Author:  Marios Dimitriadis
// Contact: kmdimitriadis@gmail.com

#include "calc_bs_reg.h"

#define DEBUG 0
#define db_print(...) \
	do { if (DEBUG) Rprintf(__VA_ARGS__); } while (0)

#define DEBUG_L 0
#define dbl_print(...) \
	do { if (DEBUG_L) Rprintf(__VA_ARGS__); } while (0)

#define RES_ROWS 1
#define RES_COLS 3
#define IDX_POS 0
#define VAL_POS 1

struct ms_t {
	double sum;
	double gt_one_lsum;
	double mean;
};

static arma::uvec adj_idxs(arma::uvec& idxs, std::vector<bool>& idxs_used, const unsigned int idxs_used_cntr) {
	int idxs_act_cntr = idxs.size() - idxs_used_cntr;
	idxs_act_cntr = std::abs(idxs_act_cntr);
	arma::uvec left_idxs(idxs_act_cntr);
	unsigned int i = 0;
	unsigned int j = 0;
	while (idxs_act_cntr) {
		if (!idxs_used[i]) {
			left_idxs[j++] = idxs[i] + 1;
			--idxs_act_cntr;
		}
		++i;
	}
	return left_idxs;
}

static double calc_stat(arma::vec& y, arma::mat& ds, const double dist, ms_t ms, const bool is_logistic) {
	double tmp = 0;
	double init = 0;
	if (is_logistic) {
		tmp = ms.sum / ds.n_rows;
		init = -2 * (ds.n_rows * tmp * std::log(tmp) + (ds.n_rows - ds.n_rows * tmp) * std::log(1 - tmp));
	}
	else {
		tmp = ms.sum / ds.n_rows;
		init = 2 * ms.gt_one_lsum - 2 * ds.n_rows * tmp * std::log(tmp);
	}
	return init - dist;
}

static void adj_vals(arma::vec& types_gen, std::vector<unsigned int>& prev_min_idxs, const double val) {
	for (unsigned int i = 0; i < prev_min_idxs.size(); i++) {
		types_gen[prev_min_idxs[i]] = val;
	}
}

static arma::vec get_idx_min(arma::vec& types_gen) {
	arma::vec idx_min(2);
	idx_min[IDX_POS] = -1;
	for (unsigned int i = 0; i < types_gen.size(); ++i) {
		if (idx_min[IDX_POS] == -1) {
			idx_min[IDX_POS] = i;
			idx_min[VAL_POS] = types_gen[i];
		}
		else if (idx_min[VAL_POS] > types_gen[i]) {
			idx_min[IDX_POS] = i;
			idx_min[VAL_POS] = types_gen[i];
		}
	}
	return idx_min;
}

static unsigned int skip_ahead(arma::uvec& cols, const unsigned int curr) {
	unsigned int i;
	for (i = curr + 1; i < cols.size() && cols(i) == cols(curr); ++i) {
	}
	return i;
}

static arma::mat rm_cols(arma::mat& src, arma::uvec& cols) {
	cols = arma::sort(cols);
	unsigned int dst_nrows = src.n_rows;
	unsigned int dst_ncols = src.n_cols - cols.size();
	arma::mat dst(dst_nrows, dst_ncols);
	unsigned int src_col = 0;
	unsigned int cols_idx = 0;
	for (unsigned int dst_col = 0; dst_col < dst_ncols; ++dst_col) {
		while (src_col < src.n_cols && cols_idx < cols.size() && src_col == cols(cols_idx)) {
			src_col++;
			cols_idx = skip_ahead(cols, cols_idx);
		}
		for (unsigned int row = 0; row < dst_nrows; ++row) {
			dst(row, dst_col) = src(row, src_col);
		}
		++src_col;
	}
	return dst;
}

static arma::vec gen_types(arma::vec& y, arma::mat& ds, arma::uvec& idxs, 
		std::vector<bool> idxs_used, arma::vec& types_gen, 
		std::vector<unsigned int> rm_idxs, const ms_t ms, const bool is_logistic) {
	for (unsigned int i = 0; i < idxs.size(); ++i) {
		if (idxs_used[i]) {
			continue;
		}
		arma::uvec minus_idxs;
		if (!rm_idxs.size()) {
			minus_idxs = {idxs[i]};
		}
		else {
			std::vector<unsigned int> tmp_idxs = rm_idxs;
			tmp_idxs.push_back(idxs[i]);
			minus_idxs = tmp_idxs;
		}
		arma::mat adj_ds = rm_cols(ds, minus_idxs);
		if (is_logistic) {
			types_gen[idxs[i]] = arma_glm_logistic(adj_ds, y, ms.mean);
		}
		else {
			types_gen[idxs[i]] = arma_glm_poisson(adj_ds, y, ms.gt_one_lsum, ms.mean);
		}
	}
	return types_gen;
}

static double gen_type(arma::vec& y, arma::mat& ds, const ms_t ms, const bool is_logistic) {
	if (is_logistic) {
		return arma_glm_logistic(ds, y, ms.mean);
	}
	return arma_glm_poisson(ds, y, ms.gt_one_lsum, ms.mean);
}

static void fill_consec(arma::uvec& idxs) {
	for (unsigned int i = 0; i < idxs.size(); i++) {
		idxs[i] = i;
	}
}

static void upd_ms(arma::vec& y, ms_t& ms, const bool is_logistic) {
	for (unsigned int i = 0; i < y.size(); i++) {
		ms.sum += y[i];
		if (!is_logistic && y[i] > 0) {
			ms.gt_one_lsum += (y[i] * std::log(y[i]));
		}
	}
	ms.mean = ms.sum / y.size();
}

static bool is_type(const std::string type) {
	if (!type.compare("logistic")) {
		return true;
	}
	else if (!type.compare("poisson")) {
		return false;
	}
	stop("Type input invalid. Exiting...\n|");
}

Rcpp::List calc_bs_reg(arma::vec& y, arma::mat& ds, const double sig, const std::string type) {
	const bool is_logistic = is_type(type);
	ms_t ms = {0};
	upd_ms(y, ms, is_logistic);
	const double sig_log = std::log(sig);
	arma::uvec idxs(ds.n_cols);
	fill_consec(idxs);
	std::vector<bool> idxs_used(ds.n_cols);
	std::fill(idxs_used.begin(), idxs_used.end(), false);
	unsigned int idxs_used_cntr = 0;
	arma::vec types_gen(ds.n_cols);
	arma::mat res(RES_ROWS, RES_COLS, arma::fill::zeros);

	db_print("Generating types.\n");
	double type_gen = gen_type(y, ds, ms, is_logistic);
	gen_types(y, ds, idxs, idxs_used, types_gen, std::vector<unsigned int>(), ms, is_logistic);
	arma::vec idx_min = get_idx_min(types_gen);
	double stat = idx_min[VAL_POS] - type_gen;
	db_print("The pre-loop stat is: %f\n", stat);
	double pvalue = R::pchisq(stat, 1, 0, 1);
	db_print("The pre-loop pvalue is: %f\n", pvalue);
	if (pvalue >= sig_log) {
		db_print("pvalue >= sig_log\n");
		unsigned int j = 0;
		res(j, 0) = idx_min[IDX_POS] + 1; res(j, 1) = stat; res(j, 2) = pvalue;
		idxs_used[idx_min[IDX_POS]] = true;
		++idxs_used_cntr;
		std::vector<unsigned int> prev_min_idxs = {(unsigned int) idx_min[IDX_POS]};
		db_print("Entering main loop.\n");
		while (pvalue > sig_log && j < (ds.n_cols - 2) && idxs_used.size() >= idxs_used_cntr) {
			++j;
			type_gen = idx_min[VAL_POS];
			adj_vals(types_gen, prev_min_idxs, type_gen + 500);
			gen_types(y, ds, idxs, idxs_used, types_gen, prev_min_idxs, ms, is_logistic);
			idx_min = get_idx_min(types_gen);
			stat = idx_min[VAL_POS] - type_gen;
			db_print("An in-loop stat is: %f\n", stat);
			pvalue = R::pchisq(stat, 1, 0, 1);
			db_print("An in-loop pvalue is: %f\n", pvalue);
			if (pvalue > sig_log) {
				db_print("pvalue > sig_log\n");
				const unsigned int extra_row = res.n_rows;
				res.resize(extra_row + 1, res.n_cols);
				res(extra_row, 0) = idx_min(IDX_POS) + 1; res(extra_row, 1) = stat; res(extra_row, 2) = pvalue;
				idxs_used[idx_min[IDX_POS]] = true;
				++idxs_used_cntr;
				prev_min_idxs.push_back(idx_min[IDX_POS]);
			}
			if (idxs_used_cntr == (idxs.size() - 1)) {
				db_print("idxs_used_cntr == (idxs.size() - 1)\n");
				arma::uvec left_idxs = adj_idxs(idxs, idxs_used, idxs_used_cntr);
				arma::mat tmp_ds = ds.col(left_idxs[0] - 1);
				const double tmp_dist = gen_type(y, tmp_ds, ms, is_logistic);
				stat = calc_stat(y, ds, tmp_dist, ms, is_logistic);
				pvalue = R::pchisq(stat, 1, 0, 1);
				if (pvalue > sig_log) {
					const unsigned int extra_row = res.n_rows;
					res.resize(extra_row + 1, res.n_cols);
					res(extra_row, 0) = left_idxs[0]; res(extra_row, 1) = stat; res(extra_row, 2) = pvalue;
					++idxs_used_cntr;
				}
			}
		}
	}
	Rcpp::List ret;
	arma::uvec left_idxs = adj_idxs(idxs, idxs_used, idxs_used_cntr);
	ret["info"] = res; ret["vars"] = left_idxs;
	return ret;
}
