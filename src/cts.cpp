#include "cts.h"

#define DEBUG 0
#define db_print(...) \
	do { if (DEBUG) Rprintf(__VA_ARGS__); } while (0)

class TestResult {
public:
    double pvalue;
    double logpvalue;
    double stat;
    int df;

    TestResult(double _pvalue, double _stat, double _logpvalue, int _df) {
        pvalue=_pvalue;
        stat=_stat;
        logpvalue=_logpvalue;
        df=_df;
    }
};

TestResult g2_test(arma::mat& data, const unsigned int x, const unsigned int y, 
		arma::uvec& cs, const unsigned int ncs, arma::uvec& dc);

static double g2_statistic(arma::uvec& counts, 
		const unsigned int xdim, const unsigned int ydim);

TestResult g2_test(arma::mat& data, const unsigned int x, const unsigned int y, arma::uvec& dc);

// Author: Giorgos Borboudakis
Rcpp::List g2_test_univ(arma::mat& data, arma::uvec& dc) {
	const unsigned int nvars = data.n_cols;
    unsigned int nout = nvars * (nvars - 1) / 2;
	arma::uvec xout(nout);
	arma::uvec yout(nout);
	arma::vec statistics(nout);
	arma::vec df(nout);

    unsigned int idx = 0;
    for(unsigned int i = 0; i < nvars; ++i) {
        for(unsigned int j = i + 1; j < nvars; ++j) {
            TestResult result = g2_test(data, i, j, dc);
            xout.at(idx) = i;
            yout.at(idx) = j;
            statistics.at(idx) = result.stat;
            df.at(idx) = (dc.at(i) - 1) * (dc.at(j) - 1);
            ++idx;
        }
    }

    Rcpp::List out;
    out["statistic"] = statistics;
    out["x"] = xout;
    out["y"] = yout;
    out["df"] = df;
    return out;
}

TestResult g2_test(arma::mat& data, const unsigned int x, const unsigned int y, arma::uvec& dc) {
	const unsigned int xdim = dc.at(x);
	const unsigned int ydim = dc.at(y);
	arma::uvec counts(xdim * ydim, arma::fill::zeros);

	for (unsigned int i = 0; i < data.n_rows; ++i) {
		const unsigned int curx = (unsigned int) data.at(i, x);
		const unsigned int cury = (unsigned int) data.at(i, y);
		counts(cury * xdim + curx)++;
	}
	const int df = (xdim - 1) * (ydim - 1);
	const double statistic = g2_statistic(counts, xdim, ydim);

	return TestResult(0, statistic, 0, df);
}

static double g2_statistic(arma::uvec& counts, 
		const unsigned int xdim, const unsigned int ydim) {
	if (arma::all(counts == 0)) {
		return 0;
	}
	double statistic = 0;
	int countsXY = 0;
	arma::uvec countsX(xdim, arma::fill::zeros);
	arma::uvec countsY(ydim, arma::fill::zeros);

	for (unsigned int x = 0; x < xdim; ++x) {
		for (unsigned int y = 0; y < ydim; ++y) {
			const unsigned int curcounts = counts.at(y * xdim + x);
			countsXY += curcounts;
			countsX.at(x) += curcounts;
			countsY.at(y) += curcounts;
		}
	}

	for (unsigned int x = 0; x < xdim; ++x) {
		if (countsX.at(x) != 0) {
			for (unsigned int y = 0; y < ydim; ++y) {
				const unsigned int curcounts = counts(y * xdim + x);
				if (countsY.at(y) && curcounts) {
					statistic += curcounts * (std::log(((double) curcounts * countsXY) / ((double) countsX.at(x) * countsY.at(y))));
				}
			}
		}
	}
	return 2 * statistic;
}

// Author: Giorgos Borboudakis
Rcpp::List g2_test(arma::mat& data, const unsigned int x, const unsigned int y, 
		arma::uvec& cs, arma::uvec& dc) {
    TestResult result = g2_test(data, x, y, cs, cs.size(), dc);
    Rcpp::List out;
    out["statistic"] = result.stat;
    out["df"] = result.df;
    return out;
}

TestResult g2_test(arma::mat& data, const unsigned int x, const unsigned int y, 
		arma::uvec& cs, const unsigned int ncs, arma::uvec& dc) {
	if (!ncs) {
		return g2_test(data, x, y, dc);
	}
	const unsigned int xdim = dc.at(x);
	const unsigned int ydim = dc.at(y);
	const unsigned int nsamples = data.n_rows;
	arma::uvec prod(ncs + 1);
	prod.at(0) = 1;
	for (unsigned int i = 1; i <= ncs; ++i) {
		prod.at(i) = prod.at(i - 1) * dc.at(cs.at(i - 1));
	}

	const unsigned int size = prod.at(ncs);
	arma::umat counts(xdim * ydim, size, arma::fill::zeros);
	for (unsigned int i = 0; i < nsamples; ++i) {
		unsigned int key = 0;
		for (unsigned int j = 0; j < ncs; ++j) {
			key += (unsigned int) data.at(i, cs.at(j)) * prod.at(j);
		}
		const unsigned int curx = (unsigned int) data.at(i, x);
		const unsigned int cury = (unsigned int) data.at(i, y);
		counts(cury * xdim + curx, key)++;
	}

	double statistic = 0;
	for (unsigned int i = 0; i < size; ++i) {
		arma::uvec tmp = counts.col(i);
		statistic += g2_statistic(tmp, xdim, ydim);
	}
	const unsigned int df = (xdim - 1) * (ydim - 1) * prod.at(ncs);

	return TestResult(0, statistic, 0, df);
}

void random_contigency_table(int* matrix, const int* nrowt, const int* ncolt, 
		const unsigned int nrow, const unsigned int ncol, const double* logfact, int* jwork, 
		const int ntotal, std::mt19937&	rng);

static int total_counts(arma::uvec& counts, const unsigned int xdim, 
		const unsigned int ydim);

static void col_counts(arma::uvec& counts, const unsigned int xdim,
		const unsigned int ydim, int* counts_y);

static void row_counts(arma::uvec& counts, const unsigned int xdim, 
		const unsigned int ydim, int* counts_x);

TestResult perm_g2_test(arma::mat& data, const unsigned int x, const unsigned int y,
		arma::uvec& cs, const unsigned int ncs, arma::uvec& dc, const unsigned int nperm);

// Author: Giorgos Borboudakis
Rcpp::List g2_test_perm(arma::mat& data, const unsigned int x, const unsigned int y,
		arma::uvec& cs, arma::uvec& dc, const unsigned int nperm) {
	TestResult result = perm_g2_test(data, x, y, cs, cs.size(), dc, nperm);
	Rcpp::List out;
	out["statistic"] = result.stat;
	out["pvalue"] = result.pvalue;
	out["x"] = x;
	out["y"] = y;
	out["df"] = result.df;
	return out;
}

TestResult perm_g2_test(arma::mat& data, const unsigned int x, const unsigned int y,
		arma::uvec& cs, const unsigned int ncs, arma::uvec& dc, const unsigned int nperm) {
	const unsigned int xdim = dc.at(x);
	const unsigned int ydim = dc.at(y);

	const unsigned int nsamples = data.n_rows;
	arma::uvec prod(ncs + 1);
	prod.at(0) = 1;
	for (unsigned int i = 1; i <= ncs; i++) {
		prod.at(i) = prod.at(i - 1) * dc.at(cs.at(i - 1));
	}

	const unsigned int size = prod.at(ncs);
	arma::umat counts(xdim * ydim, size, arma::fill::zeros);
	for (unsigned int i = 0; i < nsamples; i++) {
		unsigned int key = 0;
		for (unsigned int j = 0; j < ncs; j++) {
			key += data.at(i, cs.at(j)) * prod.at(j);
		}
		const unsigned int curx = data.at(i, x);
		const unsigned int cury = data.at(i, y);
		counts.at(cury * xdim + curx, key)++;
	}

	double statistic = 0;
	for (unsigned int i = 0; i < size; i++) {
		arma::uvec tmp = counts.col(i);	
		statistic += g2_statistic(tmp, dc.at(x), dc.at(y));
	}
	const int df = (dc.at(x) - 1) * (dc.at(y) - 1) * prod.at(ncs);

	if (!nperm) {
		return TestResult(0, statistic, 0, df);
	}

	arma::vec permstats(nperm, arma::fill::zeros);

	std::random_device rd;
	std::mt19937 rng(rd());

	int* jwork = new int[ydim - 1];
	int* ct = new int[xdim * ydim];
	int* rowcounts = new int[xdim];
	int* colcounts = new int[ydim];
	int* totals = new int[size];
	double* nrc = new double[xdim * ydim];

	int maxtotal = 0;
	for (unsigned int i = 0; i < size; i++) {
		arma::uvec tmp = counts.col(i);
		totals[i] = total_counts(tmp, xdim, ydim);
		maxtotal = (totals[i] > maxtotal) ? totals[i] : maxtotal;
	}

	double* plog = new double[1 + maxtotal];
	double* logfact = new double[1 + maxtotal];
	plog[0] = 0;
	logfact[0] = 0;
	for (int j = 1; j <= maxtotal; j++) {
		plog[j] = std::log((double) j);
		logfact[j] = logfact[j - 1] + plog[j];
	}

	for (unsigned int i = 0; i < size; i++) {
		const int ntotal = totals[i];
		if (ntotal > 0) {
			arma::uvec tmp = counts.col(i);
			row_counts(tmp, xdim, ydim, rowcounts);
			col_counts(tmp, xdim, ydim, colcounts);
			int ctr = 0;
			for (unsigned int x = 0; x < xdim; x++) {
				for (unsigned int y = 0; y < ydim; y++) {
					nrc[ctr++] = plog[ntotal] - plog[rowcounts[x]] - plog[colcounts[y]];
				}
			}

			for (unsigned int p = 0; p < nperm; p++) {
				memcpy(jwork, colcounts, (ydim - 1) * sizeof(int));
				random_contigency_table(ct, rowcounts, colcounts, xdim, ydim, 
						logfact, jwork, ntotal, rng);

				double curstat = 0;
				int cti = 0;
				for (unsigned int x = 0; x < xdim; x++) {
					if (rowcounts[x]) {
						for (unsigned int y = 0; y < ydim; y++) {
							curstat += ct[cti] * (plog[ct[cti]] + nrc[cti]);
							cti++;
						}
					}
					else {
						cti += ydim;
					}
				}
				permstats.at(p) += (2 * curstat);
			}
		}
	}
		
	double pvalue = 1;
	for (unsigned int p = 0; p < nperm; p++) {
		pvalue += (permstats.at(p) >= statistic);
	}
	pvalue /= (nperm + 1);

	return TestResult(pvalue, statistic, std::log(pvalue), df);
}

static void row_counts(arma::uvec& counts, const unsigned int xdim, 
		const unsigned int ydim, int* counts_x) {
	if (arma::all(counts == 0)) {
		return;
	}
  	memset(counts_x, 0, xdim * sizeof(int));
	for (unsigned int x = 0; x < xdim; x++) {
		for (unsigned int y = 0; y < ydim; y++) {
			counts_x[x] += counts.at(y * xdim + x);
		}
	}
}

static void col_counts(arma::uvec& counts, const unsigned int xdim,
		const unsigned int ydim, int* counts_y) {
	if (arma::all(counts == 0)) {
		return;
	}
	memset(counts_y, 0, ydim * sizeof(int));
	for (unsigned int x = 0; x < xdim; x++) {
		for (unsigned int y = 0; y < ydim; y++) {
			counts_y[y] += counts.at(y * xdim + x);
		}
	}
}

static int total_counts(arma::uvec& counts, const unsigned int xdim, 
		const unsigned int ydim) {
	if (arma::all(counts == 0)) {
		return 0;
	}
	int counts_xy = 0;
	for (unsigned int x = 0; x < xdim; x++) {
		for (unsigned int y = 0; y < ydim; y++) {
			counts_xy += counts.at(y * xdim + x);
		}
	}
	return counts_xy;
}

void random_contigency_table(int* matrix, const int* nrowt, const int* ncolt, const unsigned int nrow, const unsigned int ncol, const double* logfact, int* jwork, 
		const int ntotal, std::mt19937& rng) {
	std::uniform_real_distribution<> dist(0, 1);
	int jc = ntotal;
	int ib = 0;
  
	for (unsigned int l = 0; l < nrow - 1; ++l) {
		int ia = nrowt[l];
		int ic = jc;
		jc -= ia;
		for (unsigned int m = 0; m < ncol - 1; ++m) {
			int id = jwork[m];
			int ie = ic;
			ib = ie - ia;
			int ii = ib - id;
			ic -= id;
		  
			if (ie == 0) {
				ia = 0;
				memset(&matrix[l * ncol + m], 0, (ncol - m) * sizeof(int));
				break;
			}
		  
			//  Compute the conditional expected value of MATRIX(L,M)
		  	bool done = false;
		  	int curnlm, nlm;
		  	curnlm = nlm = (int)(((double)ia * id) / ie + 0.5);
		  	double x = exp(logfact[ia] + logfact[ib] + logfact[ic] + logfact[id] - 
					logfact[ie] - logfact[nlm] - logfact[id - nlm] - logfact[ia - nlm] - logfact[ii + nlm]);
			for (double r = dist(rng), sumprb = x; !done && r > x; r = sumprb * dist(rng)) {
				bool lsp = false;
				int nll;
				double curx, y;
				curnlm = nll = nlm;
				curx = y = sumprb = x;
				
				// Increment entry in row L, column M.
				do {
					int j = (id - curnlm) * (ia - curnlm);
					if (j == 0) {
						lsp = true;
						for (j = nll * (ii + nll); j != 0; j = nll * (ii + nll)) {
							--nll;
							y = (y * j) / ((id - nll) * (ia - nll));
							sumprb = sumprb + y;

							if (r <= sumprb) {
							curnlm = nll;
							done = true;
							break;
							}
						}
					}
					else {
						++curnlm;
						curx = (curx * j) / (curnlm * (ii + curnlm));
						sumprb = sumprb + curx;

						if (r <= sumprb) {
							done = true;
							break;
						}
						for (j = nll * (ii + nll); j != 0; j = nll * (ii + nll)) {
							--nll;
							y = (y * j) / ((id - nll) * (ia - nll));
							sumprb = sumprb + y;
					  
							if (r <= sumprb) {
								curnlm = nll;
								done = true;
								break;
							}
							if (!lsp) {
								break;
							}
						}
					}
				} while (!done && !lsp);
			}
			matrix[l * ncol + m] = curnlm;
			ia = ia - curnlm;
			jwork[m] = jwork[m] - curnlm;
		}
    	matrix[l * ncol + ncol - 1] = ia;
  	}
  	memcpy(&matrix[(nrow - 1) * ncol], jwork, (ncol - 1) * sizeof(int));
  	matrix[(nrow - 1) * ncol + ncol - 1] = ib - matrix[(nrow - 1) * ncol + ncol - 2];
}

double calc_med(arma::vec& vals);

double calc_med(arma::vec& vals, const unsigned int size);

bool adj_med_NAs(arma::mat& ds) {
	bool found_NA = false;
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		bool found_NA_col = false;
		double med = 0;
		for (unsigned int j = 0; j < ds.n_cols; j++) {
			if (!arma::is_finite(ds.at(i, j))) {
				if (!found_NA) { found_NA = true; }
				if (!found_NA_col) {
					found_NA_col = true;
					arma::vec tmp = ds.col(j);
					med = calc_med(tmp);
				}
				ds.at(i, j) = med;
			}
		}
	}
	return found_NA;
}

double calc_med(arma::vec& vals) {
    const unsigned int mid = vals.size() / 2 - 1;
    double med = 0;
    if (vals.size() % 2 == 0) {
        std::nth_element(vals.begin(), vals.begin() + mid, vals.end());
        med = (vals[mid] + *std::min_element(vals.begin() + (mid + 1), vals.end())) / 2.0;
    }
    else {
        std::nth_element(vals.begin(), vals.begin() + (mid + 1), vals.end());
        med = vals[mid + 1];
    }
    return med;
}

double find_freq(arma::vec& vals);

bool adj_freq_NAs(arma::mat& ds) {
	bool found_NA = false;
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		bool found_NA_col = false;
		double freq = 0;
		for (unsigned int j = 0; j < ds.n_cols; j++) {
			if (!arma::is_finite(ds.at(i, j))) {
				if (!found_NA) { found_NA = true; }
				if (!found_NA_col) {
					found_NA_col = true;
					arma::vec tmp = ds.col(j);
					freq = find_freq(tmp);
				}
				ds.at(i, j) = freq;
			}
		}
	}
	return found_NA;
}

double find_freq(arma::vec& vals) {
	std::sort(vals.begin(), vals.end());
	double prev = vals[0];
	double freq = vals[0];
	int curr_count = 1;
	int max_count = 1;
	for (unsigned int i = 1; i < vals.size(); i++) {
		if (vals[i] == prev) {
			curr_count++;
		}
		else {
			if (curr_count > max_count) {
				freq = vals[i - 1];
				max_count = curr_count;
			}
			prev = vals[i];
			curr_count = 1;
		}
	}
	if (curr_count == 1 && max_count == 1) {
		return *std::min_element(vals.begin(), vals.end());
	}
	return curr_count > max_count ? vals[vals.size() - 1] : freq;
}

void update(const double key, const double rank,
		arma::mat& ds, const unsigned int col,  
		std::unordered_map<double, std::vector<unsigned int>>& val_ids);

int sum(const unsigned int tart_pos, const unsigned int end_pos);

void update_init(const double key,
		const unsigned int start_pos, const unsigned int end_pos, const int cntr, 
		arma::mat& ds, const unsigned int col, 
		std::unordered_map<double, std::vector<unsigned int>>& val_ids);

void rank(arma::vec& vals, arma::mat& ds, const unsigned int col);

arma::mat calc_rank(arma::mat& ds) {
	for (unsigned int j = 0; j < ds.n_cols; j++) {
		arma::vec tmp = ds.col(j);
		rank(tmp, ds, j);
	}
	return ds;
}

void rank(arma::vec& vals, arma::mat& ds, const unsigned int col) {
	std::unordered_map<double, std::vector<unsigned int>> val_ids;
	for (unsigned int i = 0; i < vals.size(); i++) {
		if (val_ids.find(vals[i]) == val_ids.end()) {
			val_ids[vals[i]] = { i };
		}
		else {
			val_ids[vals[i]].push_back(i);
		}
	}
	std::sort(vals.begin(), vals.end());
	double key;
	int cntr;
	unsigned int prev_pos = 0;
	unsigned int curr_pos = 0;
	for (; curr_pos < vals.size();) {
		key = vals[curr_pos];
		cntr = val_ids[key].size();
		prev_pos = curr_pos;
		curr_pos += cntr;
		update_init(key, prev_pos + 1, curr_pos + 1, cntr, ds, col, val_ids);
	}
	val_ids.clear();
}

void update_init(const double key,
		const unsigned int start_pos, const unsigned int end_pos, const int cntr, 
		arma::mat& ds, const unsigned int col, 
		std::unordered_map<double, std::vector<unsigned int>>& val_ids) {
	const int old_rank_sum = sum(start_pos, end_pos);
	const double rank = (double) old_rank_sum / cntr;
	update(key, rank, ds, col, val_ids);
}

int sum(const unsigned int start_pos, const unsigned int end_pos) {
	int sum = 0;
	for (unsigned int i = start_pos; i < end_pos; i++) {
		sum += i;
	}
	return sum;
}

void update(const double key, const double rank,
		arma::mat& ds, const unsigned int col,  
		std::unordered_map<double, std::vector<unsigned int>>& val_ids) {
	for (unsigned int i = 0; i < val_ids[key].size(); i++) {
		ds(val_ids[key][i], col) = rank;
	}
}

void combn(arma::vec& vals, const int n, const unsigned int start_idx, 
		std::vector<double>& combn_data, arma::mat& combn_ds, unsigned int& combn_col);

double fact(const int n);

arma::mat find_combn(arma::vec& vals, const int n) {
	static unsigned int combn_col = 0;
	const unsigned int nrows = n;
	const unsigned int ncols = std::round(R::choose(vals.size(), n));
	arma::mat combn_ds(nrows, ncols);
	std::vector<double> combn_data(nrows);
	const unsigned int start_idx = 0;
	combn_col = 0; combn(vals, n, start_idx, combn_data, combn_ds, combn_col);
	return combn_ds;
}

double fact(const int n) {
	double fact = 1;
	for (double i = 2; i <= n; i++) {
		fact *= i;
	}
	return fact;
}

void combn(arma::vec& vals, const int n, const unsigned int start_idx, 
		std::vector<double>& combn_data, arma::mat& combn_ds, unsigned int& combn_col) {
	if (!n) {
		for (unsigned int i = 0; i < combn_ds.n_rows && combn_col < combn_ds.n_cols; i++) {
			combn_ds(i, combn_col) = combn_data.at(i);
		}
		combn_col++;
		return;
	}
	for (unsigned int i = start_idx; i <= (vals.size() - n); i++) {
		combn_data.at(combn_ds.n_rows - n) = vals(i);
		combn(vals, n - 1, i + 1, combn_data, combn_ds, combn_col);
	}
}
arma::uvec sub_col_max_min(arma::mat& ds, const bool cont) {
	int extra_val = 0;
	if (!cont) {
		extra_val = 1;
	}
	arma::uvec max_min(ds.n_cols);
	for (unsigned int j = 0; j < ds.n_cols; j++) {
		double max = ds(0, j);
		double min = ds(0, j);
		for (unsigned int i = 1; i < ds.n_rows; i++) {
			const unsigned int curr = ds(i, j);
			if (curr > max) {
				max = curr;
			}
			if (curr < min) {
				min = curr;
			}
		}
		max_min(j) = (max - min) + extra_val;
	}
	return max_min;
}

arma::mat calc_pt(arma::mat& ds, 
		const int df, const bool lower_tail, const bool log_p, 
		const double add) {
	arma::mat pt_ds(ds.n_rows, ds.n_cols);
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		for (unsigned int j = 0; j < ds.n_cols; j++) {
			pt_ds(i, j) = add + R::pt(ds(i, j), df, lower_tail, log_p);
		}
	}
	return pt_ds;
}

arma::mat ext_cols(arma::mat& ds,  const unsigned int col_a, 
		const unsigned int col_b) {
	const unsigned int ncols = 2;
	arma::mat ext_ds(ds.n_rows, ncols);
	const unsigned int ext_col_a = 0;
	const unsigned int ext_col_b = 1;
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		ext_ds(i, ext_col_a) = ds(i, col_a);
		ext_ds(i, ext_col_b) = ds(i, col_b);
	}
	return ext_ds;
}

void cp_lt(arma::mat& src, arma::mat& dst, const int val) {
	for (unsigned int i = 0; i < src.n_rows; i++) {
		for (unsigned int j = 0; j < src.n_cols; j++) {
			i > j ? dst(i, j) = val : dst(i, j) = src(i, j);
		}
	}
}

void adj_diag(arma::mat& ds, const double val) {
	for (unsigned int i = 0; i < ds.n_rows && i < ds.n_cols; i++) {
		ds(i, i) = val;
	}
}

arma::mat cbind_tran_mat(arma::mat& ds1, arma::mat& ds2) {
	const unsigned int nrows = ds1.n_cols;
	const unsigned int ncols = ds1.n_rows + ds2.n_rows;
	arma::mat ds(nrows, ncols);
	for (unsigned int i = 0; i < ds1.n_rows && i < ds2.n_rows; i++) {
		for (unsigned int j = 0; j < ds1.n_cols && j < ds2.n_cols; j++) {
			ds(j, i) = ds1(i, j);
			ds(j, i + ds1.n_rows) = ds2(i, j);
		}
	}
	return ds;
}

bool is_dupl_row(arma::mat& ds, const unsigned int lrow);

std::vector<unsigned int> get_dupl_rows_pos(arma::mat& ds);

arma::mat rm_dupl_rows(arma::mat& src);

arma::mat rbind_uniq(arma::mat& ds1, arma::mat& ds2,
		const bool ass1, const bool ass2) {
	const unsigned int nrows = ds1.n_rows + ds2.n_rows;
	unsigned int ncols;
	ds1.n_cols > ds2.n_cols ? ncols = ds1.n_cols : ncols = ds2.n_cols;
	arma::mat ds(nrows, ncols, arma::fill::zeros);
	unsigned int row = 0;
	unsigned int col = 0;
	if (ass1) {
		for (unsigned int i = 0; i < ds1.n_rows; i++) {
			for (unsigned int j = 0; j < ds1.n_cols; j++) {
				ds(row, col) = ds1(i, j);
				col++;
			}
			row++;
			col = 0;
		}
	}
	else {
		row = ds1.n_rows;
	}
	if (ass2) {
		for (unsigned int i = 0; i < ds2.n_rows; i++) {
			for (unsigned int j = 0; j < ds2.n_cols; j++) {
				ds(row, col) = ds2(i, j);
				col++;
			}
			row++;
			col = 0;
		}
	}
	return rm_dupl_rows(ds);
}

arma::mat rm_dupl_rows(arma::mat& src) {
	std::vector<unsigned int> dupls_pos = get_dupl_rows_pos(src);
	if (dupls_pos.empty()) {
		return src;
	}
	const unsigned int nrows = src.n_rows - dupls_pos.size();
	arma::mat dst(nrows, src.n_cols);
	unsigned int dupls_pos_cntr = 0;
	for (unsigned int i = 0, src_row = 0; i < dst.n_rows; i++, src_row++) {
		while (dupls_pos_cntr < dupls_pos.size() &&
				dupls_pos.at(dupls_pos_cntr) == src_row) {
			src_row++;
			dupls_pos_cntr++;
		}
		for (unsigned int j = 0; j < dst.n_cols; j++) {
			dst(i, j) = src(src_row, j);
		}
	}
	return dst;
}

std::vector<unsigned int> get_dupl_rows_pos(arma::mat& ds) {
	std::vector<unsigned int> dupls_pos;
	for (unsigned int i = 1; i < ds.n_rows; i++) {
		if (is_dupl_row(ds, i)) {
			dupls_pos.push_back(i);
		}
	}
	return dupls_pos;
}

bool is_dupl_row(arma::mat& ds, const unsigned int lrow) {
	for (unsigned int i = 0; i < lrow; i++) {
		for (unsigned int j = 0;
			 ;
			 j++) {
			if (ds(i, j) != ds(lrow, j)) {
				break;
			}
			if (j == ds.n_cols - 1) {
				return true;
			}
		}
	}
	return false;
}

arma::vec to_vec(arma::mat& ds) {
	arma::vec vals(ds.n_rows * ds.n_cols);
	unsigned int k = 0;
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		for (unsigned int j = 0; j < ds.n_cols; j++) {
			vals(k++) = ds(i, j);
		}
	}
	return vals;
}

std::vector<double> inter(arma::vec& vals1, arma::vec& vals2) {
	std::sort(vals1.begin(), vals1.end());
	std::sort(vals2.begin(), vals2.end());
	std::vector<double> inter_vals;
	for (unsigned int i = 0, j = 0; i < vals1.size() && j < vals2.size();) {
		const double curr1 = vals1(i);
		const double curr2 = vals2(j);
		if (curr1 == curr2) {
			const unsigned int size = inter_vals.size();
			if (!size || (inter_vals.back() != curr1)) {
				inter_vals.push_back(curr1);
			}
			i++;
			j++;
		}
		else if (curr1 < curr2) {
			if (vals1(vals1.size() - 1) < curr2) {
				break;
			}
			i++;
		}
		else {
			if (vals2(vals2.size() - 1) < curr1) {
				break;
			}
			j++;
		}
	}
	return inter_vals;
}

// Alters row_idxs
void append_rows(arma::mat& ds, const double val, std::vector<unsigned int>& row_idxs);

std::vector<unsigned int> index_row_eq(arma::mat& ds, std::vector<double>& vals) {
	std::vector<unsigned int> row_idxs;
	for (unsigned int i = 0; i < vals.size(); i++) {
		append_rows(ds, vals.at(i), row_idxs);
	}
	std::sort(row_idxs.begin(), row_idxs.end());
	row_idxs.erase(std::unique(row_idxs.begin(), row_idxs.end()), row_idxs.end());
	return row_idxs;
}

void append_rows(arma::mat& ds, const double val, std::vector<unsigned int>& row_idxs) {
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		for (unsigned int j = 0; j < ds.n_cols; j++) {
			if (ds(i, j) == val) {
				row_idxs.push_back(i);
			}
		}
	}
}

unsigned int skip_ahead(arma::uvec& rows, const unsigned int curr);

arma::mat rm_rows(arma::mat& src, arma::uvec& rows) {
	unsigned int dst_nrows = src.n_rows - rows.size();
	unsigned int dst_ncols = src.n_cols;
	arma::mat dst(dst_nrows, dst_ncols);
	unsigned int src_row = 0;
	unsigned int rows_idx = 0;
	for (unsigned int dst_row = 0; dst_row < dst_nrows; dst_row++) {
		while (src_row < src.n_rows && rows_idx < rows.size() && src_row == rows(rows_idx)) {
			src_row++;
			rows_idx = skip_ahead(rows, rows_idx);
		}
		for (unsigned int col = 0; col < dst_ncols; col++) {
			dst(dst_row, col) = src(src_row, col);
		}
		src_row++;
	}
	return dst;
}

unsigned int skip_ahead(arma::uvec& rows, const unsigned int curr) {
	unsigned int i;
	for (i = curr + 1; i < rows.size() && rows(i) == rows(curr); i++) {
	}
	return i;
}

unsigned int skip_ahead_std(std::vector<unsigned int> rows, const unsigned int curr);

arma::mat rm_rows_std(arma::mat& src, std::vector<unsigned int>& rows) {
	unsigned int dst_nrows = src.n_rows - rows.size();
	unsigned int dst_ncols = src.n_cols;
	arma::mat dst(dst_nrows, dst_ncols);
	unsigned int src_row = 0;
	unsigned int rows_idx = 0;
	for (unsigned int dst_row = 0; dst_row < dst_nrows; dst_row++) {
		while (src_row < src.n_rows && rows_idx < rows.size() && src_row == rows.at(rows_idx)) {
			src_row++;
			rows_idx = skip_ahead_std(rows, rows_idx);
		}
		for (unsigned int col = 0; col < dst_ncols; col++) {
			dst(dst_row, col) = src(src_row, col);
		}
		src_row++;
	}
	return dst;
}

unsigned int skip_ahead_std(std::vector<unsigned int> rows, const unsigned int curr) {
	unsigned int i;
	for (i = curr + 1; i < rows.size() && rows.at(i) == rows.at(curr); i++) {
	}
	return i;
}

arma::mat order_col(arma::mat& ds, const unsigned int col) {
	arma::mat ordered_ds(ds.n_rows, ds.n_cols);
	arma::uvec order_idxs = arma::sort_index(ds.col(col));
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		const unsigned int idx = order_idxs(i);
		for (unsigned int j = 0; j < ds.n_cols; j++) {
			ordered_ds(i, j) = ds(idx, j);
		}
	}
	return ordered_ds;
}

arma::mat form_cmat(arma::mat& ds, arma::uvec& rows, arma::uvec& cols) {
	arma::mat formed_ds(cols.size(), rows.size());
	for (unsigned int i = 0; i < rows.size(); i++) {
		for (unsigned int j = 0; j < cols.size(); j++) {
			formed_ds(j, i) = ds(rows(i), cols(j));
		}
	}
	return formed_ds;
}

arma::mat form_rmat(arma::mat& ds, arma::uvec& rows, arma::uvec& cols) {
	arma::mat formed_ds(rows.size(), cols.size());
	for (unsigned int i = 0; i < rows.size(); i++) {
		for (unsigned int j = 0; j < cols.size(); j++) {
			formed_ds(i, j) = ds(rows(i), cols(j));
		}
	}
	return formed_ds;
}

arma::mat form_rmat_std(arma::mat& ds, std::vector<unsigned int>& rows, 
		std::vector<unsigned int>& cols) {
	arma::mat formed_ds(rows.size(), cols.size());
	for (unsigned int i = 0; i < rows.size(); i++) {
		for (unsigned int j = 0; j < cols.size(); j++) {
			formed_ds(i, j) = ds(rows.at(i), cols.at(j));
		}
	}
	return formed_ds;
}

arma::mat merge_cols(arma::mat& ds, arma::uvec& idxs) {
	arma::mat merged_ds(ds.n_rows, idxs.size());
	for (unsigned int i = 0; i < idxs.size(); i++) {
		for (unsigned int j = 0; j < ds.n_rows; j++) {
			merged_ds(j, i) = ds(j, idxs(i));
		}
	}
	return merged_ds;
}

arma::mat form_ncolcmat(arma::vec& vals, arma::mat& ds) {
	const unsigned int nrows = vals.size();
	const unsigned int ncols = 1 + ds.n_cols; 
	arma::mat formed_ds(nrows, ncols);
	for (unsigned int i = 0; i < nrows; i++) {
		formed_ds(i, 0) = vals(i);
		for (unsigned int j = 1; j < ncols; j++) {
			formed_ds(i, j) = ds(i, (j - 1));
		}
	}
	return formed_ds;
}

arma::mat form_c2mat(arma::vec& vals1, arma::vec& vals2) {
	const unsigned int nrows = vals1.size();
	const unsigned int ncols = 2;
	arma::mat formed_ds(nrows, ncols);
	for (unsigned int i = 0; i < nrows; i++) {
		formed_ds(i, 0) = vals1(i);
		formed_ds(i, 1) = vals2(i);
	}
	return formed_ds;
}

arma::uvec form_vec(arma::mat& ds, const unsigned int row, arma::uvec& cols) {
	arma::uvec vals(cols.size());
	for (unsigned int i = 0; i < cols.size(); i++) {
		vals(i) = ds(row, cols(i));
	}
	return vals;
}

arma::vec form_vec_wvals(arma::mat& ds, const unsigned int row, arma::uvec& cols,
		arma::vec& vals) {
	arma::vec formed_vec(cols.size() + vals.size());
	unsigned int i;
	for (i = 0; i < cols.size(); i++) {
		formed_vec(i) = ds(row, cols(i));
	}
	for (unsigned int j = 0; i < formed_vec.size(); i++, j++) {
		formed_vec(i) = vals(j);
	}
	return formed_vec;
}

arma::mat append_row(arma::mat& ds, const unsigned int row, arma::vec& vals) {
	for (unsigned int j = 0; j < ds.n_cols; j++) {
		ds(row, j) = vals(j);
	}
	return ds;
}

std::vector<unsigned int> rsum_gt_zero_idxs(arma::mat& ds) {
	std::vector<unsigned int> idxs;
	for (unsigned int i = 0; i < ds.n_rows; i++) {
		double sum = 0;
		for (unsigned int j = 0; j < ds.n_cols; j++) {
			sum += ds(i, j);
		}
		if (sum > 0) {
			idxs.push_back(i);
		}
	}
	return idxs;
}

arma::vec form_cmat_vec(arma::mat& ds, arma::rowvec& vals) {
	arma::vec concat_vals(ds.n_rows * ds.n_cols + vals.size());
	unsigned int cd_cntr = 0;
	for (unsigned int j = 0; j < ds.n_cols; j++) {
		for (unsigned int i = 0; i < ds.n_rows; i++) {
			concat_vals(cd_cntr++) = ds(i, j);
		}
	}
	for (unsigned int i = 0; i < vals.size(); i++) {
		concat_vals[cd_cntr++] = vals(i);
	}
	return concat_vals;
}

arma::mat cbind_mat(arma::mat& ds1, arma::mat& ds2) {
	const unsigned int nrows = ds1.n_rows;
	const unsigned int ncols = ds1.n_cols + ds2.n_cols;
	arma::mat ds(nrows, ncols);
	for (unsigned int i = 0; i < ds1.n_rows && i < ds2.n_rows; i++) {
		for (unsigned int j = 0; j < ds1.n_cols; j++) {
			ds(i, j) = ds1(i, j);
		}
		for (unsigned int j = 0; j < ds2.n_cols; j++) {
			ds(i, j + ds1.n_cols) = ds2(i, j);
		}
	}
	return ds;
}

arma::mat adj_cols(arma::mat& src, const unsigned int dst_ncols) {
	const unsigned int dst_nrows = src.n_rows * src.n_cols / dst_ncols;
	arma::mat dst(dst_nrows, dst_ncols);
	unsigned int src_row = 0;
	unsigned int src_col = 0;
	unsigned int dst_row = 0;
	unsigned int dst_col = 0;
	while (src_col < src.n_cols && dst_col < dst_ncols) {
		while (src_row < src.n_rows && dst_row < dst_nrows) {
			dst(dst_row++, dst_col) = src(src_row++, src_col);
		}
		if (src_row >= src.n_rows) {
			src_row = 0;
			src_col++;
		}
		if (dst_row >= dst_nrows) {
			dst_row = 0;
			dst_col++;
		}
	}
	return dst;
}

arma::vec perm_cor(arma::mat& ds, const unsigned int r) {
	const unsigned int nrows = ds.n_rows;

    db_print("Calculating upper and lower.\n");
	arma::vec ds_c0 = ds.col(0);
	arma::vec ds_c1 = ds.col(1);
    double ds_c0_sum = 0;
    double ds_c1_sum = 0;
    double ds_c0_p2_sum = 0;
    double ds_c1_p2_sum = 0;
    double ds_sum = 0;
    for (unsigned int i = 0; i < nrows; i++) {
        const double curr_c0 = ds_c0[i];
        const double curr_c1 = ds_c1[i];
        ds_c0_sum += curr_c0;
        ds_c1_sum += curr_c1;
        ds_c0_p2_sum += std::pow(curr_c0, 2.0);
        ds_c1_p2_sum += std::pow(curr_c1, 2.0);
        ds_sum += curr_c0 * curr_c1;
    }
    const double upper = (ds_c0_sum * ds_c1_sum) / nrows;
    const double lower = std::sqrt((ds_c0_p2_sum - std::pow(ds_c0_sum, 2.0) / nrows) *
			(ds_c1_p2_sum - std::pow(ds_c1_sum, 2.0) / nrows));

    db_print("Calculating test_stat.\n");
    const double cor = (ds_sum - upper) / lower;
    const double test_stat = std::log((1 + cor) / (1 - cor));

    db_print("Calculating sxy, test_stat_abs, pvalue.\n");
    const double test_stat_abs = std::abs(test_stat);
    int sum = 1;
    for (unsigned int i = 0; i < r; i++) {
		arma::vec shuffled_ds_c0 = arma::shuffle(ds_c0);
        double sxy_sum = 0;
        for (unsigned int j = 0; j < nrows; j++) {
			sxy_sum += shuffled_ds_c0[j] * ds_c1[j];
        }
		const double adj_sxy_sum = (sxy_sum - upper) / lower;
        if (std::abs(std::log((1 + adj_sxy_sum) / (1 - adj_sxy_sum))) > test_stat_abs) {
            sum++;
        }
    }
    const double pvalue = sum / (double) (r + 1);
	arma::vec res(2); res[0] = cor; res[1] = pvalue;
	return res;
}

arma::vec cat_ci(const unsigned int x, const unsigned int y,
		arma::uvec& cs, arma::mat& ds, arma::uvec& type, const unsigned int r) {
	arma::vec ret(3);
	if (r == 1) {
		db_print("r == 1\n");
		Rcpp::List g2t = g2_test(ds, x, y, cs, type);
		double stat = 0;
		double df = g2t["df"];
		double pvalue = 0;
		if (ds.n_rows > 5 * df) {
			db_print("ds.n_rows > 5 * df\n");
			stat = g2t["statistic"];
			pvalue = R::pchisq(stat, df, false, true);
		}
		else {
			db_print("ds.n_rows <= 5 * df\n");
			g2t = g2_test_perm(ds, x, y, cs, type, 1000);
			stat = g2t["statistic"];
			df = g2t["df"];
			pvalue = std::log((double) g2t["pvalue"]);
		}
		ret[0] = stat; ret[1] = pvalue; ret[2] = df;
	}
	else {
		db_print("r != 1\n");
		Rcpp::List g2t = g2_test_perm(ds, x, y, cs, type, r);
		const double stat = g2t["statistic"];
		const double df = g2t["df"];
		const double pvalue = std::log((double) g2t["pvalue"]);
		ret[0] = stat; ret[1] = pvalue; ret[2] = df;
	}
	return ret;
}

arma::mat calc_er(arma::mat& ds, arma::mat& cor_ds, arma::vec& data_pos1, arma::vec& data_pos2, 
		arma::uvec& cs, const unsigned int ncols);

arma::mat sol_mult_sub(const unsigned int pos1, const unsigned int pos2,
		arma::uvec& cs, arma::mat& cor_ds);

arma::vec calc_condi(const unsigned int pos1, const unsigned int pos2, arma::uvec& cs, 
		arma::mat& ds, arma::mat& cor_ds, const std::string method, const unsigned int r) {
	const bool is_spearman = !method.compare("spearman");
	db_print("Calculating df.\n");
	const double df = ds.n_rows - cs.size() - 3;
	if (r == 1) {
		db_print("r == 1\n");
		db_print("Calculating adj_cor_ds.\n");
		arma::mat adj_cor_ds = sol_mult_sub(pos1, pos2, cs, cor_ds);
		db_print("Calculating stat.\n");
		const double stat = std::abs(adj_cor_ds(0, 1) / std::sqrt(adj_cor_ds(0, 0) * adj_cor_ds(1, 1)));
		db_print("Calculating test_stat_abs.\n");
		double test_stat_abs;
		if (!is_spearman) {
			test_stat_abs = std::abs(0.5 * std::log((1 + stat) / (1 - stat)) * std::sqrt(df));
		}
		else {
			test_stat_abs = std::abs(0.5 * std::log((1 + stat) / (1 - stat)) * std::sqrt(df)) / 1.029563;
		}
		const double pvalue = std::log(2) + R::pt(test_stat_abs, df, false, true);
		arma::vec ret(3); ret[0] = test_stat_abs; ret[1] = pvalue; ret[2] = df;
		return ret;
	}
	else if (r > 1) {
		db_print("r > 1\n");
		arma::vec vals_pos1 = ds.col(pos1);
		arma::vec vals_pos2 = ds.col(pos2);
		arma::vec pc_pvalue;
		if (!cs.size()) {
			db_print("!cs.size()\n");
			db_print("Calculating cor_pvalue.\n");
			arma::mat cmat = form_c2mat(vals_pos1, vals_pos2);
			pc_pvalue = perm_cor(cmat, r);
		}
		else {
			db_print("cs.size()\n");
			db_print("Calculating er.\n");
			arma::mat er = calc_er(ds, cor_ds, vals_pos1, vals_pos2, cs, 2);
			db_print("Calculating cor_pvalue.\n");
			pc_pvalue = perm_cor(er, r);
		}
		arma::vec ret(3); ret[0] = std::abs(pc_pvalue[0]) / df; ret[1] = std::log(pc_pvalue[1]); ret[2] = df;
		return ret;
	}
	return NULL;
}

arma::mat sol_mult_sub(const unsigned int pos1, const unsigned int pos2,
		arma::uvec& cs, arma::mat& cor_ds) {
	arma::uvec pos12(2); pos12[0] = pos1; pos12[1] = pos2;
	arma::mat sol_cmat = form_cmat(cor_ds, cs, cs);
	arma::mat sol_rmat = form_rmat(cor_ds, cs, pos12);
	arma::mat sol = arma::solve(sol_cmat, sol_rmat);
	arma::mat mult_lh = form_rmat(cor_ds, pos12, cs);
	arma::mat mult_res = mult_lh * sol;
	arma::mat sub_rmat = form_rmat(cor_ds, pos12, pos12);
	arma::mat sub = sub_rmat - mult_res;
	return sub;
}

arma::mat calc_er(arma::mat& ds, arma::mat& cor_ds, arma::vec& data_pos1, arma::vec& data_pos2, 
		arma::uvec& cs, const unsigned int ncols) {
	arma::vec ones(ds.n_rows, arma::fill::ones);
	arma::mat ds_cs = merge_cols(ds, cs);
	arma::mat x = form_ncolcmat(ones, ds_cs);
	arma::mat y = form_c2mat(data_pos1, data_pos2);
	arma::mat xt_x = arma::trans(x) * x;
	arma::mat xt_y = arma::trans(x) * y;
	arma::mat sol = arma::solve(xt_x, xt_y);
	arma::mat er = x * sol;
	er = y - er;
	return er;
}
