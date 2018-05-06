#include "calc_perm_cor.h"

#define DEBUG 0
#define db_print(...) \
	do { if (DEBUG) Rprintf(__VA_ARGS__); } while (0)

static double calc_pvalue_rnd_r(arma::vec& ds_c0, arma::vec& ds_c1, const unsigned int r, 
		const double upper, const double lower, const double test_stat, const unsigned int nrows) {
	const unsigned int rnd_r = std::round(std::sqrt(r));
	arma::mat lh = arma::mat(ds_c0.size(), rnd_r, arma::fill::zeros);
	arma::mat rh = arma::mat(ds_c0.size(), rnd_r, arma::fill::zeros);
	for (unsigned int i = 0; i < rnd_r; ++i) {
		lh.col(i) = arma::shuffle(ds_c0);
		rh.col(i) = arma::shuffle(ds_c1);
	}
	arma::mat sxy = (arma::trans(lh) * rh);
	unsigned int sum = 1;
	for (unsigned int i = 0; i < sxy.n_rows; ++i) {
		for (unsigned int j = 0; j < sxy.n_cols; ++j) {
			const double curr = (sxy(i, j) - upper) / lower;
			const double curr_test_stat = std::abs(std::log((1 + curr) / (1 - curr)));
			if (curr_test_stat > test_stat) {
				++sum;
			}
		}
	}
	return sum / (double) (std::pow(rnd_r, 2) + 1);
}

arma::vec calc_perm_cor(arma::vec& x, arma::vec& y, const unsigned int r) {
	const unsigned int nrows = x.size();

	db_print("Calculating upper and lower.\n");
    double ds_c0_sum = 0;
    double ds_c1_sum = 0;
    double ds_c0_p2_sum = 0;
    double ds_c1_p2_sum = 0;
    double ds_sum = 0;
	for (unsigned int i = 0; i < nrows; i++) {
		const double curr_c0 = x[i];
		const double curr_c1 = y[i];
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
    const double test_stat = std::abs(std::log((1 + cor) / (1 - cor)));

    db_print("Calculating sxy, pvalue.\n");
	const double pvalue = calc_pvalue_rnd_r(x, y, r, upper, lower, test_stat, nrows);
	arma::vec res(2); res[0] = cor; res[1] = pvalue;
	return res;
}

