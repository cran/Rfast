// Author: 	Marios Dimitriadis
// Contact: kmdimitriadis@gmail.com

#include "calc_k_nn.h"

#define DEBUG 0
#define db_print(...) \
	do { if (DEBUG) Rprintf(__VA_ARGS__); } while (0)

// Author: Manos Papadakis
static double calc_med(std::vector<double>& x){
  	double F;
  	const int sz=x.size(),middle=sz/2-1;
  	if(sz%2==0){
   		std::nth_element(x.begin(),x.begin()+middle,x.end());
    	F=(x[middle]+*(std::min_element(x.begin()+middle+1,x.end())))/2.0;
  	}else{
    	std::nth_element(x.begin(),x.begin()+middle+1,x.end());
    	F=x[middle+1];
  	}
  	return F;
}

// Author: Manos Papadakis
static arma::mat sqrt_mat(arma::mat x){
	arma::colvec f(x.n_elem);
  	for(double *start=&x[0],*startf=&f[0],*end=&(*x.end());start!=end;++start,++startf){
    	*startf=std::sqrt(*start);    
  	}
  	return f;
}

// Author: Manos Papadakis
static arma::mat calc_dist(arma::mat xnew, arma::mat x,const bool is_euclidean) {
  	const int n=x.n_cols,nu=xnew.n_cols;
  	arma::mat disa(n,nu,arma::fill::zeros);
  	if(is_euclidean) 
    	for (int i=0;i<nu;++i)
      		disa.col(i)=sqrt_mat(sum(square(x.each_col() - xnew.col(i)),0));
  	else 
    	for(int i=0;i<nu;++i)
      		disa.col(i)=(sum(abs(x.each_col() - xnew.col(i)),0)).t();
  	return disa;
}

struct method_t {
	bool is_average;
	bool is_median;
	bool is_harmonic;
};

double hsum(std::vector<double>& vals, const unsigned int k) {
	if (vals.size() == k) {
		return 1 / vals[k - 1];
	}
	return (1 / vals[k - 1]) + hsum(vals, (k + 1));
}

std::vector<double> ext_vals(arma::vec& y, arma::umat& max_dist_idxs, const unsigned int row, 
		const arma::uvec& idxs, const unsigned int upto_col_idx) {
	std::vector<double> vals(idxs[upto_col_idx] + 1);
	for (unsigned int i = 0; i < vals.size(); ++i) {
		vals[i] = y[max_dist_idxs.at(row, i)];
	}
	return vals;
}

arma::mat calc_r_harm_ests(arma::vec& y, arma::mat& dists, arma::umat& max_dist_idxs, arma::uvec& idxs) {
	arma::mat ests(max_dist_idxs.n_rows, idxs.size()); 
	for (unsigned int i = 0; i < max_dist_idxs.n_rows; ++i) {
		for (unsigned int j = 0; j < idxs.size(); ++j) {
			std::vector<double> y_vals = ext_vals(y, max_dist_idxs, i, idxs, j);
			const double harm_sum = hsum(y_vals, 1);
			ests.at(i, j) = (idxs[j] + 1) / harm_sum;
		}
	}
	return ests;
}

arma::mat calc_r_med_ests(arma::vec& y, arma::mat& dists, arma::umat& max_dist_idxs, arma::uvec& idxs) {
	arma::mat ests(max_dist_idxs.n_rows, idxs.size()); 
	for (unsigned int i = 0; i < max_dist_idxs.n_rows; ++i) {
		for (unsigned int j = 0; j < idxs.size(); ++j) {
			std::vector<double> y_vals = ext_vals(y, max_dist_idxs, i, idxs, j);
			ests.at(i, j) = calc_med(y_vals);
		}
	}
	return ests;
}

arma::mat calc_r_aver_ests(arma::vec& y, arma::mat& dists, arma::umat& max_dist_idxs, arma::uvec& idxs) {
	arma::mat ests(max_dist_idxs.n_rows, idxs.size()); 
	for (unsigned int i = 0; i < max_dist_idxs.n_rows; ++i) {
		for (unsigned int j = 0; j < idxs.size(); ++j) {
			std::vector<double> y_vals = ext_vals(y, max_dist_idxs, i, idxs, j);
			ests.at(i, j) = std::accumulate(y_vals.begin(), y_vals.end(), 0.0) / (idxs[j] + 1);
		}
	}
	return ests;
}

double find_rand_freq(std::vector<double>& vals, std::mt19937& rd_gen) {
	const unsigned int vals_size = vals.size();
	std::sort(vals.begin(), vals.end());
	std::unordered_map<double, unsigned int> val_idx;
	double prev = vals[0];
	val_idx[vals[0]] = 0;
	unsigned int curr_count = 1; unsigned int max_count = 1;
	for (unsigned int i = 1; i < vals_size; ++i) {
		if (vals[i] == prev) {
			++curr_count;
		}
		else {
			if (curr_count > max_count || curr_count == max_count) {
				if (curr_count > max_count) {
					val_idx.clear();
					max_count = curr_count;
				}
				val_idx[vals[i - 1]] = i - 1;
			}
			prev = vals[i];
			curr_count = 1;
		}
	}
	if (curr_count == 1 && max_count == 1) {
		std::uniform_int_distribution<unsigned int> rnd(0, vals.size() - 1);
		return vals[rnd(rd_gen)];
	}
	if (curr_count > max_count || curr_count == max_count) {
		if (curr_count > max_count) {
			val_idx.clear();
			max_count = curr_count;
		}
		val_idx[vals[vals_size - 1]] = vals_size - 1;
	}
	std::uniform_int_distribution<unsigned int> rnd(0, val_idx.size() - 1);
	return std::get<0>(*std::next(std::begin(val_idx), rnd(rd_gen)));
}

double find_first_freq(std::vector<double>& vals) {
	const int vals_size = vals.size();
	std::sort(vals.begin(), vals.end());
	double prev = vals[0];
	double freq = vals[0];
	int curr_count = 1; int max_count = 1;
	for (int i = 1; i < vals_size; ++i) {
		if (vals[i] == prev) {
			++curr_count;
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
	return curr_count > max_count ? vals[vals_size - 1] : freq;
}

arma::mat calc_c_ests(arma::vec& y, arma::mat& dists, arma::umat& max_dist_idxs, arma::uvec& idxs, 
		const unsigned int freq_option) {
	std::random_device rd;
	std::mt19937 rd_gen(rd());
	arma::mat ests(max_dist_idxs.n_rows, idxs.size()); 
	for (unsigned int i = 0; i < max_dist_idxs.n_rows; ++i) {
		for (unsigned int j = 0; j < idxs.size(); ++j) {
			std::vector<double> y_vals = ext_vals(y, max_dist_idxs, i, idxs, j);
			double most_freq;
			if (!freq_option) {
				most_freq = find_first_freq(y_vals);
			}
			else {
				most_freq = find_rand_freq(y_vals, rd_gen);
			}
			ests.at(i, j) = most_freq;
		}
	}
	return ests;
}

// Alters dst
void copy(std::vector<unsigned int> src, arma::umat& dst, const unsigned int row) {
	for (unsigned int i = 0; i < src.size(); ++i) {
		dst.at(row, i) = src[i];
	}
}

// Alters max_vals, max_val_idxs
std::vector<unsigned int> adj_max_vals(std::vector<double>& max_vals, std::vector<unsigned int>& max_val_idxs, 
		const double val, const unsigned int val_idx) {
	for (unsigned int i = 0; i < max_vals.size(); ++i) {
		if (max_vals[i] < val && i != max_vals.size() - 1) {
			continue;
		}
		else {
			unsigned int curr_idx = i;
			double curr_val = max_vals[curr_idx];
			unsigned int curr_val_idx = max_val_idxs[curr_idx];
			for (unsigned int j = curr_idx + 1; j < max_vals.size(); ++j) {
				const double tmp = max_vals[j];
				const unsigned int tmp_idx = max_val_idxs[j];
				max_vals[j] = curr_val;
				max_val_idxs[j] = curr_val_idx;
				curr_val = tmp;
				curr_val_idx = tmp_idx;
			}
			max_vals[i] = val;
			max_val_idxs[i] = val_idx;
			break;
		}
	}
	return max_val_idxs;
}

// Alters vals, val_idxs
unsigned int part(std::vector<double>& vals, std::vector<unsigned int>& val_idxs,
		const unsigned int pivot, const unsigned int size) {
	double val = vals[pivot];
	unsigned int curr_pos = pivot;
	for (unsigned int i = pivot + 1; i < size; ++i) {
		if (vals[i] <= val) {
			++curr_pos;
			std::swap(vals[curr_pos], vals[i]);
			std::swap(val_idxs[curr_pos], val_idxs[i]);
		}
	}
	std::swap(vals[curr_pos], vals[pivot]);
	std::swap(val_idxs[curr_pos], val_idxs[pivot]);
	return curr_pos;
}

void quick_sort(std::vector<double>& vals, std::vector<unsigned int>& val_idxs, 
		const unsigned int pivot, const unsigned int size) {
	unsigned int curr_pos;
	if (pivot >= size) {
		return;
	}
	curr_pos = part(vals, val_idxs, pivot, size);
	quick_sort(vals, val_idxs, pivot, curr_pos);
	quick_sort(vals, val_idxs, curr_pos + 1, size);
}

arma::umat calc_n_min(arma::mat& dists, const unsigned int max_num) {
	arma::umat max_dist_idxs(dists.n_rows, max_num);
	std::vector<double> max_vals(max_num);
	std::vector<unsigned int> max_val_idxs(max_num);
	unsigned int k_added = 0;
	for (unsigned int i = 0; i < dists.n_rows; ++i) {
		for (unsigned int j = 0; j < dists.n_cols; ++j) {
			if (k_added < max_num) {
				max_vals[k_added] = dists.at(i, j);
				max_val_idxs[k_added++] = j;
				if (k_added == max_num) {
					quick_sort(max_vals, max_val_idxs, 0, max_vals.size());
				}
			}
			else if (max_vals.back() > dists.at(i, j)) {
				adj_max_vals(max_vals, max_val_idxs, dists.at(i, j), j);
			}
		}
		copy(max_val_idxs, max_dist_idxs, i);
		k_added = 0;
	}
	return max_dist_idxs;
}

// Alters md
void store_method(const std::string method, method_t& md) {
	md.is_average = false;
	md.is_median = false;
	md.is_harmonic = false;
	if (!method.compare("average")) {
		md.is_average = true;
		return;
	}
	else if (!method.compare("median")) { 
		md.is_median = true;
		return;
	}
	else if (!method.compare("harmonic")) {
		md.is_harmonic = true;
		return;
	}
	Rcpp::stop("Method input invalid.\n");
}

bool is_type(const std::string type) {
	if (!type.compare("C")) {
		return true;
	}
	else if (!type.compare("R")) {
		return false;
	}
	Rcpp::stop("Type input invalid.\n");
}

bool is_dist_type(const std::string dist_type) {
	if (!dist_type.compare("euclidean")) {
		return true;
	}
	else if (!dist_type.compare("manhattan")) {
		return false;
	}
	Rcpp::stop("Distance type input invalid.\n");
}

arma::mat calc_k_nn(arma::mat& ds_extra, arma::vec& y, arma::mat& ds, arma::uvec& idxs,
		const std::string dist_type, const std::string type, const std::string method,
		const unsigned int freq_option) {
	method_t md = {0};
	const bool is_euclidean = is_dist_type(dist_type);
	const bool is_type_c = is_type(type);
	if (!is_type_c) {
		store_method(method, md);
	}
	db_print("Adjusting indexes.\n");
	db_print("Calculating distance.\n");
	arma::mat dists = arma::trans(calc_dist(arma::trans(ds_extra), arma::trans(ds), is_euclidean));
	db_print("Calculating max n distance.\n");
	arma::umat max_dist_idxs = calc_n_min(dists, idxs[idxs.size() - 1] + 1);
	db_print("Calculating estimates.\n");
	arma::mat ests;
	if (is_type_c) {
		db_print("Calculating C estimate.\n");
		ests = calc_c_ests(y, dists, max_dist_idxs, idxs, freq_option);
	}
	else {
		if (md.is_average) {
			db_print("Calculating R-average estimate.\n");
			ests = calc_r_aver_ests(y, dists, max_dist_idxs, idxs);
		}
		else if (md.is_median) {
			db_print("Calculating R-median estimate.\n");
			ests = calc_r_med_ests(y, dists, max_dist_idxs, idxs);
		}
		else {
			db_print("Calculating R-harmonic estimate.\n");
			ests = calc_r_harm_ests(y, dists, max_dist_idxs, idxs);
		}
	}
	return ests;
}
