//Author: Manos Papadakis

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace std;
using namespace arma;
using namespace Rcpp;

#ifndef MN
#define MN

struct pr{
  double first;
  int second;
};

#define SWAP(x,y,tmp) { tmp=x; x=y; y=tmp; }

mat operator+(colvec &,mat &);
colvec operator^(colvec ,const int );
colvec operator-(colvec &,colvec &);
double operator*(vec ,vec );
colvec operator+(colvec &,colvec &);
colvec operator^(const char ,const colvec );
mat operator^(mat x,const int y);
rowvec operator/(colvec x,double s);
bool my_compare_order(const pr& a,const pr& b);
bool descending_int(const int& a,const int& b);
bool descending_double(const double& a,const double& b);
bool descending_string(const string& a,const string& b);
bool cor_vecs(const pair<double,double>& a,const pair<double,double>& b);
bool s_indx_asc(const pair<int,double>& a,const pair<int,double>& b);
bool s_indx_des(const pair<int,double>& a,const pair<int,double>& b);
NumericMatrix design_matrix_regr(CharacterVector x);
vector<int> which_isFactor(DataFrame);
vec regression_only(mat, colvec);
double regression_only_col(colvec , colvec& );
double digamma(double);
double trigamma(double);
void i4mat_floyd ( int,vector<double> &);
void min_max_d(double *,double *,double &, double &);
void min_max_i(int *,int *,int &, int &);
void max_d(double *,double *, double &);
void max_i(int *,int *, int &);
void min_d(double *,double *, double &);
void min_i(int *,int *, int &);
colvec Digamma_v(colvec ,int &);
colvec Trigamma_v(colvec ,int &);
void fill_m(double *,double *,double);
rowvec colMedians(mat);
void combn(NumericVector&,const int,const int, vector<double>&,
       NumericMatrix&,int&);
NumericVector Tabulate(NumericVector,int &);
int my_round(const double);
double my_round_gen(double,const int);
int len_sort_unique_int(IntegerVector);
mat sqrt_mat(mat);
void fill_with_log(double *,double *,double *);
void max_neg_pos(int* ,int *,int &,int &,int &);
uvec Order_rmdp(colvec&);
rowvec colvar_rmdp(mat&);
double sum_pow(colvec,const double);
umat design_matrix_helper_big(CharacterVector);
colvec my_pow(colvec,const double);
NumericVector minus_mean(NumericVector&,const double);
NumericVector sqr(NumericVector&);
NumericVector group_sum(NumericVector, IntegerVector);
int increment_maybe(int, double);
void minus_c(double f[],double &,double *,int,int &);

#endif
