//Author: Manos Papadakis

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "templates.h"

using namespace std;
using namespace arma;
using namespace Rcpp;

#ifndef MN
#define MN

const double center = 1.4826;

mat operator+(colvec &,mat &);
colvec operator-(colvec &,colvec &);
double operator*(vec ,vec );
colvec operator+(colvec &,colvec &);
rowvec operator/(colvec x,double s);
bool my_compare_order_second(const pr<double,int>&,const pr<double,int>&);
bool descending_int(const int&,const int&);
bool descending_double(const double&,const double&);
bool descending_string(const string&,const string&);
bool cor_vecs(const pr<double,double>&,const pr<double,double>&);
bool s_indx_asc(const pr<int,double>&,const pr<int,double>&);
bool s_indx_des(const pr<int,double>&,const pr<int,double>&);
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
void fill_with_value(double *,double *,double);
rowvec colMedians(mat);
void combn(NumericVector&,const int,const int, vector<double>&,
       NumericMatrix&,int&);
NumericVector Tabulate(NumericVector,int &);
int my_round(const double);
double my_round_gen(double,const int);
int len_sort_unique_int(IntegerVector);
mat sqrt_mat(mat);
void max_neg_pos(int* ,int *,int &,int &,int &);
uvec Order_rmdp(colvec&);
rowvec colvar_rmdp(mat&);
double sum_pow(colvec,const double);
umat design_matrix_helper_big(CharacterVector);
colvec my_pow(colvec,const double);
NumericVector minus_mean(NumericVector&,const double);
NumericVector sqr(NumericVector&);
NumericVector group_sum(NumericVector,IntegerVector,const int);
void minus_c(double f[],double &,double *,int,int &);
double sumsqr(NumericMatrix &);
IntegerVector Order(NumericVector,const bool,const bool);
int True(int *,int *);
bool my_all(int* ,int *);
bool my_any(int* ,int *);
double total_dista(NumericMatrix, NumericMatrix,const bool);
double sum_sqrt_mat(mat);
colvec pnormc(colvec);
double sum_abs(mat,mat);
NumericVector toNumbers(string,char);
IntegerVector combine(IntegerVector,IntegerVector);
double med_helper(NumericVector::iterator,NumericVector::iterator);
double total_euclidean_dist(NumericMatrix,const bool);
NumericMatrix euclidean_dist(NumericMatrix,const bool);
#endif
