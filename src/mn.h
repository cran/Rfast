//Author: Manos Papadakis

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace std;
using namespace arma;
using namespace Rcpp;

#ifndef MN
#define MN

#define SWAP(x,y,tmp) { tmp=x; x=y; y=tmp; }

mat operator+(colvec &,mat &);
colvec operator^(colvec ,const int );
colvec operator-(colvec &,colvec &);
double operator*(vec ,vec );
colvec operator+(colvec &,colvec &);
colvec operator^(const char ,const colvec );
vec loga(vec &);
bool my_compare1(const pair<string,int>& ,const pair<string,int>&);
bool my_compare_order(const pair<double,int>& ,const pair<double,int>&);
bool descending(const double& a,const double& b);
bool cor_vecs(const pair<double,double>& a,const pair<double,double>& b);
bool s_indx_asc(const pair<int,double>& a,const pair<int,double>& b);
bool s_indx_des(const pair<int,double>& a,const pair<int,double>& b);
mat design_matrix(CharacterVector, bool);
vector<int> which_isFactor(DataFrame);
vec regression_only(mat, colvec);
double regression_only_col(colvec , colvec );
double digamma(double);
double trigamma(double);
void i4mat_floyd ( int n, vector<double> &a );
void min(double *start,double *end,double &mn);
void max(double *start,double *end, double &mx);
void min_max2(double *start,double *end,double &min, double &max);
void min_max2(double *start,double *end,double &min, double &max);
void min_max3(int *start,int *end,int &min, int &max);
void max_d(double *start,double *end, double &mx);
void max_i(int *start,int *end, int &mx);
void min_d(double *start,double *end, double &mn);
void min_i(int *start,int *end, int &mn);
void copy_sexp_d(double *x,double y[],int &len);
void copy_sexp_i(int *x,int y[],int &len);
#endif
