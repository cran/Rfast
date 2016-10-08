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
mat design_matrix(CharacterVector);
vector<int> which_isFactor(DataFrame);
vec regression_only(mat, colvec);
long double regression_only_col(colvec , colvec );
double digamma(double);
double trigamma(double);
void i4mat_floyd ( int n, vector<double> &a );

#endif
