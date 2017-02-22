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
  pr(){}
  pr(double x,int y){
    first=x;
    second=y;
  }
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
bool cor_vecs(const pair<double,double>& a,const pair<double,double>& b);
bool s_indx_asc(const pair<int,double>& a,const pair<int,double>& b);
bool s_indx_des(const pair<int,double>& a,const pair<int,double>& b);
NumericMatrix design_matrix(CharacterVector,bool);
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
rowvec colMedians(mat&);
void combn(NumericVector&,const int,const int, vector<double>&,
       NumericMatrix&,int&);
#endif
