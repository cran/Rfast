//Author: Manos Papadakis

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace std;
using namespace arma;
using namespace Rcpp;

#ifndef MN
#define MN

mat operator+(colvec &,mat &);
colvec operator^(colvec ,const int );
colvec operator-(colvec &,colvec &);
double operator*(vec ,vec );
colvec operator+(colvec &,colvec &);
colvec operator^(const char ,const colvec );
vec loga(vec &);
bool my_compare(const pair<string,int>& ,const pair<string,int>&);
mat design_matrix(CharacterVector);
vector<int> which_isFactor(DataFrame);
vec regression_only(mat, colvec);
long double regression_only_col(colvec , colvec );
unsigned int generatekey2(const string & ,unsigned  int );
static const size_t InitialFNV = 2166136261U;
static const size_t FNVMultiple = 16777619;
size_t generatekey1(const string &);
double digamma(double);
double trigamma(double);
#endif
