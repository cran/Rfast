//Author: Stefanos Fafalios

#ifndef _reg_lib_
#define _reg_lib_

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace std;
using namespace arma;
using namespace Rcpp;

vec logistic_only2(mat x, vec,int);
vec poisson_only2(mat, vec,int);
double getDeviance(int, vec);
double* removeIdx(int, double *, int);
double* removeXColumn(int, double *, int);
double* removeDIdx(int, double *, int);
double calcylogy(vec,int);
void initXcols(double*, int);
double my_lchoose(int, int);
double gold_rat2(double, vec, vec, double, vec,const int, const double);
double vmf_mle2(double, const int, const double, const double);
vec prop_regs2(mat, vec,const double,int);
colvec log1pColvec(colvec,int);
double log1pColvecSum(colvec,int);
mat bindColsToMat(vec, vec *, int, mat);
mat bindColsToMat2(int, vec *, int, mat);
vec* removeVecIdx(int, vec *, int);
double glm_logistic3(mat, vec,const double,double,int);
double glm_poisson_3(mat, vec, const double,const double,int);
double prop_reg3(mat, vec, const double, double,int);
double calc_f(vec, double, vec, double, double, int);
mat create_id_mat(int);
double calc_multinom_ini(mat,vec);
vec indexesOfNum(mat,int);
double sumabsmat(mat,mat,int);
double calcSumLog(mat,vec,int);
vector<double> sort_unique_double(vector<double>);
mat colvec_mat_cbind(vec, mat);
double calcDevRes(mat,vec,mat);
vec gold_rat3(double, vec, vec, double, vec, const int, const double);
double rint_reg2(mat, vec, vec, IntegerVector, int, const double,int);
vec rint_regs2(mat, vec, vec, IntegerVector, const double, int,int);
double qpois_reg2(mat, vec,const double,const double,int);
List varcomps_mle(NumericVector,IntegerVector,const int,const double);
double varcomps_mle2(vec, IntegerVector,int,const double);
double rint_mle2(vec, vec, IntegerVector, const double, int);
double bic_rint_reg(mat, vec, vec, IntegerVector, const int, const double, int);
vec bic_rint_regs(mat, vec, vec, IntegerVector, const double, int, int);
List glm_poisson_2(mat, vec, const double,const double, const int);
vec my_pow2(vec,vec,const double,const int);
vec weibull_mle2(vec, int, const double, const int);
mat varcomps_mle3(vec, IntegerVector,const int, int,const bool, const double,const int);
double spml_mle2(mat, vec, vec, vec, const int, const double, const int);

#endif
