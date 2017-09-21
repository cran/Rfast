//Author: Stefanos Fafalios

#ifndef _reg_lib_
#define _reg_lib_

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace std;
using namespace arma;
using namespace Rcpp;

vec logistic_only2(mat x, vec,int);
vec poisson_only2(mat, vec, const double,int);
double getDeviance(int, NumericVector);
double* removeIdx(int, double *, int);
double* removeXColumn(int, double *, int, int);
double* removeDIdx(int, double *, int);
double* removeIdx_p(int, double *, int);
double calcylogy(vec, int);
void initXcols(double*, int, int);
double my_lchoose(int, int);
double gold_rat2(double, vec, vec, double, vec, const double);
vec prop_regs2(mat, vec,const double,int);
colvec log1pColvec(colvec, int);
mat bindColsToMat(vec, vec *, int, mat);
mat bindColsToMat2(int, vec *, int, mat);
vec* removeVecIdx(int, vec *, int);
double glm_logistic3(mat, vec,const double,double,int);
double glm_poisson_3(mat, vec, const double, const double,const double,int);
double prop_reg3(mat, vec, const double, double,int);
vec colsumsVec(mat, int);
vec colsumsVecminVec(mat,mat,int);
vec group_sum2(vec, NumericVector);
vec group_sum3(vec, vec);
int get_max(NumericVector);
double gold_rat(double, NumericVector, NumericVector, double, NumericVector, const double);
double calc_f(vec, double, vec, double, double, int);
mat design_matrix2(vec,bool);
vec colmeansVec(mat,int);
mat cross_x_2(mat);
mat cross_x_y_2(mat,mat);
mat create_id_mat(int);
double calc_multinom_ini(mat,vec);
mat removefirstcol(mat);
vec indexesOfNum(mat, int);
double sumabsmat(mat,mat, int);
vec rowsumsVec(mat);
double calcSumLog(mat, vec);
vector<double> sort_unique_double(vector<double>);
mat design_matrix2(CharacterVector,bool);
mat safe_exp_m1(mat,double,int);
mat colvec_mat_cbind(vec, mat);
double calcDevRes(mat,vec,mat);
double glm_logistic2(NumericMatrix, NumericVector, const double, double,int);
vec gold_rat3(double, vec, vec, double, vec, const double);
double rint_reg2(mat, vec, vec,vec, const double,int);
vec rint_regs2(mat, vec, vec, vec, const double, int,int);
double qpois_reg2(mat, vec,const double,const double,int);

#endif
