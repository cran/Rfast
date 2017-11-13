//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include <cmath>
#include "reg_lib.h"
#include "reg_lib2.h"
#include "my_k_sorted_array.h"
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix dir_knn(NumericMatrix tXnew, NumericMatrix tX, NumericVector Y, NumericVector K, const std::string type, const bool parallel){

  double (*_function_type_)(vec, a_node*, const int);

  if(type=="C")
    _function_type_ = &most_frequent_value;
  else
    _function_type_ = &average_value;

  int d = tX.nrow(), n = tX.ncol(), nu = tXnew.ncol(), klen = K.size();
  mat txnew(tXnew.begin(), d, nu,false), tx(tX.begin(), d,n,false);
  vec y(Y.begin(), n,false);
  int k0 = max(K);

  if(k0>n)
    k0=n;

  NumericMatrix g = NumericMatrix(nu,klen);

  #ifdef _OPENMP
  #pragma omp parallel if(parallel)
  {
  #endif
    // create a sorted array of size k0
    a_node* myarray = init_array(k0);
    double tmpsum;
    #ifdef _OPENMP
    #pragma omp for
    #endif
    for (int l = 0; l < nu; l++) {
      for(int i = 0; i < n; i++){
        tmpsum = sum(tx.col(i)%txnew.col(l));
        if(tmpsum > 1)
          tmpsum = 1;
        else if(tmpsum < -1)
          tmpsum = -1;
        k_sorted_put(myarray,k0,i,acos(tmpsum));
      }

      for(int j = 0; j < klen; j++)
        g(l,j)= _function_type_(y, myarray, K[j]);

        // make all elements of the array "invalid" so that the next iteration can begin
        myarray =  refresh_array(myarray, k0);
    }
    clear_array(myarray);
  #ifdef _OPENMP
  }
  #endif

  return g;
}


RcppExport SEXP Rfast_dir_knn(SEXP tXnewSEXP,SEXP tXSEXP,SEXP YSEXP,SEXP KSEXP,SEXP typeSEXP,SEXP parallelSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type tXnew(tXnewSEXP);
    traits::input_parameter< NumericMatrix >::type tX(tXSEXP);
    traits::input_parameter< NumericVector >::type Y(YSEXP);
    traits::input_parameter< NumericVector >::type K(KSEXP);
    traits::input_parameter< const string >::type type(typeSEXP);
    traits::input_parameter< const bool >::type parallel(parallelSEXP);
    __result = wrap(dir_knn(tXnew,tX,Y,K,type,parallel));
    return __result;
END_RCPP
}
