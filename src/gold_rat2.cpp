//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "mn.h"
#include "math.h"
#include "reg_lib.h"
#include <iostream>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
using namespace std;

double gold_rat2(double n, vec ni, vec ni2, double S, vec hi2,const int size, const double tol){
  double a = 0, b = 50;
  const double ratio=0.618033988749895;
  double x1=b-ratio*b, x2=ratio*b;

  vec nix1(size),nix2(size), ni2hi2(size);
  nix1 = ni*x1;
  nix2 = ni*x2;
  ni2hi2 = ni2%hi2;
  double f1 = calc_f(nix1, n, ni2hi2, S, x1, size);
  double f2 = calc_f(nix2, n, ni2hi2, S, x2, size);
  double bmina = b - a;
  while (abs(bmina)>tol){
    if(f2>f1){
      b=x2;
      bmina = b - a;
      x2=x1;
      f2=f1;
      x1=b - ratio * (bmina);
      nix1 = ni*x1;
      f1 = calc_f(nix1, n, ni2hi2, S, x1, size);
    }
    else {
      a=x1;
      bmina = b - a;
      x1=x2;
      f1=f2;
      x2=a + ratio * (bmina);
      nix2 = ni*x2;
      f2 = calc_f(nix2, n, ni2hi2, S, x2, size);
    }
  }

  return 0.5*(x1+x2);
}
