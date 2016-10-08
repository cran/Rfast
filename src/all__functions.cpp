// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include <cstdlib>
#include "g2t.h"

using namespace std;
using namespace Rcpp;

/////////////// GEORGE ///////////////////////////

//[[Rcpp::export]]
List g2Test(NumericMatrix data, int x, int y, NumericVector cs, NumericVector dc){
  int *ics = new int[cs.size()];
  int *idc = new int[dc.size()];
  for (int i = 0; i < cs.size(); ++i) {
    ics[i] = (int) cs[i] - 1;
  }
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  TestResult result = g2Test(data, x-1, y-1, ics, cs.size(), idc);

  delete[] ics;
  delete[] idc;  
  
  List out;
  out["statistic"] = result.stat;
  out["df"] = result.df;
  return out;
}

// [[Rcpp::export]]
List g2Test_univariate_perm(NumericMatrix data, NumericVector dc, int nperm) {
  int nvars = data.ncol();
  int *ics = new int[0];
  int *idc = new int[dc.size()];
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  int nout = nvars * (nvars - 1) / 2;
  NumericVector xout(nout);
  NumericVector yout(nout);
  NumericVector pvalues(nout);
  NumericVector statistics(nout);
  
  int idx = 0;
  for(int i = 0; i < nvars; ++i) {
    for(int j = i+1; j < nvars; ++j) {
      TestResult result = permG2Test(data, i, j, ics, 0, idc, nperm);
      xout[idx] = i + 1;
      yout[idx] = j + 1;
      pvalues[idx] = result.pvalue;
      statistics[idx] = result.stat;
      ++idx;
    }
  }
  
  delete[] ics;
  delete[] idc;  
  
  List out;
  out["statistic"] = statistics;
  out["pvalue"] = pvalues;
  out["x"] = xout;
  out["y"] = yout;
  return out;
}

// [[Rcpp::export]]
List g2Test_univariate(NumericMatrix data, NumericVector dc) {
  int nvars = data.ncol();
  int *idc = new int[dc.size()];
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  int nout = nvars * (nvars - 1) / 2;
  NumericVector xout(nout);
  NumericVector yout(nout);
  NumericVector statistics(nout);
  NumericVector df(nout);
  
  int idx = 0;
  for(int i = 0; i < nvars; ++i) {
    for(int j = i+1; j < nvars; ++j) {
      TestResult result = g2Test(data, i, j, idc);
        xout[idx] = i + 1;
        yout[idx] = j + 1;
        statistics[idx] = result.stat;
        df[idx] = (idc[i] - 1) * (idc[j] - 1);
        ++idx;
    }
  }
  
  delete[] idc;  
  
  List out;
  out["statistic"] = statistics;
  out["x"] = xout;
  out["y"] = yout;
  out["df"] = df;
  return out;
}

// [[Rcpp::export]]
List g2Test_perm(NumericMatrix data, int x, int y, NumericVector cs, NumericVector dc, int nperm) {
  int *ics = new int[cs.size()];
  int *idc = new int[dc.size()];
  for (int i = 0; i < cs.size(); ++i) {
    ics[i] = (int) cs[i] - 1;
  }
  for (int i = 0; i < dc.size(); ++i) {
    idc[i] = (int) dc[i];
  }
  
  TestResult result = permG2Test(data, x-1, y-1, ics, cs.size(), idc, nperm);
  
  delete[] ics;
  delete[] idc;  
  
  List out;
  out["statistic"] = result.stat;
  out["pvalue"] = result.pvalue;
  out["x"] = x;
  out["y"] = y;
  out["df"] = result.df;
  return out;
}

////////////////// GEORGE //////////////////////////////////////////////////
