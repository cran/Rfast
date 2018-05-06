//Author: Stefanos Fafalios

#include <RcppArmadillo.h>
#include "templates.h"
#include <cmath>
#include "reg_lib.h"


using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
NumericMatrix multinom_regs(NumericVector Y0, NumericMatrix X0, const double tol,
                            const bool logged, const int maxiters){
  int n = X0.nrow(), D = X0.ncol();

  mat x(X0.begin(), n,D,false);
  mat Y1 = design_matrix_helper<mat,NumericVector>(Y0);

  vec tmpvec(Y1.n_cols);

  tmpvec = conv_to<vec>::from(mean(Y1));

  double ini = calc_multinom_ini(Y1,tmpvec);

  mat Y(n,Y1.n_cols-1);
  for(unsigned int i = 0; i < Y1.n_cols-1;i++)
    Y.col(i) = Y1.col(i+1);

  n = Y.n_rows;
  int d = Y.n_cols;

  mat id(2,d),b10(2,d),e0(d,n);

  rowvec m0(d),b0(d);
  for(int i = 0; i < d; i++)
    m0[i] = tmpvec[i+1];

  b0 = log(m0/(1-m0));
  b10.row(0) = b0;
  e0 = Y.each_row()-m0;

  id = create_id_mat(d);

  vec poia = indexesOfNum(Y1,1);
  int poiasize = poia.size();
  double exp20 = exp(20);
  vec loglik(D);
  int dx2 = 2*d;

  colvec one(n,fill::ones);
  /*--------------------------------------------------------*/
    
      mat dera(n,dx2),der2(dx2,dx2,fill::zeros), b1(2,d),b2(2,d),m1(n, d),m(n,d),e1(n,d),ma(n,d+1),crossress(2,2),X(n,2);
      vec der(dx2),idcoli(2),idcolj(2),mcolij(n),slv;
      mat::iterator slvit, b2it, b1it;
      int i=0,j=0,ij=0, k=0;
      X.col(0) = one;

      for(int l = 0; l < D; l++) {
        X.col(1) = x.col(l);

        for(i = 0; i < d; i++) {
          idcoli = id.col(i);

          dera.col(idcoli[0]) = e0.col(i)%X.col(0);
          dera.col(idcoli[1]) = X.col(1)%e0.col(i);
          for (j = i; j < d; j++) {
            if (i != j) {
              idcolj = id.col(j);
              crossress = -cross_x_y<mat,mat,vec>(m0(i) * m0(j) * X, X);
              der2(idcolj[0], idcoli[0]) =  crossress(0,0);
              der2(idcolj[0], idcoli[1]) =  crossress(0,1);
              der2(idcolj[1], idcoli[0]) =  crossress(1,0);
              der2(idcolj[1], idcoli[1]) =  crossress(1,1);

              der2(idcoli[0], idcolj[0]) =  der2(idcolj[0], idcoli[0]);
              der2(idcoli[0], idcolj[1]) =  der2(idcolj[0], idcoli[1]);
              der2(idcoli[1], idcolj[0]) =  der2(idcolj[1], idcoli[0]);
              der2(idcoli[1], idcolj[1]) =  der2(idcolj[1], idcoli[1]);
            }
            else {
              crossress = cross_x_y<mat,mat,vec>((m0(i) * (1 - m0(i))) * X, X);

              der2(idcoli[0], idcoli[0]) =  crossress(0,0);
              der2(idcoli[0], idcoli[1]) =  crossress(0,1);
              der2(idcoli[1], idcoli[0]) =  crossress(1,0);
              der2(idcoli[1], idcoli[1]) =  crossress(1,1);
            }
          }
        }
        der = conv_to<vec>::from(sum(dera));

        b1 = b10;

        slv = solve(der2, der);

        b2it = b2.begin();
        b1it = b1.begin();
        slvit = slv.begin();
        for(k=0;k<dx2;k++,b1it++,slvit++,b2it++)
          *b2it = (*b1it)+(*slvit);

        ij=2;
        while(ij++<maxiters && accu(abs(b2-b1)) > tol) {
          b1 = b2;

          m1 = clamp(exp(X*b1),0,exp20);

          m = m1.each_col()/ (sum(m1,1) + 1);

          e1 = Y - m;
          for(i = 0; i<d; i++) {
            idcoli = id.col(i);
            dera.col(idcoli[0]) = e1.col(i)%X.col(0);
            dera.col(idcoli[1]) = e1.col(i)%X.col(1);

            for (j = 0; j<d; j++) {
              if (i != j) {
                idcolj = id.col(j);

                crossress = -cross_x_y<mat,mat,vec>(X.each_col()%(m.col(i) % m.col(j)), X);
                der2(idcoli[0], idcolj[0]) =  crossress(0,0);
                der2(idcoli[0], idcolj[1]) =  crossress(0,1);
                der2(idcoli[1], idcolj[0]) =  crossress(1,0);
                der2(idcoli[1], idcolj[1]) =  crossress(1,1);

                der2(idcolj[0], idcoli[0]) =  der2(idcoli[0], idcolj[0]);
                der2(idcolj[0], idcoli[1]) =  der2(idcoli[0], idcolj[1]);
                der2(idcolj[1], idcoli[0]) =  der2(idcoli[1], idcolj[0]);
                der2(idcolj[1], idcoli[1]) =  der2(idcoli[1], idcolj[1]);
              }
              else {
                crossress = cross_x_y<mat,mat,vec>(X.each_col()%(m.col(i) % (one - m.col(i))), X);

                der2(idcoli[0], idcoli[0]) =  crossress(0,0);
                der2(idcoli[0], idcoli[1]) =  crossress(0,1);
                der2(idcoli[1], idcoli[0]) =  crossress(1,0);
                der2(idcoli[1], idcoli[1]) =  crossress(1,1);
              }
            }
          }
          der = conv_to<vec>::from(sum(dera));

          slv = solve(der2, der);
          b2it = b2.begin();
          b1it = b1.begin();
          slvit = slv.begin();
          for(k=0;k<dx2;k++,b1it++,slvit++,b2it++)
            *b2it = (*b1it)+(*slvit);
        }

        ma = colvec_mat_cbind(one, m1);

        ma = ma.each_col() / sum(ma,1);

        loglik(l) = calcSumLog(ma,poia,poiasize);
      }

  NumericMatrix ret(D,2);

      for(int i = 0; i < D; i++){
        ret(i,0) = 2 * loglik(i) - ini;
        ret(i,1) = R::pchisq(ret(i,0), d, false, logged);
      }
  return ret;
}

RcppExport SEXP Rfast_multinom_regs(SEXP Y0SEXP,SEXP X0SEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP maxitersSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type Y0(Y0SEXP);
    traits::input_parameter< NumericMatrix >::type X0(X0SEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const bool >::type logged(loggedSEXP);
    traits::input_parameter< const int >::type maxiters(maxitersSEXP);
    __result = wrap(multinom_regs(Y0,X0,tol,logged,maxiters));
    return __result;
END_RCPP
}
