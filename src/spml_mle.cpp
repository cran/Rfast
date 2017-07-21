// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace arma;
using namespace Rcpp;

//[[Rcpp::export]]
List spml_reg_helper(mat B1,mat B2,mat x,mat u,colvec ci,colvec si,const double con,const double tol){
	int i=2;
  	const double f=-0.5;
	mat tx=x.t(),a11,a12,a22,mu,der2,tmp;
	colvec tau,ptau,rat,psit,psit2,der,ctmp,cisq=square(ci),sisq=square(si);
	while ( sum_abs(B2,B1) > tol ) {
		++i;
		B1 = B2;
		mu = x * B1;
		tau = sum(u % mu,1);
		ptau = pnormc(tau);
		rat = ptau / ( exp(f * square(tau))/con + tau % ptau );
		psit = tau + rat;
		psit2 = 2 - tau % rat - square(rat);
		tmp=tx * (  psit % u.each_col() - mu);
		der = colvec(tmp.begin(),tmp.n_elem,false);
		a11 = tx * ( x.each_col() % (psit2 % cisq - 1) );
		a12 = tx * ( x.each_col() % (psit2 % ci % si ) );
		a22 = tx * ( x.each_col() % (psit2 % sisq - 1 ) );
		der2 = join_cols( join_rows(a11, a12), join_rows(a12, a22) );
		ctmp=solve(der2, der);
		tmp=mat(ctmp.begin(),x.n_cols,2,false);
		B2 = B1 - tmp;
	}
	List l;
	l["B2"]=B2;
	l["mu"]=mu;
	l["tau"]=tau;
	l["ptau"]=ptau;
	l["der2"]=der2;
	l["i"]=i;
	return l;
}

RcppExport SEXP Rfast_spml_reg_helper(SEXP B1SEXP,SEXP B2SEXP,SEXP xSEXP,SEXP uSEXP,SEXP ciSEXP,SEXP siSEXP,SEXP conSEXP,SEXP tolSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type B1(B1SEXP);
    traits::input_parameter< mat >::type B2(B2SEXP);
    traits::input_parameter< mat >::type x(xSEXP);
    traits::input_parameter< mat >::type u(uSEXP);
    traits::input_parameter< colvec >::type ci(ciSEXP);
    traits::input_parameter< colvec >::type si(siSEXP);
    traits::input_parameter< const double >::type con(conSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    __result = wrap(spml_reg_helper(B1,B2,x,u,ci,si,con,tol));
    return __result;
END_RCPP
}
