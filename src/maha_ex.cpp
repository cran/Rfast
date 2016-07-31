
#include <RcppArmadillo.h>
using namespace Rcpp;

arma::vec mahaInt(arma::mat & X,arma::vec & mu,arma::mat & sigma,const unsigned int ncores,const bool isChol);

RcppExport SEXP Rfast_mahaCpp(SEXP X, SEXP mu, SEXP sigma, SEXP ncores, SEXP isChol)
{
    RNGScope __rngScope;
    try{
      
      arma::mat X_ = as<arma::mat>(X);
      arma::vec mu_ = as<arma::vec>(mu);  
      arma::mat sigma_ = as<arma::mat>(sigma); 
      unsigned int ncores_ = as<unsigned int>(ncores);
      bool isChol_ = as<bool>(isChol);
        
      NumericVector dist = wrap( mahaInt(X_, mu_, sigma_, ncores_, isChol_) );
      dist.attr( "dim" ) = R_NilValue;
      
      return dist;
      
    } catch( std::exception& __ex__){
      forward_exception_to_r(__ex__);
    } catch(...){
      ::Rf_error( "c++ exception (unknown reason)" );
    }
    return wrap(NA_REAL);
}
