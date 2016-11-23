#include <Rcpp.h>
using namespace Rcpp;
//' @title
//' rcategorical
//' @description
//' Sample a categorical variable from an unnormalized proportion
//'
//' @param p a vector of (possoble unnormalized) probabilities to sample from
//'
// [[Rcpp::export]]
int rcategorical(NumericVector p){
  int K = p.length();
  double cumsum = 0;

  // Calculate cumsum
  for (int k = 0; k < K; k++) {
    cumsum += p(k);
    // Rcout << " cumsum:" << cumsum << std::endl;
  }

  double u = R::runif(0, 1) * cumsum;

  int x = 0;
  while(u < cumsum) {
    // Rcout << "u:" << u << " cumsum:" << cumsum << std::endl;
    cumsum -= p(x);
    x++;
  }
  x--;
  return x + 1;
}

