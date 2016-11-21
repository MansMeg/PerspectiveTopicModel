#include <Rcpp.h>
using namespace Rcpp;
//' @title
//' rcategorical
//' @description
//' Sample a categorical variable from
//'
//' @param p a vector of (pssoble unnormalized) probabilities.
//'
// [[Rcpp::interfaces(r, cpp)]]
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


/*** R
set.seed(4711)
u_prob <- c(0.7, 1.7, 1)
prob <- u_prob / sum(u_prob)
count <- integer(3)
n <- 10000
for(i in 1:n){
    x <- rcategorical(c(0.7, 1.7, 1))
    count[x] <- count[x] + 1
}

stopifnot(sum(count) == n)

stopifnot(chisq.test(count, p = prob)$p.value > 0.001)

*/
