#include <Rcpp.h>
#include "rcategorical.h"

using namespace Rcpp;

//' @title
//' Collapsed Gibbs sampler
//'
//' @description
//' The Standard Collapsed Gibbs sampler
//'
//' @param state a perspective model state file
//' @param count_matrices A list of count matrices
//' @param priors A list of priors
//' @param constants A list of constants
//'
//' @export
//[[Rcpp::export]]
List collapsed_sampler_cpp(DataFrame state, List count_matrices, List priors, List constants) {

  // Define constants
  // int D = constants["D"]; // Not currently used
  int K = constants["K"];
  int N = constants["N"];
  int V = constants["V"];

  // Define data
  IntegerVector doc = state["doc"];
  IntegerVector type = state["type"];
  IntegerVector topic = state["topic"];

  // Define counts
  IntegerMatrix n_dk = count_matrices["n_dk"];
  IntegerMatrix n_kv = count_matrices["n_kv"];
  IntegerVector n_k = count_matrices["n_k"];

  // Define priors
  double alpha = priors["alpha"];
  double beta = priors["beta"];

  // Memory allocation and pre calculations
  int d, k, v;
  int new_k;
  NumericVector u_prob(K);
  double beta_sum = beta * V;

  for (int i = 0; i < N; ++i) {
    // Rcout << "Type: " << v << " D: " << d << std::endl;
    d = doc[i] - 1;
    k = topic[i] - 1;
    v = type[i] - 1;

    // Remove current position in matrices
    n_dk(d, k) -= 1;
    n_kv(k, v) -= 1;
    n_k(k) -= 1;

    // Calculate unnormalized probabilities
    for (int j = 0; j < K; ++j) {
      u_prob[j] = (n_kv(j, v) + beta) / (n_k(j) + beta_sum);
      u_prob[j] *= (n_dk(d, j) + alpha);
    }

    // Draw indicator
    new_k = rcategorical(u_prob) - 1; // rcatgorical is R indexed

    // Set topic indicator (R indexing)
    topic(i) = new_k + 1;

    // Add current position
    n_dk(d, new_k) += 1;
    n_kv(new_k, v) += 1;
    n_k(new_k) += 1;
  }

  // Return List
  List ret, cnt_mat, tmp;
  ret["state"] = state;
  cnt_mat["n_dk"] = n_dk;
  cnt_mat["n_kv"] = n_kv;
  cnt_mat["n_k"] = n_k;
  ret["count_matrices"] = cnt_mat;

  tmp["u_prob"] = u_prob;
  tmp["new_k"] = new_k;
  ret["tmp"] = tmp;

  return ret;
}

