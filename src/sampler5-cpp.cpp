#include <Rcpp.h>
#include "pos3d.h"
#include "rcategorical.h"

using namespace Rcpp;

//' @title
//' C++ Cache optimized Perspective sampler with prior on Phi, Theta and stearing perspectives
//'
//' @description
//' C++ Cache optimized Perspective sampler with prior on Phi and Theta
//'
//' @param state a perspective model state file
//' @param count_matrices A list of count matrices
//' @param priors A list of priors
//' @param constants A list of constants
//'
//[[Rcpp::export]]
List per_sampler4_cpp(DataFrame state, List count_matrices, List priors, List constants) {

  // Define constants
  int D = constants["D"];
  int K = constants["K"];
  int N = constants["N"];
  int V = constants["V"];
  int P = constants["P"];

  // Define data
  IntegerVector doc = state["doc"];
  IntegerVector type = state["type"];
  IntegerVector topic = state["topic"];
  IntegerVector party = state["party"];
  IntegerVector perspective = state["perspective"];

  // Define counts
  IntegerMatrix n_dk = count_matrices["n_dk"];
  IntegerVector n_kvpx = count_matrices["n_kvpx"];
  IntegerVector n_kvpx_dims = n_kvpx.attr("dim");
  IntegerVector n_kpx = count_matrices["n_kpx"];
  IntegerVector n_kpx_dims = n_kpx.attr("dim");
  IntegerMatrix n_pk = count_matrices["n_pk"];
  IntegerMatrix n_xk = count_matrices["n_xk"];

  // Define priors
  double alpha = priors["alpha"];
  double betax0 = priors["betax0"];
  double betax1 = priors["betax1"];
  double alpha_pi = priors["alpha_pi"];
  double beta_pi = priors["beta_pi"];
  LogicalVector prior_types = priors["tmp_prior_types"];
  IntegerVector prior_types_map = priors["tmp_prior_types_map"];
  List prior_types_indicator = priors["tmp_prior_types_indicator"];
  LogicalVector prior_doc = priors["tmp_prior_doc"];
  IntegerVector prior_doc_map = priors["tmp_prior_doc_map"];
  List prior_doc_indicator = priors["tmp_prior_doc_indicator"];
  LogicalVector perspective_flag = priors["tmp_perspective_flag"];

  // Memory allocation and pre calculations
  int d, k, v, p, x, px, kx;
  int new_k, new_x, new_kx, new_px;
  NumericVector u_prob(K * 2);
  double alpha_beta_pi = alpha_pi + beta_pi;
  double beta_x0_sum = betax0 * V;
  double beta_x1_sum = betax1 * V;
  double n_dj_alpha, alpha_beta_pi_n_pk;
  LogicalVector prior_types_indicator_v(K);
  LogicalVector prior_doc_indicator_d(K);

  for (int i = 0; i < N; ++i) {
    // Rcout << "Type: " << v << " D: " << d << std::endl;
    d = doc[i] - 1;
    k = topic[i] - 1;
    v = type[i] - 1;
    p = party[i] - 1;
    x = perspective[i];
    px = x * (p + 1);
    if(prior_types[v]){
      // prior_types_map is R indexed
      prior_types_indicator_v = prior_types_indicator[prior_types_map[v] - 1];
      // Rcout << "Type: " << v + 1 << " Logical vector: " << prior_types_indicator_v << std::endl;
    }
    if(prior_doc[d]){
      // prior_doc_map is R indexed
      prior_doc_indicator_d = prior_doc_indicator[prior_doc_map[d] - 1];
      // Rcout << "Doc: " << d + 1 << " Logical vector: " << prior_doc_indicator_d << std::endl;
    }

    // Remove current position in matrices
    n_dk(d, k) -= 1;
    n_kvpx(pos3d(k, v, px, n_kvpx_dims)) -= 1;
    n_kpx(pos3d(k, p, x, n_kpx_dims)) -= 1;
    n_pk(p, k) -= 1;
    n_xk(x, k) -= 1;

    // Calculate unnormalized probabilities
    for (int j = 0; j < K; ++j) {
      n_dj_alpha = n_dk(d, j) + alpha;
      alpha_beta_pi_n_pk = alpha_beta_pi + n_pk(p, j);

      // Set non-prior values to zero
      if(prior_types[v]){
        if(!prior_types_indicator_v[j]){
          // Handling of prior on Phi
          u_prob[j] = 0.0;
          u_prob[j + K] = 0.0;
          // If set to zero -> skip to next j
          continue;
        }
      }
      if(prior_doc[d]){
        if(!prior_doc_indicator_d[j]){
          // Handling of prior on Theta
          u_prob[j] = 0.0;
          u_prob[j + K] = 0.0;
          // If set to zero -> skip to next j
          continue;
        }
      }

      // x == 0
      u_prob[j] = (beta_pi + n_kpx(pos3d(j, p, 0, n_kpx_dims))) / alpha_beta_pi_n_pk;
      u_prob[j] *= (n_kvpx(pos3d(j, v, 0, n_kvpx_dims)) + betax0) / (n_xk(0, j) + beta_x0_sum);
      u_prob[j] *= n_dj_alpha;

      // x == 1
      u_prob[j + K] = (alpha_pi + n_kpx(pos3d(j, p, 1, n_kpx_dims))) / (alpha_beta_pi + n_pk(p, j));
      u_prob[j + K] *= (n_kvpx(pos3d(j, v, p + 1, n_kvpx_dims)) + betax1) / (n_kpx(pos3d(j, p, 1, n_kpx_dims)) + beta_x1_sum);
      u_prob[j + K] *= n_dj_alpha;

    }

    // Draw indicator
    kx = rcategorical(u_prob) - 1; // rcatgorical is R indexed
    if(kx < K){
      new_k = kx;
      new_x = 0;
    } else {
      new_k = kx - K;
      new_x = 1;
    }
    new_px = new_x * (p + 1);

    // Set topic indicator (R indexing)
    topic(i) = new_k + 1;
    perspective(i) = new_x;

    // Add current position
    n_dk(d, new_k) += 1;
    n_kvpx(pos3d(new_k, v, new_px, n_kvpx_dims)) += 1;
    n_kpx(pos3d(new_k, p, new_x, n_kpx_dims)) += 1;
    n_pk(p, new_k) += 1;
    n_xk(new_x, new_k) += 1;
  }

  // Return List
  List ret, cnt_mat, tmp;
  ret["state"] = state;
  cnt_mat["n_dk"] = n_dk;
  cnt_mat["n_kvpx"] = n_kvpx;
  cnt_mat["n_kpx"] = n_kpx;
  cnt_mat["n_pk"] = n_pk;
  cnt_mat["n_xk"] = n_xk;
  ret["count_matrices"] = cnt_mat;

  tmp["u_prob"] = u_prob;
  tmp["kx"] = kx;
  tmp["new_x"] = new_x;
  tmp["new_k"] = new_k;
  ret["tmp"] = tmp;

  return ret;
}

