#include <Rcpp.h>
#include "pos3d.h"
#include "rcategorical.h"

using namespace Rcpp;

//' @title
//' C++ Perspective sampler
//'
//' @param state a perspective model state file
//' @param count_matrices A list of count matrices
//' @param priors A list of priors
//' @param constants A list of constants
//'
//[[Rcpp::export]]
List per_sampler_cpp(DataFrame state, List count_matrices, List priors, List constants) {

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
  IntegerVector n_vkpx = count_matrices["n_vkpx"];
  IntegerVector n_vkpx_dims = n_vkpx.attr("dim");
  IntegerVector n_kpx = count_matrices["n_kpx"];
  IntegerVector n_kpx_dims = n_kpx.attr("dim");
  IntegerMatrix n_kp = count_matrices["n_kp"];
  IntegerMatrix n_kx = count_matrices["n_kx"];

  // Define priors
  double alpha = priors["alpha"];
  double betax0 = priors["betax0"];
  double betax1 = priors["betax1"];
  double alpha_pi = priors["alpha_pi"];
  double beta_pi = priors["beta_pi"];

  // Memory allocation and pre calculations
  int d, k, v, p, x, px, kx;
  int new_k, new_x, new_kx, new_px;
  NumericVector u_prob(K * 2);
  double alpha_beta_pi = alpha_pi + beta_pi;
  double beta_x0_sum = betax0 * V;
  double beta_x1_sum = betax1 * V;
  double n_dj_alpha, alpha_beta_pi_n_kp;

  for (int i = 0; i < N; ++i) {
    d = doc[i] - 1;
    k = topic[i] - 1;
    v = type[i] - 1;
    p = party[i] - 1;
    x = perspective[i];
    px = x * (p + 1);

    // Remove current position in matrices
    n_dk(d, k) -= 1;
    n_vkpx(pos3d(v, k, px, n_vkpx_dims)) -= 1;
    n_kpx(pos3d(k, p, x, n_kpx_dims)) -= 1;
    n_kp(k, p) -= 1;
    n_kx(k, x) -= 1;

    // Calculate unnormalized probabilities
    for (int j = 0; j < K; ++j) {
      n_dj_alpha = n_dk(d, j) + alpha;
      alpha_beta_pi_n_kp = alpha_beta_pi + n_kp(j, p);

      // x == 0
      u_prob[j] = (beta_pi + n_kpx(pos3d(j, p, 0, n_kpx_dims))) / alpha_beta_pi_n_kp;
      u_prob[j] *= (n_vkpx(pos3d(v, j, 0, n_vkpx_dims)) + betax0) / (n_kx(j, 0) + beta_x0_sum);
      u_prob[j] *= n_dj_alpha;

      // x == 1
      u_prob[j + K] = (alpha_pi + n_kpx(pos3d(j, p, 1, n_kpx_dims))) / (alpha_beta_pi + n_kp(j, p));
      u_prob[j + K] *= (n_vkpx(pos3d(v, j, p + 1, n_vkpx_dims)) + betax1) / (n_kpx(pos3d(j, p, 1, n_kpx_dims)) + beta_x1_sum);
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
    n_vkpx(pos3d(v, new_k, new_px, n_vkpx_dims)) += 1;
    n_kpx(pos3d(new_k, p, new_x, n_kpx_dims)) += 1;
    n_kp(new_k, p) += 1;
    n_kx(new_k, new_x) += 1;
  }

  // Return List
  List ret, cnt_mat, tmp;
  ret["state"] = state;
  cnt_mat["n_dk"] = n_dk;
  cnt_mat["n_vkpx"] = n_vkpx;
  cnt_mat["n_kpx"] = n_kpx;
  cnt_mat["n_kp"] = n_kp;
  cnt_mat["n_kx"] = n_kx;
  ret["count_matrices"] = cnt_mat;

  tmp["u_prob"] = u_prob;
  tmp["kx"] = kx;
  tmp["new_x"] = new_x;
  tmp["new_k"] = new_k;
  ret["tmp"] = tmp;

  return ret;
}

