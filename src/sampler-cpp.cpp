#include <Rcpp.h>
#include "pos3d.h"
#include "rcategorical.h"

using namespace Rcpp;

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

/*** R
# v 0.1 Sampler working
source("../init_counts.R")
Rcpp::sourceCpp("init_counts-cpp.cpp")
source("../sampler_r.R")
set.seed(4711)
N <- 1000
D <- 17
V <- 31
K <- 10
P <- 3
state_df <- data.frame(doc = sample(1:D, size = N, replace = TRUE),
                       type = sample(1:V, size = N, replace = TRUE),
                       topic = sample(1:K, size = N, replace = TRUE),
                       party = sample(1:P, size = N, replace = TRUE),
                       perspective = sample(0:1, size = N, replace = TRUE))

  constants <- list(D = length(unique(state_df$doc)),
                    V = length(unique(state_df$type)),
                    K = length(unique(state_df$topic)),
                    P = length(unique(state_df$party)),
                    N = nrow(state_df))

  stopifnot(constants$D == D,
            constants$V == V,
            constants$K == K,
            constants$P == P,
            constants$N == N)

  icr <- init_counts_r(state_df, constants)
  icrcpp <- init_count_cpp(state_df, constants)
  icrcpp[["n_kp"]] <- apply(icrcpp$n_kpx, MARGIN=c(1, 2), sum)
  icrcpp[["n_kx"]] <- apply(icrcpp$n_kpx, MARGIN=c(1, 3), sum)

  stopifnot(all(icrcpp$n_dk == icr$n_dk),
            all(icrcpp$n_vkpx == icr$n_vkpx),
            all(icrcpp$n_kpx == icr$n_kpx))

  priors <- list(alpha = 0.1,
                 betax0 = 0.01,
                 betax1 = 0.05,
                 alpha_pi = 0.15,
                 beta_pi = 0.20)

  state_it1_cpp <- per_sampler_cpp(state = state_df, count_matrices = icrcpp, priors = priors, constants = constants)

  icrcpp_it1 <- init_count_cpp(state_it1_cpp[[1]], constants)
  icrcpp_it1[["n_kp"]] <- apply(icrcpp_it1$n_kpx, MARGIN=c(1, 2), sum)
  icrcpp_it1[["n_kx"]] <- apply(icrcpp_it1$n_kpx, MARGIN=c(1, 3), sum)

  stopifnot(sum(state_it1_cpp$count_matrices$n_dk) == sum(state_it1_cpp$count_matrices$n_vkpx),
            sum(state_it1_cpp$count_matrices$n_vkpx) == sum(state_it1_cpp$count_matrices$n_kpx))

  stopifnot(all(icrcpp_it1$n_dk == state_it1_cpp$count_matrices$n_dk),
            all(icrcpp_it1$n_vkpx == state_it1_cpp$count_matrices$n_vkpx),
            all(icrcpp_it1$n_kpx == state_it1_cpp$count_matrices$n_kpx),
            all(icrcpp_it1$n_kp == state_it1_cpp$count_matrices$n_kp),
            all(icrcpp_it1$n_kx == state_it1_cpp$count_matrices$n_kx))

  stopifnot(all(state_it1_r$state == state_it1_cpp$state),
            all(state_it1_r$count_matrices$n_dk == state_it1_cpp$count_matrices$n_dk),
            all(state_it1_r$count_matrices$n_vkpx == state_it1_cpp$count_matrices$n_vkpx),
            all(state_it1_r$count_matrices$n_kpx == state_it1_cpp$count_matrices$n_kpx),
            all(state_it1_r$count_matrices$n_kp == state_it1_cpp$count_matrices$n_kp),
            all(state_it1_r$count_matrices$n_kx == state_it1_cpp$count_matrices$n_kx))
*/

