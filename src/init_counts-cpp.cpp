#include <Rcpp.h>
#include "pos3d.h"
using namespace Rcpp;

//[[Rcpp::export]]
List init_count_cpp(DataFrame state, List constants) {
  // Define constants
  int D = constants["D"];
  int K = constants["K"];
  int N = constants["N"];
  int V = constants["V"];
  int P = constants["P"];

  IntegerMatrix n_dk = IntegerMatrix(D, K);
  IntegerVector n_vkpx = IntegerVector(V * K * (P + 1));
  IntegerVector n_vkpx_dims = IntegerVector::create(V, K, P + 1);
  IntegerVector n_kpx = IntegerVector(K * P * 2);
  IntegerVector n_kpx_dims = IntegerVector::create(K, P, 2);

  // n_kpx <- array(0L, dim = c(const$K, const$P, 2))

  IntegerVector doc = state["doc"];
  IntegerVector type = state["type"];
  IntegerVector topic = state["topic"];
  IntegerVector party = state["party"];
  IntegerVector perspective = state["perspective"];

  //  n_vkpx <- array(0L, dim = c(const$V, const$K, const$P + 1))
  //  n_kpx <- array(0L, dim = c(const$K, const$P, 2))
  int d,k,v,p,x,px,po;

  for (int i = 0; i < N; ++i) {
    d = doc[i] - 1;
    k = topic[i] - 1;
    v = type[i] - 1;
    p = party[i] - 1;
    x = perspective[i];
    px = x * (p + 1);

    // Rcout << " i" << i  << " d"  << d << " k" << k << " v"<< v << " p" << p  << " x" << x <<" px" << px << std::endl;

    // n_kpx[k, p, x] <- n_kpx[k, p, x] + 1L

    n_dk(d, k) += 1;
    // Rcout << " po" << po << std::endl;
    n_vkpx(pos3d(v, k, px, n_vkpx_dims)) += 1;
    n_kpx(pos3d(k, p, x, n_kpx_dims)) += 1;
  }

  // IntegerVector n_dk_dims = IntegerVector::create(D, K);
  // n_dk.attr("dim") = n_dk_dims;
  // Create data to return
  n_vkpx.attr("dim") = n_vkpx_dims;
  n_kpx.attr("dim") = n_kpx_dims;

  List ret;
  ret["n_dk"] = n_dk;
  ret["n_vkpx"] = n_vkpx;
  ret["n_kpx"] = n_kpx;
  return ret;
}

/*** R
source("../init_counts.R")

# Tests
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

stopifnot(all(icrcpp$n_dk == icr$n_dk),
          all(icrcpp$n_vkpx == icr$n_vkpx),
          all(icrcpp$n_kpx == icr$n_kpx))

*/

