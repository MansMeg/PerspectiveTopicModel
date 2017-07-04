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
  int d,k,v,p,x,px;

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

