#include <Rcpp.h>
#include "pos3d.h"
using namespace Rcpp;

//[[Rcpp::export]]
List init_count2_cpp(DataFrame state, List constants) {
  // Define constants
  int D = constants["D"];
  int K = constants["K"];
  int N = constants["N"];
  int V = constants["V"];
  int P = constants["P"];

  IntegerMatrix n_dk = IntegerMatrix(D, K);
  IntegerVector n_kvpx = IntegerVector(K * V * (P + 1));
  IntegerVector n_kvpx_dims = IntegerVector::create(K, V, P + 1);
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

    n_dk(d, k) += 1;
    // Rcout << " po" << po << std::endl;
    n_kvpx(pos3d(k, v, px, n_kvpx_dims)) += 1;
    n_kpx(pos3d(k, p, x, n_kpx_dims)) += 1;
  }

  // Create data to return
  n_kvpx.attr("dim") = n_kvpx_dims;
  n_kpx.attr("dim") = n_kpx_dims;

  List ret;
  ret["n_dk"] = n_dk;
  ret["n_kvpx"] = n_kvpx;
  ret["n_kpx"] = n_kpx;
  return ret;
}

