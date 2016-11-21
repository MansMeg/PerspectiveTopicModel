#include <Rcpp.h>
using namespace Rcpp;

int pos3d(int x, int y, int z, IntegerVector dims) {
  int p = x + y * dims(0) + z * dims(0) * dims(1);
  return p;
}

/*
//[[Rcpp::export]]
int pos2d(int x, int y, int z, IntegerVector dims) {
int p = x + y * dims(0) + z * dims(0) * dims(1);
return x;
}
*/

/*** R
dims <- c(7,3,13)
x <- 1:(prod(dims))
y <- array(x, dim = c(7,3,13))

for(i in 1:dims[1]){
  for(j in 1:dims[2]){
    for(k in 1:dims[3]){
# cat(i,j,k, y[i, j, k], "\n")
      stopifnot(x[pos3d(i - 1, j - 1, k - 1, dims) + 1] == y[i, j, k])
    }
  }
}

*/
