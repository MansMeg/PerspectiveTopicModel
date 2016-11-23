#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' pos3d
//' @description
//' Access positions in an vector representation of a 3D array
//'
//' @param x index 1
//' @param y index 2
//' @param z index 3
//' @param dims integer vector of size 3 defining the array
//'
//[[Rcpp::export]]
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
