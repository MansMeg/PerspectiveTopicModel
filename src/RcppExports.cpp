// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// init_count_cpp
List init_count_cpp(DataFrame state, List constants);
RcppExport SEXP PerspectiveTopicModel_init_count_cpp(SEXP stateSEXP, SEXP constantsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type state(stateSEXP);
    Rcpp::traits::input_parameter< List >::type constants(constantsSEXP);
    rcpp_result_gen = Rcpp::wrap(init_count_cpp(state, constants));
    return rcpp_result_gen;
END_RCPP
}
// pos3d
int pos3d(int x, int y, int z, IntegerVector dims);
RcppExport SEXP PerspectiveTopicModel_pos3d(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP, SEXP dimsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type z(zSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dims(dimsSEXP);
    rcpp_result_gen = Rcpp::wrap(pos3d(x, y, z, dims));
    return rcpp_result_gen;
END_RCPP
}
// rcategorical
int rcategorical(NumericVector p);
RcppExport SEXP PerspectiveTopicModel_rcategorical(SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(rcategorical(p));
    return rcpp_result_gen;
END_RCPP
}
// per_sampler_cpp
List per_sampler_cpp(DataFrame state, List count_matrices, List priors, List constants);
RcppExport SEXP PerspectiveTopicModel_per_sampler_cpp(SEXP stateSEXP, SEXP count_matricesSEXP, SEXP priorsSEXP, SEXP constantsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type state(stateSEXP);
    Rcpp::traits::input_parameter< List >::type count_matrices(count_matricesSEXP);
    Rcpp::traits::input_parameter< List >::type priors(priorsSEXP);
    Rcpp::traits::input_parameter< List >::type constants(constantsSEXP);
    rcpp_result_gen = Rcpp::wrap(per_sampler_cpp(state, count_matrices, priors, constants));
    return rcpp_result_gen;
END_RCPP
}
