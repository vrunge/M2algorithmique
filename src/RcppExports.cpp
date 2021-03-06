// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// insertion_sort_Rcpp
std::vector<double> insertion_sort_Rcpp(std::vector<double> v);
RcppExport SEXP _M2algorithmique_insertion_sort_Rcpp(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(insertion_sort_Rcpp(v));
    return rcpp_result_gen;
END_RCPP
}
// build_heap_Rcpp
NumericVector build_heap_Rcpp(NumericVector heap, unsigned int i, unsigned int n);
RcppExport SEXP _M2algorithmique_build_heap_Rcpp(SEXP heapSEXP, SEXP iSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type heap(heapSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type i(iSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(build_heap_Rcpp(heap, i, n));
    return rcpp_result_gen;
END_RCPP
}
// heap_sort_Rcpp
NumericVector heap_sort_Rcpp(NumericVector v);
RcppExport SEXP _M2algorithmique_heap_sort_Rcpp(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(heap_sort_Rcpp(v));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_M2algorithmique_insertion_sort_Rcpp", (DL_FUNC) &_M2algorithmique_insertion_sort_Rcpp, 1},
    {"_M2algorithmique_build_heap_Rcpp", (DL_FUNC) &_M2algorithmique_build_heap_Rcpp, 3},
    {"_M2algorithmique_heap_sort_Rcpp", (DL_FUNC) &_M2algorithmique_heap_sort_Rcpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_M2algorithmique(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
