// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// TSP_naif_Rcpp
IntegerVector TSP_naif_Rcpp(NumericMatrix data, std::string type);
RcppExport SEXP _M2algorithmique_TSP_naif_Rcpp(SEXP dataSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(TSP_naif_Rcpp(data, type));
    return rcpp_result_gen;
END_RCPP
}
// TSP_cheapest_Rcpp
IntegerVector TSP_cheapest_Rcpp(NumericMatrix data, std::string type);
RcppExport SEXP _M2algorithmique_TSP_cheapest_Rcpp(SEXP dataSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(TSP_cheapest_Rcpp(data, type));
    return rcpp_result_gen;
END_RCPP
}
// TSP_nearest_Rcpp
IntegerVector TSP_nearest_Rcpp(NumericMatrix data, std::string type);
RcppExport SEXP _M2algorithmique_TSP_nearest_Rcpp(SEXP dataSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(TSP_nearest_Rcpp(data, type));
    return rcpp_result_gen;
END_RCPP
}
// compute_distances
NumericMatrix compute_distances(NumericMatrix data);
RcppExport SEXP _M2algorithmique_compute_distances(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_distances(data));
    return rcpp_result_gen;
END_RCPP
}
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
// heap_sort_Rcpp
std::vector<double> heap_sort_Rcpp(std::vector<double> v);
RcppExport SEXP _M2algorithmique_heap_sort_Rcpp(SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(heap_sort_Rcpp(v));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_M2algorithmique_TSP_naif_Rcpp", (DL_FUNC) &_M2algorithmique_TSP_naif_Rcpp, 2},
    {"_M2algorithmique_TSP_cheapest_Rcpp", (DL_FUNC) &_M2algorithmique_TSP_cheapest_Rcpp, 2},
    {"_M2algorithmique_TSP_nearest_Rcpp", (DL_FUNC) &_M2algorithmique_TSP_nearest_Rcpp, 2},
    {"_M2algorithmique_compute_distances", (DL_FUNC) &_M2algorithmique_compute_distances, 1},
    {"_M2algorithmique_insertion_sort_Rcpp", (DL_FUNC) &_M2algorithmique_insertion_sort_Rcpp, 1},
    {"_M2algorithmique_heap_sort_Rcpp", (DL_FUNC) &_M2algorithmique_heap_sort_Rcpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_M2algorithmique(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
