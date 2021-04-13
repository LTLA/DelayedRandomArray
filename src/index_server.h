#ifndef INDEX_SERVER_H
#define INDEX_SERVER_H

#include "Rcpp.h"
#include <vector>

struct index_server {
    index_server(Rcpp::IntegerVector, Rcpp::List);
    int get(int, int) const;
    int max(int) const;
private:
    Rcpp::IntegerVector dim;
    std::vector<int> get_all;
    std::vector<Rcpp::IntegerVector> get_some;
};

#endif
