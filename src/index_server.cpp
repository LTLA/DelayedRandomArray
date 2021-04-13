#include "index_server.h"

index_server::index_server(Rcpp::IntegerVector d, Rcpp::List index) : dim(d), get_all(d.size()), get_some(d.size()) {
    int counter = 0;
    for (auto it = index.begin(); it != index.end(); ++it, ++counter) {
        Rcpp::Nullable<Rcpp::IntegerVector> current = *it;
        if (current.isNotNull()) {
            get_some[counter] = Rcpp::IntegerVector(current.get());
        } else {
            get_all[counter] = 1;
        }
    }
    return;
}

int index_server::get(int i, int p) const {
    if (get_all[i]) {
        return p;
    } else {
        return get_some[i][p] - 1;
    }
}

int index_server::max(int i) const {
    if (get_all[i]) {
        return dim[i];
    } else {
        return get_some[i].size();
    }
}
