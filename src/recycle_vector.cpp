#include "Rcpp.h"
#include "index_server.h"

// [[Rcpp::export(rng=false)]]
Rcpp::NumericVector recycle_vector(Rcpp::NumericVector vec, Rcpp::IntegerVector dim, Rcpp::List index) {
    index_server pos(dim, index);

    size_t ndims = dim.size();
    std::vector<std::deque<int> > preset_indices(ndims);
    size_t fullsize=1, multiplier = 1;

    for (int i = 0; i < ndims; ++i){
        const int limit = pos.max(i);
        if (!limit) {
            fullsize = 0;
            break;
        } 
        
        auto& current_idx = preset_indices[i];
        size_t current_pos = 0;
        while (current_pos < limit) {
            current_idx.push_back(pos.get(i, current_pos) * multiplier);
            ++current_pos;
        }

        multiplier *= dim[i];
        fullsize *= limit;
    }

    Rcpp::NumericVector output(fullsize);
    if (!fullsize) {
        return output;
    }

    std::vector<int> positions(ndims);
    size_t out_id = 0;
    for (int i = 0; i < ndims; ++i) {
        out_id += preset_indices[i][0];
    }

    bool finished = true;
    auto oIt = output.begin();
    do {
        (*oIt) = vec[out_id % vec.size()];
        ++oIt;

        finished = true;
        for (int i = 0; i < ndims; ++i) {
            auto& curpos = positions[i];
            out_id -= preset_indices[i][curpos];

            ++curpos;
            if (static_cast<size_t>(curpos) >= preset_indices[i].size()) {
                curpos = 0;
                out_id += preset_indices[i][0];
            } else {
                out_id += preset_indices[i][curpos];
                finished = false;
                break;
            }
        }

    } while (!finished);

    return output;
}
