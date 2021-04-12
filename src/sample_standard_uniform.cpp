#include "Rcpp.h"
#include "pcg_random.hpp"
#include "convert_seed.h"
#include "boost/random.hpp"

#include <deque>
#include <vector>

struct PositionHolder {
    PositionHolder(Rcpp::IntegerVector d, Rcpp::List index) : dim(d), get_all(d.size()), get_some(d.size()) {
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

    int get(int i, int p) const {
        if (get_all[i]) {
            return p;
        } else {
            return get_some[i][p] - 1;
        }
    }

    int max(int i) const {
        if (get_all[i]) {
            return dim[i];
        } else {
            return get_some[i].size();
        }
    }

private:
    Rcpp::IntegerVector dim;
    std::vector<int> get_all;
    std::vector<Rcpp::IntegerVector> get_some;
};

// [[Rcpp::export(rng=false)]]
Rcpp::RObject sample_standard_uniform(Rcpp::IntegerVector dim, Rcpp::IntegerVector chunkdim, Rcpp::List seeds, Rcpp::List index, int stream_start=0) {
    PositionHolder pos(dim, index);

    const int ndims = dim.size();
    Rcpp::IntegerVector outdims(ndims);
    size_t multiplier = 1;
    for (int i = 0; i < ndims; ++i){
        outdims[i] = pos.max(i);
        multiplier *= outdims[i];
    }
    Rcpp::NumericVector output(multiplier);
    if (multiplier == 0) {
        output.attr("dim") = outdims;
        return output;
    }

    // Determining the number of chunks along each dimension.
    std::vector<int> nchunks(ndims);
    {
        int counter = 0;
        for (auto it = dim.begin(); it != dim.end(); ++it, ++counter) {
            int curchunk = chunkdim[counter];
            nchunks[counter] = *it / curchunk + (*it % curchunk != 0);
        }
    }

    size_t nbuf = 1;
    for (auto c : chunkdim) {
        nbuf *= c;
    }
    std::vector<double> buffer(nbuf);

    // Setting up other workspaces.
    std::vector<int> positions(ndims), chunkpos(ndims), poscopy(ndims);
    std::vector<std::deque<int> > preset_out(ndims), preset_buf(ndims);
    bool finished = false;

    while (!finished) {
        for (int i = 0; i < ndims; ++i) {
            chunkpos[i] = pos.get(i, positions[i])/chunkdim[i];
        }
        size_t chunk_id = 0;
        int multiplier = 1;
        for (int i = 0; i < ndims; ++i) {
            chunk_id += multiplier * chunkpos[i];
            multiplier *= nchunks[i];
        }

        // Sampling across the chunk.
        pcg32 rng(dqrng::convert_seed<uint64_t>(seeds[chunk_id]), stream_start + chunk_id);
        boost::random::uniform_01<double> cpp_runif;
        for (auto bIt = buffer.begin(); bIt != buffer.end(); ++bIt) {
            *bIt = cpp_runif(rng);
        }

        // Computing the initial indices, to same time during the inner loop.
        std::copy(positions.begin(), positions.end(), poscopy.begin());
        int mult_out = 1, mult_buf = 1;

        for (int i = 0; i < ndims; ++i) {
            auto& current_out = preset_out[i];
            current_out.clear();
            auto& current_buf = preset_buf[i];
            current_buf.clear();

            auto& current_pos = poscopy[i];
            const int limit = pos.max(i), 
                      chunk_start = chunkpos[i] * chunkdim[i],
                      chunk_end = chunk_start + chunkdim[i];

            while (current_pos < limit && pos.get(i, current_pos) < chunk_end) {
                current_out.push_back(current_pos * mult_out);
                current_buf.push_back((pos.get(i, current_pos) - chunk_start) * mult_buf);
                ++current_pos;
            }

            mult_out *= outdims[i];
            mult_buf *= chunkdim[i];
        }

        // Transferring values from the buffer to the output.
        size_t out_id = 0, buf_id = 0;
        for (int i = 0; i < ndims; ++i) {
            out_id += preset_out[i][0];
            buf_id += preset_buf[i][0];
        }
        std::fill(poscopy.begin(), poscopy.end(), 0);
        bool inner_finished = false;

        do {
            output[out_id] = buffer[buf_id];

            inner_finished = true;
            for (int i = 0; i < ndims; ++i) {
                auto& curpos = poscopy[i];
                out_id -= preset_out[i][curpos];
                buf_id -= preset_buf[i][curpos];

                ++curpos;
                if (static_cast<size_t>(curpos) >= preset_out[i].size()) {
                    curpos = 0;
                }
                out_id += preset_out[i][curpos];
                buf_id += preset_buf[i][curpos];

                if (curpos) {
                    inner_finished = false;
                    break;
                }
            }
        } while (!inner_finished);

        // Updating the positions.
        finished = true;
        for (int i = 0; i < ndims; ++i) {
            auto& curpos = positions[i];
            curpos += preset_out[i].size();
            if (curpos >= pos.max(i)) {
                curpos = 0;
            } else {
                finished = false;
                break;
            }
        }
    } 

    output.attr("dim") = outdims;
    return output;
}
