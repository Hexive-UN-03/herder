#include <iostream>
// VROOM VROOM
#include <thread>
#include <future>
#include <algorithm>
// useful objects
#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include <map>
// htslibs
#include <htslib/vcf.h>
#include <htslib/synced_bcf_reader.h>
#include <htslib/tbx.h>
// I made this :D
#include <ordered_parallel_output.hpp>


using std::string;
// maybe remove later
using std::cout;
using std::endl;
using std::map;

// invoke with
    // sample_lister.cpp vcf_filespath outfile_path

int main(int argc, char *argv[]){
    // process args
    if (argc < 3){
        fprintf(stderr, "[ERROR]: Missing arguments, ensure format:\nsample_lister filepath outpath\n");
        return 1;
    }else if (argc > 3){
        fprintf(stderr, "[ERROR]: Too many arguments, ensure format:\nsample_lister filepath outpath\n");
        return 1;
    }
    string bcf_path = argv[1];
    string outfile_path = argv[2];

    htsFile *file_pointer = hts_open(bcf_path.c_str(), "r");
    if (!file_pointer){
        fprintf(stderr, "[ERROR]: Failed to open provided file\n");
        return 1;
    }
    bcf_hdr_t *hdr = bcf_hdr_read(file_pointer);
    if (!hdr){
        fprintf(stderr, "[ERROR]: Failed to open file header\n");
        return 1;
    }
    int nsamples = bcf_hdr_nsamples(hdr);
    std::ofstream outfile(outfile_path);
    for (int i = 0; i < nsamples; ++i) {
        outfile << hdr->samples[i] << "\n";
    }
}