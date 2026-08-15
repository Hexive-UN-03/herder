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
    // fast_af vcf_filepath region threads ?? samples_filepath outfile_path

// friend function to make putting together region strings easier
string convert_region(string name, int startpos, int endpos){
    string return_val = name + ":" + std::to_string(startpos) + "-" + std::to_string(endpos);
    return (return_val);
}

string calculate_region(string filepath, string region, bool using_samples, string samplepath){
    htsFile *file_pointer = hts_open(filepath.c_str(), "r");
    string fail_string = "FAILED";
    if (!file_pointer){
        fprintf(stderr, "[ERROR]: Failed to open provided file\n");
        return fail_string;
    }
    bcf_hdr_t *hdr = bcf_hdr_read(file_pointer);
    if (!hdr){
        fprintf(stderr, "[ERROR]: Failed to open file header\n");
        return fail_string;
    }
    // set samples if we found the samples path
    if (using_samples){
        if (bcf_hdr_set_samples(hdr, samplepath.c_str(), 1)){
            fprintf(stderr, "[ERROR]: Failed to apply samples when sample file was expected\n");
            return fail_string;
        }
    }
    // NOTE THAT THIS HARDCODES THIS SHIT TO ALWAYS ASSUME TABIX INDEXING LMAO
    // FIX WITH FILESYSTEM LIBRARY LATER ONCE YOU HAVE IT WORKING
    tbx_t *tbx = tbx_index_load(filepath.c_str());
    if (!tbx){
        fprintf(stderr, "[ERROR]: Failed to open provided tabix index\n");
        return fail_string;
    }
    hts_itr_t *itr = tbx_itr_querys(tbx, region.c_str());
    if (!itr){
        fprintf(stderr, "[ERROR]: Failed to query region\n");
        return fail_string;
    }
    // open a file for this region
    std::ostringstream region_outstream;
    // initialize a record to write to and kstring to store our lines
    kstring_t line_str = {0, 0, NULL};
    bcf1_t *rec = bcf_init();
    while (tbx_itr_next(file_pointer, tbx, itr, &line_str) >= 0) {
        // parse the kstring into a bcf1_t, may be inefficient, should compare this to string processing sometime
        if (vcf_parse(&line_str, hdr, rec) != 0){
            fprintf(stderr, "[ERROR]: Failed to parse line into bcf1_t\n");
            continue;
        }
        // wow so inefficient on my part, packing then unpacking lmao, need this for alts
        bcf_unpack(rec, BCF_UN_STR); 
        // make a pointer to store our genotype array
        int32_t *genotypes = NULL;
        int n_genotypes = 0;
        int n_gts = bcf_get_genotypes(hdr, rec, &genotypes, &n_genotypes);
        // make a hash table to hold our allele counts
        map <int, int> alt_counts;
        for (int i = 0; i < n_genotypes; i++){
            // add to the dict the alt values as they appear, if any, record any number of missing genotypes
            if (bcf_gt_is_missing(genotypes[i])){
                alt_counts[-1]++;
            }
            alt_counts[bcf_gt_allele(genotypes[i])]++;
        }
        // maps are smart, even if this doesn't exist it will be added to the map and evaluate to 0 during operation
        int n_non_missing = n_genotypes - alt_counts[-1];
        int alts = alt_counts.size();
        if (alts > 2){
            for (int i = 1; i < alts - 1; i++){
                // need to cast one of these to a float to make sure we don't just do int division
                float af = (float)alt_counts[i] / n_non_missing;
                region_outstream << bcf_hdr_id2name(hdr, rec->rid) << '\t' 
                    << rec->pos + 1 << '\t'
                    << rec->d.allele[0] << '\t'
                    << rec->d.allele[i] << '\t'
                    << af << endl;
            }
        }
    }
    return region_outstream.str();
}

// function to process the region argument into components and generate relatively equal regions to process
// region must follow format regionname:startpos-endpos (e.g. chr3:10000-30000)
std::vector<string> explode_region(string region, int shrapnel, bool verbose = false){
    string target = ":";
    size_t found = region.find(target);
    if (found == string::npos){
        fprintf(stderr, "[ERROR]: incorrect formatting detected in region field:\n - No : to delineate region name\n");
        return {"failed"};
    }
    string positions = region.substr(found + 1);
    string pregion = region.substr(0, found);
    target = "-";
    found = positions.find(target);
    int pos1 = -1;
    int pos2 = -1;
    try{
        pos1 = stoi(positions.substr(0, found));
        pos2 = stoi(positions.substr(found + 1));
    }catch(const std::invalid_argument& e){
        fprintf(stderr, "[ERROR]: incorrect formatting detected in region field:\n - Range failed to cast to int\n");
        return {"failed"};
    }
    // in case the error doesn't throw but we still fail to make it work, or they entered a negative number like a cheeky devil
    if (pos1 < 0 || pos2 < 0){
        fprintf(stderr, "[ERROR]: incorrect formatting detected in region field:\n - Negative number in range field, or something went horribly wrong!\n");
        return {"failed"};
    }else if(pos1 > pos2){
        fprintf(stderr, "[ERROR]: incorrect formatting detected in region field:\n - Range not in ascending order\n");
        return {"failed"};
    }
    if (verbose){
        cout << "Generating thread subregions with the following characteristics:" << endl 
            << " - Region name:    " << pregion << endl 
            << " - Start Position: " << pos1 << endl 
            << " - End Position:   " << pos2 << endl;
    }

    if (shrapnel < 1){
        fprintf(stderr, "[ERROR]: attempted to utilize less than one thread\n");
        return {"failed"};
    }
    // generate a string vector of regions to calculate
    std::vector<string> calc_regions;
    int curr_pos = pos1;
    int block_size = (pos2 - pos1) / shrapnel;
    cout << "Generated regions:" << endl;
    for (int i = 0; i < shrapnel - 1; i++){
        string build_str = pregion + ":" + std::to_string(curr_pos) + "-";
        curr_pos += block_size;
        build_str += std::to_string(curr_pos);
        curr_pos ++;
        calc_regions.push_back(build_str);
        if (verbose){
            cout << " - " << build_str << endl;
        }
    }
    calc_regions.push_back(pregion + ":" + std::to_string(curr_pos) + "-" + std::to_string(pos2));
    if (verbose){
        cout << " - " << (pregion + ":" + std::to_string(curr_pos) + "-" + std::to_string(pos2)) << endl;
    }
    return calc_regions;
}

// argc is number of command-line arguments, while argv is a pointer to an array of the system arguments passed
// keep in mind that argc is literal (1-indexed), and includes the executable itself
// cannot use std::string here as it's not real in C
int main(int argc, char *argv[]){
    // ARGUMENT HANDLING
        // update with argparse later?
    if (argc < 6){
        fprintf(stderr, "[ERROR]: Missing arguments, ensure format:\nfast_af filepath region threads samplefile outpath\n");
        return 1;
    }else if (argc > 6){
        fprintf(stderr, "[ERROR]: Too many arguments, ensure format:\nfast_af filepath region threads samplefile outpath\n");
        return 1;
    }
    string bcf_path = argv[1];
    string region = argv[2];
    int threads = atoi(argv[3]);
    // atoi will return 0 if it fails, and any input less than 1 will cause threading issues so we exclude them
    if (threads < 1){
        fprintf(stderr, "[ERROR]: Failed to parse thread count successfully (must be greater than 1)\n");
        return 1;
    }
    string samples_path = argv[4];
    string outfile_path = argv[5];

    // check if a samples file exists, and if it does, track a boolean
    bool samples_exist = false;
    std::ifstream samples_file(samples_path);
    if (samples_file.good()){
        samples_file.close();
        samples_exist = true;
    }

    // blow up the region into threads pieces
    std::vector<string> shrapnel_regions = explode_region(region, threads, true);
    if (shrapnel_regions.size() < 1 || shrapnel_regions[0] == "failed"){
        fprintf(stderr, "[ERROR]: explode_region failed\n");
        return 1;
    }
    // build a vector of tuples containing the region arguments for each call
    std::vector<std::tuple<string, string, bool, string>> processed_args;
    for (string shrap_region : shrapnel_regions){
        processed_args.emplace_back(std::make_tuple(bcf_path, shrap_region, samples_exist, samples_path));
    }
    std::ofstream outfile_stream(outfile_path);
    outfile_stream << "CHR\tPOS\tREF\tALT\tAF" << endl;
    ordered_parallel::ordered_parallel_output(outfile_stream, calculate_region, processed_args);
    outfile_stream.close();
}
