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
    // vcf_trimmer vcf_filespath samples_filepath outfile_path *region
// * = optional arguments

int main(int argc, char *argv[]){
    // process args
    if (argc < 4){
        fprintf(stderr, "[ERROR]: Missing arguments, ensure format:\nvcf_trimmer filepath samplefile outpath region\n");
        return 1;
    }else if (argc > 5){
        fprintf(stderr, "[ERROR]: Too many arguments, ensure format:\nvcf_trimmer filepath samplefile outpath region\n");
        return 1;
    }
    string bcf_path = argv[1];
    string samples_path = argv[2];
    string outfile_path = argv[3];

    // open file and apply conditions
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
    if (bcf_hdr_set_samples(hdr, samples_path.c_str(), 1)){
        fprintf(stderr, "[ERROR]: Failed to apply samples\n");
        return 1;
    }
    // initialize a record object
    bcf1_t* rec = bcf_init();
    // if the user provided a region too process seperately using the tabix index
    if (argc == 5){
        string region = argv[4];
        tbx_t *tbx = tbx_index_load(bcf_path.c_str());
        if (!tbx){
            fprintf(stderr, "[ERROR]: Failed to open tabix index\n");
            return 1;
        }
        hts_itr_t *itr = tbx_itr_querys(tbx, region.c_str());
        if (!itr){
            fprintf(stderr, "[ERROR]: Failed to query region\n");
            return 1;
        }
        // opening file post-checks for failures to prevent needless filestream creation
        htsFile* outfile = bcf_open(outfile_path.c_str(), "wz");
        if (bcf_hdr_write(outfile, hdr) != 0){
            fprintf(stderr, "[ERROR]: Failed to write header\n");
            return 1;
        }
        kstring_t line_str = {0, 0, NULL};
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
            for (int i = 0; i < n_gts; i++){
                if (bcf_gt_is_missing(genotypes[i]) == 0 && bcf_gt_allele(genotypes[i]) != 0){
                    if (bcf_write(outfile, hdr, rec) != 0){
                        fprintf(stderr, "[ERROR]: Failed to write line\n");
                        continue;
                    }
                    break;
                }
            }
        }
        bcf_close(outfile);
    }else{
        htsFile* outfile = bcf_open(outfile_path.c_str(), "wz");
        if (bcf_hdr_write(outfile, hdr) != 0){
            fprintf(stderr, "[ERROR]: Failed to write header\n");
            return 1;
        }
        while (bcf_read(file_pointer, hdr, rec) == 0) {
            int32_t *genotypes = NULL;
            int n_genotypes = 0;
            int n_gts = bcf_get_genotypes(hdr, rec, &genotypes, &n_genotypes);
            for (int i = 0; i < n_gts; i++){
                if (bcf_gt_is_missing(genotypes[i]) == 0 && bcf_gt_allele(genotypes[i]) != 0){
                    if (bcf_write(outfile, hdr, rec) != 0){
                        fprintf(stderr, "[ERROR]: Failed to write line\n");
                        continue;
                    }
                    break;
                }
            }
        }
        bcf_close(outfile);
    }
    // close and index the new vcf
    bcf_close(file_pointer);
    tbx_index_build(outfile_path.c_str(), 0, &tbx_conf_vcf);
}

