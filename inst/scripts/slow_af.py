# get it? ;)
import argparse
parser = argparse.ArgumentParser()
parser.add_argument('-i',"--input_file")
parser.add_argument('-s',"--samples_file")
parser.add_argument('-o',"--output")
parser.add_argument('--no_header', action='store_true')
args = parser.parse_args()

# our samples that were input for us to actually evaluate
requested_samples = []
# open our requested samples file
with open(args.samples_file,"rt") as samplefile:
    for line in samplefile:
        requested_samples.append(line.strip())
if len(requested_samples) < 1:
    exit()
# make a list to contain our calculated value tuples (THIS COULD GET ABSURDLY LARGE, RECONSIDER MAYBE)
variants = []

# open the headed_region vcf
with open(args.input_file, "rt") as regionfile:
    # get the first line which has our colheads (sample names, position, ref/alt etc) and split it into a list of colhead values (ordered)
    col_heads = regionfile.readline().strip().split("\t")
    # get allele frequency list (this actually means something else, but I can't type it in code that might be reviewed professionally) ;)
    gaf_list = []
    for ind, colname in enumerate(col_heads):
        # if the samplename for that column head is in the list of requested samples, add it to the list of sample columns to evaluate
        # this also prevents us from ever worrying about non-sample columns
        if colname in requested_samples:
            gaf_list.append(ind)
    # loop over the remaining variant lines
    for line in regionfile:
        line_elements = line.strip().split("\t")
        # to avoid adding an operation to every index, we're going to start by checking the number of samples and multiplying it by 2 to get our total number of alleles (update later to accomodate non-diploid organisms or X/Y etc.)
        alelle_n = len(gaf_list)*2
        # check the alternate to see if we're at a multiallelic site
        if not ',' in line_elements[4]:
            # start this as a count at 0
            allele_frequency = 0
            # for each index we care about, do our calculations, ignore everything else, we're already chuggin
            for gaf_index in gaf_list:
                # we're gonna pretend that all vcfs always have GT as the first value to make this infinitely faster, and add "weird VCF" functionality to check later
                # the format of a vcf should always have #/#, unless a missing value is marked which is designated by ./., phasing means we'll have a | instead of a / but it's still the same string index
                try:
                    allele_frequency += int(line_elements[gaf_index][0])
                    allele_frequency += int(line_elements[gaf_index][2])
                # you should never have a missing call that has one allele called still, that would make no sense, so if that happens we're nuking it here
                # offloads us from another operation per loop
                except:
                    # This works!
                    # print(f"Missing value at position {line_elements[1]} for sample {col_heads[gaf_index]}")
                    alelle_n -= 2
            # make our allele frequency actually a frequency not a count
            try:
                allele_frequency /= alelle_n
                if allele_frequency != 0:
                    # append a tuple that contains the variant information in format (pos, ref, alt, QUAL, allele_frequency) don't need chr since that will be consistent across all
                    variants.append((line_elements[1], line_elements[3], line_elements[4], line_elements[5], allele_frequency))
            except:
                pass
        # if we are at a multialellic site, we need to do extra logic, and things will be slower
        else:
            # make a list of the alternate alleles
            alts = line_elements[4].split(',')
            # populate a list of frequencies for each alt allele (without using for)
            # adding one to have a reference category as 0, as this might be as fast or faster than individual logic checks for zeroes
            alt_frequencies = [0] * (len(alts) + 1)
            for gaf_index in gaf_list:
                try:
                    # use the number associated with the alt to index into the correct alt frequency
                    alt_frequencies[int(line_elements[gaf_index][0])] += 1
                    alt_frequencies[int(line_elements[gaf_index][2])] += 1
                # if either of those fail out, just ignore this sample
                except:
                    # print(f"Missing value in sample {line_elements[gaf_index]}")
                    alelle_n -= 2
            # alts will always be 1 shorter than alt_frequencies
            for ind, alt in enumerate(alts):
                # if there's only missing calls for the samples given, this will break from the try and not apply the new variant
                try:
                    allele_frequency = alt_frequencies[ind+1]/alelle_n
                    if allele_frequency != 0:
                        variants.append((line_elements[1], line_elements[3], alt, line_elements[5], allele_frequency))
                except:
                    pass

with open(args.output, "wt") as outfile:
    if not args.no_header:
        print("POS\tREF\tALT\tQUAL\tAF", file=outfile)
    for variant in variants:
        print("\t".join(map(str, variant)), file=outfile)