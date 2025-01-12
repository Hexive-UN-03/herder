#!/bin/bash

# invoke with
# fast_af.sh vcf_path region samplefile_path output_path
vcfinput=$1
region=$2
samplefile=$3
outputname=$4

# takes in a region, generates a region file for it and runs it through slow_af.py
# invoke with run_region region
# the module calls here are not needed but for some reason massively drop efficiency when removed???
run_region () {
    # makes a file to append the actual data to with just the header sample line
    tail -n 1 "./fast_af_output.junk/header.junk" > "./fast_af_output.junk/"$1"_region_vcf.junk"
    # pulls the requested genomic region and prints it to the end of the new single line headed file
    bcftools view -H -r $1 -S $samplefile $vcfinput >> "./fast_af_output.junk/"$1"_region_vcf.junk"
    # runs slow_af on the specific region outputting a region file
    python $BASEDIR"/slow_af.py" -i "./fast_af_output.junk/"$1"_region_vcf.junk" -s $samplefile -o "./fast_af_output.junk/"$1"_region_output.junk" --no_header
}

BASEDIR=$(dirname $0)

# process our region into 16 pieces to run in parallel and store them in an array
regionstring=$(python $BASEDIR"/explode_region.py" $region)
IFS=' ' read -r -a shrapnel <<< $regionstring

# make a junk file directory to nuke later
mkdir fast_af_output.junk

# makes a file with JUST the header info
bcftools view -h -S $samplefile -o "./fast_af_output.junk/header.junk" $vcfinput

# spawn parallel processes for each range we have to generate region files and calculate them into outputs
for range in "${shrapnel[@]}"
do
    run_region $range &
done

# wait for parallel processes to complete
wait

# output our final tsv by appending the shrapnel results back together
echo "POS"$'\t'"REF"$'\t'"ALT"$'\t'"QUAL"$'\t'"AF" > $outputname
for range in "${shrapnel[@]}"
do
    cat "./fast_af_output.junk/"$range"_region_output.junk" >> $outputname
done

# clean the trash
rm -r fast_af_output.junk
