# (there used to be a source("R/platonics.R") here. it isn't needed, R collates everything
# in R/ into the package anyway, and it only worked at all while the working directory
# happened to be the package root)

# function that turns age-based sex information into inferred ages and changes the sex column to reflect more broad categories
sex_changer <- function(data_frame, sex_column_name){
  # make a new vector to append values to which is our actual sexes
  new_sexes <- c()
  inferred_age <- c()
  # for each row in the data frame's sex instance
  for (x in data_frame[[sex_column_name]]){
    # long else if to see if any of the categories that we know of for horse sex are here
    # have to use a conditional here because male is part of female
    if (grepl("male", platonic_obliterator(x)) && !(grepl("female", platonic_obliterator(x)))){
      # we don't know their castration status, or their age
      new_sexes <- c(new_sexes, "Ambiguous Male")
      inferred_age <- c(inferred_age, NA)
    }
    else if (grepl("female", platonic_obliterator(x))){
      new_sexes <- c(new_sexes, "Female")
      inferred_age <- c(inferred_age, NA)
    }
    # we don't know the sex if it's just a "foal", so this is an unknown sex situation, but we do need to still list its inferred age
    else if (grepl("foal", platonic_obliterator(x))){
      new_sexes <- c(new_sexes, "Unknown")
      # a foal is any horse below one year of age, so this will make the slider make sense for them
      inferred_age <- c(inferred_age, 0.999)
    }
    else if (grepl("gelding", platonic_obliterator(x))){
      # a gelding is a castrated male horse
      new_sexes <- c(new_sexes, "Gelding")
      # but there is no age information in the title
      inferred_age <- c(inferred_age, NA)
    }
    else if (grepl("yearling", platonic_obliterator(x))){
      # no sex information present
      new_sexes <- c(new_sexes, "Unknown")
      # we know that yearlings are older than 1, but younger than 2, so 1.5 puts them in their estimated range
      inferred_age <- c(inferred_age, 1.5)
    }
    else if (grepl("filly", platonic_obliterator(x))){
      # a filly is a non-adult (younger than 4) female horse
      new_sexes <- c(new_sexes, "Female")
      inferred_age <- c(inferred_age, 3.999)
    }
    else if (grepl("mare", platonic_obliterator(x))){
      # a mare is an adult (older than 4) female horse
      new_sexes <- c(new_sexes, "Female")
      inferred_age <- c(inferred_age, 4.001)
    }
    else if (grepl("colt", platonic_obliterator(x))){
      # a colt is a non-adult (older than 4) intact male
      new_sexes <- c(new_sexes, "Intact Male")
      inferred_age <- c(inferred_age, 3.999)
    }
    else if (grepl("stallion", platonic_obliterator(x))){
      # a stallion is an adult (older than 4) intact male
      new_sexes <- c(new_sexes, "Intact Male")
      inferred_age <- c(inferred_age, 4.001)
    }
    else{
      # if none of those phrases are part of our sex column, we have no idea what either age or sex is
      new_sexes <- c(new_sexes, "Unknown")
      inferred_age <- c(inferred_age, NA)
    }
  }
  # overwrite the existing sex column with the new sexes
  data_frame[[sex_column_name]] <- new_sexes
  # add a new column for inferred ages
  data_frame <- cbind(data_frame, inferred_age)
  return(data_frame)
}

# slicing and dicing our initial metadata .csv up into digestible values
# preferable so that I never have to update it manually, and so that it hopefully lives on past me
# might be better to return a list of the declared variables here, since that's cleaner, but I'm mostly using this function as a way of simplifying a code block and putting it somewhere else
process_csv <- function(filename, validation_vcf_path){
  # get vcf samples
  samples_out <- herder_scratch("vcf_samples.txt")
  run_herder_bin("sample_lister", c(validation_vcf_path, samples_out))
  con = file(samples_out, "r")
  vcf_samples <- readLines(con = con)
  close(con)

  # make the raw_csv available to the environment
  raw_csv <<- read.csv(filename)
  # get the ID column, which is the first column with the capital letters ID in them (not super robust but works hopefully)
  id_cat <- which(grepl("ID", colnames(raw_csv)))
  colnames(raw_csv)[id_cat] <- "Horse_ID"
  # check for sex, age, and breed columns similarly
  id_cat <- which(grepl("Breed", colnames(raw_csv), ignore.case = TRUE))
  colnames(raw_csv)[id_cat] <- "Breed"
  id_cat <- which(grepl("Sex", colnames(raw_csv), ignore.case = TRUE))
  colnames(raw_csv)[id_cat] <- "Sex"
  id_cat <- which(grepl("Age", colnames(raw_csv), ignore.case = TRUE))
  colnames(raw_csv)[id_cat] <- "Age"
  # making a processed version of our csv, to keep the original around for any cases we need it
  # enforce only the vcf available samples!
  processed_csv <- raw_csv |> dplyr::filter(Horse_ID %in% vcf_samples)
  # label all missing fields (the ^$ is a regex for empty, since it's ^ to start and $ to end)
  processed_csv$Breed <- sub("^$", "Breed Missing from Metadata", processed_csv$Breed)
  processed_csv$Sex <- sub("^$", "Sex Missing from Metadata", processed_csv$Sex)
  # make the age values which were read in as strings numeric. !!!This will obliterate any character vectors or missing values and turn them into NAs!!!
  processed_csv$Age <- as.numeric(processed_csv$Age)
  # process the breed and sex data, attempting to remove formatting to reduce categories
  processed_csv <- platonic_column(processed_csv, "Breed", TRUE)
  processed_csv <- platonic_column(processed_csv, "Sex", TRUE)

  # process the sex information in the metadata into a new category which only includes unknown, male intact/not intact/ambiguous, and female
  processed_csv <<- sex_changer(processed_csv, "Sex")

  # make lists of our unique categories
  unique_breeds <<- unique(processed_csv$Breed)
  unique_ages <<- unique(processed_csv$Age)
  unique_sexes <<- unique(processed_csv$Sex)
  return()
}

# pull one attribute (e.g. gene_name) out of the GTF attribute column for the
# WHOLE vector at once. The old per-row sapply() took ~90s on a ~1.9M-row gtf
# and, worse, collapsed to a list whenever a row had no match (crashing the
# == fill below). This is fully vectorized and returns "" for non-matches.
extract_gtf_attr <- function(x, key) {
  m <- regexpr(paste0(key, ' "[^"]+"'), x)
  out <- rep("", length(x))
  out[m != -1] <- sub(paste0(key, ' "([^"]+)"'), "\\1",
                      regmatches(x, m))
  out
}

load_gtf <- function(path) {
  # quote = "" is essential: by default read.delim treats " as a field-quote
  # char and STRIPS the quotes out of the attribute column, after which the
  # `key "value"` regex below matches nothing (this was the original bug).
  gtf <- read.delim(path, header = FALSE, comment.char = "#", quote = "",
                    col.names = c("seqname","source","feature","start","end",
                                  "score","strand","frame","attribute"),
                    stringsAsFactors = FALSE)

  # only gene/exon are drawn by the track plot; dropping CDS roughly halves the
  # rows we have to parse and keep around
  gtf <- gtf |> dplyr::filter(feature %in% c("gene", "exon"))
  gtf$gene_name <- extract_gtf_attr(gtf$attribute, "gene_name")
  gtf$gene_id   <- extract_gtf_attr(gtf$attribute, "gene_id")
  gtf$gene_name[gtf$gene_name == ""] <- gtf$gene_id[gtf$gene_name == ""]

  gtf
}
