# the whole point of the af plot and the gene track being stacked is that a variant lines
# up with the gene it's sitting in. that only holds if the two plots reserve exactly the
# same amount of space either side of their panel, and it is very easy to break by adding
# a legend or nudging a margin, so it gets a test.
# the panel is the only null-width cell in a ggplot gtable and everything else is absolute,
# so if the left and right gutters match then the panels match at ANY device width

# measure the inches of furniture to the left and right of the panel
gutters <- function(p, width_in = 11, height_in = 1.85) {
  pdf(NULL, width = width_in, height = height_in)
  on.exit(dev.off())
  g  <- ggplot2::ggplotGrob(p)
  pn <- g$layout[g$layout$name == "panel", ]
  n  <- length(g$widths)
  c(left  = sum(grid::convertWidth(g$widths[seq_len(pn$l - 1)], "in", valueOnly = TRUE)),
    right = sum(grid::convertWidth(g$widths[seq(pn$r + 1, n)], "in", valueOnly = TRUE)))
}

# a handful of fake variants and two fake genes, enough to draw every state
fake_af <- data.frame(POS = seq(1e6, 2e6, length.out = 50), AF = seq(0, 1, length.out = 50))
fake_gtf <- data.frame(
  seqname   = c("1", "1", "1", "1", "1"),
  feature   = c("gene", "exon", "exon", "gene", "exon"),
  start     = c(1200000, 1200000, 1250000, 1600000, 1600000),
  end       = c(1300000, 1210000, 1300000, 1700000, 1650000),
  strand    = c("+", "+", "+", "-", "-"),
  gene_name = c("GENEA", "GENEA", "GENEA", "GENEB", "GENEB"),
  stringsAsFactors = FALSE
)

test_that("every track reserves the same space either side of its panel", {
  roi_start <- 1e6
  roi_end <- 2e6
  reference <- gutters(generate_af_plot(fake_af, roi_start, roi_end))

  states <- list(
    # the zoomed af plot drops its legend, which must not change the panel width
    af_zoom     = generate_af_plot(fake_af, roi_start, roi_end,
                                   title = "Zoomed Allele Frequencies", show_legend = FALSE),
    with_genes  = generate_gene_track_plot(fake_gtf, "chr1", roi_start, roi_end, c("GENEA", "GENEB")),
    # the three blank/prompt states used to be theme_void, which lined up with nothing
    nothing_here   = generate_gene_track_plot(fake_gtf[0, ], "chr1", roi_start, roi_end, NULL),
    pick_something = generate_gene_track_plot(fake_gtf, "chr1", roi_start, roi_end, NULL),
    none_of_those  = generate_gene_track_plot(fake_gtf, "chr1", roi_start, roi_end, "NOT_A_GENE")
  )

  for (nm in names(states)) {
    expect_equal(gutters(states[[nm]]), reference, info = nm)
  }
})

test_that("genes_in_region only returns genes that overlap the window", {
  # GENEA is 1.2-1.3Mb, GENEB is 1.6-1.7Mb
  expect_equal(genes_in_region(fake_gtf, "chr1", 1e6, 2e6), c("GENEA", "GENEB"))
  expect_equal(genes_in_region(fake_gtf, "chr1", 1e6, 1.4e6), "GENEA")
  # a window that only clips the tail end of GENEB should still find it
  expect_equal(genes_in_region(fake_gtf, "chr1", 1.65e6, 3e6), "GENEB")
  expect_equal(genes_in_region(fake_gtf, "chr1", 1.4e6, 1.5e6), character(0))
})

test_that("a region only picks up genes on its own chromosome", {
  # same coordinates, but GENEC is sat on chr2
  two_chrom <- rbind(fake_gtf, data.frame(
    seqname = "2", feature = "gene", start = 1200000, end = 1300000,
    strand = "+", gene_name = "GENEC", stringsAsFactors = FALSE))

  expect_equal(genes_in_region(two_chrom, "chr1", 1e6, 2e6), c("GENEA", "GENEB"))
  expect_equal(genes_in_region(two_chrom, "chr2", 1e6, 2e6), "GENEC")
  # the vcf says "chr1" and ensembl says "1", both have to land on the same place
  expect_equal(genes_in_region(two_chrom, "1", 1e6, 2e6),
               genes_in_region(two_chrom, "chr1", 1e6, 2e6))
  # and if we genuinely don't know the chromosome, fall back to not filtering rather
  # than silently returning nothing
  expect_equal(genes_in_region(two_chrom, NULL, 1e6, 2e6), c("GENEA", "GENEB", "GENEC"))
})
