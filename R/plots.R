#' Plotting functions for the allele frequency tab
#'
#' @import ggplot2
#' @noRd

# these all used to live inside app_server, which meant nothing outside of a running
# app could ever call them (very annoying when you want to make a figure for a poster).
# they don't touch any reactives, so they move out here with no changes to how they behave

# the af plot and the gene track sit stacked on top of each other in separate plotOutputs,
# and they HAVE to stay separate: the brush and click only give us usable coordinates on a
# single-panel ggplot, so gluing them into one image with patchwork would quietly break
# nearPoints and the brush zoom. that leaves us doing the alignment by hand.
# two plots rendered at the same width line up if and only if the stuff on either side of
# the panel is the same size:
#   left  = plot margin + y axis title + y axis text + y tick length
#   right = plot margin + whatever legend is hanging off the side
# so everything below exists to make those two numbers identical in every track we draw,
# including the blank "nothing here" ones
track_geom <- function(roi_start, roi_end, ylim = NULL, clip = "on"){
  list(
    # pin the expansion instead of leaving it to the default, so a future change to one
    # plot can't silently knock it out of step with the other
    scale_x_continuous(expand = expansion(mult = 0.05),
                       labels = scales::label_number(big.mark = ",", accuracy = 1)),
    # coord_cartesian rather than xlim() because we want to zoom, not throw data away
    coord_cartesian(xlim = c(roi_start, roi_end), ylim = ylim, clip = clip),
    theme_minimal(base_size = 13),
    theme(
      # every one of these is a source of left-hand gutter, and we want zero of it
      axis.title.y        = element_blank(),
      axis.text.y         = element_blank(),
      axis.ticks.y        = element_blank(),
      # not redundant with the line above! a blanked tick still gets its length reserved
      # in the layout, so without this the two plots differ by a couple of points
      axis.ticks.length.y = unit(0, "pt"),
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      panel.grid.minor.x  = element_blank(),
      # the legend goes on the bottom purely because a legend on the right eats about
      # 0.6in of panel width, and the gene track has no legend to eat the same amount
      legend.position       = "bottom",
      legend.direction      = "horizontal",
      legend.justification  = "center",
      # title above the bar, not beside it. beside it the "0.00" tick label runs back
      # underneath the word "Frequency" and the two collide
      legend.title.position = "top",
      legend.key.width      = unit(1.8, "in"),
      legend.key.height     = unit(0.12, "in"),
      legend.box.spacing    = unit(4, "pt"),
      plot.margin           = margin(4, 14, 4, 14),
      plot.title.position   = "plot"
    ),
    # y is NULL and not "" on purpose, an empty string is still a real text grob and it
    # still claims its margin
    labs(x = "Genomic Position", y = NULL)
  )
}

# general af_plot function
# takes the window explicitly now, since track_geom needs it to match the gene track
generate_af_plot <- function(df, roi_start, roi_end,
                             title = "Allele Frequencies", show_legend = TRUE){
  ggplot(df, aes(x = POS, y = 0, color = AF)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_point(size = 5) +
    scale_color_gradient(low = "red", high = "green", limits = c(0, 1), name = "Frequency",
                         guide = if (show_legend) "colourbar" else "none") +
    # ylim is pinned so the zoomed copy has the same y domain as the full one, otherwise
    # the 10px nearPoints threshold means something different in each of them
    track_geom(roi_start, roi_end, ylim = c(-1, 1), clip = "on") +
    labs(title = title)
}

# the vcf calls chromosomes "chr1" and ensembl calls them "1", so we can't just compare
# the two strings. lowercase it and pull the chr off the front and they agree again
platonic_chrom <- function(x){
  sub("^chr", "", tolower(as.character(x)))
}

# cut the annotation down to one chromosome. this was NOT being done before, and the
# consequence was quietly horrible: a 2Mb window on chr1 was matching everything between
# 1 and 2Mb on all 32 chromosomes, so the picker offered 2339 genes and the track was
# drawing features that have nothing to do with the region you're looking at
chrom_subset <- function(df, chrom){
  if (is.null(chrom) || is.na(chrom) || !nzchar(chrom)){
    return(df)
  }
  df[platonic_chrom(df$seqname) == platonic_chrom(chrom), ]
}

# small helper so the plot and the picker agree on what counts as "in region"
genes_in_region <- function(df, chrom, roi_start, roi_end) {
  df <- chrom_subset(df, chrom)
  g <- df[df$feature == "gene" & df$end >= roi_start & df$start <= roi_end, ]
  sort(unique(g$gene_name))
}

# a centred message on an otherwise blank track (the empty / prompt state)
# this keeps the full track_geom rather than theme_void, because a void plot has no axis
# and no fixed width, so the whole column used to jump sideways the moment you picked a gene
gene_track_message <- function(roi_start, roi_end, msg) {
  ggplot() +
    annotate("text", x = (roi_start + roi_end) / 2, y = 0,
             label = msg, size = 5, color = "grey45") +
    track_geom(roi_start, roi_end, ylim = c(-1, 1), clip = "off")
}

# label_size is only here so the poster script can crank the gene names up without having
# to reimplement the plot
generate_gene_track_plot <- function(df, chrom, roi_start, roi_end, selected = NULL,
                                     label_size = 3.6) {
  # one chromosome only, otherwise the "region" is just a coordinate range and we end up
  # drawing genes from every chromosome at once
  df <- chrom_subset(df, chrom)
  snipped <- df |> dplyr::filter(end >= roi_start, start <= roi_end)

  genes <- snipped |> dplyr::filter(feature == "gene")
  exons <- snipped |> dplyr::filter(feature == "exon")
  # one body per gene (Ensembl can carry duplicate gene rows for PAR genes etc.)
  genes <- genes[!duplicated(genes$gene_name), ]
  n_feat <- nrow(genes)

  # nothing annotated here at all
  if (n_feat == 0){
    return(gene_track_message(roi_start, roi_end, "No annotated genes in this region"))
  }
  # empty/prompt state: show the count until the user picks genes to display
  if (is.null(selected) || length(selected) == 0){
    return(gene_track_message(roi_start, roi_end,
      paste0(n_feat, " feature", if (n_feat == 1) "" else "s",
             " in this region — select genes above to display them")))
  }

  # keep only the genes (and their exons) the user asked for
  genes <- genes[genes$gene_name %in% selected, ]
  exons <- exons[exons$gene_name %in% selected, ]
  if (nrow(genes) == 0){
    return(gene_track_message(roi_start, roi_end,
      paste0(n_feat, " feature", if (n_feat == 1) "" else "s",
             " in this region — none of the selected genes are here")))
  }

  # Collapse transcript isoforms: merge all overlapping exons within a gene into
  # one clean set of blocks, otherwise every transcript's exons stack up.
  merge_intervals <- function(s, e) {
    o <- order(s); s <- s[o]; e <- e[o]
    rs <- s[1]; re <- e[1]; out_s <- c(); out_e <- c()
    for (j in seq_along(s)) {
      if (s[j] <= re) { re <- max(re, e[j]) }
      else { out_s <- c(out_s, rs); out_e <- c(out_e, re); rs <- s[j]; re <- e[j] }
    }
    data.frame(start = c(out_s, rs), end = c(out_e, re))
  }
  if (nrow(exons) > 0) {
    exons <- do.call(rbind, lapply(split(exons, exons$gene_name), function(g) {
      m <- merge_intervals(g$start, g$end)
      m$gene_name <- g$gene_name[1]
      m
    }))
  }

  # Greedy interval packing by genomic overlap only: each gene drops into the
  # first lane whose previous gene has already ended, otherwise a new lane opens.
  # Non-overlapping genes share a lane, so we use the fewest rows possible.
  genes <- genes[order(genes$start), ]
  lane_end <- numeric(0)
  genes$lane <- NA_integer_
  for (i in seq_len(nrow(genes))) {
    free <- which(genes$start[i] > lane_end)
    if (length(free) == 0) {
      lane_end <- c(lane_end, genes$end[i])
      genes$lane[i] <- length(lane_end)
    } else {
      genes$lane[i] <- free[1]
      lane_end[free[1]] <- genes$end[i]
    }
  }
  n_lanes <- max(genes$lane)
  exons$lane <- genes$lane[match(exons$gene_name, genes$gene_name)]
  exons <- exons[!is.na(exons$lane), ]

  # clamp to the visible window so partial features and their labels stay in-panel
  genes$vstart <- pmax(genes$start, roi_start)
  genes$vend   <- pmin(genes$end,   roi_end)
  if (nrow(exons) > 0) {
    exons$vstart <- pmax(exons$start, roi_start)
    exons$vend   <- pmin(exons$end,   roi_end)
  }
  # point the strand arrow in the direction of transcription
  genes$x1 <- ifelse(genes$strand == "-", genes$vend, genes$vstart)
  genes$x2 <- ifelse(genes$strand == "-", genes$vstart, genes$vend)

  exon_h <- 0.28   # half-height of an exon block (lanes are 1 unit apart)

  ggplot() +
    # thin gene body with a strand arrow
    geom_segment(data = genes,
                 aes(x = x1, xend = x2, y = lane, yend = lane),
                 linewidth = 0.8, color = "grey45",
                 arrow = grid::arrow(length = grid::unit(0.10, "inches"),
                                     type = "closed")) +
    # exon blocks
    geom_rect(data = exons,
              aes(xmin = vstart, xmax = vend, ymin = lane - exon_h, ymax = lane + exon_h),
              fill = "steelblue", color = "steelblue4", linewidth = 0.25) +
    # label every gene above its block; check_overlap drops any that collide
    geom_text(data = genes,
              aes(x = (vstart + vend) / 2, y = lane + 0.42, label = gene_name),
              size = label_size, fontface = "bold", vjust = 0, check_overlap = TRUE) +
    # clip is off here so the gene labels sitting above the top lane don't get chopped
    track_geom(roi_start, roi_end, ylim = c(0.3, n_lanes + 1), clip = "off")
}
