#' @export
intersect_df_from_granges <- function(chip_gr, dmr_gr, min_overlap = 1L) {
  overlap_df <- GenomicRanges::findOverlaps(chip_gr, dmr_gr, minoverlap = min_overlap)
  from_gr <- chip_gr[overlap_df@from]
  from_gr$name_peak <- paste(
    as.character(GenomicRanges::seqnames(from_gr)),
    paste(
      as.character(from_gr@ranges@start),
      as.character(from_gr@ranges@start + from_gr@ranges@width - 1),
      sep = "-"
    ),
    sep = ":"
  )
  to_gr <- dmr_gr[overlap_df@to]
  to_gr$name_dmr <- from_gr$name_dmr

  intersect_df <- tibble::tibble(
    region = from_gr$name_peak,
    freqC = as.numeric(to_gr$freqC),
    # dmr_fdr = from_gr$min_smoothed_fdr,
    # dmr_log_fdr = -log10(from_gr$min_smoothed_fdr),
    # dmr_maxdiff = from_gr$maxdiff,
    # dmr_meandiff = from_gr$meandiff,
    # dmr_num_cpg = from_gr$`no.cpgs`,
    mean_chip_fold_enrichment = from_gr$mean_chip_fold_enrichment,
    # chip_qval = from_gr$`-log10(qvalue)`
    # feature_name = from_gr$`overlapping.genes`,
    # feature_type = from_gr$`type`
  )
  return(intersect_df)
}
