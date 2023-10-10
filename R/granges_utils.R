#' Convert a MACS2 peaks file to a GRanges object
#'
#' @param file_path Path to the MACS2 peaks file.
#'
#' @param macs_file The type of MACS2 file to read in.
#'
#' @export
macs2_to_ranges_df <- function (file_path, macs_file = "peaks_xls") {
  .data <- rlang::.data
  readr::read_table(
    file = file_path,
    col_types = readr::cols(
      chr = readr::col_character(),
      start = readr::col_character(),
      length = readr::col_character(),
      pileup = readr::col_character(),
      `-log10(pvalue)` = readr::col_character(),
      fold_enrichment = readr::col_character(),
      `-log10(qvalue)` = readr::col_character()
    ),
    comment = "#"
  ) |>
    dplyr::mutate(chr = paste0("chr", .data$chr)) |>
    dplyr::mutate(
      end = as.numeric(.data$end),
      chip_fold_enrichment = as.numeric(.data$fold_enrichment),
      `-log10(pvalue)` = as.numeric(.data$`-log10(pvalue)`),
      `-log10(qvalue)` = as.numeric(.data$`-log10(qvalue)`)
    ) |>
    dplyr::select(
      .data$chr, .data$start, .data$end,
      .data$chip_fold_enrichment,
      .data$`-log10(pvalue)`,
      .data$`-log10(qvalue)`
    ) |>
    dplyr::rename(
      chrom = .data$chr,
      chromStart = .data$start,
      chromEnd = .data$end
    ) |>
    dplyr::arrange(.data$chrom, .data$chromStart, .data$chromEnd) |>
    dplyr::distinct()
}

#' Convert a data frame to a GRanges object
#'
#' @param df A data frame with columns chrom, chromStart, chromEnd, and other
#'   columns to be kept in the GRanges object.
#'
#' @export
ranges_df_to_granges <- function (df) {
  GenomicRanges::makeGRangesFromDataFrame(
    df = df,
    keep.extra.columns = TRUE
  )
}
