#' Read Bismark CpG output into a Data Frame
#' @export
read_bismark_cpg <- function(path) {
  bismark_df <- path |>
    readr::read_table(
      col_types = readr::cols(
        chrBase = readr::col_character(),
        chr = readr::col_character(),
        base = readr::col_character()
      )
    ) |>
    dplyr::mutate(
      chr = stringr::str_c("chr", chr),
      base = as.numeric(base)
    )
}

#' Read Bismark output into a GRanges object
#' @export
bismark_df_to_granges <- function(bismark_df) {
  bismark_granges <- bismark_df |>
    dplyr::mutate(
      start = as.numeric(base),
      end = as.numeric(start),
      freqC = freqC
    ) |>
    dplyr::rename(
      chrom = chr,
      chromStart = start,
      chromEnd = end
    ) |>
    dplyr::select(
      chrom, chromStart, chromEnd, freqC
    ) |>
    dplyr::arrange(chrom, chromStart, chromEnd) |>
    dplyr::distinct() |>
    GenomicRanges::makeGRangesFromDataFrame(
      keep.extra.columns = TRUE
    ) |>
    GenomicRanges::sort()
}
