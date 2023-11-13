#' @title Convert a `sum_df` to a GRanges object
#' @description Convert a `sum_df` to a GRanges object.
#'   Additional metadata is kept.
#' @param sum_df A data frame with a region column.
#' @export
sum_df_to_gr <- function(sum_df) {
  gr <- sum_df |>
    dplyr::select(region) |>
    dplyr::mutate(
      chrom = stringr::str_remove(region, ":.*$"),
      chromStart = stringr::str_remove(region, "^.*:") |> stringr::str_remove("-.*$"),
      chromEnd = stringr::str_remove(region, "^.*-")
    ) |>
    dplyr::arrange(.data$chrom, .data$chromStart, .data$chromEnd) |>
    dplyr::distinct() |>
    chipscape::ranges_df_to_granges()
  return(gr)
}
