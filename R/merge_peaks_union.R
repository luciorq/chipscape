#' Union
#' Merge ChIP-Seq peaks by the union of the two peak sets.
#' @param gr1 The first GRanges object.
#' @param gr2 The second GRanges object.
#' @export
merge_peaks_union <- function(gr1, gr2) {
  union_gr <- GenomicRanges::union(
    x = gr1,
    y = gr2,
    ignore.strand = TRUE
  )
  union_df <- tibble::tibble(
    chrom = as.character(union_gr@seqnames),
    chromStart = union_gr@ranges@start,
    chromEnd = union_gr@ranges@start + union_gr@ranges@width - 1,
    region_name = stringr::str_c(chrom, ":", chromStart, "-", chromEnd)
  )
  gr1_hits <- GenomicRanges::findOverlaps(gr1, union_gr)
  peaks_df <- tibble::tibble(
    chrom = as.character(gr1@seqnames),
    chromStart = gr1@ranges@start,
    chromEnd = gr1@ranges@start + gr1@ranges@width - 1,
    chip_fold_enrichment = gr1@elementMetadata$chip_fold_enrichment
  )
  id_df <- peaks_df[gr1_hits@from, ] |>
    tibble::rowid_to_column("peak_id") |>
    dplyr::select(peak_id, chip_fold_enrichment)
  gr1_union_df <- union_df[gr1_hits@to, ] |>
    tibble::rowid_to_column("peak_id") |>
    dplyr::left_join(id_df, by = "peak_id") |>
    dplyr::select(region_name, chip_fold_enrichment) |>
    dplyr::group_by(region_name) |>
    dplyr::summarize(chip_fold_enrichment = max(chip_fold_enrichment)) |>
    dplyr::ungroup() |>
    dplyr::rename(chip_fold_1 = chip_fold_enrichment) |>
    dplyr::distinct()

  # for gr2
  gr2_hits <- GenomicRanges::findOverlaps(gr2, union_gr)
  peaks_df <- tibble::tibble(
    chrom = as.character(gr2@seqnames),
    chromStart = gr2@ranges@start,
    chromEnd = gr2@ranges@start + gr2@ranges@width - 1,
    chip_fold_enrichment = gr2@elementMetadata$chip_fold_enrichment
  )
  id_df <- peaks_df[gr2_hits@from, ] |>
    tibble::rowid_to_column("peak_id") |>
    dplyr::select(peak_id, chip_fold_enrichment)
  gr2_union_df <- union_df[gr2_hits@to, ] |>
    tibble::rowid_to_column("peak_id") |>
    dplyr::left_join(id_df, by = "peak_id") |>
    dplyr::select(region_name, chip_fold_enrichment) |>
    dplyr::group_by(region_name) |>
    dplyr::summarize(chip_fold_enrichment = max(chip_fold_enrichment)) |>
    dplyr::ungroup() |>
    dplyr::rename(chip_fold_2 = chip_fold_enrichment) |>
    dplyr::distinct()

  chip_union_df <- union_df |>
    dplyr::left_join(gr1_union_df, by = "region_name") |>
    dplyr::left_join(gr2_union_df, by = "region_name") |>
    dplyr::mutate(
      chip_fold_1 = dplyr::if_else(is.na(chip_fold_1), NA_integer_, chip_fold_1),
      chip_fold_2 = dplyr::if_else(is.na(chip_fold_2), NA_integer_, chip_fold_2),
      # mean_chip_fold_enrichment = (chip_fold_1 + chip_fold_2)/2
      mean_chip_fold_enrichment = min(chip_fold_1, chip_fold_2, (chip_fold_1 + chip_fold_2)/2, na.rm = TRUE)
    ) |>
    dplyr::select(-c(chip_fold_1, chip_fold_2)) |>
    dplyr::arrange(-mean_chip_fold_enrichment) |>
    dplyr::distinct()
  return(chip_union_df)
}
