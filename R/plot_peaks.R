#' Plot Peaks
#' Plot ChIP peaks against CpG methylation frequency.
#' @param sum_df The summary data frame.
#' @param experiment_name The name of the experiment. Used for the file name.
#' @export
plot_chip_peak_vs_cpg <- function(sum_df, experiment_name) {
  #color_pallete <- rev(c("#ffc542", "#ff5f1f", "#fc3134", "#000000", "#000000"))
  color_pallete <- rev(c("#ff5f1f", "#fc3134", "#000000", "#000000"))
  color_pallete <- grDevices::colorRampPalette(color_pallete)(100)
  plot_obj <- sum_df |>
    ggplot2::ggplot(ggplot2::aes(x = mean_chip_fold_enrichment, y = freqC, fill = freqC, size = num_cpg)) +
    ggplot2::geom_point(shape = 21, color = "gray") +
    ggplot2::geom_vline(xintercept = 0, color = "gray40") +
    ggplot2::geom_hline(yintercept = 50, color = "red", linetype = "dotted") +
    ggplot2::labs(
      title = glue::glue("{experiment_name}"),
      x = "H3K27me3 Fold Enrichment",
      y = "Mean Methylated CpG\nFrequency",
      fill = "Mean Methylated CpG Frequency",
      size = "Number of CpG\nin Peak"
    ) +
    ggplot2::scale_fill_gradientn(colors = color_pallete) +
    ggplot2::theme_bw()

  ggplot2::ggsave(
    filename = fs::path("figs", glue::glue("{ experiment_name }-peaks_vs_cpg"), ext = "png"),
    plot = plot_obj,
    device = "png",
    width = 7,
    height = 5
  )

  ggplot2::ggsave(
    filename = fs::path("figs", glue::glue("{ experiment_name }-peaks_vs_cpg"), ext = "eps"),
    plot = plot_obj,
    device = "eps",
    width = 7,
    height = 5
  )
  return(plot_obj)
}
