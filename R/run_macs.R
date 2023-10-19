#' Run MACS2 Peak Calling
#'
#' @param sample_name The name of the sample.
#'
#' @param treatment_bam The path to the treatment BAM file.
#'
#' @param control_bam The path to the control BAM file.
#'
#' @param cutoff The cutoff to use for peak calling.
#'
#' @param broad_cutoff The cutoff to use for broad peak calling.
#'
#' @param broad Whether to call broad peaks.
#'
#' @param env_name The name of the Conda environment to use.
#'
#' @export
run_macs2 <- function(
    sample_name,
    treatment_bam,
    control_bam,
    cutoff = 0.05,
    broad_cutoff = 0.1,
    broad = FALSE,
    env_name = "macs2-env") {
  output_dir <- fs::path("macs2", sample_name)
  fs::dir_create(output_dir)

  if (!isTRUE(condathis::env_exists(env_name))) {
    condathis::create_env(
      packages = c("macs2"),
      env_name = env_name
    )
  }

  summit_args <- c(
    "--call-summits"
  )
  broad_args <- NULL

  if (isTRUE(broad)) {
    broad_args <- c(
      "--broad", "--broad-cutoff", broad_cutoff
    )
    summit_args <- NULL
  }
  condathis::run(
    "macs2", "callpeak",
    "-t", treatment_bam,
    "-c", control_bam,
    "-g", "hs",
    "-q", cutoff,
    summit_args,
    broad_args,
    "-n", sample_name,
    "--outdir", output_dir,
    env_name = env_name
  )
}
