#' Run MACS2 Peak Calling
#'
#' @param sample_name The name of the sample. This value will be used
#'   as the prefix for the output files and output directory.
#'
#' @param treatment_bam The path to the treatment BAM file. This argument
#'   accepts a vector of paths.
#'
#' @param control_bam The path to the control BAM file. If `NULL` then
#'   no control is used. This argument accepts a vector of paths.
#'
#' @param cutoff The cutoff to use for peak calling. Defaults to `0.05`.
#'
#' @param broad_cutoff The cutoff to use for broad peak calling.
#'   Defaults to `0.1`.
#'
#' @param broad Logical. Whether to call broad peaks. Defaults to `FALSE`.
#'
#' @param model Logical. Whether to model the fragment length.
#'    For paired-end samples this should be `FALSE`. Defaults to `TRUE`.
#'
#' @param bedgraph Logical. Whether to output the bedgraph file.
#'   Defaults to `FALSE`.
#'
#' @param genome The genome to use. Defaults to `hs`. If a non-model organism is
#'   used, please use the genome length as stated in MACS2 documentation.
#'
#' @param input_format The input format. Defaults to `BAMPE` for BAM from
#'   paired-end experiment. Other options are are stated in the MACS2
#'   documentation.
#'
#' @param env_name The name of the Conda environment to use.
#'
#' @export
run_macs2 <- function(
    sample_name,
    treatment_bam,
    control_bam = NULL,
    cutoff = 0.05,
    broad_cutoff = 0.1,
    broad = FALSE,
    model = TRUE,
    bedgraph = FALSE,
    genome = "hs",
    input_format = "BAMPE",
    env_name = "macs2-env") {
  if (!isTRUE(condathis::env_exists(env_name))) {
    condathis::create_env(
      packages = c("macs2"),
      env_name = env_name
    )
  }
  output_dir <- fs::path("macs2", sample_name)
  if (!fs::dir_exists(dirname(output_dir))) {
    fs::dir_create(dirname(output_dir))
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
  if (isFALSE(model)) {
    model_args <- c(
      "--nomodel"
    )
  } else {
    model_args <- NULL
  }
  bdg_arg <- NULL
  if (isTRUE(bedgraph)) {
    bdg_arg <- c(
      "--bdg"
    )
  }
  if (isTRUE(is.null(control_bam))) {
    control_args <- NULL
  } else {
    control_args <- c("-c", control_bam)
  }
  condathis::run(
    "macs2", "callpeak",
    "-t", treatment_bam,
    control_args,
    "--format", input_format,
    bdg_arg,
    "-g", genome,
    "-q", cutoff,
    summit_args,
    model_args,
    broad_args,
    "-n", sample_name,
    "--outdir", output_dir,
    env_name = env_name
  )
}
