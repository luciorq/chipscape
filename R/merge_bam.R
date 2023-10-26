#' Merge BAM Files
#' @param bam_paths Character Vector. Vector of paths to BAM files.
#' @param output_path Path to where to save files.
#' @param env_name The name of the Conda environment to use.
#' @export
merge_bam <- function(bam_paths, output_path, env_name = "samtools-env") {
  if (!isTRUE(condathis::env_exists(env_name))) {
    condathis::create_env(
      packages = c("samtools"),
      env_name = env_name
    )
  }
  if (!fs::dir_exists(dirname(output_path))) {
    fs::dir_create(dirname(output_path))
  }
  condathis::run(
    "samtools", "cat",
    "-o", output_path,
    # "--threads", min(parallelly::availableCores(), 4),
    # "--output-fmt", "BAM",
    # "-b",
    bam_paths,
    # stdout = output_path,
    env_name = "samtools-env"
  )
}

#' Sort BAM Files
#' @param bam_path Character. Path to BAM file.
#' @param env_name The name of the Conda environment to use.
#' @export
sort_bam <- function(bam_path, env_name = "samtools-env") {
  if (!isTRUE(condathis::env_exists(env_name))) {
    condathis::create_env(
      packages = c("samtools"),
      env_name = env_name
    )
  }
  output_path <- stringr::str_replace(bam_path, ".bam$", "_sorted.bam")
  if (!fs::dir_exists(dirname(output_path))) {
    fs::dir_create(dirname(output_path))
  }
  condathis::run(
    "samtools", "sort",
    "-o", output_path,
    "--threads", min(parallelly::availableCores(), 4),
    # "--output-fmt", "BAM",
    # "-b",
    bam_path,
    # stdout = output_path,
    env_name = "samtools-env"
  )
}
