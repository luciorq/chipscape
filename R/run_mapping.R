#' Run Mapping of FASTQ Files
#'
#' Run mapping step for FASTQ files using `minimap2`.
#'   Run minimap2 in the closest way possible to `bowtie2` implementation for
#'   short reads.
#'   Check [this GitHub Issue](https://github.com/tyjo/coptr/issues/2) for
#'   description of the comparison with `bowtie2`.
#'
#' @param sample_name The name of the sample.
#'
#' @param fastq_path The path to the 1 or more FASTQ files. Those can be GZIP,
#'   there is no need to uncompress them.
#'
#' @param reference_fasta The path to the reference FASTA file,
#'   the file can be GZIP compressed.
#'
#' @param env_name The name of the Conda environment to use.
#'
#' @export
run_mapping <- function(
    sample_name,
    fastq_path,
    reference_fasta,
    env_name = "minimap2-env") {
  output_dir <- fs::path("mapping", sample_name)
  fs::dir_create(output_dir)
  output_file <- fs::path(output_dir, sample_name, ext = "sam")
  if (!isTRUE(condathis::env_exists(env_name))) {
    condathis::create_env(
      packages = c("minimap2"),
      env_name = env_name
    )
  }

  # TODO(luciorq): Check for Index file, if not creeate it with -d <FILENAME>
  # + argument for minimap2.

  # minimap2 -ax sr --secondary=yes -k 15 -w 10 <FASTA> <FASTQ1> <FASTQ2>
  condathis::run(
    "minimap2",
    "-ax", "sr",
    "--secondary=yes",
    "-k", 15,
    "-w", 10,
    "-o", output_file,
    reference_fasta,
    fastq_path,
    env_name = env_name
  )
}
