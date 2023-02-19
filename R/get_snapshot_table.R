#' Load Snapshot Data
#'
#' @param file_name A string indicating the name of the csv table to be read in.
#'
#' @return A tibble containing the dataset passed to the file_name argument.
#' @export
#'
#' @examples
#' get_snapshot_table("b1s")
#' get_snapshot_table("mothers")
get_snapshot_table <- function(file_name) {
    # Path to the snapshot folder on the ENDIA shared drive
    snapshot_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"

    # combine file name and path to get location of desired file to read in
    file_path = paste0(snapshot_path, file_name, ".csv")

    # Read in specified file from path
    readr::read_csv(
        file_path,
        guess_max = 1000,
        progress = TRUE,
        show_col_types = TRUE
    )
}
