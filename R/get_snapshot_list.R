#' Get a list of snapshot tables
#'
#' @param pattern Optional: A REGEX string to match only specified tables.
#'
#' @param cols A character vector of columns to include in each table of the output list.
#'
#' @param include_table_name Logical: if True, include the table name as a column in the output for each table in the list (default is FALSE).
#'
#' @return A list of snapshot tables.
#' @export
#'
#' @examples
#' get_snapshot_list(pattern = "b[0-9]{1,2}")
get_snapshot_list <- function(pattern = NULL, cols, include_table_name = FALSE) {
    # Path to the snapshot folder on the ENDIA shared drive
    snapshot_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"

    # List files in snapshot folder
    snapshot_files <- base::list.files(snapshot_path)

    # Optionally filter based on pattern passed to function
    if (missing(pattern)) {

        # Filter to only csv files
        snapshot_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "\\.csv$")]

        # Remove .csv from end of files
        snapshot_files <- stringr::str_remove(snapshot_files, "\\.csv")
    }
    else {
        # Filter to only csv files
        snapshot_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "\\.csv$")]

        # Remove .csv from end of files
        snapshot_files <- stringr::str_remove(snapshot_files, "\\.csv")

        # Match specified pattern
        snapshot_files <-
            snapshot_files[stringr::str_detect(snapshot_files, pattern)]
    }

    # Map function that reads in each dataset to the visit_files list and return list
    snapshot_files |>
        purrr::set_names() |>
        purrr::map(
            ENDIA::get_snapshot_table,
            include_table_name = include_table_name,
            guess_max = Inf,
            cols = cols,
            .progress = TRUE
        )
}
