get_snapshot_list <- function(pattern = NULL) {
    # Path to the snapshot folder on the ENDIA shared drive
    snapshot_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"

    # List files in snapshot folder
    snapshot_files <- base::list.files(snapshot_path)

    # Optionally filter based on pattern passed to function
    if (missing(pattern)) {
        # Filter to only csv files
        snapshot_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "\\.csv$")]
    }
    else {
        # Filter to only csv files
        snapshot_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "\\.csv$")]
        # Match specified pattern
        snapshot_files <-
            snapshot_files[stringr::str_detect(snapshot_files, pattern)]
    }

    # Remove .csv from end of files
    snapshot_files <- stringr::str_remove(snapshot_files, "\\.csv")

    # Map function that reads in each dataset to the visit_files list and return list
    snapshot_files |>
        purrr::set_names() |>
        purrr::map(
            ENDIA::get_snapshot_table,
            include_table_name = FALSE,
            guess_max = Inf,
            .progress = TRUE
        )
}
