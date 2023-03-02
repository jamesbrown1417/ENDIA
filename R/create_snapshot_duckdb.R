#' Load snapshot files into an in memory duckdb database
#'
#' @return An in memory duckdb database.
#' @export
#'
#' @examples
#' create_snapshot_duckdb()
create_snapshot_duckdb <- function() {
    # Initialise in memory DB
    con <-
        DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:", read_only = FALSE)

    # Get the snapshot path
    snapshot_path <-
        "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"

    # Get list of files in the snapshot path
    snapshot_files <- list.files(snapshot_path, pattern = ".csv$")

    # Remove unwanted files from snapshot
    snapshot_files <-
        snapshot_files[stringr::str_detect(snapshot_files, "List of Tables", negate = TRUE)]

    # For loop to load files into duckdb database
    for (file in snapshot_files) {
        name = stringr::str_remove(file, "\\.csv")
        filepath = paste0(snapshot_path, file)
        command = paste0("CREATE TABLE ",
                         name,
                         " AS SELECT * FROM read_csv_auto('",
                         filepath,
                         "');")
        DBI::dbExecute(con, command)
    }
    return(con)
}
