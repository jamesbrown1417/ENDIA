#' Load Snapshot Data
#'
#' @param table_name A string indicating the name of the csv table to be read in.
#'
#' @param cols An optional character vector of columns to be included in the tibble.
#'
#'@param include_table_name Logical: if TRUE, include the table name as a column in the output tibble.
#'
#' @return A tibble containing the dataset passed to the table_name argument.
#' @export
#'
#' @examples
#' get_snapshot_table("b1s")
#' get_snapshot_table("mothers")
get_snapshot_table <-
    function(table_name, cols, include_table_name = FALSE) {
        # Path to the snapshot folder on the ENDIA shared drive
        snapshot_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"

        # combine file name and path to get location of desired file to read in
        file_path = paste0(snapshot_path, table_name, ".csv")

        # Read in specified file from path
        # If no cols supplied, read in all
        if (missing(cols)) {
            output_data <-
                readr::read_csv(file_path,
                                progress = TRUE,
                                show_col_types = TRUE)
        }

        # If cols supplied, read only those cols
        else {
            output_data <- readr::read_csv(
                file_path,
                col_select = cols,
                progress = TRUE,
                show_col_types = TRUE
            )
        }

        # If table_name argument is TRUE add table name variable to output
        if (include_table_name) {
            output_data$table_name <- paste0(table_name)
        }

        # Return the table
        return(output_data)
    }
