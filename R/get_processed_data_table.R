#' Load processed data
#'
#' @param table_name Name of the processed data table desired.
#' @param cols An optional character vector of columns to be included in the tibble.
#' @param include_table_name Logical: if TRUE, include the table name as a column in the output tibble.
#'
#' @return A tibble containing the dataset passed to the table_name argument.
#' @export
#'
#' @examples
#' get_processed_data_table("mother_ids")
get_processed_data_table <-
    function(table_name, cols, include_table_name = FALSE) {
        # Path to the processed data folder on the ENDIA shared drive
        processed_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Processed/"

        # combine file name and path to get location of desired file to read in
        file_path = base::paste0(processed_path, table_name, ".csv")

        # Read in specified file from path
        # If no cols supplied, read in all
        if (missing(cols)) {
            output_data <- readr::read_csv(file_path,
                                           progress = FALSE,
                                           show_col_types = FALSE)
        }

        # If cols supplied, read only those cols
        else {
            output_data <- readr::read_csv(
                file_path,
                col_select = cols,
                progress = FALSE,
                show_col_types = FALSE
            )
        }

        # If table_name argument is TRUE add table name variable to output
        if (include_table_name) {
            output_data$table_name <- paste0(table_name)
        }

        # Return the table
        return(output_data)
    }
