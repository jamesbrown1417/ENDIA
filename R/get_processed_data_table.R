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
get_processed_data_table <- function(table_name, cols, include_table_name = FALSE, sheet = NULL) {
    # Path to the processed data folder on the ENDIA shared drive
    processed_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Processed/"

    # list files matching csv or xlsx in processed path
    file_list <- list.files(processed_path, pattern = "(\\.csv)|(\\.xlsx)")

    # Match table name with list
    format_string <- file_list[stringr::str_detect(file_list, table_name)][1]

    # Identify the file format
    file_format <- tools::file_ext(format_string)

    if(file_format == "csv") {
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
    } else if(file_format == "xlsx") {
        # combine file name and path to get location of desired file to read in
        file_path = base::paste0(processed_path, table_name, ".xlsx")

        # If cols argument supplied, return a warning
        if (!missing(cols)) {
            warning("Column selection is not supported for xlsx files.")
        }

        # Read the excel file
        output_data <- readxl::read_excel(file_path, sheet = sheet)

    } else {
        stop("Unsupported file format!")
    }

    # If table_name argument is TRUE add table name variable to output
    if (include_table_name) {
        output_data$table_name <- paste0(table_name)
    }

    # Return the table
    return(output_data)
}
