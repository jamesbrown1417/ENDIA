#' Process A Completed Data Request Form
#'
#' @param data_request_filepath The filepath of a completed data request form
#'
#' @return A list of extracted priority variable spreadsheets
#' @export
#'
#' @examples
process_data_request <- function(data_request_filepath) {
    # Load in data request completed forms
    clinical_variables <-
        readxl::read_excel(data_request_filepath, sheet = "Data Dictionary Clinical")
    sample_variables <-
        readxl::read_excel(data_request_filepath, sheet = "Data Dictionary Samples")
    visits_selected <-
        readxl::read_excel(data_request_filepath, sheet = "Visit Selection")

    # Get requested variables---------------------------------------------------

    # Clinical
    clinical_variables_requested <-
        clinical_variables |>
        dplyr::filter(!is.na(`Requested (X)`)) |>
        dplyr::select(var_name = `Variable Name`, Spreadsheet) |>
        dplyr::filter(
            !var_name %in% c(
                "structured_participant_id",
                "biological_mother_id",
                "gestational_mother_id",
                "infant_id",
                "father_id"
            )
        ) # These are selected by default so no need to select here

    # Samples
    sample_variables_requested <-
        sample_variables |>
        dplyr::filter(!is.na(`Requested (X)`)) |>
        dplyr::select(var_name = `Variable Name`, Spreadsheet) |>
        dplyr::filter(
            !var_name %in% c(
                "structured_participant_id",
                "biological_mother_id",
                "gestational_mother_id",
                "infant_id",
                "father_id"
            )
        ) # These are selected by default so no need to select here

    # Max Visit Number
    max_visit_number <-
        visits_selected |>
        dplyr::mutate(Visit = ENDIA::fix_visit_numbers(Visit, how = "both")) |>
        dplyr::filter(!is.na(`Requested (X)`)) |>
        dplyr::pull(Visit) |>
        max()

    # Get all tables with a variable requested from it--------------------------

    # Clinical
    clinical_spreadsheets <-
        clinical_variables |>
        dplyr::filter(!is.na(`Requested (X)`)) |>
        dplyr::distinct(Spreadsheet) |>
        dplyr::pull(Spreadsheet)

    # Samples
    sample_spreadsheets <-
        sample_variables |>
        dplyr::filter(!is.na(`Requested (X)`)) |>
        dplyr::distinct(Spreadsheet) |>
        dplyr::pull(Spreadsheet)

    # Create a function to map over tables to extract variables-----------------
    get_vars <- function(df, vars_requested) {
        # Filter to vars requested for table
        requested_variables <-
            vars_requested |>
            dplyr::filter(df == Spreadsheet) |>
            dplyr::pull(var_name)

        # If input df is from a multi-sheet table, handle manually
        if (df == "maternal_vitamin_d_results") {
            table <-
                ENDIA::get_priority_variables_table("maternal_vitamin_d_and_HbA1c_results", sheet = "Maternal Vitamin D Results")
        }

        else if (df == "maternal_hba1c_results") {
            table <-
                ENDIA::get_priority_variables_table("maternal_vitamin_d_and_HbA1c_results", sheet = "Maternal HbA1c Results")
        }

        else {
            # Otherwise read in the table normally
            table <- ENDIA::get_priority_variables_table(df)
        }

        # Select variables
        table <-
            table |>
            dplyr::select(dplyr::matches("_id$|^visit_"),
                          dplyr::all_of(requested_variables))

        # Return Table
        return(table)
    }

    # Map function to read variables to list of spreadsheets--------------------

    # Clinical
    list_of_requested_dataframes_clinical <-
        purrr::map(clinical_spreadsheets,
                   get_vars,
                   clinical_variables_requested) |>
        purrr::set_names(clinical_spreadsheets)

    # Samples
    list_of_requested_dataframes_sample <-
        purrr::map(sample_spreadsheets,
                   get_vars,
                   sample_variables_requested) |>
        purrr::set_names(sample_spreadsheets)

    # List of requested variables combined--------------------------------------
    list_of_requested_dataframes_overall <-
        c(list_of_requested_dataframes_clinical,
          list_of_requested_dataframes_sample,
          max_visit_number)

    # Return List---------------------------------------------------------------
    return(list_of_requested_dataframes_overall)
}
