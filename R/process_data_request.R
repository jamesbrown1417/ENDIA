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
    visits_selected <-
        readxl::read_excel(data_request_filepath, sheet = "Visit Selection")
    sample_variables <-
        readxl::read_excel(data_request_filepath, sheet = "Data Dictionary Samples")

    # Get requested variables---------------------------------------------------

    # Visits
    visits_requested <-
        visits_selected |>
        dplyr::filter(!is.na(`Requested (X)`)) |>
        dplyr::pull(Visit) |>
        ENDIA::fix_visit_numbers(how = "both")

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
          list_of_requested_dataframes_sample)

    # Create function to filter visits based on selected range
    filter_visits <- function(df) {
        if ("visit_number" %in% names(df)) {
            df |>
                dplyr::mutate(visit_number = ENDIA::fix_visit_numbers(visit_number, how = "both")) |>
                dplyr::filter(visit_number %in% visits_requested) |>
                dplyr::arrange(structured_participant_id, visit_number)
        }
        else {
            df |> dplyr::arrange(structured_participant_id)
        }
    }

    # Apply function to filter based on visit requested
    list_of_requested_dataframes_overall <-
        map(list_of_requested_dataframes_overall, filter_visits)

    # Return List---------------------------------------------------------------
    return(list_of_requested_dataframes_overall)
}
