#' Load priority variables table
#'
#' @param table_name Name of the priority variables table desired
#' @param participant_subset Name of the participant subset to return results for (defaults to Overall)
#'
#' @return A tibble containing the dataset passed to the table_name argument.
#'
#' @export
#'
#' @examples
#' get_priority_variables_table("infant_priority_variables")
#' get_priority_variables_table("maternal_samples", participant_subset = "pp017")
get_priority_variables_table <-
    function(table_name, participant_subset = "Overall") {
        # %notin% function
        `%notin%` = base::Negate(`%in%`)

        # Path to the priority variables output data folder on the ENDIA shared drive
        priority_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Automated Reports/Extract Priority Variables/Output files/JB Current xlsx Files/"

        # Make participant subset the right case
        if (stringr::str_detect(participant_subset,
                                stringr::regex("^overall$", ignore_case = TRUE))) {
            participant_subset = stringr::str_to_title(participant_subset)
        }
        else {
            participant_subset = stringr::str_to_upper(participant_subset)
        }

        # If participant subset not in the list, return an error
        if (participant_subset %notin% c("Overall", "NCC3","PP017", "PP019", "PP027", "PP041", "PP044")) {base::stop("Invalid participant subset")}

        # combine file name and path to get location of desired file to read in
        file_path = paste0(
            priority_path,
            participant_subset,
            "/",
            table_name,
            "_",
            stringr::str_to_lower(participant_subset),
            ".xlsx"
        )

        # Read in specified file from path
        output_data <- readxl::read_excel(file_path,
                                          guess_max = 5000,
                                          progress = FALSE)

        # Return the table
        return(output_data)
    }
