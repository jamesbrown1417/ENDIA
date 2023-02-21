#' Get person IDs for each structured participant ID
#'
#' @param who a character vector: one or more of gestational_mother, biological_mother, infant, father. All IDs are returned by default.
#'
#' @return A tibble with each structured participant ID and the requested person IDs.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' get_participant_ids(who = c("gestational_mother", "biological_mother"))
get_participant_ids <-
    function(who = c("gestational_mother", "biological_mother", "infant", "father")) {
        # Get valid options list
        who_valid_options <-
            base::match.arg(
                who,
                choices = c(
                    "gestational_mother",
                    "biological_mother",
                    "infant",
                    "father"
                ),
                several.ok = TRUE
            )
        # If length of valid options is different to input return an error
        if (base::length(who_valid_options) != base::length(who)) {
            problem_variables <- base::setdiff(who, who_valid_options)
            problem_variables_string <-
                base::paste(problem_variables, collapse = ", ")
            error_message = base::paste(
                'One or more incorrect options provided to who:',
                problem_variables_string,
                '\n who should be one of gestational_mother, biological_mother, infant, father'
            )
            base::stop(error_message)
        }

        # Gestational Mother IDs
        if ("gestational_mother" %in% who_valid_options) {
            # Get table from processed data folder
            gestational_mother_ids <-
                ENDIA::get_processed_data_table("mother_ids")

            # Get only gestational mothers and get structured participant id and gestational mother id
            gestational_mother_ids <-
                gestational_mother_ids |>
                dplyr::filter(.data$gestational_mother == 1) |>
                dplyr::select(.data$structured_participant_id,
                              gestational_mother_id = .data$mother_id)
        }
        # If not specified return NULL
        else {
            gestational_mother_ids <- NULL
        }

        # Biological Mother IDs
        if ("biological_mother" %in% who_valid_options) {
            # Get table from processed data folder
            biological_mother_ids <-
                ENDIA::get_processed_data_table("mother_ids")

            # Get only biological mothers and get structured participant id and biological mother id
            biological_mother_ids <-
                biological_mother_ids |>
                dplyr::filter(.data$biological_mother == 1) |>
                dplyr::select(.data$structured_participant_id,
                              biological_mother_id = .data$mother_id)
        }
        # If not specified return NULL
        else {
            biological_mother_ids <- NULL
        }

        # Infant IDs
        if ("infant" %in% who_valid_options) {
            # Get table from processed data folder
            infant_ids <-
                ENDIA::get_processed_data_table("infant_ids")

        }
        # If not specified return NULL
        else {
            infant_ids <- NULL
        }

        # Father IDs
        if ("father" %in% who_valid_options) {
            # Get table from processed data folder
            father_ids <-
                ENDIA::get_processed_data_table("father_ids")

        # Select structured participant id and father id
        father_ids <-
            father_ids |>
            dplyr::select(.data$structured_participant_id, .data$father_id)
        }
        # If not specified return NULL
        else {
            father_ids <- NULL
        }

        # Get a list of the ID tables
        table_list <-
            base::list(
                "gestational_mother_ids" = gestational_mother_ids,
                "biological_mother_ids" = biological_mother_ids,
                "infant_ids" = infant_ids,
                "father_ids" = father_ids
            )

        # Get list of IDs requested
        tables_to_keep <- base::paste0(who_valid_options, "_ids")

        # Filter list
        table_list <- table_list[names(table_list) %in% tables_to_keep]

        # Bind cols in table list to get output
        purrr::reduce(table_list, .f = dplyr::left_join, by = "structured_participant_id")
    }
