#' Get information about each ENDIA participant's latest visit
#'
#' @return A tibble containing the ID, latest visit number, date and url, and days since the latest visit
#' @export
#' @importFrom rlang .data
#' @examples
#' get_latest_visit()
get_latest_visit <- function() {
    # Get list of all visits and visit dates
    visit_list <-
        ENDIA::get_snapshot_list(
            pattern = "^[btv][0-9]{1,2}s$",
            cols = c("structured_participant_id", "participant_id", "id", "visit_date"),
            include_table_name = TRUE
        )

    # Collapse into a single dataframe
    visit_df <- purrr::reduce(visit_list, dplyr::bind_rows)

    # Fix visit names, Remove missing visit_dates
    visit_df <-
        visit_df |>
        dplyr::transmute(structured_participant_id = .data$structured_participant_id,
                  latest_visit_number = ENDIA::fix_visit_numbers(.data$table_name, how = "factor"),
                  latest_visit_url =  glue::glue("https://registry.endia.org.au/participants/{.data$participant_id}/{.data$latest_visit_number}/{.data$id}/edit"),
                  latest_visit_date = .data$visit_date,
                  date_today = lubridate::today()) |>
        dplyr::filter(!is.na(.data$latest_visit_date))

    # Keep only latest visit
    latest_visit_df <-
        visit_df |>
        dplyr::group_by(.data$structured_participant_id) |>
        dplyr::filter(.data$latest_visit_number == base::max(.data$latest_visit_number, na.rm = TRUE)) |>
        dplyr::ungroup()

    # Calculate days since latest visit, order by most recent visit, remove MEG and return tibble
        latest_visit_df |>
        dplyr::mutate(days_since_latest_visit = .data$date_today - .data$latest_visit_date) |>
        dplyr::arrange(.data$days_since_latest_visit) |>
        dplyr::filter(stringr::str_detect(.data$structured_participant_id, "MEG", negate = TRUE))
}
