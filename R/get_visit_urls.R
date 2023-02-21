#' Get the ENDIA frontend registry URL for specified visits
#'
#' @param visits One of "all" (default), "pregnancy", "birth" or "postnatal".
#'
#' @return A tibble containing the structured participant ID, the visit number and the URL of the visit.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' get_visit_urls()
#' get_visit_urls(visits = "pregnancy")
#' get_visit_urls(visits = "birth")
#' get_visit_urls(visits = "postnatal")
get_visit_urls <- function(visits = "all") {
    # Path to the snapshot folder on the ENDIA shared drive
    snapshot_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"

    # List files in snapshot folder
    snapshot_files <- base::list.files(snapshot_path)

    # Get only visit files specified
    if (visits == "all") {
        visit_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "[tbv][0-9]{1,2}s\\.csv")]
    }
    else if (visits == "pregnancy") {
        visit_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "[t][0-9]{1,2}s\\.csv")]
    }
    else if (visits == "birth") {
        visit_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "[b][0-9]{1,2}s\\.csv")]
    }
    else if (visits == "postnatal") {
        visit_files <-
            snapshot_files[stringr::str_detect(snapshot_files, "[v][0-9]{1,2}s\\.csv")]
    }
    else {
        stop("Incorrect visits argument")
    }

    # Remove .csv from end of files
    visit_files <- stringr::str_remove(visit_files, "\\.csv")

    # Map function that reads in each dataset to the visit_files list
    visits_list <-
        purrr::map(
            visit_files,
            ENDIA::get_snapshot_table,
            cols = c("structured_participant_id", "id", "participant_id"),
            include_table_name = TRUE,
            .progress = TRUE
        )

    # Bind rows together and return
    all_visits <- dplyr::bind_rows(visits_list)

    # Remove s at the end of table_name
    all_visits$table_name <-
        stringr::str_remove(all_visits$table_name, "s$")

    # Create visit_url variable
    all_visits$visit_url <-
        glue::glue(
            "https://registry.endia.org.au/participants/{all_visits$participant_id}/{all_visits$table_name}/{all_visits$id}/edit"
        )

    # Select only needed variables
    all_visits <-
    all_visits |>
        dplyr::select(.data$structured_participant_id,
                      visit_number = .data$table_name,
                      .data$visit_url)

    # Make visit number an ordered factor
    all_visits$visit_number <- factor(
        all_visits$visit_number,
        levels =
            c(
                "t1",
                "t2",
                "t3",
                "b1",
                "b2",
                "v1",
                "v2",
                "v3",
                "v4",
                "v5",
                "v6",
                "v7",
                "v8",
                "v9",
                "v10",
                "v11",
                "v12",
                "v13",
                "v14",
                "v15",
                "v16",
                "v17",
                "v18",
                "v19",
                "v20",
                "v21",
                "v22",
                "v23",
                "v24"
            ),
        ordered = TRUE
    )

    # Order the tibble by structured participant ID and visit number and return
    all_visits |> dplyr::arrange(.data$structured_participant_id, .data$visit_number)
}
