get_visit_urls <- function(visits = "all") {
    # Path to the snapshot folder on the ENDIA shared drive
    snapshot_path = "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"

    # List files in snapshot folder
    snapshot_files <- base::list.files(snapshot_path)

    # Get only visit files
    visit_files <- snapshot_files[stringr::str_detect(snapshot_files, "[tbv][0-9]{1,2}s\\.csv")]

    # Remove .csv from end of files
    visit_files <- stringr::str_remove(visit_files, "\\.csv")

    # Map an anonymous function that reads in each dataset to the visit_files list
    visits_list <-
        purrr::map(
            visit_files,
            ENDIA::get_snapshot_table,
            cols = c("structured_participant_id", "id", "participant_id"),
            include_table_name = TRUE
        )

    # Bind rows together and return
    all_visits <- dplyr::bind_rows(visits_list)

    # Remove s at the end of table_name
    all_visits$table_name <- stringr::str_remove(all_visits$table_name, "s$")

    # Create visit_url variable
    all_visits$visit_url <- glue::glue("https://registry.endia.org.au/participants/{all_visits$participant_id}/{all_visits$table_name}/{all_visits$id}/edit")

    # Select only needed variables and return table
    dplyr::select(all_visits, structured_participant_id, visit_number = table_name, visit_url)
}
