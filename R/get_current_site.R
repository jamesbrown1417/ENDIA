#' Get each participants current follow-up site
#'
#' @return A tibble with structured participant ID and
#' @export
#' @importFrom rlang .data
#' @examples
#' get_current_site()
get_current_site <- function(){
    # Read in participants table
    participants <- ENDIA::get_snapshot_table("participants")

    # Remove MEG participants
    participants <- participants[-stringr::str_which(participants$structured_participant_id, "MEG"),]

    # Get current Site variable
    participants$current_site <- participants$follow_up_site
    participants$current_site <- base::factor(participants$current_site)
    participants$original_site = base::factor(stringr::str_extract(participants$structured_participant_id, "[A-Z]{2,3}-[A-Z]{2,8}-[A-Z]{2,8}"))
    participants$transferred <- base::ifelse(participants$original_site == participants$follow_up_site | is.na(participants$follow_up_site), 0, 1)
    participants$current_site <- dplyr::if_else(participants$transferred == 0, participants$original_site, participants$current_site)

    # Return structured participant ID, current site and original site variables
    participants |>
        dplyr::select(.data$structured_participant_id, .data$current_site)
}
