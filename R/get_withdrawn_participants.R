#' Get withdrawn patients, either T1D diagnoses or explicit withdrawals.
#'
#' @return A tibble of withdrawn ENDIA participants.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
get_withdrawn_participants <- function(){
    # Read in tables
    withdrawals = ENDIA::get_snapshot_table("participant_withdrawals")
    t1d_diagnoses = ENDIA::get_snapshot_table("type1_diabetes_diagnoses")

    # Select only relevant columns
    withdrawals <-
    withdrawals |>
        dplyr::transmute(.data$structured_participant_id,
                         withdrawal_date = .data$withdrawn_date,
                         type = "Withdrawal")

    t1d_diagnoses <-
    t1d_diagnoses |>
        dplyr::transmute(.data$structured_participant_id,
                         withdrawal_date = .data$diagnosis_date,
                         type = "T1D Diagnosis")

    # Bind rows together and return tibble
        dplyr::bind_rows(withdrawals, t1d_diagnoses) |>
        dplyr::arrange(.data$withdrawal_date)
}
