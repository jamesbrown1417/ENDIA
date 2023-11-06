#===============================================================================
# Function to get site summaries as of a certain date
#===============================================================================

get_site_summaries_at_date <- function() {
    #===============================================================================
    # Get relevant information
    #===============================================================================

    # Get infant population
    infant_population <-
        ENDIA::get_processed_data_table("infant_population", cols = "structured_participant_id")

    # Add follow up site
    infant_population <-
        infant_population |>
        dplyr::left_join(ENDIA::get_current_site())

    # Add birth dates
    infant_population <-
        infant_population |>
        dplyr::left_join(ENDIA::get_snapshot_table(
            "b1s",
            cols = c("structured_participant_id", "delivery_date")
        ))

    # Add Infant T1D Status
    infant_population <-
        infant_population |>
        dplyr::left_join(ENDIA::get_snapshot_table(
            "type1_diabetes_diagnoses",
            cols = c("structured_participant_id", "diagnosis_date")
        )) |>
        dplyr::rename(t1d_diagnosis_date = diagnosis_date)

    # Add withdrawal status
    infant_population <-
        infant_population |>
        dplyr::left_join(ENDIA::get_snapshot_table(
            "participant_withdrawals",
            cols = c("structured_participant_id", "withdrawn_date")
        )) |>
        dplyr::rename(withdrawal_date = withdrawn_date)

    # Add all visits
    all_participant_info <-
        ENDIA::get_visit_urls() |>
        dplyr::select(-visit_url) |>
        dplyr::left_join(infant_population, by = "structured_participant_id") |>
        dplyr::filter(structured_participant_id %in% infant_population$structured_participant_id) |>
        dplyr::filter(!is.na(visit_date)) |>
        dplyr::mutate(visit_date = lubridate::ymd(visit_date)) |>
        dplyr::mutate(current_date = lubridate::ymd(Sys.Date())) |>
        dplyr::arrange(structured_participant_id, desc(visit_number))

    #===============================================================================
    # Get inactive status
    #===============================================================================

    all_participant_inactive_status <-
        all_participant_info |>
        dplyr::group_by(structured_participant_id) |>
        dplyr::slice_head(n = 1) |>
        dplyr::mutate(days_since_last_visit = round(lubridate::time_length(
            lubridate::interval(visit_date, current_date), unit = "days"
        ), digits = 3))


    # Make an indicator variable for whether a participant is inactive, judged by two visits worth of days + 15 since latest previous visit:
    all_participant_inactive_status$inactive <- 0

    # Last visit was between v8 and 17 - 365 days is two visits worth
    all_participant_inactive_status$inactive <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) >= 13 &
                as.integer(all_participant_inactive_status$visit_number) <= 22 &
                all_participant_inactive_status$days_since_last_visit >= 380,
            1,
            all_participant_inactive_status$inactive
        )

    # Last visit v6 or less - two visits is 183 days
    all_participant_inactive_status$inactive <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) <= 11 &
                all_participant_inactive_status$days_since_last_visit >= 198,
            1,
            all_participant_inactive_status$inactive
        )

    # Last visit was v7 - two visits is 274 days
    all_participant_inactive_status$inactive <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) == 12 &
                all_participant_inactive_status$days_since_last_visit >= 289,
            1,
            all_participant_inactive_status$inactive
        )

    # Last visit was v18 - two visits is 548 days
    all_participant_inactive_status$inactive <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) == 23 &
                all_participant_inactive_status$days_since_last_visit >= 563,
            1,
            all_participant_inactive_status$inactive
        )

    # Last visit was v19 or greater - two visits is 730 days
    all_participant_inactive_status$inactive <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) >= 24 &
                all_participant_inactive_status$days_since_last_visit >= 745,
            1,
            all_participant_inactive_status$inactive
        )

    #===============================================================================
    # Overdue Status
    #===============================================================================

    all_participant_inactive_status$overdue <- 0

    # Last visit was between v8 and 17 - 365 days is two visits worth
    all_participant_inactive_status$overdue <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) >= 13 &
                as.integer(all_participant_inactive_status$visit_number) <= 22 &
                dplyr::between(
                    all_participant_inactive_status$days_since_last_visit,
                    182,
                    379
                ),
            1,
            all_participant_inactive_status$overdue
        )

    # Last visit v6 or less - two visits is 183 days
    all_participant_inactive_status$overdue <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) <= 11 &
                dplyr::between(
                    all_participant_inactive_status$days_since_last_visit,
                    91,
                    197
                ),
            1,
            all_participant_inactive_status$overdue
        )

    # Last visit was v7 - two visits is 274 days
    all_participant_inactive_status$overdue <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) == 12 &
                dplyr::between(
                    all_participant_inactive_status$days_since_last_visit,
                    137,
                    288
                ),
            1,
            all_participant_inactive_status$overdue
        )

    # Last visit was v18 - two visits is 548 days
    all_participant_inactive_status$overdue <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) == 23 &
                dplyr::between(
                    all_participant_inactive_status$days_since_last_visit,
                    274,
                    562
                ),
            1,
            all_participant_inactive_status$overdue
        )

    # Last visit was v19 or greater - two visits is 730 days
    all_participant_inactive_status$overdue <-
        ifelse(
            as.integer(all_participant_inactive_status$visit_number) >= 24 &
                dplyr::between(
                    all_participant_inactive_status$days_since_last_visit,
                    365,
                    744
                ),
            1,
            all_participant_inactive_status$overdue
        )

    ##%######################################################%##
    #                                                          #
    ####             Tidy up various scenarios              ####
    #                                                          #
    ##%######################################################%##

    # Withdrawn
    all_participant_inactive_status$withdrawn = 0

    all_participant_inactive_status$withdrawn <-
        ifelse(
            !is.na(all_participant_inactive_status$withdrawal_date),
            1,
            all_participant_inactive_status$withdrawn
        )

    # T1D Diagnosis
    all_participant_inactive_status$t1d_diagnosis = 0

    all_participant_inactive_status$t1d_diagnosis <-
        ifelse(
            !is.na(all_participant_inactive_status$t1d_diagnosis_date),
            1,
            all_participant_inactive_status$t1d_diagnosis
        )

    # Turned 10
    all_participant_inactive_status$turned_10 = 0

    all_participant_inactive_status$current_age <-
        round(lubridate::time_length(
            lubridate::interval(
                all_participant_inactive_status$delivery_date,
                all_participant_inactive_status$current_date
            ),
            unit = "years"
        ), digits = 3)

    all_participant_inactive_status$turned_10 <-
        ifelse(
            all_participant_inactive_status$current_age >= 10,
            1,
            all_participant_inactive_status$turned_10
        )

    # Tidy up overlapping scenarios-------------------------------------------------

    # If a participant is withdrawn or diagnosed with T1D then turned 10 is 0
    all_participant_inactive_status$turned_10 <-
        ifelse(
            all_participant_inactive_status$withdrawn == 1 |
                all_participant_inactive_status$t1d_diagnosis == 1,
            0,
            all_participant_inactive_status$turned_10
        )

    # If a participant is withdrawn, diagnosed with T1D or turned 10 then inactive is 0
    all_participant_inactive_status$inactive <-
        ifelse(
            all_participant_inactive_status$withdrawn == 1 |
                all_participant_inactive_status$t1d_diagnosis == 1 |
                all_participant_inactive_status$turned_10 == 1,
            0,
            all_participant_inactive_status$inactive
        )

    # If a participant is withdrawn, diagnosed with T1D or turned 10 then overdue is 0
    all_participant_inactive_status$overdue <-
        ifelse(
            all_participant_inactive_status$withdrawn == 1 |
                all_participant_inactive_status$t1d_diagnosis == 1 |
                all_participant_inactive_status$turned_10 == 1,
            0,
            all_participant_inactive_status$overdue
        )

    # Create an up to date category if everything else is zero
    all_participant_inactive_status$up_to_date <-
        ifelse(
            all_participant_inactive_status$withdrawn == 0 &
                all_participant_inactive_status$t1d_diagnosis == 0 &
                all_participant_inactive_status$turned_10 == 0 &
                all_participant_inactive_status$inactive == 0 &
                all_participant_inactive_status$overdue == 0,
            1,
            0
        )

    # Create a variable called status
    all_participant_inactive_status <-
        all_participant_inactive_status |>
        dplyr::mutate(
            status = dplyr::case_when(
                withdrawn == 1 ~ "Withdrawn",
                t1d_diagnosis == 1 ~ "T1D Diagnosis",
                turned_10 == 1 ~ "Turned 10",
                inactive == 1 ~ "Inactive",
                overdue == 1 ~ "Overdue",
                up_to_date == 1 ~ "Up to Date"
            )
        )

    # Make status an ordered factor
    all_participant_inactive_status$status <-
        factor(
            all_participant_inactive_status$status,
            levels = c(
                "Up to Date",
                "Overdue",
                "Inactive",
                "Withdrawn",
                "T1D Diagnosis",
                "Turned 10"
            ),
            ordered = TRUE
        )

    # Select only required variables
    all_participant_inactive_status <-
        all_participant_inactive_status |>
        dplyr::transmute(
            structured_participant_id,
            current_site,
            infant_dob = delivery_date,
            current_date,
            last_visit_date = visit_date,
            last_visit_number = visit_number,
            days_since_last_visit,
            withdrawal_date,
            T1D_diagnosis_date = t1d_diagnosis_date,
            turned_10_date = dplyr::if_else(status == "Turned 10", delivery_date + lubridate::years(10), NA),
            status
        ) |>
        dplyr::ungroup() |>
        dplyr::arrange(days_since_last_visit)

    return(all_participant_inactive_status)
}
