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

    # If no visits are selected, return all visits
    if (length(visits_requested) == 0 | is.null(visits_requested)) {
        visits_requested <-
            ENDIA::fix_visit_numbers(c(
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
        ), how = "both")
    }

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
            dplyr::select(dplyr::matches("_id$|pregnancy_number|^visit_"),
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
        purrr::map(list_of_requested_dataframes_overall, filter_visits)

    # Separate maternal priority variables into static and dynamic variables if it exists
    if ("maternal_priority_variables" %in% names(list_of_requested_dataframes_overall)) {

    list_of_static_variables <-
        c("maternal_dob",
          "maternal_birth_country",
          "maternal_education",
          "maternal_suburb",
          "maternal_postcode",
          "IRSAD_Decile",
          "IRSAD_Percentile",
          "IRSD_Decile",
          "IRSD_Percentile",
          "monash_remoteness_category",
          "ABS_remoteness_classification",
          "maternal_suburb_enrol",
          "maternal_postcode_enrol",
          "IRSAD_Decile_enrol",
          "IRSAD_Percentile_enrol",
          "IRSD_Decile_enrol",
          "IRSD_Percentile_enrol",
          "monash_remoteness_category_enrol",
          "ABS_remoteness_classification_enrol",
          "maternal_height",
          "pre_pregnancy_weight",
          "pre_preg_bmi",
          "gestational_weight_gain",
          "GA_at_gestational_weight_gain",
          "maternal_preconception_smoking",
          "maternal_smoking_during_preg_any",
          "confirmed_due_date",
          "calculated_conception_date",
          "maternal_age_at_conception",
          "assisted_conception",
          "assisted_conception_categories",
          "maternal_med_history_t2d",
          "maternal_med_history_gestational_diabetes",
          "maternal_med_history_coeliac",
          "maternal_med_history_kidney_disease",
          "maternal_med_history_eye_disease",
          "pre_existing_thyroid_problems",
          "pre_existing_autoimmune_disorder",
          "gravida",
          "parity",
          "nulliparous",
          "prior_pregnancies_lt_20_weeks",
          "prior_pregnancies_lt_37_weeks",
          "prior_pregnancies_gt_37_weeks",
          "prior_living_biological_children",
          "prev_perinatal_deaths",
          "perinatal_death_category",
          "prev_multiple_births",
          "prev_births_GA_weeks_1",
          "prev_births_prev_birth_date_1",
          "prev_births_mode_of_birth_1",
          "prev_births_GA_weeks_2",
          "prev_births_prev_birth_date_2",
          "prev_births_mode_of_birth_2",
          "prev_births_GA_weeks_3",
          "prev_births_prev_birth_date_3",
          "prev_births_mode_of_birth_3",
          "prev_births_GA_weeks_4",
          "prev_births_prev_birth_date_4",
          "prev_births_mode_of_birth_4",
          "prev_births_GA_weeks_5",
          "prev_births_prev_birth_date_5",
          "prev_births_mode_of_birth_5",
          "prev_births_GA_weeks_6",
          "prev_births_prev_birth_date_6",
          "prev_births_mode_of_birth_6",
          "prev_births_GA_weeks_7",
          "prev_births_prev_birth_date_7",
          "prev_births_mode_of_birth_7",
          "prev_births_GA_weeks_8",
          "prev_births_prev_birth_date_8",
          "prev_births_mode_of_birth_8",
          "prev_births_GA_weeks_9",
          "prev_births_prev_birth_date_9",
          "prev_births_mode_of_birth_9",
          "t1d_management_mdi",
          "t1d_management_pump",
          "t1d_management_mdi_pump",
          "t1d_management_oral_metformin",
          "t1d_management_unknown",
          "recapture_diabetes_management_cgm_at_conception",
          "recapture_diabetes_management_cgm_commenced_during_pregnancy",
          "recapture_diabetes_management_cgm_gestation_started",
          "any_GDM",
          "GA_at_any_GDM",
          "any_PET",
          "GA_at_any_PET",
          "any_antibiotics_use_in_pregnancy",
          "proband_mother",
          "proband_sibling",
          "proband_donor_oocyte",
          "proband_mother_diagnosis_date",
          "proband_sibling_1_diagnosis_date",
          "proband_sibling_2_diagnosis_date",
          "maternal_HLA_6DR_gestational",
          "maternal_HLA_1_gestational",
          "maternal_HLA_2_gestational",
          "maternal_IL2RA_gestational",
          "maternal_HLA_6DR_biological",
          "maternal_HLA_1_biological",
          "maternal_HLA_2_biological",
          "maternal_IL2RA_biological",
          "biological_mother_GRS1"
          )

    # Get static maternal variables
    static_maternal <-
        list_of_requested_dataframes_overall$maternal_priority_variables |>
        dplyr::select(structured_participant_id,
                      gestational_mother_id,
                      biological_mother_id,
                      infant_id,
                      endia_pregnancy_number_gestational,
                      endia_pregnancy_number_biological,
                      dplyr::any_of(list_of_static_variables))

    static_maternal <-
        static_maternal |>
        dplyr::mutate(missing_count = base::rowSums(base::is.na(static_maternal))) |>
        dplyr::arrange(structured_participant_id, (missing_count)) |>
        dplyr::group_by(structured_participant_id) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup() |>
        dplyr::select(-missing_count)

    # Get dynamic maternal variables
    dynamic_maternal <-
        list_of_requested_dataframes_overall$maternal_priority_variables |>
        dplyr::select(-dplyr::any_of(list_of_static_variables)) |>
        dplyr::select(dplyr::any_of(c(
            "structured_participant_id",
            "gestational_mother_id",
            "endia_pregnancy_number_gestational",
            "biological_mother_id",
            "endia_pregnancy_number_biological",
            "infant_id",
            "maternal_dob",
            "maternal_birth_country",
            "maternal_education",
            "maternal_suburb",
            "maternal_suburb_enrol",
            "maternal_postcode",
            "maternal_postcode_enrol",
            "IRSAD_Decile",
            "IRSAD_Decile_enrol",
            "IRSAD_Percentile",
            "IRSAD_Percentile_enrol",
            "IRSD_Decile",
            "IRSD_Decile_enrol",
            "IRSD_Percentile",
            "IRSD_Percentile_enrol",
            "monash_remoteness_category",
            "monash_remoteness_category_enrol",
            "ABS_remoteness_classification",
            "ABS_remoteness_classification_enrol",
            "confirmed_due_date",
            "calculated_conception_date",
            "maternal_age_at_conception",
            "assisted_conception",
            "assisted_conception_categories",
            "pre_pregnancy_weight",
            "maternal_height",
            "pre_preg_bmi",
            "gestational_weight_gain",
            "GA_at_gestational_weight_gain",
            "visit_number",
            "visit_date",
            "GA_at_visit",
            "pregnancy_weight",
            "vit_D_deficiency",
            "ga_at_vit_d_deficiency_onset",
            "iron_deficiency",
            "ga_at_iron_deficiency_onset",
            "maternal_med_history_t2d",
            "maternal_med_history_gestational_diabetes",
            "maternal_med_history_coeliac",
            "maternal_med_history_kidney_disease",
            "maternal_med_history_eye_disease",
            "pre_existing_thyroid_problems",
            "pre_existing_autoimmune_disorder",
            "gravida",
            "parity",
            "nulliparous",
            "prior_pregnancies_lt_20_weeks",
            "prior_pregnancies_lt_37_weeks",
            "prior_pregnancies_gt_37_weeks",
            "prior_living_biological_children",
            "prev_perinatal_deaths",
            "perinatal_death_category",
            "prev_multiple_births",
            "prev_births_GA_weeks_1",
            "prev_births_prev_birth_date_1",
            "prev_births_mode_of_birth_1",
            "prev_births_GA_weeks_2",
            "prev_births_prev_birth_date_2",
            "prev_births_mode_of_birth_2",
            "prev_births_GA_weeks_3",
            "prev_births_prev_birth_date_3",
            "prev_births_mode_of_birth_3",
            "prev_births_GA_weeks_4",
            "prev_births_prev_birth_date_4",
            "prev_births_mode_of_birth_4",
            "prev_births_GA_weeks_5",
            "prev_births_prev_birth_date_5",
            "prev_births_mode_of_birth_5",
            "prev_births_GA_weeks_6",
            "prev_births_prev_birth_date_6",
            "prev_births_mode_of_birth_6",
            "prev_births_GA_weeks_7",
            "prev_births_prev_birth_date_7",
            "prev_births_mode_of_birth_7",
            "prev_births_GA_weeks_8",
            "prev_births_prev_birth_date_8",
            "prev_births_mode_of_birth_8",
            "prev_births_GA_weeks_9",
            "prev_births_prev_birth_date_9",
            "prev_births_mode_of_birth_9",
            "t1d_management_mdi",
            "t1d_management_pump",
            "t1d_management_mdi_pump",
            "t1d_management_oral_metformin",
            "t1d_management_unknown",
            "recapture_diabetes_management_cgm_at_conception",
            "recapture_diabetes_management_cgm_commenced_during_pregnancy",
            "recapture_diabetes_management_cgm_gestation_started",
            "gdm",
            "ga_at_gdm_onset",
            "any_GDM",
            "GA_at_any_GDM",
            "gdm_management_diet",
            "gdm_management_oral_meds",
            "gdm_management_insulin",
            "gdm_management_unknown",
            "PET",
            "any_PET",
            "GA_at_any_PET",
            "pre_existing_HT",
            "pre_existing_HT_resulting_in_PET",
            "GA_at_pre_existing_HT_resulting_in_PET",
            "gestational_HT",
            "GA_at_gestational_HT",
            "mild_to_moderate_PET",
            "GA_at_mild_to_moderate_PET",
            "severe_PET",
            "GA_at_severe_PET",
            "eclampsia",
            "GA_at_eclampsia",
            "HELLP_syndrome",
            "GA_at_HELLP_syndrome",
            "GBS",
            "urinary_infection",
            "GA_at_urinary_infection",
            "bacterial_vaginosis",
            "GA_at_bacterial_vaginosis",
            "candida",
            "GA_at_candida",
            "genitourinary_infection_diagnosis_details",
            "other_genitourinary_infection_1",
            "other_genitourinary_infection_category_1",
            "other_genitourinary_infection_details_1",
            "GA_at_other_genitourinary_infection_1",
            "other_genitourinary_infection_2",
            "other_genitourinary_infection_category_2",
            "other_genitourinary_infection_details_2",
            "GA_at_other_genitourinary_infection_2",
            "other_genitourinary_infection_3",
            "other_genitourinary_infection_category_3",
            "other_genitourinary_infection_details_3",
            "GA_at_other_genitourinary_infection_3",
            "cellulitis",
            "GA_at_cellulitis",
            "gastroenteritis",
            "GA_at_gastroenteritis",
            "respiratory_tract_infection",
            "GA_at_respiratory_tract_infection",
            "infection_diagnosis_details",
            "other_infection_1",
            "other_infection_category_1",
            "other_infection_details_1",
            "GA_at_other_infection_1",
            "other_infection_2",
            "other_infection_category_2",
            "other_infection_details_2",
            "GA_at_other_infection_2",
            "other_infection_3",
            "other_infection_category_3",
            "other_infection_details_3",
            "GA_at_other_infection_3",
            "other_reason_antibiotic_1",
            "other_reason_antibiotic_details_1",
            "GA_at_other_reason_antibiotic_1",
            "antibiotics_use_in_pregnancy",
            "antibiotics_use_in_pregnancy_unknown_date",
            "any_antibiotics_use_in_pregnancy",
            "maternal_antibiotics_use_in_pregnancy_questionnaire",
            "maternal_preconception_smoking",
            "lq_completion_date",
            "GA_at_lq_completion",
            "lq_not_collected",
            "maternal_smoking",
            "household_smoking",
            "maternal_smoking_during_preg_any",
            "adults_in_home",
            "children_in_home",
            "furred_pets",
            "Alcohol",
            "Energy",
            "Fat",
            "Protein",
            "Carbohydrate",
            "Fibre",
            "dietary_pattern_score_PC1",
            "dietary_pattern_score_PC2",
            "ppaq_completion_date",
            "ppaq_mets_per_week",
            "proband_mother",
            "proband_sibling",
            "proband_donor_oocyte",
            "proband_mother_diagnosis_date",
            "proband_sibling_1_diagnosis_date",
            "proband_sibling_2_diagnosis_date",
            "maternal_HLA_6DR_gestational",
            "maternal_HLA_1_gestational",
            "maternal_HLA_2_gestational",
            "maternal_IL2RA_gestational",
            "maternal_HLA_6DR_biological",
            "maternal_HLA_1_biological",
            "maternal_HLA_2_biological",
            "maternal_IL2RA_biological",
            "biological_mother_GRS1"
        )))

    # Add to list
    list_of_requested_dataframes_overall$mat_priority_vars_static <- static_maternal
    list_of_requested_dataframes_overall$mat_priority_vars_dynamic <- dynamic_maternal

    # Remove the original maternal priority variables
    list_of_requested_dataframes_overall$maternal_priority_variables <- NULL

    # Reorder the list
    num_elements <- base::length(list_of_requested_dataframes_overall)
    new_order <- c((num_elements-1):num_elements, 1:(num_elements-2))
    list_of_requested_dataframes_overall <- list_of_requested_dataframes_overall[new_order]
    }

    # Return List---------------------------------------------------------------
    return(list_of_requested_dataframes_overall)
}
