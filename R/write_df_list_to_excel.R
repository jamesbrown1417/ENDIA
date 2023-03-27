#' Write a List of DataFrames to Excel
#'
#' @param df_list A named list of dataframes to write out to an xlsx file
#' @param output_path The desired path of the excel workbook
#'
#' @return Nothing
#' @export
#'
#' @examples
write_df_list_to_excel <- function(df_list, output_path){
    sheet_names <- names(df_list)
    output_file <- output_path

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Add sheets to workbook
    purrr::map2(df_list,
         sheet_names,
         ~ {
             openxlsx::addWorksheet(wb, sheetName = .y)
             openxlsx::writeData(
                 wb,
                 .y,
                 .x,
                 startCol = 1,
                 startRow = 1,
             )
             openxlsx::setColWidths(wb, .y, cols = 1:ncol(.x), widths = "auto")
         })

    # Save workbook
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
}
