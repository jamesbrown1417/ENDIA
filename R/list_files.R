#' List File Names in ENDIA Data Locations
#'
#' @param location A string, either snapshot, processed or priority_variables, which is the location to search for file names
#'
#' @return A character vector of file names in the specified location
#' @export
#'
#' @examples
#' list_files(location = "snapshot")
list_files <- function(location = c("snapshot", "processed", "priority_variables")){
    # Get valid options list
    location_valid_options <-
        base::match.arg(
            location,
            choices = c(
                "snapshot",
                "processed",
                "priority_variables"),
            several.ok = FALSE
        )

    # Get path based on input
    if (location == "snapshot") path <- "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Snapshot/"
    if (location == "processed") path <- "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Data/Processed/"
    if (location == "priority_variables") path <- "S:/HealthSciences/SPRH/Paediatrics/Diabetes Research Group/Statistics and Data Management/Automated Reports/Extract Priority Variables/Output files/JB Current xlsx Files/Overall"

    # Get all files ending with csv or xls
    files_list <- base::list.files(path, pattern = "csv|xls")

    # Filter to remove open files starting with ~
    files_list <- files_list[stringr::str_detect(files_list, "^~", negate = TRUE)]

    # Remove file extension
    files_list <- stringr::str_remove(files_list, "\\..*$")

    # Remove overall
    files_list <- stringr::str_remove(files_list, "_overall$")

    # Return files list
    return(files_list)
}
