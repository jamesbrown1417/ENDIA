#' Fix visit number column
#'
#' @param visit_col A visit number vector containing the default values of visit from the snapshot tables e.g. v1, v2, ...
#' @param how A string indicating how to fix the visit vector
#'  Options are:
#'      factor - which will create an ordered factor
#'      zero_pad - left pad the visit number with zeros to ensure consistent length (e.g. v1 becomes v01)
#'      both - Apply both factor and zero_pad options.
#'
#' @return A vector containing the transformed visit number vector.
#' @export
#'
#' @examples
#' visit_number_col <- c("t1", "v1", "v2", "v3")
#' fix_visit_numbers(visit_number_col, how = "factor")
fix_visit_numbers <- function(visit_col, how = "both") {
    # %notin% function
    `%notin%` = base::Negate(`%in%`)

    # Return error if wrong argument is passed
    if (how %notin% c("factor", "zero_pad", "both")) {
        stop("Incorrect option provided to how. Should be one of factor, zero_pad or both")
    }

    # if how = factor, return input as an ordered factor
    if (how == "factor") {
        base::factor(
            visit_col,
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
    }

    # if how = zero_pad, return input with padded zero to ensure equal length
    else if (how == "zero_pad") {
        # Perform substitutions for each visit type
        visit_col = stringr::str_replace(visit_col, "^t(?=[1-9]{1}$)", "t0")
        visit_col = stringr::str_replace(visit_col, "^b(?=[1-9]{1}$)", "b0")
        visit_col = stringr::str_replace(visit_col, "^v(?=[1-9]{1}$)", "v0")

        # Return zero padded string
        return(visit_col)
    }

    # if how = both, first zero pad, then create an ordered factor
    else if (how == "both") {
        # Perform substitutions for each visit type
        visit_col = stringr::str_replace(visit_col, "^t(?=[1-9]{1}$)", "t0")
        visit_col = stringr::str_replace(visit_col, "^b(?=[1-9]{1}$)", "b0")
        visit_col = stringr::str_replace(visit_col, "^v(?=[1-9]{1}$)", "v0")

        # Create an ordered factor
        visit_col =
            base::factor(
                visit_col,
                levels =
                    c(
                        "t01",
                        "t02",
                        "t03",
                        "b01",
                        "b02",
                        "v01",
                        "v02",
                        "v03",
                        "v04",
                        "v05",
                        "v06",
                        "v07",
                        "v08",
                        "v09",
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

        # Return zero padded string
        return(visit_col)
    }
}
