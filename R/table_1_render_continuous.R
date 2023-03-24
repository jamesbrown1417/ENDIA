#' Render Continuous Variable in Table 1 Package
#'
#' @param x variable to be rendered
#'
#' @return Use in Table 1 as an argument to correctly format continous variables
#' @export
#'
#' @examples
table_1_render_continuous <- function(x) {
    with(table1::stats.default(x),
         c("",
           "Mean (SD)" = paste(
               formatC(round(MEAN, 1), digits = 1, format = "f"),
               " (",
               formatC(round(SD, 1), digits = 1, format = "f"),
               ")",
               sep = ""),
           "Median [1Q, 3Q]" = paste(
               formatC(round(MEDIAN, 1),
                       digits = 1,
                       format = "f",),
               " [",
               formatC(round(Q1, 1),
                       digits = 1,
                       format = "f"),
               ", ",
               formatC(round(Q3, 1),
                       digits = 1,
                       format = "f"),
               "]",
               sep = ""
           )
         ))
}
