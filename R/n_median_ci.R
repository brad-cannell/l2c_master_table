#' Calculate N, Median, and 95% Confidence Interval of a Numeric Column
#'
#' @param .data A data frame
#' @param .col A numeric column
#' @param .digits Number of digits to round to
#'
#' @return A data frame
#' @export
#' @importFrom rlang !!
#'
#' @examples
#' mtcars |>
#'   n_median_ci(mpg, 1)
#'   
n_median_ci <- function(.data, .col, .digits) {
  .data |>  
    dplyr::summarise(
      var    = !! rlang::enquo(.col) |> rlang::as_name(),
      n      = sum(!is.na({{.col}})),
      n_miss = sum(is.na({{.col}})),
      median = stats::median({{.col}}, na.rm = TRUE),
      lcl    = sort(na.omit({{.col}}))[stats::qbinom(.025, 
                                                     length(na.omit({{.col}})), 
                                                     0.5)
                                       ],
      ucl    = sort(na.omit({{.col}}))[stats::qbinom(.975, 
                                                     length(na.omit({{.col}})), 
                                                     0.5)
                                       ]
    ) |> 
    meantables::mean_format("median (lcl - ucl)", digits = .digits) |> 
    dplyr::select(var, n, formatted_stats) |>
    dplyr::mutate(statistic = 'Median (95% CI)') |>
    dplyr::relocate(var, statistic)
}

# For testing
# mtcars |>
#   n_median_ci(mpg, 1)