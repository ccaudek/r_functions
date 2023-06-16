#' \code{scoring_sias()} generates the total score of the Social Interaction
#' Anxiety Scale (SIAS)
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 10 columns should contain the items of the COPE
#'   Inventory in a numeric format. Each item should be named
#'   `sias_1`, `sias_2`, ..., `sias_10`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the SIAS Scale.
#' @export
#' @examples
#' \code{dat <- scoring_sias(d)}
#'
scoring_sias <- function(d) {

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # Reverse items 8 and 10.
  columns_to_recode <- c("sias_8", "sias_10")

  d <- d %>%
    mutate_at(
      all_of(columns_to_recode),
      ~ case_when(
        . == 1 ~ 5,
        . == 2 ~ 4,
        . == 3 ~ 3,
        . == 4 ~ 2,
        . == 5 ~ 1
      )
    )

  # Compute total score.
  d <- d %>%
    rowwise() %>%
    mutate(
      sias_score = sum(c_across(sias_1:sias_19))
    )

  sias_scale <- d |>
    dplyr::select(user_id, sias_score)

  return(sias_scale)
}
