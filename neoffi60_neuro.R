#' \code{scoring_neoffi60_neuro()} generates the total score of the 
#' Neuroticism Scale of the NEO-FFI-60.
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 12 columns should contain the items of the Neuroticism subscale
#'   in a numeric format. Each item should be named
#'   `neuroticism_1`, `neuroticism_2`, ..., `neuroticism_12`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the Neuroticism Scale of the NEO-FFI-60.
#' @export
#' @examples
#' \code{dat <- scoring_neoffi60_neuro(d)}
#'
scoring_neoffi60_neuro <- function(d) {
  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # Debugging for the groundhog_day project.
  # d <- rio::import(here("data", "prep", "quest_scales", "neoffi60_neuro_items.csv"))

  d$neuroticism <-
    (5 - d$neuroticism_1) +
    d$neuroticism_2 +
    d$neuroticism_3 +
    (5 - d$neuroticism_4) +
    d$neuroticism_5 +
    d$neuroticism_6 +
    (5 - d$neuroticism_7) +
    d$neuroticism_8 +
    d$neuroticism_9 +
    (5 - d$neuroticism_10) +
    d$neuroticism_11 +
    d$neuroticism_12

  neuroticism_scores <- d |>
    dplyr::select(
      user_id, neuroticism
    )

  return(neuroticism_scores)
}
