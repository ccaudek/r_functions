#' \code{scoring_bdi2(d)} generates the total score of the BDI-II Scale.
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 21 columns should contain the items of the COPE
#'   Inventory in a numeric format. Each item should be named
#'   `bdi2_1`, `bdi2_2`, ..., `bdi2_21`.
#' @returns A DataFrame with user_id user_id as the first column,
#' followed by the BDI-II scores.
#' @export
#' @examples
#' \code{dat <- scoring_bdi2(d)}
#'
scoring_bdi2 <- function(d) {

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # d <- rio::import(here("data", "prep", "quest_scales", "bdi2_items.csv"))

  # BDI-II items.
  bdi2_items <- d %>%
    dplyr::select(-user_id)

  d$bdi2_score <- rowSums(bdi2_items)

  # user_id and BDI-II scores
  bdi2_scale <- d |>
    dplyr::select(user_id, bdi2_score)

  return(bdi2_scale)
}
