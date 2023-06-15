#' \code{scoring_bsq14(d)} generates the total score of the BSQ-14 Scale.
#'
#' @param d A DataFrame
#' @details The input DataFrame must have user_id as the first column,
#' followed by the 14 items of the BSQ-14 in numeric format. The items
#' must be named `bsq14_1`, ... `bsq14_14`.
#' @returns A DataFrame with user_id as the first column and the BSQ-14
#' scores.
#' @export
#' @examples
#' \code{dat <- scoring_bsq14(d)}
#'
scoring_bsq14 <- function(d) {

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  # d <- rio::import(here("data", "prep", "quest_scales", "bsq14_items.csv"))

  if (length(unique(d[, 1])) < 10)
    stop("Error: the first column is not user_id!")

  # BSQ-14 items.
  bsq14_items <- d %>%
    dplyr::select(-user_id)

  d$bsq14_score <- rowSums(bsq14_items)
  # hist(d$bsq14_score)
  # mean(d$bsq14_score)

  # BSQ-14 subscales and subject code
  bsq14_scale <- d |>
    dplyr::select(user_id, bsq14_score)

  return(bsq14_scale)
}
