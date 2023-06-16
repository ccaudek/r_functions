#' \code{scoring_nates(d)} generates the subscales of the 
#' No Attachment to Ego Scale (NATES).
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 7 columns should contain the items of the NATES
#'   Scale in a numeric format. Each item should be named
#'   `nates_1`, `nates_2`, ..., `nates_7`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the NATES Scale.
#' @export
#' @examples
#' \code{dat <- scoring_nates(d)}
#'
scoring_nates <- function(d) {

    suppressPackageStartupMessages({
      library("tidyverse")
      library("rio")
    })

    if (length(unique(d[, 1])) < 10) {
      stop("Error: the first column must be user_id!")
    }

  # debugging for the groundhog_day project
  # d <- rio::import(here::here("data", "prep", "quest_scales", "nates_items.csv"))

  d$nates_score <- d %>%
    dplyr::select(-user_id) |>
    rowSums()

  nates_scale <- d |>
    dplyr::select(user_id, nates_score)

  return(nates_scale)
}

# eof ----
