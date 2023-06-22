#' \code{scoring_rscs()} generates the total score of the 
#' Relational Self-Compassion Scale.
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 16 columns should contain the items of the SCS
#'   in a numeric format. Each item should be named
#'   `rscs_1`, `rscs_2`, ..., `rscs_16`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the RSCS Scale.
#' @export
#' @examples
#' \code{dat <- scoring_rscs(d)}
#'
scoring_rscs <- function(d) {

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # Debugging for the groundhog_day project.
  # d <- rio::import(here("data", "prep", "quest_scales", "rscs_items.csv"))

  # SS
  d$ss <- d$rscs_4 + d$rscs_12 + d$rscs_13 
  
  # OS
  d$os <- d$rscs_9 + d$rscs_10 + d$rscs_15 + d$rscs_16
  
  # SO
  d$so <- d$rscs_3 + d$rscs_5 + d$rscs_14 + (5 - d$rscs_8) + (5 - d$rscs_11)
  
  # OO
  d$oo <- d$rscs_1 + (5 - d$rscs_2) + (5 - d$rscs_6) + (5 - d$rscs_7)
  
  # Total score
  d$rscs_ts <- d$ss + d$os + d$so + d$oo

  rscs_scores <- d |> 
    dplyr::select(
      user_id, ss, os, so, oo, rscs_ts
    )

  return(rscs_scores)
}

# eof ----


