#' \code{scoring_cerq(d)} generates the total score of the CERQ Scale.
#'
#' @param d A DataFrame
#' @details The input DataFrame must have user_id as the first column,
#' followed by the 36 items of the CERQ in numeric format. The items
#' must be named ``cerq_1`, ... `cerq_36`.
#' @returns A DataFrame with user_id as the first column and the CERQ
#' scores.
#' @export
#' @examples
#' \code{dat <- scoring_cerq(d)}
#'
#' Coding from:
#' https://www.frontiersin.org/articles/10.3389/fpsyg.2017.02075/full
#'
#' Adaptive strategies:
#'
#' Positive refocusing
#' Refocus on planning
#' Positive reappraisal
#' Putting into perspective
#'
#' Maladaptive strategies:
#'
#' Acceptance
#' Rumination
#' Self-blame
#' Catastrophizing
#' Other-blame
scoring_cerq <- function(d){

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  # d <- rio::import(here::here("data", "prep", "quest_scales", "cerq_items.csv"))

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }

  df <- d |>
    dplyr::select(-user_id)

  # Create a vector to store the subscale sums
  subscale_sums <- matrix(NA, nrow = nrow(df), ncol = 9)

  # Iterate over each subscale
  for (i in 1:9) {
    # Calculate the sum of items for the current subscale
    start_index <- (i - 1) * 4 + 1
    end_index <- i * 4
    subscale_sums[, i] <- rowSums(df[, start_index:end_index])
  }

  cerq_subscales <- subscale_sums |>
    as.data.frame()

  colnames(cerq_subscales) <- c(
    "self_blame", "acceptance", "rumination", "positive_refocusing", 
    "refocus_on_planning", "positive_reappraisal", "putting_into_perspective",
    "catastrophizing", "other_blame"
  )

  cerq_subscales$user_id <- d$user_id

  cerq_subscales <- cerq_subscales %>%
    relocate(user_id)

  return(cerq_subscales)
}