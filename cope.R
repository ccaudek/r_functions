#' \code{scoring_cope(d)} generates the subscales of COPE Inventory.
#'
#' The COPE Inventory was developed to assess a broad range of coping
#' responses, several of which had an explicit basis in theory.
#' The inventory includes some responses that are expected to be
#' dysfunctional, as well as some that are expected to be functional.
#' It also includes at least 2 pairs of polar-opposite tendencies.
#' These were included because each scale is unipolar (the absence of
#' this response does not imply the presence of its opposite), and
#' because people engage in a wide range of coping during a given
#' period, including both of each pair of opposites.
#'
#' \url{https://local.psy.miami.edu/people/faculty/ccarver/availbale-self-report-instruments/cope/}
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 60 columns should contain the items of the COPE
#'   Inventory in a numeric format. Each item should be named
#'   `cope_1`, `cope_2`, ..., `cope_60`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the COPE Inventory.
#' @export
#' @examples
#' \code{dat <- scoring_cope(d)}
#'
scoring_cope <- function(d) {

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # d <- rio::import(here::here("data", "prep", "quest_scales", "cope_items.csv"))

  # Social support
  d$social_support <-
    (d$cope_4 + d$cope_14 + d$cope_30 +
      d$cope_45 + d$cope_11 + d$cope_23 +
      d$cope_34 + d$cope_52 + d$cope_3 +
      d$cope_17 + d$cope_28 + d$cope_46)

  # Avoiding strategies
  d$avoiding_strategies <-
    (d$cope_6 + d$cope_27 + d$cope_40 +
      d$cope_57 + d$cope_9 + d$cope_24 +
      d$cope_37 + d$cope_51 + d$cope_2 +
      d$cope_16 + d$cope_31 + d$cope_43 +
      d$cope_12 + d$cope_26 + d$cope_35 +
      d$cope_53)

  # Positive attitude
  d$positive_attitude <- (d$cope_10 + d$cope_22 +
    d$cope_41 + d$cope_49 + d$cope_1 +
    d$cope_29 + d$cope_38 + d$cope_59 +
    d$cope_13 + d$cope_21 + d$cope_44 +
    d$cope_54)

  # Problem orientation
  d$problem_orientation <- (d$cope_5 + d$cope_25 + d$cope_47 +
    d$cope_58 + d$cope_19 + d$cope_32 +
    d$cope_39 + d$cope_56 + d$cope_15 +
    d$cope_33 + d$cope_42 + d$cope_55)

  # Transcendent orientation
  d$transcendent_orientation <- (abs(d$cope_8 - 5) +
    abs(d$cope_20 - 5) + abs(d$cope_36 - 5) +
    abs(d$cope_50 - 5) + d$cope_7 +
    d$cope_18 + d$cope_48 + d$cope_60)

  # user_id and COPE subscales
  cope_scales <- data.frame(
    user_id = d$user_id,
    social_support = d$social_support,
    avoiding_strategies = d$avoiding_strategies,
    positive_attitude = d$positive_attitude,
    problem_orientation = d$problem_orientation,
    transcendent_orientation = d$transcendent_orientation
  )

  return(cope_scales)
}
