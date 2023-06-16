#' \code{scoring_scs()} generates the total score of the 
#' Self-Compassion Scale and the not-reversed scores of the 
#' six subscales (sk, ch, mi, sj, is, oi).
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 26 columns should contain the items of the SCS
#'   in a numeric format. Each item should be named
#'   `scs_1`, `scs_2`, ..., `scs_26`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the COPE Inventory.
#' @export
#' @examples
#' \code{dat <- scoring_cope(d)}
#'
scoring_scs <- function(d) {

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # Debugging for the groundhog_day project.
  # d <- rio::import(here("data", "prep", "quest_scales", "scs_items.csv"))

  # Self-Kindness
  d$self_kindness <- d$scs_5 + d$scs_12 + d$scs_19 + d$scs_23 + d$scs_26

  # Self-Judgment
  d$self_judgment <-
    abs(d$scs_1 - 6) + abs(d$scs_8 - 6) + abs(d$scs_11 - 6) +
    abs(d$scs_16 - 6) + abs(d$scs_21 - 6)

  # Common Humanity
  d$common_humanity <- d$scs_3 + d$scs_7 + d$scs_10 + d$scs_15

  # Isolation
  d$isolation <-
    abs(d$scs_4 - 6) + abs(d$scs_13 - 6) + abs(d$scs_18 - 6) +
    abs(d$scs_25 - 6)

  # Mindfulness
  d$mindfulness <- d$scs_9 + d$scs_14 + d$scs_17 + d$scs_22

  # Overidentification
  d$over_identification <-
    abs(d$scs_2 - 6) + abs(d$scs_6 - 6) + abs(d$scs_20 - 6) +
    abs(d$scs_24 - 6)

  d$neg_self_compassion <- d$self_judgment + d$isolation +
    d$over_identification

  d$pos_self_compassion <- d$self_kindness + d$common_humanity +
    d$mindfulness

  # The sk, ch, mi, sj, is, oi columns are the *not reversed* scores
  # of the six SCS subscales.
  d$sk <- d$self_kindness
  d$ch <- d$common_humanity
  d$mi <- d$mindfulness

  d$sj <- d$scs_1 + d$scs_8 + d$scs_11 + d$scs_16 + d$scs_21
  d$is <- d$scs_4 + d$scs_13 + d$scs_18 + d$scs_25
  d$oi <- d$scs_2 + d$scs_6 + d$scs_20 + d$scs_24

  d$scs_ts <- d$neg_self_compassion + d$pos_self_compassion

  scs_scores <- data.frame(
    user_id = d$user_id,
    self_kindness = d$sk,
    common_humanity = d$ch,
    mindfulness = d$mi,
    self_judgment = d$sj,
    isolation = d$is,
    over_identification = d$oi,
    scs_total_score = d$scs_ts
  )

  return(scs_scores)
}
