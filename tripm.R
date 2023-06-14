#' \code{scoring_tripm()} generates the total score of the Tri-PM Scale
#' @param data.frame.
#' @return data.frame.
#' The first column has the subjects' ids; the following columns are the 58
#' items of the Tri-PM The items must be named `tripm_1`, ... `tripm_58`.
#' # Step 1: Coding Responses
#'
#' For items followed by [F]-i.e., items 2, 4, 10, 11, 16, 21, 25, 30, 33, 35,
#' 39, 41, 44, 47, 50, 52, 57 - code responses as follows: True = 0; Somewhat
#' true = 1; Somewhat false = 2; False = 3.
#'
#' Code responses for all other items as follows: True = 3; Somewhat true = 2;
#' Somewhat false = 1; False = 0.
#'
#' Step 2: Computing Scale Scores and Total Scores
#'
#' Boldness subscale (19 items)-Sum coded responses for the following items:
#'   1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 32, 35, 38, 41, 44, 47, 50, 54, 57
#'
#' Meanness subscale (19 items)-Sum coded responses for the following items:
#'   2, 6, 8, 11, 14, 17, 20, 23, 26, 29, 33, 36, 39, 40, 42, 45, 48, 52, 55
#'
#' Disinhibition subscale (20 items)-Sum coded responses for the following items:
#'   3, 5, 9, 12, 15, 18, 21, 24, 27, 30, 31, 34, 37, 43, 46, 49, 51, 53, 56, 58
#'
#' Total Psychopathy score-Sum scores across the three subscales.
#'
#' In the input dataframe, the jugments are scored as follows:
#' True = 3; Somewhat true = 2; Somewhat false = 1; False = 0.

scoring_tripm <- function(d) {
  # d <- rio::import(here("data", "prep", "quest_scales", "tripm_items.csv"))

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }

  # Boldness items.
  dat_b <- d %>%
    dplyr::select(
      tripm_1, tripm_7, tripm_13, tripm_19, tripm_22, tripm_28,
      tripm_32, tripm_38, tripm_54,
      # reverse scoring
      tripm_4, tripm_10, tripm_16,
      tripm_25, tripm_35, tripm_41, tripm_44, tripm_47, tripm_50,
      tripm_57
    )

  # presence of boldness
  keys_b <- c(rep(1, 9), rep(-1, 10))
  clean_b <- psych::reverse.code(keys_b, dat_b, mini = 0, maxi = 3)
  d$boldness <- rowSums(clean_b)
  # hist(d$boldness)
  # psych::alpha(clean_b)
  # mean(d$boldness)

  # Meanness subscale
  dat_m <- d %>%
    dplyr::select(
      tripm_6, tripm_8, tripm_14, tripm_17, tripm_20, tripm_23,
      tripm_26, tripm_29, tripm_36, tripm_40, tripm_42,
      tripm_45, tripm_48, tripm_55,
      # reverse coded
      tripm_2, tripm_11, tripm_33, tripm_39, tripm_52
    )

  # presence of meanness
  keys_m <- c(rep(1, 14), rep(-1, 5))
  clean_m <- psych::reverse.code(keys_m, dat_m, mini = 0, maxi = 3)
  d$meanness <- rowSums(clean_m)
  # hist(d$meanness)
  # mean(d$meanness)

  # TRIPM: Disinhibition subscale
  dat_d <- d %>%
    dplyr::select(
      tripm_3, tripm_5, tripm_9, tripm_12, tripm_15, tripm_18, tripm_24,
      tripm_27, tripm_31, tripm_34, tripm_37, tripm_43, tripm_46, tripm_49,
      tripm_51, tripm_53, tripm_56, tripm_58,
      # reversed coded
      tripm_21, tripm_30
    )

  # presence of dishinibition
  keys_d <- c(rep(1, 18), rep(-1, 2))
  clean_d <- psych::reverse.code(keys_d, dat_d, mini = 0, maxi = 3)
  d$disinhibition <- rowSums(clean_d)
  # hist(d$disinhibition)
  # mean(d$disinhibition)

  # TRIPM subscales and subject code
  tripm_scale <- d |>
    dplyr::select(user_id, boldness, disinhibition, meanness)

  return(tripm_scale)
}
