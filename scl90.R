#' \code{scoring_scl90(d)} generates the subscales of the Sympthoms 
#' Checklist - 90 (scl-90).
#' 
#' ITEM CODING: Each item is scored on a scale from 0 to 4 based on how much an 
#' individual was bothered by each item in the last week:0 = Not at all, 
#' 1 = A little bit, 2 = Moderately, 3 = Quite a bit, 4 = Extremely
#' 
#' SCALES
#' Somatization: 1, 4, 12, 27, 40, 42, 48, 49, 52, 53, 56, and 58
#' Obsessive-compulsive: 3, 9, 10, 28, 38, 45, 46, 51, 55, and 65
#' Interpersonal sensibility: 6, 21, 34, 36, 37, 41, 61, 69, and 73
#' Depression: 5, 14, 20, 22, 26, 29, 30, 31, 32, 54, 71, and 79
#' Anxiety: 2, 17, 23, 33, 39, 57, 72, 78, 80, and 86
#' Anger-hostility: 11, 24, 63, 67, 74, and 81
#' Phobic-anxiety: 13, 25, 47, 50, 70, 75, and 82
#' Paranoid ideation: 8, 18, 43, 68, 76, and 83
#' Psychoticism: 7, 16, 35, 62, 77, 84, 85, 87, 88, and 90
#' Additional items: 19, 44, 59, 60, 64, and 89
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the user
#'   identifier.
#' - The next 21 columns should contain the items of the scl-90
#'   Scale in a numeric format. Each item should be named
#'   `scl_1`, `scl_2`, ..., `scl_90`.
#' @returns A DataFrame with user_id user_id as the first column,
#' followed by the subscales of the scl-90 Scale.
#' @export
#' @examples
#' \code{dat <- scoring_scl90(d)}
#'
scoring_scl90 <- function(d){

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # d <- rio::import(here::here("data", "prep", "quest_scales", "scl90_items.csv"))

  # SCL-90: Somatization subscale
  d$scl90_somatization <- d %>%
    dplyr::select(
      c("scl90_1", "scl90_4", "scl90_12", "scl90_27",
        "scl90_40", "scl90_42", "scl90_48", "scl90_49", 
        "scl90_52", "scl90_53", "scl90_56", "scl90_58"
      )
    ) |>
    rowSums()

  # SCL-90: Obsessive-compulsive
  d$scl90_osbsess_comp <- d %>%
    dplyr::select(
      c("scl90_3", "scl90_9", "scl90_10", "scl90_28",
        "scl90_38", "scl90_45", "scl90_46", "scl90_51", 
        "scl90_55", "scl90_65"
      )
    ) |>
    rowSums()
  
  # SCL-90: Interpersonal sensibility: 6, 21, 34, 36, 37, 41, 61, 69, and 73
  d$scl90_interp_sens <- d %>%
    dplyr::select(
      c("scl90_6", "scl90_21", "scl90_34", "scl90_36",
        "scl90_37", "scl90_41", "scl90_61", "scl90_69", 
        "scl90_73"
      )
    ) |>
    rowSums()
  
  # SCL-90: Depression: 5, 14, 20, 22, 26, 29, 30, 31, 32, 54, 71, and 79
  d$scl90_depression <- d %>%
    dplyr::select(
      c("scl90_4", "scl90_14", "scl90_20", "scl90_22",
        "scl90_26", "scl90_29", "scl90_30", "scl90_31", 
        "scl90_32", "scl90_54", "scl90_71", "scl90_79"
      )
    ) |>
    rowSums()
  
  # Anxiety: 2, 17, 23, 33, 39, 57, 72, 78, 80, and 86
  d$scl90_anxiety <- d %>%
    dplyr::select(
      c("scl90_2", "scl90_17", "scl90_23", "scl90_33", "scl90_39",
        "scl90_57", "scl90_72", "scl90_78", "scl90_80", 
        "scl90_86"
      )
    ) |>
    rowSums()
  
  # Anger-hostility: 11, 24, 63, 67, 74, and 81
  d$scl90_anger_hostility <- d %>%
    dplyr::select(
      c("scl90_11", "scl90_24", "scl90_63", "scl90_67",
        "scl90_74", "scl90_81"
      )
    ) |>
    rowSums()
  
  # Phobic-anxiety: 13, 25, 47, 50, 70, 75, and 82
  d$scl90_phobic_anxiety <- d %>%
    dplyr::select(
      c("scl90_13", "scl90_25", "scl90_47", "scl90_50",
        "scl90_70", "scl90_75", "scl90_82"
      )
    ) |>
    rowSums()
  
  # Paranoid ideation: 8, 18, 43, 68, 76, and 83
  d$scl90_paranoid_ideation <- d %>%
    dplyr::select(
      c("scl90_8", "scl90_18", "scl90_43", "scl90_68",
        "scl90_76", "scl90_83"
      )
    ) |>
    rowSums()
  
  # Psychoticism: 7, 16, 35, 62, 77, 84, 85, 87, 88, and 90
  d$scl90_psychoticism <- d %>%
    dplyr::select(
      c("scl90_7", "scl90_16", "scl90_35", "scl90_62",
        "scl90_77", "scl90_84", "scl90_85", "scl90_87", 
        "scl90_88", "scl90_90"
      )
    ) |>
    rowSums()
  
  # Sleep disorder
  d$scl90_sleep_disorder <- d %>%
    dplyr::select(
      c("scl90_44", "scl90_64", "scl90_66"
      )
    ) |>
    rowSums()
  
  d$scl90_ts <- d %>%
    dplyr::select(-user_id) |>
    rowSums() / 90
  
  scl90_scales <- d[, c(1, 92:102)]
  
  return(scl90_scales)
}
