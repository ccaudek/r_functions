#' \code{scoring_ius12(d)} generates the total score of the
#' Intolerance of Uncertainty Scale short form (IUS-12).
#'
#' \url{https://iris.uniroma1.it/retrieve/e3835315-79da-15e8-e053-a505fe0a3de9/Lauriola_Hierarchical_2016.pdf}
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 12 columns should contain the items of the IUS-12
#'   Scale in a numeric format. Each item should be named `ius_1`,
#'   `ius_2`, ..., `ius_12`.
#' @returns A DataFrame with user_id user_id as the first column,
#' followed by the subscales of the IUS-12 Scale.
#' @export
#' @examples
#' \code{dat <- scoring_ius12(d)}
#'
scoring_ius12 <- function(d){

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # debugging for the groundhog_day project
  # d <- rio::import(here("data", "prep", "quest_scales", "ius12_items.csv"))

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }

  # Prospective IU items
  prospective_iu <- d %>%
    dplyr::select(
      ius_1, ius_2, ius_3, ius_4, ius_5, ius_6, ius_7
    )

  d$prospective_iu <- rowSums(prospective_iu)

  # Inhibitory IU items
  inhibitory_iu <- d %>%
    dplyr::select(
      ius_8, ius_9, ius_10, ius_11, ius_12
    )

  d$inhibitory_iu <- rowSums(inhibitory_iu)

  d$ius_tot <- d$prospective_iu + d$inhibitory_iu

  # user_id and IUS subscales
  ius_scale <- d |>
    dplyr::select(user_id, inhibitory_iu, prospective_iu, ius_tot)

  return(ius_scale)
}
