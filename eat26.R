#' \code{scoring_eat26(d)} generates the bulimia, oral_control, and
#' dieting subscales of the EAT-26 questionnaire.
#'
#' dieting: Pathological avoidance of fattening foods and shape
#' preoccupations; individuals who score high on this factor
#' may be described as overestimators of their body size and
#' who are dissatisfied with their shape and desire to be smaller.
#'
#' bulimia and food preoccupation: similar to the previous
#' factor, but is positively related to bulimia and heavier body
#' weight. High scores on this factor may be associated with poor
#' outcome.
#'
#' oral_control: factor largely comprised of items relecting 
#' self-control about food as well as those who acknowledge social
#' pressure to gain weight. High scores on this factor are related 
#' to lower weight and absence of bulimia.
#'
#' #' Items 1-25 are scored as follows: 
#' Always = 3; Usually = 2; Often = 1; Other answers = 0
#' Item 26 is scored in the opposite direction (Never = 3 etc.)
#' Total Test Score:
#' Add item scores for a “total test score.”
#' Is the total 20 or more?
#' No__  Yes__, make a referral
#' Behavioral Questions:
#' Did the respondent check any of the boxes as shown below?
#' No__  Yes__, make a referral
#' \url{https://www.nyeatingdisorders.org/pdf/EAT-26IntpretScoring-Test-3-20-10.pdf}
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the user
#'   identifier.
#' - The next 26 columns should contain the items of the EAT-26
#'   questionnaire in a numeric format. Each item should be named
#'   `eat26_1`, `eat26_2`, ..., `eat26_26`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the EAT-26 questionnaire.
#' @export
#' @examples
#' \code{dat <- scoring_eat26(d)}
#'
scoring_eat26 <- function(d){

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }

  # debugging for the groundhog_day project
  # d <- rio::import(here::here("data", "prep", "quest_scales", "cope_items.csv"))

  d1 <- d %>%
    mutate(across(2:27, ~ case_when(
      . == 6 ~ 0, # always
      . == 5 ~ 0,
      . == 4 ~ 0,
      . == 3 ~ 1,
      . == 2 ~ 2,
      . == 1 ~ 3, # never
      TRUE ~ NA_integer_
    )))

  # Reverse item 26.
  d2 <- d1 %>%
    mutate(
      eat26_26 = case_when(
        eat26_26 == 0 ~ 3,
        eat26_26 == 1 ~ 2,
        eat26_26 == 2 ~ 1,
        eat26_26 == 3 ~ 0,
        TRUE ~ NA_integer_
      )
    )

  # Compute the EAT-26 subscales.
  d2$dieting <- with(
    d2,
    eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 +
    eat26_14 + eat26_16 + eat26_17 + eat26_22 + eat26_23 +
    eat26_24 + eat26_26
  )

  d2$bulimia <- with(
    d2,
    eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
  )

  d2$oral_control <- with(
    d2,
    eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
  )

  eat26_subscales <- d2 |>
    dplyr::select(
      user_id, bulimia, dieting, oral_control
    )

  # Add total score
  eat26_subscales$eat26_tot <- eat26_subscales$bulimia +
    eat26_subscales$dieting + eat26_subscales$oral_control

  return(eat26_subscales)
}
