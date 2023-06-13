#' \code{scoring_eat26()} generates the bulimia, oral_control, and
#' dieting subscales of the EAT-26 questionnaire. 
#' @param data.frame.
#' @return data.frame.
#' # https://www.nyeatingdisorders.org/pdf/EAT-26IntpretScoring-Test-3-20-10.pdf
#' EAT-26 Test Score:
#' Test Items and Total Test Score:
#' Items 1-25 are scored as follows: Always = 3; Usually = 2; Often = 1; Other answers = 0
#' Item 26 is scored in the opposite direction (Never = 3 etc.)
#' Total Test Score: Add item scores for a “total test score.”
#' Is the total 20 or more?  
#' No__  Yes__, make a referral
#' Behavioral Questions:
#' Did the respondent check any of the boxes as shown below?  
#' No__  Yes__, make a referral
scoring_eat26 <- function(d) {
  
  if (length(unique(d[, 1])) < 10) 
    stop("Error: the first column is not user_id!")
  
  # foo <- d
  # foo[, 2:27] <- lapply(foo[, 2:27], function(x) {
  #   x <- ifelse(x == 1, 0,
  #     ifelse(x == 2, 0,
  #       ifelse(x == 3, 0,
  #         ifelse(x == 4, 1,
  #           ifelse(x == 5, 2,
  #             ifelse(x == 6, 3, NA_integer_)
  #           )
  #         )
  #       )
  #     )
  #   )
  # })

d1 <- d %>%
  mutate(across(2:27, ~ case_when(
    . == 6 ~ 0, # mai
    . == 5 ~ 0,
    . == 4 ~ 0,
    . == 3 ~ 1,
    . == 2 ~ 2,
    . == 1 ~ 3, # sempre
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
  
  # Compute the 3 subscales.
  
  # Pathological avoidance of fattening foods and shape preoccupations; individuals who
  # score high on this factor may be described as overestimators of their body size and
  # who are dissatisfied with their shape and desire to be smaller.
  d2$dieting <- with(
    d2,
    eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + eat26_14,
    eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + eat26_26
  )
  
  # bulimia and food preoccupation: similar to the previous factor, but is positively
  # related to bulimia and heavier body weight. High scores on this factor may be
  # associated with poor outcome.
  d2$bulimia <- with(
    d2,
    eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
  )
  
  # factor largely comprised of items relecting self-control about food as well as
  # those who acknowledge social pressure to gain weight. High scores on this factor
  # are related to lower weight and absence of bulimia.
  d2$oral_control <- with(
    d2,
    eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
  )
  
  eat26_subscales <- d2 |> 
    dplyr::select(
      user_id, bulimia, dieting, oral_control
    )
  
  # Compute total score.
  eat26_subscales$eat26_tot <- 
    eat26_subscales$bulimia + eat26_subscales$dieting + 
    eat26_subscales$oral_control
  
  eat26_subscales
}

