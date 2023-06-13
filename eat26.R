#' \code{scoring_eat26()} generates the bulimia, oral_control, and
#' dieting subscales of the EAT-26 questionnaire. 
#' @param data.frame.
#' @return data.frame.
#' # https://www.nyeatingdisorders.org/pdf/EAT-26IntpretScoring-Test-3-20-10.pdf
scoring_eat26 <- function(d) {
  
  # Compute total score.
  eat26_all <- d %>%
    rowwise() %>%
    mutate(
      eat26_tot = sum(c_across(eat26_1:eat26_26))
    )
  
  # Pathological avoidance of fattening foods and shape preoccupations; individuals who
  # score high on this factor may be described as overestimators of their body size and
  # who are dissatisfied with their shape and desire to be smaller.
  d$dieting <- with(
    d,
    eat26_1 + eat26_6 + eat26_7 + eat26_10 + eat26_11 + eat26_12 + eat26_14,
    eat26_16 + eat26_17 + eat26_22 + eat26_23 + eat26_24 + eat26_26
  )
  
  # bulimia and food preoccupation: similar to the previous factor, but is positively
  # related to bulimia and heavier body weight. High scores on this factor may be
  # associated with poor outcome.
  d$bulimia <- with(
    d,
    eat26_3 + eat26_4 + eat26_9 + eat26_18 + eat26_21 + eat26_25
  )
  
  # factor largely comprised of items relecting self-control about food as well as
  # those who acknowledge social pressure to gain weight. High scores on this factor
  # are related to lower weight and absence of bulimia.
  d$oral_control <- with(
    d,
    eat26_2 + eat26_5 + eat26_8 + eat26_13 + eat26_15 + eat26_19 + eat26_20
  )
  
  eat26_subscales <- d |> 
    dplyr::select(
      user_id, bulimia, dieting, oral_control, eat26_tot
    )
  
  eat26_subscales
}

