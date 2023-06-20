#' \code{scoring_ders()} generates the total score of the 
#' Difficulty Emotion Regulation Strategies (DERS).
#' 
#' The Difficulties in Emotion Regulation Scale (DERS) is an instrument 
#' measuring emotion regulation problems. The 36 items self-report scale asks 
#' respondents how they relate to their emotions in order to produce scores on 
#' the following subscales.
#' 
#' Nonacceptance of emotional responses
#' Difficulty engaging in goal-directed behaviour
#' Impulse control difficulties
#' Lack of emotional awareness
#' Limited access to emotion regulation strategies
#' Lack of emotional clarity
#' 
#' Gratz, K. L., & Roemer, L. (2004). Multidimensional assessment of emotion 
#' regulation and dysregulation: Development, factor structure, and initial 
#' validation of the difficulties in emotion regulation scale. Journal of 
#' psychopathology and behavioral assessment, 26(1), 41-54.
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 16 columns should contain the items of the DERS
#'   in a numeric format. Each item should be named
#'   `ders_1`, `ders_2`, ..., `ders_36`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the DERS scale.
#' @export
#' @examples
#' \code{dat <- scoring_ders(d)}
#'
scoring_ders <- function(d) {

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # Debugging for the groundhog_day project.
  d <- rio::import(here("data", "prep", "quest_scales", "ders_items.csv"))

  # Nonacceptance of emotional responses
  # Difficulty engaging in goal-directed behaviour
  # Impulse control difficulties
  # Lack of emotional awareness
  # Limited access to emotion regulation strategies
  # Lack of emotional clarity
  
  # SUBSCALES:
  #   
  # Nonacceptance of emotional responses: 11, 12, 21, 23, 25, 29
  # Tendency to have a negative secondary or non accepting reaction to one’s own distress
  # Difficulty engaging in goal-directed behaviour: 13, 18, 20R, 26, 33
  # Difficulty in concentrating and/or accomplishing tasks when experiencing negative emotions
  # Impulse control difficulties: 3, 14, 19, 24R, 27, 32
  # Difficulty remaining in control of one’s behaviour when experiencing negative emotions
  # Lack of emotional awareness: 2R, 6R, 8R, 10R, 17R, 34R
  # Reflects a lack of awareness or inattention to emotional responses
  # Limited access to emotion regulation strategies: 15, 16, 22R, 28, 30, 31, 35, 36
  # Reflects the belief that there is little one can do to regulate oneself once upset
  # Lack of emotional clarity: 1R, 4, 5, 7R, 9
  # Reflects the extent to which an individual knows and is clear about his or her emotions
  
  # model <- ' ner  =~ NA*ders_11 + ders_12 + ders_21 + ders_23 + ders_25 + ders_29
  #           degb =~ NA*ders_13 + ders_18 + ders_20 + ders_26 + ders_33
  #           icd =~ NA*ders_3 + ders_14 + ders_19 + ders_24 + ders_32
  #           lea =~ NA*ders_2 + ders_6 + ders_8 + ders_10 + ders_17 + ders_34
  #           laers =~ NA*ders_15 + ders_16 + ders_22 + ders_28 + ders_30 + ders_31 + ders_35 + ders_36
  #           lec =~ NA*ders_1 + ders_4 + ders_5 + ders_7 + ders_9
  # '
  # 
  # fit <- cfa(model, data = d[, 2:37], std.lv = TRUE)
  # fit_indices <- fitMeasures(fit)
  
  d$ner   = d$ders_11 + d$ders_12 + d$ders_21 + d$ders_23 + d$ders_25 + d$ders_29
  d$degb  = d$ders_13 + d$ders_18 + (6 - d$ders_20) + d$ders_26 + d$ders_33
  d$icd   = d$ders_3 + d$ders_14 + d$ders_19 + (6 - d$ders_24) + d$ders_32
  d$lea   = (6 - d$ders_2) + (6 - d$ders_6) + (6 - d$ders_8) + (6 - d$ders_10) + 
            (6 - d$ders_17) + (6 - d$ders_34)
  d$laers = d$ders_15 + d$ders_16 + (6 - d$ders_22) + d$ders_28 + d$ders_30 + 
            d$ders_31 + d$ders_35 + d$ders_36
  d$lec   = (6 - d$ders_1) + d$ders_4 + d$ders_5 + (6 - d$ders_7) + d$ders_9
  
  # Total score
  d$ders_ts <- d$ner + d$degb + d$icd + d$lea + d$laers + d$lec

  ders_scores <- d |> 
    dplyr::select(
      user_id, ner, degb, icd, lea, laers, lec, ders_ts
  )

  return(ders_scores)
}
