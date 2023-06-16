#' \code{scoring_dass21(d)} generates the subscales of the Depression
#' Anxiety Stress Scales (DASS-21).
#' Reference:
#' Lovibond, S.H. & Lovibond, P.F. (1995). Manual for the Depression 
#' Anxiety Stress Scales (2nd. Ed.). Sydney: Psychology Foundation.
#' \url{https://www.healthfocuspsychology.com.au/tools/dass-21/}
#'
#' The DASS-21 is the short form of the DASS-42, a self-report scale 
#' designed to measure the negative emotional states of depression, 
#' anxiety and stress. As the three scales of the DASS have been 
#' shown to have high internal consistency and to yield meaningful 
#' discriminations, the scales should meet the needs of both researchers 
#' and clinicians who wish to measure current state or change in state 
#' over time (e.g., in the course of treatment). This scale is suitable 
#' for clinical and non-clinical settings.
#'
#' Please read each statement and choose a number 0, 1, 2 or 3 that
#' indicates how much the statement applied to you over the past week.
#' There are no right or wrong answers. Do not spend too much time on
#' any statement.
#'
#' 1. I found it hard to wind down
#' 2. I was aware of dryness of my mouth
#' 3. I couldn’t seem to experience any positive feeling at all
#' 4. I experienced breathing difficulty (eg, excessively rapid breathing, breathlessness in the absence of physical exertion)
#' 5. I found it difficult to work up the initiative to do things
#' 6. I tended to over-react to situations
#' 7. I experienced trembling (eg, in the hands)
#' 8. I felt that I was using a lot of nervous energy
#' 9. I was worried about situations in which I might panic and make a fool of myself
#' 10. I felt that I had nothing to look forward to
#' 11. I found myself getting agitated
#' 12. I found it difficult to relax
#' 13. I felt down-hearted and blue
#' 14. I was intolerant of anything that kept me from getting on with what I was doing
#' 15. I felt I was close to panic
#' 16. I was unable to become enthusiastic about anything
#' 17. I felt I wasn’t worth much as a person
#' 18. I felt that I was rather touchy
#' 19. I was aware of the action of my heart in the absence of physical exertion (eg, sense of heart rate increase, heart missing a beat)
#' 20. I felt scared without any good reason
#' 21. I felt that life was meaningless
#'
#' Scoring Guide
#' DASS-21 Scoring  |	Depression  |	Anxiety  |	Stress |
#' Normal           | 0-4	        | 0-3      | 0-7     |
#' Mild	            | 5-6         | 4-5      |	8-9    |
#' Moderate         | 7-10        | 6-7      | 10-12   |
#' Severe           | 11-13       | 8-9      | 13-16   |
#' Extremely Severe | 14+         | 10+      | 17+     |
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the user
#'   identifier.
#' - The next 21 columns should contain the items of the DASS-21
#'   Scale in a numeric format. Each item should be named
#'   `dass21_1`, `dass21_2`, ..., `dass21_21`.
#' @returns A DataFrame with user_id user_id as the first column,
#' followed by the subscales of the DASS-21 Scale.
#' @export
#' @examples
#' \code{dat <- scoring_dass21(d)}
#'
scoring_dass21 <- function(d){

  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column must be user_id!")
  }

  # d <- rio::import(here::here("data", "prep", "quest_scales", "dass21_items.csv"))

  # DASS-21: Stress subscale
  d$dass21_stress <- d %>%
    dplyr::select(
      c("dass21_1", "dass21_6", "dass21_8", "dass21_11",
      "dass21_12", "dass21_14", "dass21_18")
    ) |>
    rowSums()

  # DASS-21: Anxiety subscale
  d$dass21_anxiety <- d %>%
    dplyr::select(
      c("dass21_2", "dass21_4", "dass21_7", "dass21_9",
      "dass21_15", "dass21_19", "dass21_20")
    ) |>
    rowSums()

  # DASS-21: Depression subscale
  d$dass21_depression <- d %>%
    dplyr::select(
      c("dass21_3", "dass21_5", "dass21_10", "dass21_13",
      "dass21_16", "dass21_17", "dass21_21")
    ) |>
    rowSums()

  # user_id and DASS-21 subscales
  dass21_scales <- d |>
    dplyr::select(
      user_id, dass21_stress, dass21_anxiety, dass21_depression
    )

  return(dass21_scales)
}
