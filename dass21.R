#' Scoring DASS-21
#'
#' The \code{scoring_dass21} function performs the scoring of the DASS-21 scale.
#' 
#' Gets as input the raw items and returns the three DASS-21 sub-scales.
#' @param scoring a data.frame or \code{seq} object
#' @name scoring
#' @export
scoring_dass21 <- function(d) {
  
  # d <- rio::import(here::here("data", "prep", "quest_scales", "dass21_items.csv"))
  
  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }
  
  # DASS-21: Stress subscale
  d$dass21_stress <- d %>% 
    dplyr::select(
      c("dass21_1", "dass21_6", "dass21_8", "dass21_11", "dass21_12", 
        "dass21_14", "dass21_18")
    ) |> 
    rowSums()
  
  # DASS-21: Anxiety subscale
  d$dass21_anxiety <- d %>% 
    dplyr::select(
      c("dass21_2", "dass21_4", "dass21_7", "dass21_9", "dass21_15", 
        "dass21_19", "dass21_20")
    ) |> 
    rowSums()
  
  # DASS-21: Depression subscale
  d$dass21_depression <- d %>% 
    dplyr::select(
      c("dass21_3", "dass21_5", "dass21_10", "dass21_13", "dass21_16", 
        "dass21_17", "dass21_21")
    ) |> 
    rowSums()
  
  # DASS-21 subscales and subject code
  dass21_scales <- d |> 
    dplyr::select(user_id, dass21_stress, dass21_anxiety, dass21_depression)
  
  return(dass21_scales)
}


