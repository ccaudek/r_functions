#' \code{scoring_bsq14()} generates the total score of the BSQ-14 Scale
#' @param data.frame.
#' @return data.frame.
#' The first column has the subjects' ids; the following columns are the 58 
#' items of the Tri-PM The items must be named `bsq14_1`, ... `bsq14_14`.
scoring_bsq14 <- function(d) {
  
  # d <- rio::import(here("data", "prep", "quest_scales", "bsq14_items.csv"))
  
  # BSQ-14 items.
  bsq14_items <- d %>% 
    dplyr::select(-user_id)
  
  d$bsq14_score <- rowSums(bsq14_items)
  # hist(d$bsq14_score)
  # mean(d$bsq14_score)
  
  # BSQ-14 subscales and subject code
  bsq14_scale <- d |> 
    dplyr::select(user_id, bsq14_score)
  
  bsq14_scale
}
