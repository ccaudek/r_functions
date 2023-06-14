#' \code{scoring_bdi2()} generates the total score of the BDI-2 Scale
#' @param data.frame.
#' @return data.frame.
#' The first column has the subjects' ids; the following columns are the 58 
#' items of the Tri-PM The items must be named `bdi2_1`, ... `bdi2_21`.
scoring_bdi2 <- function(d) {
  
  # d <- rio::import(here("data", "prep", "quest_scales", "bdi2_items.csv"))
  
  if (length(unique(d[, 1])) < 10) 
    stop("Error: the first column is not user_id!")
  
  # BDI-2 items.
  bdi2_items <- d %>% 
    dplyr::select(-user_id)
  
  d$bdi2_score <- rowSums(bdi2_items)
  # hist(d$bdi2_score)
  # mean(d$bdi2_score)
  
  # BDI-2 subscales and subject code
  bdi2_scale <- d |> 
    dplyr::select(user_id, bdi2_score)
  
  bdi2_scale
}
