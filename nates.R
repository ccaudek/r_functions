#' \code{scoring_nates()} generates the total score of the No Attachment to
#' Ego Scale
#' @param data.frame.
#' @return data.frame.
#' The first column has the subjects' ids; the following columns are the 7 
#' items of the NATES. The items must be named `nates_1`, ... `nates_7`.
scoring_nates <- function(d) {
  
  # d <- rio::import(here::here("data", "prep", "quest_scales", "nates_items.csv"))
  
  if (length(unique(d[, 1])) < 10) 
    stop("Error: the first column is not user_id!")
  
  d$nates_score <- d %>% 
    dplyr::select(-user_id) |> 
    rowSums() 
  
  nates_scale <- d |> 
    dplyr::select(user_id, nates_score)
  
  return(nates_scale)
}

# eof ----


