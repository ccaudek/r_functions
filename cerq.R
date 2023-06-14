#' \code{scoring_cerq()} generates the total score of the CERQ Scale
#' @param data.frame.
#' @return data.frame.
#' The first column must be `user_id`; the following columns are the 26 
#' items of the CERQ. The items must be named `cerq_1`, ... `cerq_36`.
#' 
#' Coding from:
# https://www.frontiersin.org/articles/10.3389/fpsyg.2017.02075/full

# Adaptive strategies: 
# 
# Positive refocusing
# Refocus on planning
# Positive reappraisal
# Putting into perspective

# Maladaptive strategies:
# 
# Acceptance
# Rumination
# Self-blame
# Catastrophizing
# Other-blame
scoring_cerq <- function(d) {
  
  # d <- rio::import(here::here("data", "prep", "quest_scales", "cerq_items.csv"))
  
  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }
  
  df <- d |> 
    dplyr::select(-user_id)
  
  # Create a vector to store the subscale sums
  subscale_sums <- matrix(NA, nrow = nrow(df), ncol = 9)
  
  # Iterate over each subscale
  for (i in 1:9) {
    # Calculate the sum of items for the current subscale
    start_index <- (i - 1) * 4 + 1
    end_index <- i * 4
    subscale_sums[, i] <- rowSums(df[, start_index:end_index])
  }
  
  cerq_subscales <- subscale_sums |> 
    as.data.frame()
  
  colnames(cerq_subscales) <- c(
    "self_blame", "acceptance", "rumination", "positive_refocusing", 
    "refocus_on_planning", "positive_reappraisal", "putting_into_perspective",
    "catastrophizing", "other_blame"
  )
  
  cerq_subscales$user_id <- d$user_id
  
  rio::export(
    cerq_subscales, 
    here::here("data", "prep", "quest_scales", "cerq_scores.csv")
  )
  
# eof ----
  
  
  
  
  
  
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


