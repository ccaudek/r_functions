#' \code{scoring_rosenberg()} generates the total score of the Rosenberg
#' Self-Esteem Scale (RSES).
#' @param data.frame.
#' @return data.frame.
#' The first column is the user_id; the following columns are the 10
#' items of the RSES. The items must be named `ros_1`, ... `ros_10`.
#' The scale ranges from 0-30. Scores between 15 and 25 are within normal 
#' range; scores below 15 suggest low self-esteem. The Italian version 
#' responses are between 1 and 4 (the original response scale is between 0 
#' and 3).
scoring_rosenberg <- function(d) {
  
  # d <- rio::import(here::here("data", "prep", "quest_scales", "rosenberg_items.csv"))

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }

  # reversed items
  rses <- d %>%
    mutate(
      across(
        c(ros_3, ros_5, ros_8, ros_9, ros_10),
        ~ case_match(
          .,
          1 ~ 4L,
          2 ~ 3L,
          3 ~ 2L,
          4 ~ 1L
        )
      )
    )

  # Compute total score.
  rses <- rses %>%
    rowwise() %>%
    mutate(
      ros_tot = sum(c_across(ros_1:ros_10))
    )

  rses_scale <- data.frame(
    user_id = rses$user_id,
    rosenberg_score = rses$ros_tot
  )

  return(rses_scale)
}

# eof ----
