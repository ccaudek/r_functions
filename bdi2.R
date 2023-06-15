#' \code{scoring_bdi2(d)} generates the total score of the BDI-II Scale.
#'
#' @param d A DataFrame
#' @details The input DataFrame must have user_id as the first column,
#' followed by the 21 items of the BDI-II in numeric format. The items
#' must be named `bdi2_1`, ..., `bdi2_21`.
#' @returns A DataFrame with user_id as the first column and the BDI-II
#' scores.
#' @export
#' @examples
#' \code{dat <- scoring_bdi2(d)}
#'
scoring_bdi2 <- function(d) {
  # d <- rio::import(here("data", "prep", "quest_scales", "bdi2_items.csv"))

  if (length(unique(d[, 1])) < 10)
    stop("Error: the first column is not user_id!")

  # BDI-2 items.
  bdi2_items <- d %>%
    dplyr::select(-user_id)

  d$bdi2_score <- rowSums(bdi2_items)
  # hist(d$bdi2_score) # nolint
  # mean(d$bdi2_score) # nolint

  # user_id and BDI-2 score
  bdi2_scale <- d |>
    dplyr::select(user_id, bdi2_score)

  return(bdi2_scale)
}
