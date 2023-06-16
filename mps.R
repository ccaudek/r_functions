#' \code{scoring_fmps(d)} generates the subscales of the Frost Multidimensional
#' Perfectionism Scale (F-MPS).
#'
#' La MPS misura 4 dimensioni. Di queste, due catturano
#' aspetti positivi del perfezionismo, la tendenza a porsi standard
#' personali elevati (scala PS, Personal Standard) e la tendenza a
#' preferire l’ordine e l’organizzazione (scala O, Organizzazione).
#' Le altre due dimensioni colgono gli aspetti negativi del perfezionismo,
#' ovvero la tendenza a preoccuparsi eccessivamente di commettere errori
#' (scala CM, Concern over Mistakes) e a porsi dubbi eccessivi sulle
#' proprie azioni (scala D) e la presenza di genitori ipercritici e con
#' aspettative eccessivamente elevate nei confronti dei figli.
#'
#' \url{https://www.researchgate.net/profile/Caterina-Lombardo/publication/235767769_Italian_adaptation_of_the_multidimensional_perfectionism_scale_mpsAdattamento_italiano_della_multidimensional_perfectionism_scale_mps/links/00463536b7e0b30f6b000000/Italian-adaptation-of-the-multidimensional-perfectionism-scale-mps-Adattamento-italiano-della-multidimensional-perfectionism-scale-mps.pdf}
#'
#' @param d A DataFrame
#' @details The input DataFrame should have the following structure:
#' - The first column should be `user_id`, representing the
#'   user identifier.
#' - The next 35 columns should contain the items of the F-MPS
#'   Inventory in a numeric format. Each item should be named
#'   `ffmps_1`, `ffmps_2`, ..., `ffmps_35`.
#' @returns A DataFrame with user_id user_id as the first
#' column, followed by the subscales of the F-MPS Scale.
#' @export
#' @examples
#' \code{dat <- scoring_cope(d)}
#' 

#'
scoring_fmps <- function(d) {
  suppressPackageStartupMessages({
    library("tidyverse")
    library("rio")
  })

  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }

  # debugging for the groundhog_day project
  # d <- rio::import(here::here("data", "prep", "quest_scales", "fmps_items.csv"))

  # scala di Preoccupazioni per gli errori e Dubbi sulle azioni (CMD)
  d$fmps_cmd <- with(
    d,
    fmps_9 + fmps_10 + fmps_13 + fmps_14 + fmps_17 + fmps_18 + fmps_21 +
      fmps_23 + fmps_24 + fmps_25 + fmps_28 + fmps_32 + fmps_33 + fmps_34
  )

  # scala di Standard personali elevati (PS)
  d$fmps_ps <- with(
    d,
    fmps_1 + fmps_4 + fmps_6 + fmps_12 + fmps_16 + fmps_19 + fmps_30
  )

  # scala di Aspettative e critiche genitoriali (PEPC)
  d$fmps_pepc <- with(
    d,
    fmps_3 + fmps_5 + fmps_11 + fmps_15 + fmps_20 + fmps_22 + fmps_26 +
      fmps_35
  )

  # scala di Organizzazione (O)
  d$fmps_or <- with(
    d,
    fmps_2 + fmps_7 + fmps_8 + fmps_27 + fmps_29 + fmps_31
  )

  d$fmps_tot <- with(
    d,
    fmps_cmd + fmps_ps + fmps_pepc + fmps_or
  )

  fmps_subscales <- d |>
    dplyr::select(
      user_id, fmps_cmd, fmps_ps, fmps_pepc, fmps_or, fmps_tot
    )

  return(fmps_subscales)
}
