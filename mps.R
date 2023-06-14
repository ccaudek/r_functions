#' \code{scoring_mps()} generates the subscales of the Frost Multidimensional 
#' Perfectionism Scale (F-MPS). 
#' @param data.frame.
#' @return data.frame.
#' https://www.researchgate.net/profile/Caterina-Lombardo/publication/235767769_Italian_adaptation_of_the_multidimensional_perfectionism_scale_mpsAdattamento_italiano_della_multidimensional_perfectionism_scale_mps/links/00463536b7e0b30f6b000000/Italian-adaptation-of-the-multidimensional-perfectionism-scale-mps-Adattamento-italiano-della-multidimensional-perfectionism-scale-mps.pdf
#' La MPS misura 4 dimensioni. Di queste, due catturano
#' aspetti positivi del perfezionismo, la tendenza a porsi standard 
#' personali elevati (scala PS, Personal Standard) e la tendenza a 
#' preferire l’ordine e l’organizzazione (scala O, Organizzazione). 
#' Le altre due dimensioni colgono gli aspetti negativi del perfezionismo, 
#' ovvero la tendenza a preoccuparsi eccessivamente di commettere errori 
#' (scala CM, Concern over Mistakes –) e a porsi dubbi eccessivi sulle 
#' proprie azioni (scala D) e la presenza di genitori ipercritici e con 
#' aspettative eccessivamente elevate nei confronti dei figli.
scoring_mps <- function(d) {
  
  if (length(unique(d[, 1])) < 10) 
    stop("Error: the first column is not user_id!")
  
  # scala di Preoccupazioni per gli errori e Dubbi sulle azioni (CMD)
  d$mps_cmd <- with(
    d,
    mps_9 + mps_10 + mps_13 + mps_14 + mps_17 + mps_18 + mps_21 +
      mps_23 + mps_24 + mps_25 + mps_28 + mps_32 + mps_33 + mps_34
  )
  
  # scala di Standard personali elevati (PS)
  d$mps_ps <- with(
    d,
    mps_1 + mps_4 + mps_6 + mps_12 + mps_16 + mps_19 + mps_30
  )
  
  # scala di Aspettative e critiche genitoriali (PEPC)
  d$mps_pepc <- with(
    d,
    mps_3 + mps_5 + mps_11 + mps_15 + mps_20 + mps_22 + mps_26 +
      mps_35
  )
  
  # scala di Organizzazione (O)
  d$mps_or <- with(
    d,
    mps_2 + mps_7 + mps_8 + mps_27 + mps_29 + mps_31
  )
  
  d$mps_tot <- with(
    d,
    mps_cmd + mps_ps + mps_pepc + mps_or
  )
  
  mps_subscales <- d |> 
    dplyr::select(user_id, mps_cmd, mps_ps, mps_pepc, mps_or, mps_tot)
  
  return(mps_subscales)
}

