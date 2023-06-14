#' \code{scoring_ius12()} generates the total score of the IUS-12 Scale
#' @param data.frame.
#' @return data.frame.
#' The first column has the subjects' ids; the following columns are the 58 
#' items of the Tri-PM The items must be named `ius_1`, ... `ius_12`.
#' https://iris.uniroma1.it/retrieve/e3835315-79da-15e8-e053-a505fe0a3de9/Lauriola_Hierarchical_2016.pdf

scoring_ius12 <- function(d) {
  
  # d <- rio::import(here("data", "prep", "quest_scales", "ius12_items.csv"))
  
  if (length(unique(d[, 1])) < 10) {
    stop("Error: the first column is not user_id!")
  }
  
  # Prospective IU items.
  prospective_iu <- d %>% 
    dplyr::select(
      ius_1, ius_2, ius_3, ius_4, ius_5, ius_6, ius_7
    )
  
  d$prospective_iu <- rowSums(prospective_iu)
  # hist(d$prospective_iu)
  # psych::alpha(clean_b)
  # mean(d$prospective_iu)
  
  # Inhibitory IU items
  inhibitory_iu <- d %>% 
    dplyr::select(
      ius_8, ius_9, ius_10, ius_11, ius_12
    )
  
  # presence of Inhibitory IU
  d$inhibitory_iu <- rowSums(inhibitory_iu)
  # hist(d$inhibitory_iu)
  # mean(d$inhibitory_iu)
  
  d$ius_tot <- d$prospective_iu + d$inhibitory_iu
  
  # IUS subscales and subject code
  ius_scale <- d |> 
    dplyr::select(user_id, inhibitory_iu, prospective_iu, ius_tot)
  
  return(ius_scale)
}
