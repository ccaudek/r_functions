
scoring_dass_21 <- function(d) {
  
  # Convert factors to character
  dat <- d %>% 
    mutate(across(where(is.factor), as.character))

  # Select only the numeric columns of the items.
  items <- dat %>% 
    dplyr::select_if(is.numeric)
  
  n_items <- dim(items)[2]
  
  items_names <- sprintf("DASS_%s", seq(1:n_items))
  colnames(items) <- items_names
  
  # DASS-21: Stress subscale
  temp_s <- items %>% 
    dplyr::select(
      c("DASS_1", "DASS_6", "DASS_8", "DASS_11", "DASS_12", "DASS_14", "DASS_18")
    )
  items$stress <- rowSums(temp_s)
  
  # DASS-21: Anxiety subscale
  temp_a <- items %>% 
    dplyr::select(
      c("DASS_2", "DASS_4", "DASS_7", "DASS_9", "DASS_15", "DASS_19", "DASS_20")
    )
  items$anxiety <- rowSums(temp_a)
  
  # DASS-21: Depression subscale
  temp_d <- items %>% 
    dplyr::select(
      c("DASS_3", "DASS_5", "DASS_10", "DASS_13", "DASS_16", "DASS_17", "DASS_21")
    )
  items$depression <- rowSums(temp_d)
  
  # Select subj_code (assume that only one column is of type character)
  temp <- dat %>% 
    dplyr::select(where(is.character))
  
  items$subj_code <- temp$subj_code
  
  items
    
}


