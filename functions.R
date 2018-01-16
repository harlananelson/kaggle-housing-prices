find_fields <-function(d,target_type_of = 'character'){  
  #' Identify fields in a data frame of a selected type_of
  #' 
  #' @param d The data frame
  #' @param target_type_of The type_of for fields selected.
  t_types <- sapply(d,type_of)  
  sapply(d,'n_distinct') >= 2
  return(names(d)[t_types %in% target_type_of & sapply(d,'n_distinct') >= 2])
}
numeric_character <- function(d,num_distinct = 30){  
  #' Identify the fields that could be either nominal or interval.
  #'
  #' \code{numeric_character} If a field of type integer or double has less than num_distinct
  #' levels, it might be nominal and not interval
  #' @param d The data frame
  names(d)[(sapply(d,'n_distinct') <= num_distinct) & (sapply(d, type_of) %in% c('integer','double'))]
}
possible_class<-function(d,num_distinct = 30){
  #' Identify fields the could be nominal
  #' 
  #' @param d The data frame
  #' @param num_distinct Fields with less distinct levels might be nominal
  (sapply(d,'n_distinct') <= num_distinct) 
}

