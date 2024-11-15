stratify_dataset <- function(dataset, stratify_flag, stratify_var){
  returnval <- dataset

  if(stratify_flag == T){
    if(class(returnval[[stratify_var]]) %in% c("factor","logical")){
      returnval %<>%
        #filter(!is.na(!!as.symbol(input$select_stratify))) %>%  ################### check this line, dont throw them out so quickly
        mutate(stratify_by = paste0(str_to_title(gsub("_"," ",stratify_var))," = ",get(stratify_var)))
    }
    else{
      returnval %<>% mutate(stratify_by = "No stratification")
    }
  }
  else{
    returnval %<>% mutate(stratify_by = "No stratification")
  }

  returnval %<>% mutate(stratify_by = as.factor(stratify_by))

  return(returnval)
}

df_column_datatype_autocast <- function(df){
  returnval <- df %>%
    mutate(across(everything(), ~ {
      if (is.POSIXct(.)) {
        if (any(hour(.) != 0, minute(.) != 0, second(.) != 0)) {
          .  # Keep it as POSIXct with a timestamp
        } else {
          # Add a midnight timestamp (00:00:00) to POSIXct with only a date component
          . <- ymd_hms(format(., "%Y-%m-%d 00:00:00"))
        }
      } else if (is.Date(.)) {
        . <- ymd_hms(format(., "%Y-%m-%d 00:00:00"))  # Convert Date to POSIXct with a midnight timestamp
      } else if (is.logical(.)) {
        as.factor(.)
      } else if (is.factor(.)) {
        .
      } else if (is.numeric(.) || is.integer(.)) {
        .
      } else if (is.character(.)) {
        # Check if the entire column can be cast to a numeric
        if (all(suppressWarnings(!is.na(as.numeric(.))))) {
          . <- as.numeric(.)  # Cast to numeric
        } else if (length(unique(.)) < 20) {
          . <- as.factor(.)  # Cast to factor
        } else { . }
      }
      .  # Leave it as is if none of the conditions match
    }))

  return(returnval)

}
