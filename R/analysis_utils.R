# functions to generate dataset and variable summary tables


generate_summary_table_dataset <- function(dataset) {
  # Compute statistics for each column

  summary_df <- NULL

  for(column_name in names(dataset)) {
    column <- dataset[[column_name]]

    # Check if the column is numeric before calculating the statistics

    if(is.numeric(column)) {
      max_decimal_places <- max(sapply(column, function(x) {
        nchar(sub("^[^.]*\\.?", "", as.character(x)))
      }))
      # print(paste0("max decimal places for column '", column_name,"' = ", max_decimal_places))
      rounding_decimal_places <- max_decimal_places + 1
      format_string <- paste0("%.",rounding_decimal_places,"f")

      statistics_list <- list(
        variable = column_name,
        type = class(dataset[[column_name]]),
        missing_num = sum(is.na(dataset[[column_name]])),
        missing_pct = paste0(sprintf("%.f",round(sum(is.na(dataset[[column_name]]))/length(dataset[[column_name]]),2)*100),'%'),
        unique_values = length(unique(dataset[[column_name]])),
        mean = sprintf(format_string, mean(column, na.rm = TRUE)),
        median = sprintf(format_string, median(column, na.rm = TRUE)),
        mode = mode(column),
        variance = sprintf(format_string, var(column, na.rm = TRUE)),
        std_deviation = sprintf(format_string, sd(column, na.rm = TRUE)),
        range = diff(range(column, na.rm = TRUE)),
        min = min(column, na.rm = TRUE),
        max = max(column, na.rm = TRUE),
        iqr = IQR(column, na.rm = TRUE),
        skewness = sprintf("%.3f",skewness(column, na.rm = TRUE)),
        kurtosis = sprintf("%.3f",kurtosis(column, na.rm = TRUE) - 3)  # Excess kurtosis
      )
      # print(paste0("adding ", column_name," as numeric"))
    } else {
      statistics_list <- list(
        variable = column_name,
        type = class(dataset[[column_name]]),
        missing_num = sum(is.na(dataset[[column_name]])),
        missing_pct = paste0(sprintf("%.f",round(sum(is.na(dataset[[column_name]]))/length(dataset[[column_name]]),2)*100),'%'),
        unique_values = nlevels(dataset[[column_name]]),
        mean = NA,
        median = NA,
        mode = mode(column),
        variance = NA,
        std_deviation = NA,
        range = NA,
        min = NA,
        max = NA,
        iqr = NA,
        skewness = NA,
        kurtosis = NA
      )
      # print(paste0("adding ", column_name," as non-numeric"))
    }

    summary_df <- rbind(summary_df, statistics_list %>% as.data.frame)
  }

  return(summary_df)
}

generate_summary_table_variable <- function(dataset, variable_of_interest, indep_var=NA, stratify_var=NA, stratify_flag=NA, destination="screen"){

  variable_of_interest_type = class(dataset[[variable_of_interest]])
  summary_table_dataset <- generate_summary_table_dataset(dataset)

  if(variable_of_interest_type %in% c("numeric","integer")){
    # draw from  the summary table that has already been created for display on the first tab of dataset exploration
    # filter for that variable and transpose the row to a column
    summary_table_variable <- summary_table_dataset %>% filter(variable == variable_of_interest) %>% t %>% as.data.frame %>%
      rownames_to_column(var="Element") %>% setNames(c("Element","Value")) %>% mutate(Element = str_to_title(gsub("_"," ",Element)))

    # Create a table with basic statistics
    summary_table_variable <- flextable::qflextable(summary_table_variable) %>%
      flextable::set_table_properties(width = .5, align = "center")
  }
  else if (variable_of_interest_type %in% c("factor","logical")){
    # draw from  the summary table that has already been created for display on the first tab of dataset exploration
    # filter for that variable and transpose the row to a column
    summary_table_variable <- summary_table_dataset %>% filter(variable == variable_of_interest) %>%
      select(variable, type, missing_num, missing_pct, unique_values, mode) %>%
      mutate(categories = cat(levels(dataset[[variable_of_interest]]), sep="\n")) %>% t %>% as.data.frame %>%
      rownames_to_column(var="Element") %>% setNames(c("Element","Value")) %>% mutate(Element = str_to_title(gsub("_"," ",Element)))

    # Create a table with basic statistics
    summary_table_variable <- flextable::qflextable(summary_table_variable) %>%
      flextable::set_table_properties(width = .5, align = "center")
  }

  return(summary_table_variable)
}
