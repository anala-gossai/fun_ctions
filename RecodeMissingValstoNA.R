RecodeMissingValstoNA <- function(data,
                                  column_names = "ALL",
                                  missing_vals = "Unknown") {
  # Summary: Takes a data frame and recodes 
  # the missing values as NA
  
  # Arg:
  #   data: The data frame for which you'd like to replace 
  #         missing values with NA
  #   column_names: The names of the columns in the data frame
  #                 for which you'd like to replace the missing values 
  #                 with NA. Defaults to all columns in the dataset 
  #   missing_vals: a list of the ways in which missing values are
  #                 coded as missing. Defaults to "Unknown"
  
  # Example:
  # RecodeMissingValstoNA(data = demographics,
  #                       column_names = c("race", "agegroup"),
  #                       missing_vals = c('Unknown', 'Unknown/missing'))    

  # Output: a data frame with the missing values coded as NA,
  # with the same factor levels maintained 
  if(column_names == "ALL") {
    column_names = colnames(data)
  }
  
  for (col in column_names) {
    
    levelz <- NULL
    
    if(class(data[[col]]) == "factor") {
      levelz <- levels(data[[col]]) 
      levelz <- levelz[!(levelz %in% missing_vals)]
    }
    
    data <- data %>% 
      rename_("column_interest" = col) %>% 
      mutate(column_interest = ifelse(column_interest %in% missing_vals, NA, as.character(column_interest)))
    
    if(!is.null(levelz)){
      data <- data %>% 
        mutate(column_interest = factor(column_interest, levels = levelz))
    }
    
    data <- data %>%
      rename_(.dots = setNames(list('column_interest'), col))
  }
  data
}
