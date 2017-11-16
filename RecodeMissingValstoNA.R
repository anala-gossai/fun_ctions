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
  
  # Output: a data frame with the missing values coded as NA,
  # with the same factor levels maintained 
  
  # Example:
  # library(dplyr)
  # race <- c("A", "A", "B", "Unknown", "C", "D")
  # agegroup <- c("10-20", "20-30", "Unknown/missing", "Unknown/missing", "65-75", "35-45")
  # demographics <- cbind(race, agegroup) %>% 
  #     as.data.frame()
  # RecodeMissingValstoNA(data = demographics,
  #                       column_names = c("race", "agegroup"),
  #                       missing_vals = c('Unknown', 'Unknown/missing'))    
  require(dplyr)  
  
  if(all(column_names == "ALL")) {
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
