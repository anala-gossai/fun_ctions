AddLabel <- function(data, 
                     column_dict) {
    # Summary: function to add a label to a column in a data frame
    # using the above dictionary
    
    # Arg:
    #   data: the data frame of interest
    #   column_dict: the dictionary holding the column
    #                names and their labels
    
    # Output
    #   The original data frame, with just a label 
    #   added to each column requsted
    
    # Example:
    # library(dplyr)
    # data(iris)
    # iris_dict <- list(
    #     "Sepal.Length" = list(label = "Sepal length (cm)"),
    #     "Sepal.Width" = list(label = "Sepal width (cm)")
    # )
    # AddLabel(data = iris,
    #          column_dict = iris_dict) %>% 
    #     View()
    require(dplyr) # Version ‘0.5.0’
    require(Hmisc) # Version ‘4.0.3’
    
    label_columns <- names(column_dict)  
    
    data <- as.data.frame(data)
    
    for(col in label_columns) {
        current_label <- column_dict[[col]]$label
        
        if(!is.null(col)) {
            
            Hmisc::label(data[, col]) <- current_label
            data <- data 
        }
    }
    return(data)
}
