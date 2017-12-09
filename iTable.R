iTable <- function(table_data, 
                   round_digits = 1,
                   simple = 'f',
                   page_length = 25,
                   header_colour = "#2c3e50",
                   header_text_colour = "#18bc9c") {
    # Summary: formats a dataframe into an html table, 
    # with rounded numbers as approriate
    
    # Arg: 
    #   table_data: dataframe object for which table should be based 
    #   round_digits: number of digits to show for numbers 
    #   simple: fancy (i.e., with scrolling, search) or simple table (default
    #           is a fancy table)
    #   page_length: number of table rows to show on one page
    #   header_colour: color to header background (defaults to flatly theme)
    #   header_text_colour: color of header text (defaults to flatly theme)
    
    # Output: a formatted datatable object with ability to interact
    
    # Example
    # library(dplyr)
    # data(mtcars)
    # iTable(mtcars)
    # mtcars %>% 
    #     mutate(brand = row.names(.)) %>% 
    #     select(brand, everything()) %>% 
    #     arrange(desc(hp)) %>% 
    #     iTable(page_length = 10)
    require(dplyr) # Version ‘0.5.0’
    require(DT) # Version ‘0.2’
    
    numeric.columns <- sapply(table_data, is.numeric)
    format.columns <- names(table_data[ , numeric.columns])
    
    if (simple == 't') {
        
        dt <- datatable(table_data, 
                        rownames = FALSE,
                        options = list(dom = 't', 
                                       initComplete = JS(
                                           "function(settings, json) {",
                                           paste0("$(this.api().table().header()).css({'background-color':'", header_colour,"'
                                           , 'color': '", header_text_colour, "'});"),
                                           "}"),
                                       pageLength = page_length),
                        escape = TRUE) 
    } else {
        
        dt <- datatable(table_data, 
                        rownames = FALSE, 
                        filter = 'bottom',
                        options = list(initComplete = JS(
                            "function(settings, json) {",
                            paste0("$(this.api().table().header()).css({'background-color':'", header_colour, "'
                            , 'color': '", header_text_colour, "'});"),
                            "}"), 
                            scrollX = TRUE, 
                            autoWidth = TRUE,
                            pageLength = page_length),
                        escape = TRUE)
    }
    
    dt %>%
        formatRound(columns = format.columns, 
                    round_digits)
}
