uBlogSavePlot <- function(widget,
                          file_name) {
    # Summary: A function to specifically be used with 
    # the u_blog developed by Anala (https://github.com/anala-gossai/u_blog)
    # that takes and html widget, saves it to the appropriate
    # blog folder, and then resets the working directory. 
    
    # Arg:
    #   widget: the name of the widget object (can also
    #           be piped using %>%)
    #   file_name: the name of the file the widget is
    #              to be saved as (e.g., "plot")
    
    # Output: the HTML widget in Anala's static/figures
    # blog directory 
    
    # Example:
    # please see use cases at 
    # https://github.com/anala-gossai/u_blog
    require(htmlwidgets)
    currentWD <- getwd()
    
    setwd("/Users/analagossai/u_blog/static")
    
    if(dir.exists("figures") == FALSE) {
        dir.create("figures", 
                   showWarnings = FALSE)
    }
    
    setwd("figures")
    
    saveWidget(widget, 
               paste0(file_name, ".html"), 
               selfcontained = TRUE)
    
    setwd(currentWD)
}
