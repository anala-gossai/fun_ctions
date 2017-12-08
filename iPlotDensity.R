iPlotDensity <- function(data,
                         cts_var, 
                         group_var,
                         x_axis_label = gsub('_|\\.', ' ', cts_var),
                         colour_scheme_hex = "Auto") {
    # Summary: show density plot of a continuous variable
    
    # Arg: 
    #   data: the dataset containing a continuous variable
    #         as well as a categorical variable 
    #   cts_var: the continuous variable of interest (e.g., age)
    #   group_var: the categorical variable to group
    #              the continuous variable (e.g., gender)
    #   x_axis_label: the label related to what cts_var represents
    #                 to display along x-axis; defaults to cleaner
    #                 version of cts_var
    #   colour_scheme_hex: hex colours of preference; has default
    #                      colour scheme available 
    
    # Output: a density plot of the continuous variable 
    #         for each strata of the categorical variable
    
    # Example: 
    # set.seed(416905)
    # library(dplyr)
    # age <- rnorm(100, 18, 2)
    # gender <- sample(c("male", "female", "unknown"), 100, replace = TRUE)
    # demographics <- cbind(age, gender) %>%
    #     as.data.frame() %>%
    #     mutate(age = as.numeric(age))
    # iPlotDensity(data = demographics,
    #              cts_var = "age",
    #              group_var = "gender")
    require(dplyr) # Version ‘0.5.0’
    require(highcharter) # Version ‘0.4.0’
    
    group_var_vals <- unique(data[[group_var]])
    
    if (colour_scheme_hex == "Auto") {
        
        colour_scheme = colorRampPalette(
            c("#242582",
              "#552D67",
              "#F64C72",
              "#99738E",
              "#05386B",
              "#190061",
              "#3500D3")
            )(length(group_var_vals))
    } else {
        
        colour_scheme = colorRampPalette(
            colour_scheme_hex
            )(length(group_var_vals))
    }
    
    chart <- highchart()
    
    counter = 1
    
    for (group_var_val in group_var_vals) {
        series <- data %>%
            mutate_(var = group_var) %>%
            filter(var == group_var_val) %>%
            select_(cts_var) 
        
        series <- series[!is.na(series)]
        chart <- chart %>%
            hc_add_series_density(density(series), 
                                  name = group_var_val, 
                                  area = TRUE, 
                                  color = colour_scheme[[counter]],
                                  fillOpacity = 0.1,
                                  lineWidth = 3)
        counter = counter + 1
    }
    
    chart %>%
        hc_xAxis(title = list(text = x_axis_label)) %>%
        hc_yAxis(title = list(text = 'density')) %>%
        hc_legend(itemHoverStyle =  list(color = '#FF8300'))
}
