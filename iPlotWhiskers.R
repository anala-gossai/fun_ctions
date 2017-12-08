iPlotWhiskers <- function(data, 
                          cts_vars, 
                          group_var,
                          x_axis_label = gsub('_|\\.', ' ', group_var),
                          y_axis_label = gsub('_|\\.', ' ', cts_vars),
                          colour_scheme_hex = "Auto") {
    # Summary: show whiskers plot of a continuous 
    # variable by a categorical variable
    
    # Arg: 
    #   data: the dataset containing a continuous variable
    #         as well as a categorical variable
    #   cts_vars: the continuous variable(s) of interest
    #   group_var: the categorical variable to divide up
    #            the continuous variable
    #   x_axis_label: the label related to what group_var represents
    #                 to display along x-axis; defaults to cleaner
    #                 version of group_var
    #   y_axis_label: the label related to what cts_var represents
    #                 to display along y-axis; defaults to cleaner
    #                 version of cts_var
    #   colour_scheme_hex: hex colours of preference; has default
    #                      colour scheme available 
    
    # Output: an interactive whiskers plot of the 
    # continuous variable for each strata of the categorical 
    # variable
    
    # Example:
    # data(iris)
    # iPlotWhiskers(data = iris,
    #               cts_vars = c("Petal.Length", "Petal.Width"),
    #               group_var = "Species",
    #               y_axis_label = "Length (cm)")
    require(dplyr) # Version ‘0.5.0’
    require(highcharter) # Version ‘0.4.0’
    
    cts_var_vals <- unique(cts_vars)
    
    if (colour_scheme_hex == "Auto") {
        
        colour_scheme = colorRampPalette(
            c("#242582",
              "#552D67",
              "#F64C72",
              "#99738E",
              "#05386B",
              "#190061",
              "#3500D3")
        )(length(cts_var_vals))
    } else {
        
        colour_scheme = colorRampPalette(
            colour_scheme_hex
            )(length(cts_var_vals))
    }
    
    chart <- highchart()
    
    for (i in 1:length(cts_var_vals)) {
        cts_var <- cts_var_vals[i]
        
        chart <- chart %>% 
            hc_add_series_boxplot(data[, cts_var], 
                                  data[, group_var],
                                  name = cts_var,
                                  color = colour_scheme[i]) %>%
            hc_legend(itemHoverStyle =  list(color = '#FF8300'))
    }
    
    chart %>% 
        hc_xAxis(title = list(text = x_axis_label)) %>% 
        hc_yAxis(title = list(text = y_axis_label))
}
