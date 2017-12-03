iPlotBar <- function(data, 
                     cat_var, 
                     group_var, 
                     x_axis_label = list(text = gsub('_|\\.', ' ', cat_var)), 
                     y_axis_label = list(text = 'N'),
                     colour_scheme_hex = "Auto") {
    # Summary: show bar plot for a categorical variable 
    
    # Arg:
    #    data: the dataset containing a categorical variable to be counted
    #          as well as a categorical variable  to group by 
    #    cat_var: the categorical to be counted (e.g., age group)
    #    group_var: the categorical variable to group
    #               the other categorical variable (e.g., gender)
    #    x_axis_label: the label related to what cat_var represents
    #                  to display along x-axis; defaults to cleaner
    #                  version of cat_var
    #    y_axis_label: title on the y axis, defaults to 'N' (i.e., counts)
    #   colour_scheme_hex: hex colours of preference; has default
    #                      colour scheme available 
    
    # Output: a bar plot of the categorical variable for each strata of the
    #         other categorical variable of interest 
    
    # Example
    # set.seed(416905)
    # library(dplyr)
    # age <- sample(c("10-15y", "16-20y", "21-30y", "31-40y"), 100, replace = TRUE)
    # gender <- sample(c("male", "female", "unknown"), 100, replace = TRUE)
    # demographics <- cbind(age, gender) %>%
    #     as.data.frame() %>%
    #     filter(!(gender == "female" & age == "10-15y"))
    # iPlotBar(data = demographics,
    #          cat_var = "age",
    #          group_var = "gender")
    require(dplyr) # Version ‘0.5.0’
    require(highcharter) # Version ‘0.4.0’
    
    group_var_vals <- sort(unique(data[[group_var]]))
    
    bar.data <- data %>%
        group_by_(group_var, cat_var) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        data.frame()
    
    all.combo <- bar.data %>% 
        select_(group_var, cat_var) %>% 
        expand.grid() %>% 
        unique()
    
    bar.data.n <- full_join(bar.data, all.combo) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        arrange_(group_var, cat_var)
    
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
        
        colour_scheme = colour_scheme_hex
    }
    
    chart <- highchart()
    
    counter = 1
    
    for (group_var_val in group_var_vals) {
        series <- bar.data.n %>%
            mutate_(var = group_var) %>%
            filter(var == group_var_val) %>%
            select_("n") %>% 
            unlist() %>% 
            as.numeric()
        
        series <- series[!is.na(series)]
        chart <- chart %>%
            hc_chart(type = "column") %>% 
            hc_add_series(data = series,
                          name = group_var_val, 
                          area = TRUE, 
                          color = colour_scheme[[counter]],
                          fillOpacity = 0.1,
                          lineWidth = 3)
        counter = counter + 1
    }
    
    chart %>%
        hc_xAxis(title = x_axis_label, 
                 categories = sort(unique(data[[cat_var]]))) %>%
        hc_yAxis(title = y_axis_label) %>%
        hc_legend(itemHoverStyle =  list(color = '#FF8300'))
}
