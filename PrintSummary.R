PrintSummary <- function(data,
                         cat_var,
                         cts_var) {
    # Summary: create a summary table for a
    # continuous variable by another categorical
    # variable
    
    # Arg:
    #   data: the data frame the contains the grouping variable
    #         and the continuous variable we’re interested in
    #   cat_var: the grouping variable
    #   cts_var: the continuous variable
    
    # Output: a data frame that is grouped by the categorical variable
    #   of interest, and shows the mena, median, IQR,
    #   minimum, maximum of the continuous
    #   variable of interest
    
    # Example:
    # data(mtcars)
    # PrintSummary(mtcars, “vs”, “mpg”)
    require(dplyr)
    require(stats)
    
    data %>%
        mutate_(cts = cts_var) %>%
        group_by_(cat_var) %>%
        summarise(
            minimum          = min(cts,            na.rm = TRUE),
            `lower quartile` = quantile(cts, 0.25, na.rm = TRUE),
            median           = median(cts,         na.rm = TRUE),
            mean             = mean(cts,           na.rm = TRUE),
            `upper quartile` = quantile(cts, 0.75, na.rm = TRUE),
            maximum          = max(cts,            na.rm = TRUE)
        ) %>%
        ungroup()
}
