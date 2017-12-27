corrScatterPlot <- function(dataset,
                            x_col,
                            y_col,
                            x_axis_title = x_col,
                            y_axis_title = y_col,
                            axisTextSize = 10,
                            corr_method = "spearman") {
    # Summary: scatterplot of 2 continus variables with 
    # correlation metric displayed on the plot, as well 
    # as straight line of best fit through points 
    
    # Arg:
    #   dataset: data frame containing the columns for 
    #            each continuous variable for x and y
    #   x_col: name of the x column
    #   y_col: name of the y_column
    #   x_axis_title: label for the x axis; defaults to 
    #                 the variable name for x values 
    #   y_axis_title: label for the y axis; defaults to 
    #                 the variable name for y values
    #   axisTextSize: size of plot font; defaults to 10 pt
    #   corr_method: method used to calculate correlation; 
    #                defaults to Spearman's correlation 
    #                coefficient 
    
    # Output: Returns a scatterplot as well as 
    # the Spearman's rho with linear model line  
    
    # Example:
    # library(dplyr)
    # set.seed(416905)
    # data <- cbind(seq(1:1000),
    #               seq(1:1000)*rexp(1000)) %>%
    #     as.data.frame()
    # corrScatterPlot(dataset = data,
    #                 x_col = "V1",
    #                 y_col = "V2",
    #                 x_axis_title = "V 1",
    #                 y_axis_title = "V 2",
    #                 axisTextSize = 15)
    require(dplyr)
    require(corrplot)
    require(ggplot2)
    
    # Calculate correlation coefficient 
    n <- nrow(dataset)
    r <- as.numeric(cor.test(dataset[[x_col]], dataset[[y_col]], 
                             method = corr_method)$estimate) %>% 
        signif(2)
    spearmanLabel <- paste0("rho == ", r)
    
    plot.data <- 
        dataset %>% 
        select_(x_col,
                y_col) %>% 
        rename_("x_axis_values" = x_col,
                "y_axis_values" = y_col)
    
    pc <- predict(prcomp(~ x_axis_values + y_axis_values,
                         plot.data))[, 1]
    
    # Draw the scatterplot 
    ggplot(plot.data, 
               aes(x = x_axis_values, 
                   y = y_axis_values,
                   color = pc)) +
        geom_abline(slope = 1, 
                    intercept = 0, 
                    color = "gray") +
        geom_point(shape = 16, 
                   size = 2, 
                   show.legend = FALSE, 
                   alpha = .4) +
        scale_color_gradient(low = "#32aeff", 
                             high = "#f2aeff") +
        geom_smooth(method = lm, 
                    se = TRUE,
                    color = '#5051DB') +
        xlab(x_axis_title) + 
        ylab(y_axis_title) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "#fafafa", colour = "grey"),
              plot.background = element_rect(colour = NA, fill = NA),
              axis.ticks = element_blank(),
              panel.border = element_blank(),
              axis.title = element_text(size = axisTextSize * 1.2, face = "bold"),
              plot.title = element_text(size = axisTextSize * 1.4, face = "bold"),
              axis.text.x = element_text(size = axisTextSize, face = "bold"),
              axis.text.y = element_text(size = axisTextSize, face = "bold")) + 
        annotate("text", 
                 x = 0.95*max(plot.data[["x_axis_values"]]), 
                 y = 0.9*min(plot.data[["y_axis_values"]]), 
                 parse = TRUE, 
                 label = as.character(spearmanLabel)) +
        theme(legend.position = "none")
}
