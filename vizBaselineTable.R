multiggPlot <- function(..., 
                        cols = 1, 
                        layout = NULL,
                        plotlist = NULL) {
  # Summary: a function to print multiple ggplots
  # on a single page 
  
  # Args:
  #   ...:      ggplot objects to be combined in order listed
  #   cols:     Number of columns in layout; defaults to 1 column
  #   layout:   A matrix specifying the layout
  #             If present, 'cols' is ignored
  #   plotlist: list of ggplot objects to be plotted 
  
  # Output: 
  #   A series of ggplots in the arrangement specified by the user. 
  #   If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  #   then plot 1 will go in the upper left, 2 will go in the upper right, and
  #   3 will go all the way across the bottom.

  # Reference: Thanks to
  # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ !
  
  # Example:
  # library(ggplot2)
  # data(iris)
  # plot1 <- ggplot(data = iris, 
  #                 aes(x = Sepal.Length, 
  #                     y = Sepal.Width)) +
  #   geom_point(aes(color = Species, 
  #                  shape = Species)) + 
  #   geom_smooth(color = "darkgray") +
  #   theme_minimal()
  # 
  # plot2 <- ggplot(data = iris, 
  #                 aes(x = Petal.Length, 
  #                     y = Petal.Width)) +
  #   geom_point(aes(color = Species, 
  #                  shape = Species)) + 
  #   geom_smooth(color = "darkgray") +
  #   theme_minimal()
  # 
  # plot3 <- ggplot(data = iris,
  #                 aes(x = Species)) +
  #   geom_bar() +
  #   theme_minimal()
  # 
  # multiggPlot(plot2, plot1, plot3,
  #             layout = matrix(c(1,1,2,3), nrow=2, byrow=TRUE))
  require(dplyr)
  require(grid)
  
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols), 1),
                     ncol = cols, 
                     nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    
      suppressMessages(
          suppressWarnings(
              print(plots[[1]]
                    )))
    
  } else {
    
    # Set up the page for multiple plots
    grid.newpage()
    pushViewport(viewport(
      layout = grid.layout(nrow(layout), 
                           ncol(layout)))
      )
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      suppressMessages(suppressWarnings(
          print(plots[[i]], 
                vp = viewport(layout.pos.row = matchidx$row,
                              layout.pos.col = matchidx$col))))
    }
  }
}

#............................................................................#

vizBaselineTable <- function(df, 
                             varnames = names(df), 
                             labels = NULL, 
                             layout = NULL, 
                             ncols = 2, 
                             groupvar = NULL) {
  # Summary: produces a visualization of baseline table variables
  
  # Args:
  #   df: One of two vectors whose sample covariance is to be calculated.
  #   varnames: The other vector. x and y must have the same length, greater than one,
  #   labels: x-axis label. if present, must equal the length of varnames
  #   layout: matrix of desired layout; if present, ncols is ignored, 
  #            e.g., matrix(c(1:4), nrow = 2, ncol = 2) will produce a 2 x 2 grid
  #   ncols: # of columns desired in output
  #   groupvar: variable name to use in grouping 
  
  # Output:
  #   A matrix of ggplots whose size is determined by the user input
  #   In histograms of continuous variables, a blue line represents the mean
  #   and the red line represents the median
  
  # Example:
  # data(iris)
  # library(dplyr)
  # df <- iris %>%
  #   mutate(Random = sample(c(rep("A", 10), rep("B", 5), "C"),
  #                          nrow(iris),
  #                          replace = TRUE))
  # vizBaselineTable(df, varnames = names(df)[-(length(names(df))) + 1],
  #                  groupvar = "Species",
  #                  layout = matrix(c(1, 1, 2, 3, 4, 5), 3, 2, byrow = T))
  # vizBaselineTable(df, varnames = names(df)[-(length(names(df))) + 1],
  #                  groupvar = "Species")
  # vizBaselineTable(df, varnames = names(df),
  #                  ncols = 2)
  # vizBaselineTable(df, varnames = names(df),
  #                  layout = matrix(c(1,2,3,4,5,6), 3, 2, byrow = T))
  require(dplyr)
  require(ggplot2)
  l <- list()
  
  for (i in 1:length(varnames)) {
    
    varclass <- class(df[[varnames[i]]])
    
    if (varclass %in% c("numeric", "integer", "difftime")) {
      
      if (length(groupvar) == 0) {
        
        mu <- mean(df[[varnames[i]]])
        M <- median(df[[varnames[i]]])
        
        l[[i]] <- ggplot(data = df, 
                         aes_string(x = varnames[i])) + 
          geom_histogram(fill = "#242582", 
                         color = "darkgray")  +
          geom_vline(xintercept = mu, 
                     color = "#99738E", 
                     lwd = 1.75, 
                     linetype = 2) +
          geom_vline(xintercept = M, 
                     color = "#F64C72", 
                     lwd = 1.75, 
                     linetype = 3) + 
          theme_minimal()
        
      } else {
        
        l[[i]] <- ggplot(data = df,
                         aes_string(x = varnames[i],
                                    group = groupvar,
                                    fill = groupvar)) +
          geom_histogram(position = "identity",
                         alpha = 0.5, 
                         binwidth = 0.25,
                         color = "gray") +
          scale_fill_manual(values = c("#242582",
                                       "#552D67",
                                       "#F64C72",
                                       "#99738E",
                                       "#05386B",
                                       "#190061",
                                       "#3500D3")) +
          theme_minimal()
      }

      if (length(labels) > 0) { l[[i]] <- l[[i]] + xlab(labels[i]) }
    
    } else {
        
        if (length(groupvar) == 0) {
          
          l[[i]] <- ggplot(data = df, 
                           aes_string(x = varnames[i])) + 
            geom_bar(fill = "#242582", 
                     color = "darkgray") + 
            theme_minimal()
          
        } else {
  
          l[[i]] <- ggplot(data = df) + 
            geom_bar(aes_string(x = varnames[i], 
                                fill = groupvar), 
                     position = "dodge") +
            scale_fill_manual(values = c("#242582",
                                         "#552D67",
                                         "#F64C72",
                                         "#99738E",
                                         "#05386B",
                                         "#190061",
                                         "#3500D3")) +
            theme_minimal()
        }
      
      if (length(labels) > 0) { l[[i]] <- l[[i]] + xlab(labels[i]) }
    }
  }
  
  suppressMessages(
      suppressWarnings(
          multiggPlot(plotlist = l, 
                      layout   = layout, 
                  cols     = ncols)
  ))
}
