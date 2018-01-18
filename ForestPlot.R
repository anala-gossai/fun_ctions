ForestPlot <- function(dataz,
                       x_axis_labels,
                       y_axis_title,
                       point_estimate_label,
                       lower_bound_label,
                       upper_bound_label,
                       bottom_margin_size = 5.5,
                       left_margin_size = 4.1,
                       xlim_nudge = 0,
                       font_scale = 1) {
  # Summary: a function to show many point estimates
  # with some interval values as whishkers (e.g., plot
  # an odds ratio estimate with it's 95% confidence intervals
  # from logistic regression)
  
  # Arg:
  #    dataz: a dataset containing the risk, CI lower and upper
  #           bound, and labels for each model. Each row
  #           should be a different model that has OR, CI, and a label
  #    x_axis_labels: something to describe each model
  #    y_axis_title: Name of the risk estimate 
  #    point_estimate_label: the column name for the column with the risk estimate
  #    lower_bound_label: column name for lower CI bound
  #    upper_bound_label: column name for upper CI bound
  #    bottom_margin_size: lower par for margin, make bigger if have 
  #                        long labels, default provided
  #    left_margin_size: left par for margin, make bigger if have
  #                      long labels, default provided 
  #    xlim_nudge: to add any additional space on the x-axis
  #    font_scale: to increase or decrease the font size 
  
  # Output: A plot that shows the point estimate as a dot with CIs
  # as whiskers (y-axis), with the different estimates displayed along the 
  # x-axis with what the model is described by the labels.
  
  # Example:
  # set.seed(416905)
  # library(dplyr)
  # fake.data <- as.data.frame(runif(20, 1, 20)) %>%
  #     rename(or = `runif(20, 1, 20)`) %>%
  #     mutate(upper_bound = or + (runif(1, 1, 10)),
  #            lower_bound = or - (runif(1, 1, 15))) %>%
  #     mutate(model_number = paste("model", rownames(.))) %>%
  #     mutate_each(funs(ifelse(.<0, 0, .)))
  # ForestPlot(dataz                = fake.data,
  #            x_axis_labels        = "model_number",
  #            y_axis_title         = "Odds Ratio (95% CI)",
  #            point_estimate_label = "or",
  #            lower_bound_label    = "lower_bound",
  #            upper_bound_label    = "upper_bound",
  #            font_scale           = 0.4)
  mainPoint <- dataz[[point_estimate_label]]; 
  maincilo <- dataz[[lower_bound_label]]; 
  mainciup <- dataz[[upper_bound_label]]; 
  labelz <- dataz[[x_axis_labels]];
  x <- 1:length(mainPoint); 
  
  par(mar = c(bottom_margin_size, left_margin_size, 2.1, 2.1))
  plot(x, mainPoint, 
       xaxt = 'n', 
       pch = 15, 
       cex = 2*font_scale, 
       ylab = y_axis_title, 
       xlab = " ",
       ylim = c(sort(na.omit(subset(maincilo, maincilo>-Inf)))[1]*0.9, 
                (sort(na.omit(subset(mainciup, mainciup<Inf)), TRUE)[1]*1.1)), 
       xlim = c(1-.1, max(x)-xlim_nudge + .2), 
       cex.lab = 1.4*font_scale, 
       cex.axis = 1.8*font_scale)
  axis(1, at = x, labels = labelz, cex.axis = 1.6*font_scale, las = 2)
  abline(h = tryCatch(as.numeric(dataz %>% filter(sampling_fraction == 999) %>% select_(point_estimate_label)), error = function(e) NA))
  abline(h = tryCatch(as.numeric(dataz %>% filter(sampling_fraction == 999) %>% select_(lower_bound_label)), error = function(e) NA), lty = 2)
  abline(h = tryCatch(as.numeric(dataz %>% filter(sampling_fraction == 999) %>% select_(upper_bound_label)), error = function(e) NA), lty = 2)
  # infinity values
  abline(v = c(which(mainciup==Inf|is.na(mainciup))), lwd=1) 
  # CI around OR
  epsilon = 0.07
  for(i in 1:length(x)) {
    up = mainciup[i]
    low = maincilo[i]
    segments(x[i],low , x[i], up, lwd=2.5)
    segments(x[i]-epsilon, up , x[i]+epsilon, up, lwd=2.5)
    segments(x[i]-epsilon, low , x[i]+epsilon, low, lwd=2.5)
  }
  # Print values of OR and CI
  text(x + 0.3, mainPoint, labels = round(mainPoint, digits=2), cex=1.7*font_scale)
  text(x, (maincilo-0.5), labels = round(maincilo, digits=2), cex=1.5*font_scale, col="gray50")
  text(x, (mainciup+0.5), labels = round(mainciup, digits=2), cex=1.5*font_scale, col="gray50")
}
