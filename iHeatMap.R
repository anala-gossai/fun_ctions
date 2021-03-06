iHeatMap <- function(object, 
                     max_colour = "#18bc9c",
                     min_colour = "#FF8300",
                     ...) {
    # Summary: create an interactive heatmap 
    # of correlation values 
    
    # Arg: 
    #   object: a matrix of correlation values, 
    #           that range between -1 and 1 
    #   max_colour: the colour used for 1
    #   min_colour: the colour used for -1
    #   ... : other arguments passed to hchart
    #         from highcharter 
    
    # Output: An interactive heatmap 
    # displaying the correlation matrix 
    
    # Example:
    # set.seed(416905)
    # matrix <- matrix(runif(100, min = -1, max = 1), 
    #                  nrow = 10, 
    #                  ncol = 10)
    # colnames(matrix) <- letters[1:10]
    # rownames(matrix) <- letters[1:10]
    # iHeatMap(matrix)
    require(highcharter)
    require(tidyr)
    require(dplyr)
    
    df <- as.data.frame(object)
    is.num <- sapply(df, is.numeric)
    df[is.num] <- lapply(df[is.num], round, 2)
    dist <- NULL
    
    x <- y <- names(df)
    
    df <- tbl_df(cbind(x = y, df)) %>% 
        gather(y, dist, -x) %>% 
        mutate(x = as.character(x),
               y = as.character(y)) %>% 
        left_join(data_frame(x = y,
                             xid = seq(length(y)) - 1), 
                  by = "x") %>% 
        left_join(data_frame(y = y,
                             yid = seq(length(y)) - 1), 
                  by = "y")
    
    ds <- df %>% 
        select_("xid", "yid", "dist") %>% 
        list_parse2()
    
    fntltp <- JS("function(){
                 return this.series.xAxis.categories[this.point.x] + ' ~ ' +
                 this.series.yAxis.categories[this.point.y] + ': <b>' +
                 Highcharts.numberFormat(this.point.value, 2)+'</b>';
                 ; }")
    
    cor_colr <- list(list(0, min_colour),
                     list(0.5, '#F8F5F5'), # white is 0
                     list(1, max_colour))
    
    highchart() %>% 
        hc_chart(type = "heatmap") %>% 
        hc_xAxis(categories = y, title = NULL) %>% 
        hc_yAxis(categories = y, title = NULL) %>% 
        hc_add_series(data = ds) %>% 
        hc_plotOptions(
            series = list(
                boderWidth = 0,
                dataLabels = list(enabled = TRUE))) %>% 
        hc_tooltip(formatter = fntltp) %>% 
        hc_legend(align = "right", 
                  layout = "vertical",
                  margin = 0, 
                  verticalAlign = "top",
                  y = 25, 
                  symbolHeight = 280) %>% 
        hc_colorAxis(stops = cor_colr,
                     min = -1,
                     max = 1)
}
