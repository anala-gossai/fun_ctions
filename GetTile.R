GetTile <- function(x, n) {
    # Summary: function to generate percentiles
    # based on probabilities and range of values
    
    # Arg: 
    #   x: numeric vector whose sample quantiles are wanted
    #   n: the number of percentiles of interest
    
    # Output
    #   The percentiles for the x of interest
    
    # Example:
    # GetTile(1:100, 3)
    require(stringr)
    probabilities <- seq(0 + 1/n, 1 - 1/n, 1/n)
    
    quantiles_from_prob <- stats::quantile(x, probs = probabilities, na.rm = TRUE)
    
    n_seq <- seq(1, n - 1, 1)
    
    # generate the ifelse statement
    ifelse_nested <- 
        str_c(str_c("ifelse (x <= ", quantiles_from_prob, ", ", n_seq, collapse = ", "), 
              ", ", n, str_c(rep(")", times = n - 1), collapse = ""))
    
    eval(parse(text = ifelse_nested))
}
