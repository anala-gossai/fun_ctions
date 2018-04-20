rhoConfIntZ <- function(rho,
                        N,
                        confidence = 95) {
    # Summary: 95% CI for the Spearman’s correlation coefficient 
    # will be calculated using Fisher’s z-transformation 
    # on the Spearman’s rho
    
    # Arg:
    #   rho: Spearman's rho (e.g., as calculated using 
    #        cor.test(..., method = "spearman"))
    #   N: sample size 
    #   confidence: the confidence level desired as a percentage;
    #               defaults to 95% confidence levels (numeric
    #               value required)
    
    # Output: 2 numbers - the upper and lower bounds of the
    # confidence intervals about the Spearman's rho estimate 
    
    # Example
    # set.seed(416905)
    # n = 100
    # x <- runif(n, 0, 200)
    # y <- x + runif(n, 0, 50)
    # r <- as.numeric(cor.test(x, y, method = "spearman")$estimate)
    # rhoConfIntZ(rho = r,
    #            N = n)
    zr                          <- atanh(rho)
    std.err                     <- sqrt(1/(N-3))
    p                           <- (100 - confidence)/2/100
    conf.p                      <- qnorm(p = p, 
                                         mean = 0, 
                                         sd = 1, 
                                         lower.tail = FALSE)
    lo.conf.lim                 <- tanh(zr - (std.err*conf.p))
    hi.conf.lim                 <- tanh(zr + (std.err*conf.p))
    confidence.intervals        <- c(lo.conf.lim, hi.conf.lim)
    names(confidence.intervals) <- c(
        paste0("Lower ", confidence, "% CI"),
        paste0("Upper ", confidence, "% CI")
        )
    return(confidence.intervals)
}
