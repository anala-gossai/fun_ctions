corStars <- function(m,
                     method = "spearman") {
    # Summary: A function to output a correlation
    # matrix for correlations between continuous variables 
    
    # Arg:
    #   m: matrix of continuous variables
    #   method: the type of correlation to compute 
    #           (i.e., 'pearson' or 'spearman'); defaults
    #           to spearman
    
    # Output: a data frame with the lower half of the 
    # the triangle denoting signficance level of the 
    # correlations with stars, a full matrix of 
    # the correlation p-values and point estimates. 
    
    # Example: 
    # library(dplyr)
    # data(iris)
    # rp <- corStars(iris %>%
    #                    select(-Species))
    # View(rp$p.value.level)
    # View(rp$p.value)
    # View(rp$corr)
    require(Hmisc) # Version ‘4.0.3’
    require(corrplot) # Version ‘0.77’
    
    m <- as.matrix(m)
    
    r <- rcorr(m, type = c(method))$r
    p <- rcorr(m, type = c(method))$P
    
    # note the level of significance 
    mystars <- ifelse(p < .001, "***", 
                      ifelse(p < .01, "** ", 
                             ifelse(p < .05, "* ", " ")))
    Rnew <- matrix(paste(mystars), ncol = ncol(m)) 
    diag(Rnew) <- paste(diag(p), " ", sep="") 
    rownames(Rnew) <- colnames(m) 
    colnames(Rnew) <- paste(colnames(m), "", sep="") 
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew) 
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    
    object.list <- list(p.value.level = Rnew, 
                        p.value = p,
                        corr = r)
}
