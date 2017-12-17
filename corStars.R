corStars <- function(m,
                     method = "spearman") {
    
    require(Hmisc)
    require(corrplot)
    
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
