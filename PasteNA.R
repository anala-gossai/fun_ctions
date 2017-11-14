PasteNA <- function(...,
                    sep=", ") {
    # Summary: functon to paste together things in a vector 
    # without spaces and ignoring things that are NA
    
    # Arg: 
    #   ... the list of things that you'd like to paste together
    #       which doesn't have to be c() together
    #   sep: how you would like the list of things pasted together
    #        to be separated, if at all. The default separator is ,
    
    # Output: 
    #   A combined group of the provided elements as a single object
    
    # Example:
    #   paste3("a", NA, "n", NA, "a", NA, "l", "a", sep = "")
    
    L <- list(...)
    
    L <- lapply(L, function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^", sep, "|", sep, "$)"), "",
               gsub(paste0(sep, sep), sep,
                    do.call(paste, c(L, list(sep = sep)))))
    is.na(ret) <- ret == ""
    
    ret
}