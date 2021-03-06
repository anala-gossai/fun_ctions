ListAppend <- function(lst, 
                       obj) {
    # Summary: little function to grow a list
    
    # Arg
    #   lst: a list to which something can be added to
    #   obj: what you'd like to be added to the list
    
    # Output
    #   A list which has the original list 
    #   with the new item added to it
    
    # Example:
    # x <- NULL
    # x <- ListAppend(x, "a")
    # x <- ListAppend(x, "b")
    # x <- ListAppend(x, "c")
    # print(x)
    lst[[length(lst)+1]] <- obj
    return(lst)
}
