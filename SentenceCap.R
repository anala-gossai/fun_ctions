SentenceCap <- function(s) {
  # Summary: Capitalize the first letter
  # of a string of characters as you might
  # want in a sentence. 
  
  # Arg: 
  #   s: a string of characters
  
  # Output: The same string of characters, s, 
  # with first letter capitalized 
  
  # Example:
  # stringz <- "hi, I'm A BUNny" 
  # SentenceCap(stringz) # "Hi, i'm a bunny"
  sub("(.)", ("\\U\\1"), tolower(s), pe = TRUE)
}
