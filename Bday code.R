x <- c(801016016025, 20401025, 190501014)

print( 
  gsub(" ", "", sapply(strsplit(paste(x),'0'), function(y) paste(LETTERS[as.numeric(y)], collapse = ' ')), fixed = TRUE)
)
