per_prod <- function(x){
  a <- as.numeric(unlist(strsplit(as.character(x),'')))
  b <- prod(a)
  b
}

persistence <- function(x){
y <- 0
repeat {
  x <- per_prod(x)
  y <- y + 1
  if (nchar(x) == 1){
    break
  }
}
y
}

ans <- data.frame(Number = 10:10000)
for (i in 1:nrow(ans)) {
  ans$Persistence[i] <- persistence(ans$Number[i])
}

ans_fin_2 <- ans[match(unique(ans$Persistence), ans$Persistence),]
colnames(ans_fin_2)[1] <- "Min_Number"

# Final answer
ans_fin <- data.frame(Number = 1:5)
for (k in 1:nrow(ans_fin)){
  ans_fin$min_persist[k] <- min(ans$Number[ans$Persistence==k])
}

