dich_as_met <- function(x){
  name <- unique(x)
  result <- x == name[which.min(nchar(name))]
  return(as.numeric(result))
}

test2 <- sample(c("ja", "nein"), 20, replace = TRUE)

dich_as_met(test2)
#  [1] 0 0 1 1 1 1 1 0 1 0 0 0 0 1 1 1 1 1 1 1