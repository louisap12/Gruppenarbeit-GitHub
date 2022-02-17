dich_as_met <- function(x){
  return(as.numeric(factor(x)))
}

test <- data.frame( met = sample(1:100 , 20 ), 
                    kat = sample( c("sehr gut","gut","schlecht"),20, replace=TRUE),
                    dich = sample(0:1, 20, replace = TRUE))

dich_as_met(test$dich)
# [1] 2 1 2 1 1 2 1 1 2 2 1 1 1 1 1 2 1 2 2 1