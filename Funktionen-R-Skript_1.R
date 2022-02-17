## Funktionen-R-Skript 1
## zu Aufgabe 3


# Datensatz zum ausprobieren:
# zunaechst mit einer metrischen (hier jetzt erstmal nur natuerliche Zahlen) und einer kategoriellen Spalte

set.seed(12)
# ich ergänze das Test-Data Frame um eine kategorielle Variable
test <- data.frame( met = sample(1:100 , 20 ), 
                    kat = sample( c("sehr gut","gut","schlecht"),20, replace=TRUE),
                    dich = sample(0:1, 20, replace = TRUE))
test


# Die Ausgabe mit cat, alternativ vielleicht in data.frame, liste oder matrix ?
sum_met <- function(x){
   AM <- mean(x)
   med <- median(x)
   var <- var(x)
   sd <- sd(x)
   quartile <- quantile(x, probs= c(0.25,0.75) )
   return({
   cat("\n Mittelwerte:\n","Arithmetische Mittel:",AM,"\n" ,"Median:",med,"\n","------------") 
   cat( "\n Streuungsmaße:\n","Varianz:",var,"\n","Standardabweichung:",sd,"\n","------------")
   cat("\n Quartile\n","0.25-Quartil:",quartile[1],"\n","0.75-Quartil:",quartile[2] )
   })
   }

sum_met(test$met)



