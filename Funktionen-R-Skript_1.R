## Funktionen-R-Skript 1
## zu Aufgabe 3


# Datensatz zum ausprobieren:
# zunaechst mit einer metrischen (hier jetzt erstmal nur natuerliche Zahlen) und einer kategoriellen Spalte


set.seed(12)
# ich ergänze das Test-Data Frame um eine kategorielle Variable

k <- as.factor( sample( c("sehr gut","gut","schlecht"),20, replace=TRUE))
k <- ordered( k , levels = c("schlecht", "gut", "sehr gut") )

# ich ergänze das Test-Data Frame um eine kategorielle Variable
test <- data.frame( met = sample(1:100 , 20 ), 
                    kat = k,
                    dich = sample(0:1, 20, replace = TRUE))
test



# a)
sum_met <- function(x){
   AM <- mean(x)
   med <- median(x)
   var <- var(x)
   sd <- sd(x)
   quartile <- quantile(x, probs= c(0.25,0.75) )
   return( list( "Arithmetische Mittel" = AM, "Median" = med, "Varianz" = var, "Standardabweichung" = sd,
                 "Quartile"= quartile) )
   }

sum_met(test$met)

# b)
kat <- function(x){
  Modalwert <- names(table(x)[table(x) == max(table(x))]) ##Auspraegung, die am haeufigsten vorkommt
  Maximum <- max(x) 
  Minimum <- min(x)
  Spannweite <- length(unique(x)) ##Anzahl der verschiedenen Auspraegungen der Variablen
  uebersicht_kat <- list("Modalwert" = Modalwert, "Maximum" = Maximum, "Minimum" = Minimum, "Spannweite" = Spannweite) ##bringe Werte in eine Liste
  return(uebersicht_kat) ##gibt die Liste aus
}
kat(test$kat)

# c)
sum_kor <- function(x,y){
  if( (is.factor(x) | is.factor(y)) != TRUE){ stop("Die Variablen muessen kategoriell sein.") }
  if( length(x) != length(y) ){ stop("Beide Objekte muessen dieselbe Laenge haben.")}
  kor <- cor( as.numeric(x), as.numeric(y) )
  kov <- cov( as.numeric(x), as.numeric(y) )
  return( list( Kovarianz = kov, Korrelation = kor) )
}



# d)
met_dich <- function(x,y){
  # x soll die metrische Variable sein
  # y soll die dichotome Variable sein
  y <- dich_as_met(y)
  return(list("Korrelation" = cor(x,y), "Kovarianz" = cov(x,y), 
              "Lineares Modell" = lm(y~x)))
}

met_dich(test$met, test$dich)






