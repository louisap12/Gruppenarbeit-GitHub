## Funktionen-R-Skript 1
## zu Aufgabe 3


# Datensatz zum Erproben der Funktionen:

# Vorbereitungen fuer den Testdatensatz:
set.seed(12)

k <- as.factor( sample( c("sehr gut","gut","schlecht"),20, replace=TRUE))
k <- ordered( k , levels = c("schlecht", "gut", "sehr gut") )

# Zusammensetzung des Datensatzes:
test <- data.frame( met = sample(1:100 , 20 ), 
                    kat = k,
                    dich = sample(0:1, 20, replace = TRUE))
test



# a)
# Funktion die geeignete deskriptive Statistiken fuer metrische Variablen ausgibt:

sum_met <- function(x){ 
  if( is.numeric(x) != TRUE){ stop("Die Variable muss metrisch skaliert sein.") } # Warnmeldung falls Variable
   AM <- mean(x)                                                                  # nicht metrisch ist.
   med <- median(x)    # Hier werden alle Statistiken berechnet.
   var <- var(x)
   sd <- sd(x)
   quartile <- quantile(x, probs= c(0.25,0.75) )
   extrm <- c( Minimum = min (x), Maximum = max (x))
   return( list( "Arithmetische Mittel" = AM, "Median" = med, "Varianz" = var, "Standardabweichung" = sd,
                 "Quartile"= quartile, "Extremwerte" = extrm) )      # Alle benoetigten Groessen werden in einer Liste ausgegeben.
   }

# Kleine Probe um Funktion zu testen:
sum_met(test$met)


# b)
# Funktion die geeignete deskriptive Statistiken fuer kategorielle Variablen ausgibt:

kat <- function(x){
  if(is.character(x)){stop("Die Variable ist nicht kategoriell")}
  Modalwert <- names(table(x)[table(x) == max(table(x))]) ##Auspraegung, die am haeufigsten vorkommt
  Maximum <- max(x) 
  Minimum <- min(x)
  Spannweite <- length(unique(x)) ##Anzahl der verschiedenen Auspraegungen der Variablen
  uebersicht_kat <- list("Modalwert" = Modalwert, "Maximum" = Maximum, "Minimum" = Minimum, "Spannweite" = Spannweite) ##bringe Werte in eine Liste
  return(uebersicht_kat) ##gibt die Liste aus
}
kat(test$kat)


# c)
# Funktion zur Berechnung eines Zusammenhanges zwischen zwei kategoriellen Variablen:

kat_zsh <- function(x,y){
  # Zunaechst eine Bedingung, die die beiden Vektoren erfuellen sollen:
  if( length(x) != length(y) ){ stop("Beide Objekte muessen dieselbe Laenge haben.")}
  kor <- cor( as.numeric(x), as.numeric(y) )
  kov <- cov( as.numeric(x), as.numeric(y) )
  return( list( Kovarianz = kov, Korrelation = kor) )  # Ausgabe in einer Liste
}

# Weiteres Kategorielles Objekt zum testen:
p <- as.factor( sample( c("super","mittel","miserabel"),20, replace=TRUE) )
p <- ordered( p , levels = c("miserabel", "mittel", "super") )

# Kleiner Testdurchlauf der Funktion.
kat_zsh( p, test$kat)


# d)
# Funktion die geeignete deskriptive Statistiken fuer den Zusammenhang von einer metrischen
# und einer dichotomen Variable ausgibt:

met_dich <- function(x,y){
  # x soll die metrische Variable sein
  # y soll die dichotome Variable sein
  if(!is.numeric(x)) stop("Der erste Vektor ist nicht metrisch")
  if(length(levels(factor(y))) != 2) stop("Der zweite Vektor ist nicht dichotom")
  y <- dich_as_met(y)
  return(list("Korrelation" = cor(x,y), "Kovarianz" = cov(x,y), 
              "Lineares Modell" = lm(y~x)))
}

met_dich(test$met, test$dich)
met_dich(test$met, test$kat)


# e)
# Funktion zurquantilbasierten Katergorisierung einer mindestens ordinal skalierten Variable: 

# (Vorraussetzung: Merkmalsauspraegungen numerisch oder als factor mit geordneten Levels )

qkat <- function(y){
  if ( (is.numeric(y) == TRUE) || (is.factor(y) == TRUE) )  {
    
    x <- as.numeric(y)
    x
    
    n <- x[which( x <= quantile(x, 1/3))]
    
    m <- x
    m <- m [which( ! m <= quantile(x, 1/3))]
    m <- m [which( ! m > quantile(x, 2/3))]
    
    h <- x[which( x > quantile(x, 2/3))]  
    
    l <- list(n, m, h)
    
    names(l) <- c("niedrig (<= 1/3-Quantil)", "mittel (1/3-Quantil < & <= 2/3-Quantil)", "hoch (2/3-Quantil <)")
    
    return(l)
  }
  else{
    stop("Nicht moeglich, da Variable nicht mindestens ordinalskaliert.")
  }
}

qkat(test$met)
qkat(test$kat)
qkat(test$dich)


# f)
# Funktion zur geeigneten Visualisierug von drei oder vier kategoriellen Variablen:

visual <- function(x){
  barplot(table(x),xlab = "Studienfach", ylab = "Häufigkeit", 
       main = "Visualisierung des Studienfachs")
}

