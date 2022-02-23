# Auswertung des Datensatzes aus Aufgabe 1

library(readr)
Datensatz <- read_csv("Datensatz.csv")
Datensatz


# Vorbereitung des Datensatzes:

str(Datensatz)
# Interesse in Mathe und in Programmieren muessen in an factor Objekt, welches eine Rangordnung unter
# den levels hat, umgewandelt werden.
# Die Variable Mathe-LK muss lediglich in ein factor Objekt umgewandelt werden.

Datensatz$`Int. Mathe` <- as.factor( Datensatz$`Int. Mathe`)
Datensatz$`Int. Mathe` <- ordered( Datensatz$`Int. Mathe` , levels = c("1", "2", "3", "4", "5", "6", "7") )

Datensatz$`Int. Prog.` <- as.factor( Datensatz$`Int. Prog.`)
Datensatz$`Int. Prog.` <- ordered( Datensatz$`Int. Prog.` , levels = c("1", "2", "3", "4", "5", "6", "7") )

Datensatz$`Mathe-LK` <- as.factor( Datensatz$`Mathe-LK`)

# Nun haben alle Variablen den korrekten Objekttyp:
str(Datensatz)



# Anwendung der in Aufgabe 2 erstellten Funktionen: -----------------------------------------------------



# Funktion aus a) - Altersstruktur 

sum_met <- function(x){ 
  if( is.numeric(x) != TRUE){ stop("Die Variable muss metrisch skaliert sein.") } # Warnmeldung falls Variable
  AM <- mean(x)                                                                  # nicht metrisch ist.
  med <- median(x)    # Hier werden alle Statistiken berechnet.
  var <- var(x)
  sd <- sd(x)
  quartile <- quantile(x, probs= c(0.25,0.75) )
  extrm <- c( Minimum = min (x), Maximum = max (x))
  return( list( "Arithmetische Mittel" = AM, "Median" = med, "Varianz" = var, "Standardabweichung" = sd,
                "Quartile"= quartile, "Etremwerte" = extrm) )      # Alle benoetigten Groessen werden in einer Liste ausgegeben.
}

sum_met(Datensatz$Alter)    # Das durchschnittliche Alter liegt bei 24.34 Jahren bzw. (bezueglich des
                            # Alters sinnvoller) liegt der Median bei 25 Jahren.
                            # Die Standardabweichung von etwa 2.1 und die Quartile, welche bei
# $`Arithmetische Mittel`   # 23 und 26 liegen, deuten auf eine fuer Studenten uebliche
# [1] 24.34                 # Altersstruktur hin.

# $Median                   # Auch die groesste und die kleinste Beobachtung liegt in keinem fuer
# [1] 25                    # Studenten unueblichen Alter. (keine Ausreisser)

# $Varianz
# [1] 4.408485

# $Standardabweichung
# [1] 2.099639

# $Quartile
# 25% 75% 
#   23  26 

# $Etremwerte
# Minimum Maximum 
# 19      30 

# Fazit: (1) Die Individuen des Datensatzes (Studenten) haben im Mittel (Median) ein Alter von 25
#            Jahren und unterschreiten/ueberschrieten das Alter von 19 bzw. 30 nicht.






# Funktion aus b) - Interesse an Mathe und Programmieren (i) (und Abfrage der Teinahme am Mathe-LK (ii))

kat <- function(x){
  if(is.character(x)){stop("Die Variable ist nicht kategoriell")}
  else Modalwert <- names(table(x)[table(x) == max(table(x))]) ##Auspraegung, die am haeufigsten vorkommt
  Maximum <- max(x) 
  Minimum <- min(x)
  Spannweite <- length(unique(x)) ##Anzahl der verschiedenen Auspraegungen der Variablen
  uebersicht_kat <- list("Modalwert" = Modalwert, "Maximum" = Maximum, "Minimum" = Minimum, "Spannweite" = Spannweite) ##bringe Werte in eine Liste
  return(uebersicht_kat) ##gibt die Liste aus
}


# (i) Interesse an Mathe und Programmieren

sapply( Datensatz[ ,5:6], kat)

#         Int. Mathe Int. Prog.
# Modalwert  "2"        "5"       
# Maximum    7          7         
# Minimum    1          1         
# Spannweite 7          7     

# Am haeufigsten wurde beim Interesse am Fach Mathematik die eher niedrigere Kategorie 2 gewaehlt.
# Hingegen wurde beim Interersse am Programmieren am haeufigsten die recht hohe Kategorie 5 gewaehlt
# Auffaellig ist, dass beide Variablen mit 7 die gleiche Spannweite und das gleiche Minimum (1) und
# Maximum (7) aufweisen. D.h.: Es wurden alle Kategorien gewaehlt.
# Kurzgesagt deuten die Ergebnisse auf ein hoeheres Interesse an Programmieren in der (Gesamt-)Gruppe hin.

# Moegliche Studienfaecher ausfindig machen:
unique( Datensatz$Studienfach)
# [1] "Data Science" "Statistik"    "Informatik"   "Mathe"


ds <- Datensatz[ Datensatz$Studienfach == "Data Science", ]
st <- Datensatz[ Datensatz$Studienfach == "Statistik", ]
inf <- Datensatz[ Datensatz$Studienfach == "Informatik", ]
ma <- Datensatz[ Datensatz$Studienfach == "Mathe", ]


# Data Science
sapply( ds[ ,5:6], kat)         # Beim Vergleich der Modalwerte ergibt sich:
# Int. Mathe Int. Prog.
# Modalwert  "4"        "6"     # Data Science Studierende haben einen groesseren Modalwert beim
# Maximum    7          7       # Interesse am Programmiern (6), als bei Mathe (4).
# Minimum    2          1         
# Spannweite 6          7     


# Statistik
sapply( st[ ,5:6], kat)
# Int. Mathe Int. Prog.         # Die Gruppe der Statistik Studierenden weist hingegen, genau anders
# Modalwert  "7"        "5"     # herum in Mathe einen groesseren Modalwert als im Programmieren auf.
# Maximum    7          7       # Im Vergleich der Gruppen, ist hier der Modalwert von 7 (bei Int.Mathe)
# Minimum    1          1       # am hoechsten. 
# Spannweite 7          7       


# Informatik
sapply( inf[ ,5:6], kat)  
# Int. Mathe Int. Prog.         # Auch Informatik Studierende, weisen mehr Interesse am Programmieren auf,
# Modalwert  "2"        "5"     # jedoch ist der Modalwert von 5 (bei Int.Prog.) im Vergleich zu den Data
# Maximum    7          7       # Science Studierenden mit einem Modalwert von 6 etwas geringer.
# Minimum    1          1       # Ausserdem ist der Modalwert (bei Int.Mathe) von 2 im Vergleich zu den 
# Spannweite 7          7       # anderen Gruppen am geringsten.


# Mathe
sapply( ma[ ,5:6], kat)         # Mit dem Modalwert von 5 bei Int.Mathe scheint im Vergleich zum Modalwert 
# Int. Mathe Int. Prog.         # von 4 bei Int.Prog ein nur leicht hoeheres Interesse fuer Mathe
# Modalwert  "5"        "4"     # vorhanden zu sein.
# Maximum    7          7       # Jedoch ist auffaellig, das keine niedrigen Bewertungen bei 1 oder 2 
# Minimum    3          1       # bei Int.Mathe vorkommen. 
# Spannweite 5          6   


# Fazit: (1) Der hoechste/niedrigste Modalwert bezueglich des Mathe Interesse weisen Statistik/Informatik
#            Studierende auf.
#        (2) Der hoechste/niedrigste Modalwert bezueglich des Interesse am Programmieren weisen
#            Data Science/Mathe Studierende auf.





# (ii) Mathe- LK (Diese Variable ist zwar dichotom, aber diese Auswertung passte thematisch gut
#                 an diese Stelle, sodass sie im Abschnitt zur Funktion kat ist. )

table(Datensatz$`Mathe-LK`) # Die Mehrheit der Studierenden hat einen Mathe-LK belegt.
# nein   ja                 # Da 100 Beobachtungen vorliegen, haben 68% einen Mathe-LK belegt
# 32   68                   # und 32% nicht.


# Data Science            # Recht ausgeglichenes Verhaeltniss.
table(ds$`Mathe-LK`)
# ja nein 
# 13   15 

# Statistik               # Deutlich mehr Studierende haben den Mathe-LK besucht.
table(st$`Mathe-LK`)
# ja nein 
# 24    4 

# Informatik              # Etwas mehr Studierende haben den Mathe-LK besucht.(auch eher ausgeglichen)
table(inf$`Mathe-LK`)
# ja nein 
# 17   12 

# Mathe
table(ma$`Mathe-LK`)      # Nahezu alle Studierende haben den Mathe-LK besucht.
# ja nein 
# 14    1 


# Fazit: (1) In den Studienrichtungen Mathe und Statistik wird deutlich haeufiger der Mathe-LK besucht.
#        (2) In den Studienrichtungen Informatik und Data Science ist das Verhaeltniss zu Mathe-LK
#            Besuchern und Nicht-Mathe-LK Besuchern eher ausgeglichen.






# Funktion aus c)

kat_zsh <- function(x,y){
  # Zunaechst eine Bedingung, die die beiden Vektoren erfuellen sollen:
  if( length(x) != length(y) ){ stop("Beide Objekte muessen dieselbe Laenge haben.")}
  kor <- cor( as.numeric(x), as.numeric(y) )
  kov <- cov( as.numeric(x), as.numeric(y) )
  return( list( Kovarianz = kov, Korrelation = kor) )  # Ausgabe in einer Liste
}

kat_zsh(Datensatz$`Int. Prog.`, Datensatz$`Int. Mathe`)
#$Kovarianz
#[1] -0.3070707

#$Korrelation
#[1] -0.08523765

kat_zsh(Datensatz$Alter, Datensatz$`Int. Mathe`)
#$Kovarianz
#[1] -0.6169697

#$Korrelation
#[1] -0.154403

kat_zsh(Datensatz$Alter, Datensatz$`Int. Prog.`)
#$Kovarianz
#[1] 0.320202

#$Korrelation
#[1] 0.08056303






# Funktion aus d) - Zusammenang zwischen Interesse an Mathe/Programmieren
# und Mathe-LK

# Interesse an Mathe und Programmieren muessen erst wieder in eine metrische Variable
# umgewandelt werden und Mathe_Lk in einen dichotomen character

Datensatz$`Int. Mathe` <- as.numeric(Datensatz$`Int. Mathe`)
Datensatz$`Int. Prog.` <- as.numeric(Datensatz$`Int. Prog.`)
Datensatz$`Mathe-LK` <- as.character(Datensatz$`Mathe-LK`)


# Zunaechst wenden wir die Helferfuntion dich_as_met an, damit
# man in d) mit den dichotomen Variablen rechnen kann

dich_as_met <- function(x){
  name <- unique(x)
  result <- x == name[which.min(nchar(name))]
  return(as.numeric(result))
}



met_dich <- function(x,y){
  # x soll die metrische Variable sein
  # y soll die dichotome Variable sein
  if(!is.numeric(x)) stop("Der erste Vektor ist nicht metrisch")
  if(length(levels(factor(y))) != 2) stop("Der zweite Vektor ist nicht dichotom")
  y <- dich_as_met(y)
  return(list("Korrelation" = cor(x,y), "Kovarianz" = cov(x,y), 
              "Lineares Modell" = lm(y~x)))
}


met_dich(Datensatz$`Int. Mathe`, Datensatz$`Mathe-LK`)
#$Korrelation
#[1] 0.08875763

#$Kovarianz
#[1] 0.07919192

#$`Lineares Modell`

#Call:
  #lm(formula = y ~ x)

#Coefficients:
#  (Intercept)    x  
#0.58992      0.02187 
  
# Man kann erkennen, dass die Werte unabhängig voneinader sind, da die Korrelation
# nahezu 0 ist. Demenstprechend besteht kein Zusammenhang zwischen dem Interesse an 
# Mathe und ob man in der Schule Mathe-LK hatte.


# Regressionsgerade: 
y <- dich_as_met(Datensatz$`Mathe-LK`)
y

plot( y ~ Datensatz$`Int. Mathe` )
abline( 0.58992, 0.02187)




met_dich(Datensatz$`Int. Prog.`, Datensatz$`Mathe-LK`)
#$Korrelation
#[1] 0.03869798

#$Kovarianz
#[1] 0.03434343

#$`Lineares Modell`

#Call:
#  lm(formula = y ~ x)

#Coefficients:
#  (Intercept)     x  
#0.637350     0.009584 

# Hier kann man sehen, dass es auch zwischen dem Interesse an Programmieren und dem  
# Mathe-LK keinen wirklichen Zusammenhang gibt.

# Interpretation: Der Mathe-LK in der Schule beeinflusst nicht das Interesse an
# Mathe und Programmieren im Studium.


# Regressionsgerade:
plot( y ~ Datensatz$`Int. Prog.` )
abline( 0.637350 , 0.009584)




# Wandle Variablen wieder in die korrekten Datentypen um
Datensatz$`Int. Mathe` <- as.factor( Datensatz$`Int. Mathe`)
Datensatz$`Int. Mathe` <- ordered( Datensatz$`Int. Mathe` , levels = c("1", "2", "3", "4", "5", "6", "7") )

Datensatz$`Int. Prog.` <- as.factor( Datensatz$`Int. Prog.`)
Datensatz$`Int. Prog.` <- ordered( Datensatz$`Int. Prog.` , levels = c("1", "2", "3", "4", "5", "6", "7") )

Datensatz$`Mathe-LK` <- as.factor( Datensatz$`Mathe-LK`)






# Funktion aus e):
qkat <- function(y){
  if ( (is.numeric(y) == TRUE) || (is.ordered(y) == TRUE) )  {
    
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

qkat(Datensatz$`Int. Mathe`)

# Ausgabe:

#$`niedrig (<= 1/3-Quantil)`
#[1] 3 2 2 1 3 2 2 2 2 2 3 3 2 2 1 3 3 1 2 3 3 3 2 2 2 2 2 3 3 1
#[31] 1 1 1 3 1 2 3 3 2 3 3

#$`mittel (1/3-Quantil < & <= 2/3-Quantil)`
#[1] 5 5 4 5 5 4 4 4 5 5 4 4 5 4 5 5 5 5 4 5 5 5 5 4 4 4 4 4 4 4
#[31] 5 4

#$`hoch (2/3-Quantil <)`
#[1] 7 7 7 7 7 6 6 7 6 7 6 7 7 7 7 6 7 7 6 6 7 7 6 6 6 7 6


# Im nierdrigen Bereich liegen: 1,2,3
# Im mittleren Bereich liegen: 4,5
# Im hohen Bereich liegen: 6,7


qkat(Datensatz$`Int. Prog.`)

# $`niedrig (<= 1/3-Quantil)`
# [1] 1 4 2 1 3 3 1 4 3 2 4 2 2 1 1 3 1 4 2 3 3 2 4 1 4 3 4 1 1 3 4 3 4 4 4 2 4 3 3 4 1 1 4 2

# $`mittel (1/3-Quantil < & <= 2/3-Quantil)`
# [1] 6 6 6 6 6 5 6 5 5 6 5 6 5 5 5 6 5 5 6 5 6 5 5 5 6 5 6 5 6 5 5 5 5 5 6 5 5 6 6 6 6

# $`hoch (2/3-Quantil <)`
# [1] 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7

# Im nierdrigen Bereich liegen: 1,2,3,4
# Im mittleren Bereich liegen: 5,6
# Im hohen Bereich liegen: 7


# Fazit: (1) Da bei Int.Prog mehr Bewertungsstufen im niedrigen Bereich liegen, muessen im mittleren 
#            und oberen Bereich haeufiger die Sufen 5 bis 6 gewaehlt worden sein, als bei Int.Mathe.
#        (2) Bei Int.Mathe verteilen sich die  Bewertungsstufen besser auf die Kategorien niedrig,
#            mittel und hoch.




# Funktion aus f):
visual <- function(x,i){
  p <- length(i)
  q <- sqrt(p)
  if(round(q)^2 == p)
    par(mfcol = c(q,q))
  else
    par(mfcol=c(q,q+1))
  for (u in 1:p) {
    barplot(table(x[i[u]]), xlab = colnames(Datensatz)[i[u]],
            ylab = "abs. Häufigkeit" ,main = "Visualisierung")
  }
}

visual(Datensatz,3:6)

# Das Alter ist nahzu normalverteilt um 25 Jahre. Lediglich im Bereich 26 bis 27 liegen weniger
# Beobachtungen, als im Bereich unter 25 Jahre.


# Bei den Studiengaengen sind Data Science,Statistik und Informatik nahezu gleich oft vertreten.
# Hingegen gibt es deutlich weniger Mathe Studenten ( etwa die Haelte vergleichen mit den obigen ).


# Hier bestaetigen sich bei der Verteilung der Bewertungsstufen die Erkenntnisse aus e). Beim Mathe-
# Interesse sind die Beobachtungen deutlich gleichmaessiger verteilt als beim Programmierungs-
# Interesse. Ledglich die Bewertungen 1 und 6 kommen etwas weniger haeufig vor.

# Beim Programmierungs-Interesse ist eine Tendez zur Linksschiefe erkennbar.
# Es leigen mehr Beobachtungen in Bereich der hoeheren Bewertungsstufen.



