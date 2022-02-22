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


