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



# Anwendung der in Aufgabe 2 erstellten Funktionen:



