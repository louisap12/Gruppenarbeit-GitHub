# Dataframe alternativ zu Matrix erstellen -> leichterer Zugriff auf Spalten

m <- data.frame(numeric(100), numeric(100), numeric(100), numeric(100), numeric(100), numeric(100))
names(m) <- c("ID", "Alter", "Studienfach", "Int. Mathe", "Int. Prog.", "Mathe-LK")
View(m)

m[,1] <- 1:100
m

# beim Alter abrunden damit nur ganze Zahlen
m[,2] <- floor(rnorm(100, 25 , 2))
m

s <- c( rep("Statistik", 30), rep("Data Science", 30), rep("Informatik", 25),rep("Mathe", 15))
s

m[,3] <- sample(s, 100, replace = TRUE)
m

im <- 1:7
m[m$Studienfach != "Mathe", 4] <- sample(im, sum(m$Studienfach != "Mathe"), replace = TRUE)

# fuer Mathematiker hoehere W'keit grosses Interesse an Mathematik zu haben  
im2 <- c(rep(1, 2), rep(2, 5), rep(3, 8), rep(4, 11), rep(5, 14), rep(6, 17), rep(7, 20))
m[m$Studienfach == "Mathe", 4] <- sample(im2, sum(m$Studienfach == "Mathe"), replace = TRUE)


ip <- 1:7
m[m$Studienfach != "Informatik", 5] <- sample(ip, sum(m$Studienfach != "Informatik"), replace = TRUE)

# fuer Informatiker hoehere W'keit grosses Interesse am Porgrammieren zu haben  
ip2 <- c(rep(1, 2), rep(2, 5), rep(3, 8), rep(4, 11), rep(5, 14), rep(6, 17), rep(7, 20))
m[m$Studienfach == "Informatik", 5] <- sample(im2, sum(m$Studienfach == "Informatik"), replace = TRUE)

# bei Statistikern und Mathematikern Mathe-LK wahrscheinlicher
lk <- c(rep("ja", 80), rep("nein",20))
lk2 <- c(rep("ja", 60), rep("nein",40))
lk3 <- c("ja", "nein")

m[m$Studienfach == "Statistik" | m$Studienfach == "Mathe", 6] <-  sample(lk, 
                                                                         sum(m$Studienfach == "Statistik" | m$Studienfach == "Mathe"), replace = TRUE)
m[m$Studienfach == "Data Science", 6] <- sample(lk2, sum(m$Studienfach == "Data Science"), replace = TRUE)
m[m$Studienfach == "Informatik", 6] <- sample(lk3, sum(m$Studienfach == "Informatik"), replace = TRUE)


datensatz <- function(){
  m <- data.frame(numeric(100), numeric(100), numeric(100), numeric(100), numeric(100), numeric(100))
  names(m) <- c("ID", "Alter", "Studienfach", "Int. Mathe", "Int. Prog.", "Mathe-LK")
  m[,1] <- 1:100
  m[,2] <- floor(rnorm(100, 25 , 2))
  s <- c( rep("Statistik", 30), rep("Data Science", 30), rep("Informatik", 25),rep("Mathe", 15))
  m[,3] <- sample(s, 100, replace = TRUE)
  im <- 1:7
  m[m$Studienfach != "Mathe", 4] <- sample(im, sum(m$Studienfach != "Mathe"), replace = TRUE)
  im2 <- c(rep(1, 2), rep(2, 5), rep(3, 8), rep(4, 11), rep(5, 14), rep(6, 17), rep(7, 20))
  m[m$Studienfach == "Mathe", 4] <- sample(im2, sum(m$Studienfach == "Mathe"), replace = TRUE)
  ip <- 1:7
  m[m$Studienfach != "Informatik", 5] <- sample(ip, sum(m$Studienfach != "Informatik"), replace = TRUE)
  ip2 <- c(rep(1, 2), rep(2, 5), rep(3, 8), rep(4, 11), rep(5, 14), rep(6, 17), rep(7, 20))
  m[m$Studienfach == "Informatik", 5] <- sample(im2, sum(m$Studienfach == "Informatik"), replace = TRUE)
  lk <- c(rep("ja", 80), rep("nein",20))
  lk2 <- c(rep("ja", 60), rep("nein",40))
  lk3 <- c("ja", "nein")
  m[m$Studienfach == "Statistik" | m$Studienfach == "Mathe", 6] <-  sample(lk, 
                                                                           sum(m$Studienfach == "Statistik" | m$Studienfach == "Mathe"), replace = TRUE)
  m[m$Studienfach == "Data Science", 6] <- sample(lk2, sum(m$Studienfach == "Data Science"), replace = TRUE)
  m[m$Studienfach == "Informatik", 6] <- sample(lk3, sum(m$Studienfach == "Informatik"), replace = TRUE)
  return(m)
}

set.seed(123)
datensatz()
#      ID Alter  Studienfach Int. Mathe Int. Prog. Mathe-LK
# 1     1    23    Statistik          1          2       ja
# 2     2    24        Mathe          5          2       ja
# 3     3    28        Mathe          5          5       ja
# 4     4    25    Statistik          6          4       ja
# 5     5    25        Mathe          3          6       ja
# 6     6    28 Data Science          1          4       ja
# 7     7    25   Informatik          2          2       ja
# 8     8    22 Data Science          4          5       ja
# 9     9    23    Statistik          2          4       ja
# 10   10    24   Informatik          1          7       ja
# 11   11    27        Mathe          4          6       ja
# 12   12    25 Data Science          5          1       ja
# 13   13    25 Data Science          5          7       ja
# 14   14    25    Statistik          1          2       ja
# 15   15    23 Data Science          7          1       ja
# 16   16    28 Data Science          1          7       ja
# 17   17    25 Data Science          5          2       ja
# 18   18    21   Informatik          5          6       ja
# 19   19    26    Statistik          2          3       ja
# 20   20    24 Data Science          2          2     nein
# 21   21    22    Statistik          7          7       ja
# 22   22    24 Data Science          6          3     nein
# 23   23    22   Informatik          7          5     nein
# 24   24    23   Informatik          5          7       ja
# 25   25    23   Informatik          1          5     nein
# 26   26    21 Data Science          1          1     nein
# 27   27    26   Informatik          5          7     nein
# 28   28    25   Informatik          5          5       ja
# 29   29    22 Data Science          4          4     nein
# 30   30    27 Data Science          7          2     nein
# 31   31    25 Data Science          5          3     nein
# 32   32    24 Data Science          5          5       ja
# 33   33    26    Statistik          7          5       ja
# 34   34    26    Statistik          7          7       ja
# 35   35    26        Mathe          7          6       ja
# 36   36    26 Data Science          4          3       ja
# 37   37    26 Data Science          2          6       ja
# 38   38    24 Data Science          5          6     nein
# 39   39    24    Statistik          1          2       ja
# 40   40    24        Mathe          5          5       ja
# 41   41    23   Informatik          1          2     nein
# 42   42    24   Informatik          2          5       ja
# 43   43    22    Statistik          1          4       ja
# 44   44    29    Statistik          2          2       ja
# 45   45    27    Statistik          5          1       ja
# 46   46    22 Data Science          1          4     nein
# 47   47    24   Informatik          7          6     nein
# 48   48    24   Informatik          3          6       ja
# 49   49    26    Statistik          2          1       ja
# 50   50    24   Informatik          2          3       ja
# 51   51    25 Data Science          5          4     nein
# 52   52    24    Statistik          7          5       ja
# 53   53    24    Statistik          4          4       ja
# 54   54    27 Data Science          1          2       ja
# 55   55    24 Data Science          5          2       ja
# 56   56    28    Statistik          2          4       ja
# 57   57    21    Statistik          5          7       ja
# 58   58    26 Data Science          6          3       ja
# 59   59    25 Data Science          2          5       ja
# 60   60    25   Informatik          2          4     nein
# 61   61    25    Statistik          6          6       ja
# 62   62    23    Statistik          4          2       ja
# 63   63    24 Data Science          3          7     nein
# 64   64    22    Statistik          4          3       ja
# 65   65    22    Statistik          3          3       ja
# 66   66    25    Statistik          3          3       ja
# 67   67    25        Mathe          5          6       ja
# 68   68    25        Mathe          6          1     nein
# 69   69    26 Data Science          3          1       ja
# 70   70    29   Informatik          5          5     nein
# 71   71    24    Statistik          7          3       ja
# 72   72    20 Data Science          3          4       ja
# 73   73    27   Informatik          7          4     nein
# 74   74    23    Statistik          2          4       ja
# 75   75    23    Statistik          7          5       ja
# 76   76    27    Statistik          7          2       ja
# 77   77    24        Mathe          6          2       ja
# 78   78    22    Statistik          3          4       ja
# 79   79    25        Mathe          7          7       ja
# 80   80    24 Data Science          1          2       ja
# 81   81    25   Informatik          5          2     nein
# 82   82    25    Statistik          7          1       ja
# 83   83    24    Statistik          6          4       ja
# 84   84    26    Statistik          7          3       ja
# 85   85    24   Informatik          6          6     nein
# 86   86    25    Statistik          1          1       ja
# 87   87    27 Data Science          4          5       ja
# 88   88    25    Statistik          2          3     nein
# 89   89    24 Data Science          6          4     nein
# 90   90    27   Informatik          2          4     nein
# 91   91    26    Statistik          4          2       ja
# 92   92    26        Mathe          6          5       ja
# 93   93    25 Data Science          1          5       ja
# 94   94    23        Mathe          5          2       ja
# 95   95    27    Statistik          4          6       ja
# 96   96    23   Informatik          5          4     nein
# 97   97    29 Data Science          5          2       ja
# 98   98    28 Data Science          3          2       ja
# 99   99    24    Statistik          7          1     nein
# 100 100    22 Data Science          5          3       ja


# deinen Pfad loesche ich damit das unabhaenigig vom Computer gespeichert werden kann
# anstelle von m wuerde ich beim Speichern der Tabelle datensatz() schreiben da sonst das Dataframe
# aus den Vorueberlegungen und nicht das von der Funktion generierte gespeichert wird

write.csv(datensatz(), "Datensatz.csv")