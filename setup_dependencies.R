#### SETUP

## eine Liste der benötigen Pakete steht unter dependencies.csv zur Verfügung
## mit diesem Code können sie automatisch installiert werden, falls nötig

dep <- read.csv2("dependencies.csv")
dep_ls <- as.list(unique(dep$Package))

new.packages <- dep_ls[!(dep_ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## da in der csv-Datei in der Spalte $source abgelegt wurde, 
## in welchem Skript das Paket benötigt wird, kann auch danach gefiltert werden, 
## sollten nur bestimmte Skripte ausgeführt werden sollen, z. B. wird so eine Liste der Pakete erstellt, 
## die im Skript zur Darstellung der 14C-Daten benötigt wird:

dep_c14 <- as.list(dep$Package[dep$Source == "/analysis/paper/scripts/14C-daten.R"])


## Obacht, einige der Pakete sind nicht auf CRAN verfügbar!

## c14bazAAR von nevrome: siehe https://docs.ropensci.org/c14bazAAR/ --> install.packages("c14bazAAR", repos = c(ropensci = "https://ropensci.r-universe.dev"))

## DataScienceR: von happy rabbit auf GitHub --> install_github("happyrabbit/DataScienceR") 