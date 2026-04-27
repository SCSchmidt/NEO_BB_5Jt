### Keramikanalyse: Verzierung Datenvorbereitung

library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(viridis)
library(ggh4x)

scherben <- read.csv2("./analysis/data/raw_data/scherben.csv", sep = "|")

load("./analysis/data/derived_data/gef_umbruch.RData")

gef_umbruch <- gef_umbruch2

verzierungen <-  read.csv2("./analysis/data/raw_data/verzierungen.csv", sep = "|") 


##  landkreise ordnen
verzierungen$kreis <- factor(verzierungen$kreis, levels = c("Landkreis Harz", "Leipzig-Land", "Landkreis Wittenberg", "Teltow-Fläming" ,"Havelland",  "Dahme-Spreewald", "Spree-Neiße", "Oder-Spree"  , "Märkisch-Oderland", "Uckermark", "Kreis Rügen", "Zachodniopomorskie","Dolnośląskie", "Lebus", "Wielkopolskie", "Kujawsko-Pomorskie", "Landkreis Hoyerswerda", "Landkreis Görlitz" ))

verzierungen <- verzierungen |>
  mutate(kultur_gr = case_when(
    gefäßnummer %in% scherben$gefäßnummer[scherben$kultur2 == "Guhrau"] ~ "Guhrau",
    str_detect(kultur, "SBK")~ "SBK",
    str_detect(kultur, "BKK")~ "BKK",
    str_detect(kultur, "FBG|Friesack")~ "FBG",
    str_detect(kultur, "Rös") ~ "Rössen")
  )


verzierungen <- verzierungen |>
  mutate(kreislabel = case_when(
    kreis ==  "Landkreis Harz" ~ "HZ",
    kreis == "Leipzig-Land" ~ "LL",
    kreis == "Landkreis Wittenberg" ~ "LW",
    kreis == "Teltow-Fläming" ~ "TF" ,
    kreis == "Elbe-Elster" ~ "EE",
    kreis == "Havelland" ~ "HVL",
    kreis == "Landkreis Barnim" ~ "BAR",
    kreis == "Dahme-Spreewald" ~ "DSW",
    kreis == "Spree-Neiße" ~ "SN", 
    kreis ==  "Oder-Spree" ~ "LOS" ,
    kreis == "Märkisch-Oderland" ~ "MOL",
    kreis == "Uckermark" ~ "UM",
    kreis == "Zachodniopomorskie" ~ "ZP",
    kreis == "Dolnośląskie" ~ "DS",
    kreis == "Lebus" ~ "Leb",
    kreis == "Wielkopolskie" ~"WP",
    kreis == "Kujawsko-Pomorskie" ~ "KP",
    kreis == "Landkreis Hoyerswerda" ~ "LH", 
    kreis == "Landkreis Görlitz" ~ "LG",
    kreis == "Kreis Rügen" ~ "Rü" ))

verzierungen$kreislabel <- factor(verzierungen$kreislabel, levels = c("HZ", "LL", "LW", "TF" ,"EE", "HVL",  "DS", "SN", "LOS"  , "MOL", "UM", "BAR",  "Rü", "ZP","DSW", "Leb", "WP", "KP", "LH", "LG" ))

verzierungen <- inner_join(verzierungen, unique(scherben[,c("gefäßnummer", "kultur2")]), by = "gefäßnummer")


## farben

safe_colorblind_palette13 <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888","black")

## Gruppieren der Verzierungstechniken

verzierungen$technik <- tolower(verzierungen$technik)

verzierungen$technik <-factor(verzierungen$technik, ordered = TRUE,
                              levels = c("ritzlinie", "schnitt", "meißelstich, tremolierend", "fingerkniff", "fingernagelabdruck", "fingertupfen", "einzelstich", "einzelstich o. rössener doppelstich","rössener doppelstich", "doppelstich, parallel",  "doppelstich, versetzt",  "dreifachstich, parallel"  , "dreifachstich, versetzt", "vierfachstich, parallel"  , "vierfachstich, versetzt", "fünfzinkiger stich, parallel",  "fünfzinkig, versetzt", "delle", "leiste", "lochung" , "durchstich von hinten zugepropft", "inkrustation" , "freigelassene fläche"  ))

verzierungen <- verzierungen |>
  mutate(technik_gr = case_when(
    technik ==  "ritzlinie" ~ "geritzt",
    technik ==  "schnitt" ~ "geschnitten",
    technik == "meißelstich, tremolierend" ~ "Meißelstich, tremolierend",  
    technik == "fingerkniff" |   technik ==  "fingernagelabdruck" |   technik ==  "fingertupfen" ~ "Fingerverzierung", 
    technik == "einzelstich" ~ "Einzelstich",
    technik == "einzelstich o. rössener doppelstich" ~ "Einzelstich o. Rössener Doppelstich",
    technik == "rössener doppelstich" ~ "Rössener Doppelstich",
    technik == "doppelstich, parallel" |   technik ==   "doppelstich, versetzt" ~ "Doppelstich",
    technik ==  "dreifachstich, parallel"  |  technik ==  "dreifachstich, versetzt" ~ "Dreifachstich",
    technik == "vierfachstich, parallel"  |  technik ==  "vierfachstich, versetzt" ~"Vierfachstich",
    technik == "fünfzinkiger stich, parallel" |  technik ==   "fünfzinkig, versetzt" ~ "Fünffachstich",
    technik == "delle" |  technik ==  "leiste" |  technik ==  "durchstich von hinten zugepropft" ~ "plastisch verziert",
    technik == "inkrustation" ~ "Inkrustation"  ,
    TRUE ~ NA
  ))



# Technik und Ausführung

verzierungen$ausfuehrung <- tolower(verzierungen$ausfuehrung)
verzierungen$ausfuehrung <- factor(verzierungen$ausfuehrung, ordered = TRUE,
                                   levels = c("sehr flach", "flach", "mittel", "tief", "sehr tief"))


verzierungen$groeße <- tolower(verzierungen$groeße)
verzierungen$groeße <- factor(verzierungen$groeße, ordered = TRUE,
                              levels = c("sehr klein", "klein", "mittel", "groß", "sehr groß"))

save(verzierungen, file = "./analysis/data/derived_data/verzierungen.RData")


