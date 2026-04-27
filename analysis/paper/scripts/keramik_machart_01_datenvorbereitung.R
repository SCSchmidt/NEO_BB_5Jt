### Machart-Analyse: Datenvorbereitung

library(dplyr)
library(stringr)

## Daten einladen

scherben <- read.csv2("./analysis/data/raw_data/scherben.csv", sep = "|")


## für die Zuordnung, ob Scherben verziert sind oder micht

verzierungen <-  read.csv2("./analysis/data/raw_data/verzierungen.csv", sep = "|") 

scherben <- scherben |>
  mutate(verz_n = case_when (
    gefäßnummer %in% verzierungen$gefäßnummer ~ "verz",
    TRUE ~ "unverz"
  )) 

## Landkreise sortieren


scherben$kreis <- factor(scherben$kreis, levels = c("Landkreis Harz", "Leipzig-Land", "Landkreis Wittenberg", "Teltow-Fläming" ,"Elbe-Elster", "Havelland",  "Dahme-Spreewald", "Spree-Neiße", "Oder-Spree"  , "Märkisch-Oderland", "Uckermark",  "Zachodniopomorskie","Dolnośląskie", "Lebus", "Wielkopolskie", "Kujawsko-Pomorskie", "Landkreis Hoyerswerda", "Landkreis Görlitz" ))


scherben <- scherben |>
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

scherben$kreislabel <- factor(scherben$kreislabel, levels = c("HZ", "LL", "LW", "TF" ,"EE", "HVL",  "DS", "SN", "LOS"  , "MOL", "UM", "BAR",  "Rü", "ZP","DSW", "Leb", "WP", "KP", "LH", "LG" ))


## Kulturbezeichnungen vereinheitlichen

scherben <- scherben |>
  mutate(kultur_gr = case_when(
    str_detect(kultur2, "Guhrau") ~ "Guhrau", # bearbeitung von oben nach unten: obere haben vorrang
    str_detect(kultur, "SBK")~ "SBK",
    str_detect(kultur, "BKK")~ "BKK",
    str_detect(kultur, "FBG|Friesack")~ "FBG",
    str_detect(kultur, "Rös") ~ "Rössen" )
  ) 

scherben$kultur2[scherben$kultur2 == "Rös"] <- "Rössen"


## ordinale Gruppen zuweisen, um spätere Filterungen zu erleichtern


### Bruchpotential: wie gut sind Magerungen zu erkennen
scherben$bruchpotential <- tolower(scherben$bruchpotential)
scherben$bruchpotential <- factor(scherben$bruchpotential, ordered = TRUE,
                                  levels = c("frische brüche", "moderner bruch", "moderner bruch, gewaschen", "beschreibung", "gut gewaschen", "gewaschen", "schlecht gewaschen" ,  "ungewaschen"  , "ungewaschen, wegen inkrustation", "nur innen abgerollte oberfläche",  "oberfläche","verwaschen" ,  "nicht erkennbar"   ) )

## Größe und Menge der Magerung in ordered factor

scherben$magerungsgr <- tolower(scherben$magerungsgr)
scherben$magerungsgr <- factor(scherben$magerungsgr, ordered = TRUE,
                               levels= c("sehr fein", "fein", "mittel", "grob", "sehr grob") )

scherben$magerungsmenge <- tolower(scherben$magerungsmenge)
scherben$magerungsmenge <- factor(scherben$magerungsmenge, ordered = TRUE,
                                  levels= c("wenig", "mittel", "viel") )

# Oberfläche

scherben <- scherben |>
  mutate(ofl = 
           case_when(str_detect(ofl_außen, "Schlick") ~ "Schlicker",
                     ofl_außen == "lackiert" ~ "nicht erhalten",
                     ofl_außen == "verkrustet" ~ "nicht erhalten",
                     ofl_außen == "überprägt" ~ "nicht erhalten",
                     ofl_außen == "NA" ~ "nicht erhalten",
                     str_detect(ofl_außen, "rauh") ~ "rauh",
                     TRUE ~ ofl_außen))

scherben$ofl[scherben$ofl == "nicht erhalten"] <- NA

scherben$ofl <- factor(scherben$ofl, levels = c("Schlicker", "glatt, poliert, glänzend", "glatt, poliert, matt", "glatt, matt", "geglättet", "rauh", "grob verstrichen"), ordered = TRUE)

## Oberflächenerhaltung in ordered factor

scherben <- scherben |>
  mutate(erh_ofl_k = case_when(
    erhaltung_ofl_außen == "Stark abgerollt" ~ "nicht erhalten",
    erhaltung_ofl_außen ==  "nicht erhalten" ~ "nicht erhalten",
    erhaltung_ofl_außen ==  "gelackt"  ~ "nicht erhalten",
    erhaltung_ofl_außen == "stark abgerollt" ~ "nicht erhalten",
    erhaltung_ofl_außen ==  "stark erodiert"   ~ "nicht erhalten",
    erhaltung_ofl_außen == "extrem abgerollt" ~ "nicht erhalten",
    erhaltung_ofl_außen ==  "überlagert"  ~ "nicht erhalten",
    erhaltung_ofl_außen == "stark verrollt" ~ "nicht erhalten",
    erhaltung_ofl_außen ==  "erodiert"      ~ "nicht erhalten",
    erhaltung_ofl_außen ==  "überprägt"   ~ "nicht erhalten",
    erhaltung_ofl_außen ==  "abgerollt"  ~ "nicht erhalten",
    is.na(erhaltung_ofl_außen)   ~ "nicht erhalten",
    erhaltung_ofl_außen ==   "gut, Rand erodiert"     ~ "gut erhalten",
    erhaltung_ofl_außen ==    "gut erhalten"    ~ "gut erhalten",
    erhaltung_ofl_außen ==     "etwas abgerollt"    ~ "z. T. erhalten",
    erhaltung_ofl_außen ==      "beschädigt"    ~ "z. T. erhalten",
    erhaltung_ofl_außen ==      "leicht verrollt"       ~ "z. T. erhalten",
    erhaltung_ofl_außen ==      "gut, z.T. erodiert"  ~ "z. T. erhalten",
    erhaltung_ofl_außen ==     "zT gut erhalten"   ~ "z. T. erhalten",
    erhaltung_ofl_außen ==      "teilw. Gut,"   ~ "z. T. erhalten",
    erhaltung_ofl_außen ==    "gut"    ~ "gut erhalten",
    erhaltung_ofl_außen == "viel abgeplatzt"   ~ "nicht erhalten",
    erhaltung_ofl_außen == "NA"   ~ "nicht erhalten"
  ))

scherben$erh_ofl_k <- factor(scherben$erh_ofl_k , levels = c("gut erhalten", "z. T. erhalten", "nicht erhalten") )


## save

save(scherben, file = "./analysis/data/derived_data/scherben.RData")

gef_infos <- scherben |>
  select(gefäßnummer, kreis, kreislabel, FO, Bef, verz_n, gemarkung, land, stufe) |>
  unique()

save(gef_infos, file = "./analysis/data/derived_data/gef_infos.RData")

