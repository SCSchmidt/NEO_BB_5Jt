### Keramikanalyse: Gefäßform


library(dplyr)
library(stringr)
library(ggplot2)


### Datenvorbereitung

gef <- read.csv2("./analysis/data/raw_data/gefaesse.csv", sep = "|")

scherben <- read.csv2("./analysis/data/raw_data/scherben.csv", sep = "|")

scherben <- scherben |>
  filter(gefäßnummer != "", gefäßnummer != "'" )

## Kreislabel
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



# Kulturen vereinheitlichen

scherben <- scherben |>
  mutate(kultur_gr = case_when(
    str_detect(kultur2, "Guhrau") ~ "Guhrau", # bearbeitung von oben nach unten: obere haben vorrang
    str_detect(kultur, "SBK")~ "SBK",
    str_detect(kultur, "BKK")~ "BKK",
    str_detect(kultur, "FBG|Friesack")~ "FBG",
    str_detect(kultur, "Rös") ~ "Rössen" )
  )


gef <- left_join(gef, scherben[,c("FO", "Bef", "gefäßnummer", "kultur", "stufe", "fundort", "kreis", "kreislabel", "gemarkung", "land", "kultur2", "kultur_gr") ], by = "gefäßnummer", multiple = "first") #FO und Befund dazu

library(stringr)

gef$durchm_r <- as.numeric(gef$durchm_r)
gef$durchm_s <- as.numeric(gef$durchm_s)
gef$durchm_b <- as.numeric(gef$durchm_b)
gef$durchm_bod <- as.numeric(gef$durchm_bod)
gef$h_unterteil <- as.numeric(gef$h_unterteil)
gef$h_oberteil <- as.numeric(gef$h_oberteil)




## vertikale Umbruchstärke zu Gefäß hinzufügen

## vertikale Umbruchsstärke wurde in der Tabelle "scherben" aufgenommen

gef_umbruch <- scherben |>
  select(gefäßnummer, lage_im_gef, vert_umbruch, sicherheit_orient) |>
  unique()


library(tidyr)

gef_umbruch <- gef_umbruch |>
  filter(sicherheit_orient != "unsicher") |>
  filter(lage_im_gef != "unklar") |>
  filter(lage_im_gef != "Henkelscherbe") |>
  filter(lage_im_gef != "Hals o. Schulter?") |>
  filter(!str_detect(lage_im_gef,"oder") ) |>
  pivot_wider(names_from = lage_im_gef,
              values_from = vert_umbruch ) 

gef_umbruch <- left_join(gef, gef_umbruch, by = "gefäßnummer")

gef_umbruch <- gef_umbruch |> # Hals.schulter.umbruch als ersatz für verschiedene Versionen des Hals-... Umbruchs
  mutate(Hals.schulter.umbruch = case_when(
    !is.na(`Hals-Schulter-Umbruch`) ~ `Hals-Schulter-Umbruch`,
    !is.na(`Hals-Schulter-Bauch`) ~ `Hals-Schulter-Bauch`,
    !is.na(`Hals-Schulter-Bauch-Boden`) ~ `Hals-Schulter-Bauch-Boden`,
    !is.na(`Hals-Boden`) ~ `Hals-Boden`,
    TRUE ~ NA
  )) |>
  mutate(Schulter.bauch.umbruch = case_when(   # Schulter-bauch.umbruch als ersatz für verschiedene Versionen des Hals-... Umbruchs
    !is.na(`Schulter-Bauch-Umbruch`) ~ `Schulter-Bauch-Umbruch`,
    !is.na(`Hals-Schulter-Bauch`) ~ `Hals-Schulter-Bauch`,
    !is.na(`Hals-Schulter-Bauch-Boden`) ~ `Hals-Schulter-Bauch-Boden`,
    !is.na(`Hals-Boden`) ~ `Hals-Boden`,
    !is.na(`Schulter-Bauch-Boden`) ~ `Schulter-Bauch-Boden`,
    TRUE ~ NA
  ))

# das hat diverse NAs und mehrere Zeilen pro Gefäß produziert
# update: NULL values zu NA

gef_umbruch[] <- lapply(gef_umbruch, as.character); gef_umbruch[gef_umbruch == "NULL"] <- NA

# diese NAs durch andere Werte ersetzen, nach https://stackoverflow.com/questions/74535332/how-to-merge-rows-with-duplicate-id-replacing-nas-with-data-in-the-other-row-a


gef_umbruch <- gef_umbruch |> 
  group_by(gefäßnummer) |>
  summarise(across(everything(), ~ ifelse(any(complete.cases(.x)),
                                          first(.x[!is.na(.x)]),
                                          NA)))


gef_umbruch$kreislabel <- factor(gef_umbruch$kreislabel, ordered = TRUE,
                                 levels = c("HZ", "LL", "LW", "TF" ,"EE", "HVL",  "DS", "SN", "LOS"  , "MOL", "UM", "BAR",  "Rü", "ZP","DSW", "Leb", "WP", "KP", "LH", "LG" ))


## Verhältnismaße nach Czerniak berechnen
# nach Czerniak 1980, der das sehr erfolgreich für Gefäßformklassifikationen benutzt hat


gef_umbruch$h_insg <- as.numeric(gef_umbruch$h_insg)
gef_umbruch$h_hals <- as.numeric(gef_umbruch$h_hals)

gef_umbruch$durchm_r <- as.numeric(gef_umbruch$durchm_r)
gef_umbruch$durchm_s <- as.numeric(gef_umbruch$durchm_s)
gef_umbruch$durchm_b <- as.numeric(gef_umbruch$durchm_b)
gef_umbruch$durchm_bod <- as.numeric(gef_umbruch$durchm_bod)
gef_umbruch$h_unterteil <- as.numeric(gef_umbruch$h_unterteil)
gef_umbruch$h_oberteil <- as.numeric(gef_umbruch$h_oberteil)




#1 R1:H1
gef_umbruch$verh_durchm_r_h_insg <- gef_umbruch$durchm_r / gef_umbruch$h_insg

#2
gef_umbruch$verh_durchm_r_b <- gef_umbruch$durchm_r / gef_umbruch$durchm_b

# 3. R1:R2 = Randdurchmesser zu Schulterdurchmesser  
gef_umbruch$verh_durchm_r_s <- gef_umbruch$durchm_r / gef_umbruch$durchm_s

# 3.2. R2:R3 = Schulterdurchm zu Bauchdurchmesser 

gef_umbruch$verh_durchm_s_b <-gef_umbruch$durchm_s / gef_umbruch$durchm_b

# 4. wirklich: R1:H2 = Randdurchmesser zu Halshöhe!!

gef_umbruch$verh_durchm_r_h_hals <- gef_umbruch$durchm_r / gef_umbruch$h_hals

# 5. R1:H4 = Randdurchmeser zu Bodendurchmesser  

gef_umbruch$verh_durchm_r_bod <- gef_umbruch$durchm_r / gef_umbruch$durchm_bod


# 6. R3:H1 = Bauchdurchmesser zu Gesamthöhe  

gef_umbruch$verh_durchm_b_h_insg <- gef_umbruch$durchm_b / gef_umbruch$h_insg


# 7. H2:H5 = Halslänge zu Körperlänge  
gef_umbruch$verh_h_hals_h_insg <- gef_umbruch$h_hals / gef_umbruch$h_insg

gef_umbruch$verh_h_hals_h_korpus <- gef_umbruch$h_hals / (gef_umbruch$h_oberteil + gef_umbruch$h_unterteil)

# 8. H3:H4 = Höhe Oberteil zu Höhe Unterteil  


gef_umbruch$verh_o_u <- gef_umbruch$h_oberteil/gef_umbruch$h_unterteil



gef_umbruch$Hals.schulter.umbruch <- factor(gef_umbruch$Hals.schulter.umbruch, ordered = TRUE,
                                            levels = c("keine Wölbung","schwache Wölbung" ,"mittlere Wölbung", "starke Wölbung" , "scharfer Umbruch"  ))

gef_umbruch$Schulter.bauch.umbruch <- factor(gef_umbruch$Schulter.bauch.umbruch, ordered = TRUE,
                                             levels = c("keine Wölbung","schwache Wölbung" ,"mittlere Wölbung", "starke Wölbung" , "scharfer Umbruch"  ))

gef_umbruch2 <- gef_umbruch |> filter(kreislabel != "NA") |> filter(!is.na(kreislabel))

save(gef_umbruch2, file = "./analysis/data/derived_data/gef_umbruch.RData")

