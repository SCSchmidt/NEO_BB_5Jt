## Rscript: Datensäuberung für Standortanalyse

library(sf)
library(viridis)
library(ggplot2)
library(ggh4x)
library(tidyr)
library(FD)
library(dplyr)
library(vegan)

# 1- Fundstellen in BB 

## für Veröffentlichung sind die Koordinaten auf zwei Nachkommastellen reduziert worden und die Infos als csv abgelegt:

# EPSG des BLDAM ist 25833
#sites <- st_read("./analysis/data/geodata/Fst_Standortanalysen_BB_LBK_Guhrau.shp", crs = 25833)
#coords <- st_coordinates(sites)
#sites3 <- cbind(st_drop_geometry(sites), round(coords, 2) )
#write.csv2(sites3, "./analysis/data/geodata/sites_tab.csv")

sites <- read.csv("./analysis/data/geodata/sites_tab.csv", sep = ";", dec = ",")

# Kreislabels einführen

sites <- sites |>
  mutate(Kreislabel = case_when(
    KREIS == "Teltow-Fläming" ~ "TF" ,
    KREIS == "Elbe-Elster" ~ "EE",
    KREIS == "Havelland" ~ "HVL",
    KREIS == "LandKreis Barnim" ~ "BAR",
    KREIS == "Dahme-Spreewald" ~ "DSW",
    KREIS == "Spree-Neiße" ~ "SPN", 
    KREIS ==  "Oder-Spree" ~ "LOS" ,
    KREIS == "Märkisch-Oderland" ~ "MOL",
    KREIS == "Uckermark" ~ "UM",
    KREIS == "Brandenburg an der Havel Städte" ~ "BRB",
    KREIS == "Brandenburg (Havel)" ~ "BRB",
    KREIS == "Cottbus Städte" ~ "CB",
    KREIS == "Frankfurt (Oder)" ~ "FF",
    KREIS == "Frankfurt am Oder Städte" ~ "FF",
    KREIS == "Oberhavel" ~ "OH",
    KREIS == "Oberspreewald-Lausitz" ~ "OSL",
    KREIS == "Ostprignitz-Ruppin" ~ "OPR",
    KREIS == "Potsdam" ~ "P",
    KREIS == "Potsdam Städte" ~ "P",
    KREIS == "Potsdam-Mittelmark" ~ "PM",
    KREIS == "Prignitz" ~ "PR",
    KREIS == "Barnim" ~ "BAR" )      )

# vereinheitlichen der Kulturgruppen und Zuweisung der Beilfundstellen, die erst im Nachgang recherchiert wurden
sites <- sites |>
  mutate(kultur2 = case_when(
    KULTUR == "Mesolithikum" ~ "Meso",
    KULTUR == "Rössener" ~ "Rös",
    KULTUR == "LBK Siedlungen" ~ "LBK Siedl.",
    KULTUR == "SBK?" ~ "SBK",
    TRUE ~ KULTUR
  )) |>
  filter(Kreislabel != "CB") |>
  mutate(kultur2 = case_when(
    FST %in% 
      c("Frankfurt 108",
        "Wernsdorf 6",
        "Beeskow 5",
        "Kossenblatt 3",
        "Bad Freienwalde 8",
        "Blankenburg 26",
        "Blankenburg 8",
        "Brüssow 67",
        "Niederlandin 12/5 (13)",
        "Prenzlau 23",
        "Ribbeck 9/21 (9)",
        "Altfriesack 6", 
        "Altranft 6",
        "Treuenbrietzen 39",
        "Potsdam 58",
        "Bornim 8") ~ "Rös. Beil/Axt",
    TRUE ~ kultur2
  )) |>
  filter(FST != "Zeestow 14/8 (14)") # Zeestow nicht relevant

# ordnen für Darstellungen, die nach Bundesländer differenzieren (SBK nicht in SRK und SBK aufteilen, da sonst zu viele Spalten)
sites$kultur2 <- factor(sites$kultur2, ordered = TRUE,
                        levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "Guhrau", "Rös", "Rös. Beil/Axt"))

## GG ist immer Grundgesamtheit -> Rasterpunkte über das Bundesland 

## Korrekturen von zwei mehrperiodigen Fundstellen (doppeln des Eintrags mit zweiter Datierung):
bredow_roes <- sites |> filter(LFD == 7 & FID == 3007)

bredow_roes$FID <- "3007.2"
bredow_roes$kultur2 <- "Rös"
bredow_roes$KULTUR<- "Rössener"
bredow_roes$Fundart <- ""

bredow_roes$QUELLE <- "Kirsch 1993 / (c) Denkmaldaten BLDAM 2021"

sites <- rbind(sites, bredow_roes)

sites$kultur2[sites$FST == "Potsdam 16"] <- "Rös"
sites$FST[sites$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"


potsdam_sbk <- sites |> filter (FST == "Potsdam 3/13 (3)")
potsdam_sbk$FID <- "265.2"
potsdam_sbk$KULTUR <- "Stichbandkeramik"
potsdam_sbk$kultur2 <- "SBK"
potsdam_sbk$Fundart <- "Lesefund"

sites <- rbind(sites, potsdam_sbk)

save(sites, file = "./analysis/data/derived_data/sites.RData")

# 2- Naturraum-Informationen

# 2.1 bereits abgefragte Daten (Punktkoordinate)

# Bereinigen und umwandeln
#sites_naturraum <- st_read(".analyis/data/geodata/standortanalyse_BB_naturraum_bs.shp", crs = 25833)
#coords <- st_coordinates(sites_naturraum)
#sites3 <- cbind(st_drop_geometry(sites_naturraum), round(coords, 2) )
#write.csv2(sites3, "./analysis/data/geodata/sites_naturraum_tab.csv")

# 2.2. Naturraum nach Scholz in Tabelle umwandeln (Daten heruntergeladen von https://metaver.de/trefferanzeige?docuuid=E56B3332-5572-47BA-9D8D-386FE0F999D1)

#naturraum <- st_read("/media/sophie/Volume/Nextcloud/NEO/Daten/GeoDaten/Brandenburg/naturraum_scholz/shp/natraum_scholz.shp")
#naturraum <- st_drop_geometry(naturraum)
#write.csv2(naturraum, "./analysis/data/geodata/naturraum_tab.csv")


