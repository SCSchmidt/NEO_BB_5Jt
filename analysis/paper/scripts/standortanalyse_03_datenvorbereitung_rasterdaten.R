### Daten einladen der Punktrasterabfragen

source("analysis/paper/scripts/standortanalyse_01_datenvorbereitung.R")

## Reduktion der Informationsgenauigkeit und Ablage als CSV
# r_info <- st_read("../data/geodata/abfrage_BB_DGM-derivate_eros_2.shp", crs = 25833)
#coords <- st_coordinates(r_info)
#sites3 <- cbind(st_drop_geometry(r_info), round(coords, 2) )
#write.csv2(sites3, "./analysis/data/geodata/r_info_tab.csv")

r_info <- read.csv2("./analysis/data/geodata/r_info_tab.csv")

r_info <- r_info |>
  mutate(kreislabel = case_when(
    KREIS == "Teltow-Fläming" ~ "TF" ,
    KREIS == "Elbe-Elster" ~ "EE",
    KREIS == "Havelland" ~ "HVL",
    KREIS == "Landkreis Barnim" ~ "BAR",
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

r_info <- r_info |>
  mutate(kultur2 = case_when(
    KULTUR == "Mesolithikum" ~ "Meso",
    KULTUR == "Rössener" ~ "Rös",
    KULTUR == "LBK Siedlungen" ~ "LBK Siedl.",
    KULTUR == "SBK?" ~ "SBK",
    TRUE ~ KULTUR
  )) |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(kreislabel %in% c("TF","HVL","P","PM") ) ~ "SRK",
    TRUE ~ kultur2
  )) |>
  filter(kreislabel != "CB") |>
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
  filter(FST != "Zeestow 14/8 (14)")

r_info$kultur2 <- factor(r_info$kultur2, ordered = TRUE,
                         levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))


## Korrekturen von mehrperiodigen Fundstellen:
bredow_roes <- r_info |> filter(FID == 3007)

bredow_roes$FID <- "3007.2"
bredow_roes$kultur2 <- "Rös"
bredow_roes$KULTUR<- "Rössener"
bredow_roes$Fundart <- ""

bredow_roes$QUELLE <- "Kirsch 1993 / (c) Denkmaldaten BLDAM 2021"

r_info <- rbind(r_info, bredow_roes)

r_info$kultur2[r_info$FST == "Potsdam 16"] <- "Rös"
r_info$FST[r_info$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"


potsdam_sbk <- r_info |> filter (FST == "Potsdam 3/13 (3)")
potsdam_sbk$FID <- "265.2"
potsdam_sbk$KULTUR <- "Stichbandkeramik"
potsdam_sbk$kultur2 <- "SBK"
potsdam_sbk$Fundart <- "Lesefund"

r_info <- rbind(r_info, potsdam_sbk)

# Regionen nach https://gl.berlin-brandenburg.de/regionalplanung-im-land-brandenburg/regionale-planungsgemeinschaften/ 
## -> nur HAvelland und Fläming getrennt: zwei sehr unterschiedliche Landstriche, zusammenfügen nicht sinnvoll

library(stringr)

r_info <- r_info |>
  mutate(kr_group = case_when(
    str_detect("UM|BAR", kreislabel) ~ "Uckermark-Barnim",
    str_detect("PR|OPR|OHV",kreislabel) ~ "Prignitz-Oberhavel",
    str_detect("DSW|EE|OSL|SPN",kreislabel) ~ "Lausitz-Spreewald",
    str_detect("HVL|PM|P|BRB",kreislabel) ~ "Havelland",
    str_detect("TF",kreislabel) ~ "Teltow-Fläming",
    
    str_detect("MOL|FF|LOS",kreislabel) ~ "Oderland-Spree"
  )          )

save(r_info, file = "./analysis/data/derived_data/raster_based_standortpara_BB.RData") 

## mit Naturräumen verknüpfen

#bs_nat <- st_read("./analysis/data/geodata/standortanalyse_BB_naturraum_bs.shp", crs = 25833)
#coords <- st_coordinates(bs_nat)
#sites3 <- cbind(st_drop_geometry(bs_nat), round(coords, 2) )
#sites3$FID <- as.character(bs_nat$FID)
#write.csv2(sites3, "./analysis/data/geodata/bs_nat_tab.csv")

bs_nat <- read.csv2("./analysis/data/geodata/bs_nat_tab.csv")

bs_nat$FID <- as.character(bs_nat$FID)

r_info_bs_nat <- left_join(r_info, bs_nat[,c("FID", "ertrag_bez", "ertrag_kur", "NAME_HAUPT", "NAME_UNTER")], by = "FID")

library(stringr)

r_info_bs_nat2 <- r_info_bs_nat[!str_detect(rownames(r_info_bs_nat), "\\.1"),] # komische Dopplungen raus


## großer Vergleichsdatensatz (Rasterpunkte alle 1500m entfernt) einladen

# Umwandlung von shape in csv
#HG <-  st_read("./analysis/data/geodata/abfrage_BB_regelm_punkte_1500_DGM-derivate.shp", crs = 25833)
#coords <- st_coordinates(HG)
#sites3 <- cbind(st_drop_geometry(HG), round(coords, 2) )
#write.csv2(sites3, "./analysis/data/geodata/HG_tab.csv")

HG <- read.csv2( "./analysis/data/geodata/HG_tab.csv")

HG$FST <- paste0("Rasterpunkt ", as.character(HG$id))
HG$FID <-  as.character(HG$id)

HG_for_merge <- HG |> select(LFD = id, FID, FST, KREIS = NAME_3, kultur2, c(DGM:Bodenabtra) )

HG_for_merge <- HG_for_merge |>
  mutate(KREIS = case_when(
    KREIS == "Frankfurt am Oder Städte" ~ "Frankfurt (Oder)",
    KREIS == "Brandenburg an der Havel Städte" ~ "Brandenburg (Havel)",
    KREIS == "Potsdam Städte" ~ "Potsdam",
    KREIS == "Cottbus Städte" ~ "Cottbus",
    TRUE ~ KREIS
  ))

r_info_for_merge <- r_info |> select(LFD, FID, FST, KREIS, kultur2, c(DGM:Bodenabtra) )

r_info_HG <- rbind(as.data.frame(HG_for_merge), as.data.frame(r_info_for_merge) )

r_info_HG <- r_info_HG |>
  mutate(KREIS = case_when(
    KREIS == "Frankfurt am Oder Städte" ~ "Frankfurt (Oder)",
    KREIS == "Brandenburg an der Havel Städte" ~ "Brandenburg (Havel)",
    KREIS == "Potsdam Städte" ~ "Potsdam",
    KREIS == "Cottbus Städte" ~ "Cottbus",
    TRUE ~ KREIS
  )) |>
  mutate(kreislabel = case_when(
    KREIS == "Teltow-Fläming" ~ "TF" ,
    KREIS == "Elbe-Elster" ~ "EE",
    KREIS == "Havelland" ~ "HVL",
    KREIS == "Landkreis Barnim" ~ "BAR",
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

r_info_HG_bs_nat <- left_join(r_info_HG, bs_nat[,c("FID", "ertrag_bez", "ertrag_kur", "NAME_HAUPT", "NAME_UNTER")], by = "FID")


r_info_HG <- r_info_HG |>
  mutate(kr_group = case_when(
    str_detect("UM|BAR", kreislabel) ~ "Uckermark-Barnim",
    str_detect("PR|OPR|OHV",kreislabel) ~ "Prignitz-Oberhavel",
    str_detect("DSW|EE|OSL|SPN",kreislabel) ~ "Lausitz-Spreewald",
    str_detect("HVL|PM|P|BRB",kreislabel) ~ "Havelland",
    str_detect("TF",kreislabel) ~ "Teltow-Fläming",
    str_detect("MOL|FF|LOS",kreislabel) ~ "Oderland-Spree"
  )          ) |>
  filter(kultur2 != "GG")

r_info_HG$kultur2 <- as.character(r_info_HG$kultur2)

r_info_HG <- r_info_HG |>
  dplyr::mutate(kultur2 = case_when(
    kultur2 == "SBK" & KREIS %in% c("Uckermark","Spree-Neiße", "Ostprignitz", "Frankfurt (Oder)", "Frankfurt am Oder Städte" , "Dahme-Spreewald", "Oberspreewald-Lausitz" ,"Elbe-Elster", "Märkisch-Oderland", "Oder-Spree", "Prignitz",  "Oberhavel",  "Barnim") ~ "SRK",
    TRUE ~ kultur2))


r_info_HG$kultur2 <- factor(r_info_HG$kultur2, ordered = TRUE,
                            levels = c("GG alle 1500m", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK",  "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))




