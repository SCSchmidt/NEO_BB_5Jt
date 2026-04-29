## Analyse über Polen, Brandenburg und Mitteldeutschland mit Daten der SGDBE

library(sf)
library(viridis)
library(ggplot2)
library(ggh4x)

library(dplyr)

#sites <- st_read("./analysis/data/geodata/Fst_Standortanalysen_2.shp", crs = 25833) #Fst ohne Kleinpolen und Heiligkreuz
#coords <- st_coordinates(sites)
#sites3 <- cbind(st_drop_geometry(sites), round(coords, 2) )

#write.csv2(sites3, "./analysis/data/geodata/sites_fst-analyse2_tab.csv")

sites <- read.csv2("./analysis/data/geodata/sites_fst-analyse2_tab.csv")

sites <- sites |>
  mutate(kultur2 = case_when(
    KULTUR == "BKK?" & LAND == "Deutschland" ~ "Guhrau",
    KULTUR == "BKK" & LAND == "Deutschland" ~ "Guhrau",
    KULTUR == "BKK?" & LAND == "Polen" ~ "BKK",
    KULTUR == "SBK" & LAND == "Polen" ~ "SRK",
    KULTUR == "SBK" & BUNDESLAND == "Brandenburg" & KREIS != "Teltow-Fläming" ~ "SRK",
    FST == "Prettin 6" ~ "SRK",
    KULTUR == "SBK?" ~ "SRK",
    KULTUR == "Rössen" ~ "Rös",
    KULTUR == "Rössener" ~ "Rös",
    KULTUR == "Mesolithikum" ~ "Meso",
    TRUE ~ KULTUR
  ))

sites$fid <- as.integer(sites$FID)

sites$kultur2[sites$kultur2 == "SRK?"] <- "SRK"


#sampling_sites <- st_read("./analysis/data/geodata/sampling_punkte.shp", crs = 25833) #sampling punkte ohne Kleinpolen und Heiligkreuz
#coords <- st_coordinates(sampling_sites)
#sites3 <- cbind(st_drop_geometry(sampling_sites), round(coords, 2) )
#write.csv2(sites3, "./analysis/data/geodata/sampling_sites_fst-analyse2_tab.csv")

sampling_sites <- read.csv2("./analysis/data/geodata/sampling_sites_fst-analyse2_tab.csv")

sampling_sites$name_pl <- gsub("województwo", "Woj.", sampling_sites$name_pl)

library(stringr)
sampling_sites$name_pl <- str_to_title(sampling_sites$name_pl)

sampling_sites <- sampling_sites |>
  mutate(BUNDESLAND = case_when(
    startsWith(adm1_code, "DEU") ~ name_de,
    startsWith(adm1_code, "POL") ~ name_pl
  ))

sampling_sites <- sampling_sites |>
  mutate(LAND = case_when(
    startsWith(adm1_code, "DEU") ~ "Deutschland",
    startsWith(adm1_code, "POL") ~ "Polen"
  ))


#### Verteilung der Fundstellen auf untersch. Bundesländer - Plot

vgl_bundesl <- as.data.frame(table(sites$BUNDESLAND, sites$kultur2))

vgl_bundesl |>  
  ggplot()+
  geom_tile(aes(x = Var2,
                y = Var1,
                fill = Freq))+
  scale_fill_viridis(trans = scales::log_trans(base = 10),
                     na.value = "white")+
  geom_text(aes(x = Var2,
                y = Var1,
                label = Freq ), # format forces not to use eulersche zahl
            col = "white")+
  labs(fill = "Anz. Fundstellen \n(Skala logarithmiert)",
       x = "Kulturgruppe",
       y = "Bundesland")+
  theme_bw()

ggsave("./analysis/figures/Fst_Bundesland_ohneLBK.png", dpi = 300, width = 17.5, height = 10, units = "cm")


### Standortparameter der arch. Fundstellen einladen

ls <- list.files("./analysis/data/geodata/standortanalyse_sgdbe_2025-02-04/")  # die Dateien (output der Catchment-Analyse)

var_vec <- c() #empty vector # um Überblick zu behalten

library(tidyr)

for (i in ls){
  path <- paste0("./analysis/data/geodata/standortanalyse_sgdbe_2025-02-04/", i) 
  d <- read.csv2(path, sep = ",", dec = ".") # lies eine Datei ein
  j <- substring(i, 1, nchar(i) - 4) # extrahier den eigentlichen Variablennamen
  
  colnames(d) <- c("id", "obj", "area", "area_p")
  
  standort_sites <- left_join(sites, d, by = c("fid" = "id") ) # mit den Geo-Informationen der Fst verbinden
  
  standort_sites <- standort_sites |> 
    st_drop_geometry() |>
    group_by(fid, FST, GEMEINDE, KREIS, BUNDESLAND, LAND, FLUR, kultur2, obj) |> # aufsummieren der Teilflächen
    summarise(sum_p_area = sum(area_p)) |>
    ungroup()  |>
    pivot_wider(names_from = obj,                      # pivot wider, um eine Zeile pro Fst zu erreichen
                id_cols = c(fid, FST, GEMEINDE, KREIS, BUNDESLAND, LAND, FLUR, kultur2),
                values_from = sum_p_area, 
                names_prefix = paste0(j, "_") ) # Variablenname vor Ausprägung d. Variablen
  
  assign(paste0("site_", j), standort_sites) # Variable d. Datensatzes ändern
  
  var_vec <- c(var_vec, j) # gather all variables -> Überblick behalten
  
}

# alle Datensätze verschmelzen:

df_list <- list(site_AGLI1NNI, site_AGLI2NNI, site_AWC_SUB, site_AWC_TOP, site_BS_SUB, site_BS_TOP, site_DGH, site_PARMADO, site_PARMASE, site_TXSRFDO, site_TXSRFSE, site_ZMAX, site_ZMIN, site_WRBFU, site_FAO85FU)

all_standortpara <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

all_standortpara <- all_standortpara |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0) ))

save(all_standortpara, file = "./analysis/data/derived_data/all_standortparameter_2025-01-07.RData")


######### sampling points Standortparameter laden

ls <- list.files("./analysis/data/geodata/standort_sampling/")  # die Dateien (output der Catchment-Analyse)

var_vec <- c() #empty vector # um Überblick zu behalten

sampling_sites2 <- sampling_sites |>
  st_drop_geometry() 

library(tidyr)

for (i in ls){
  path <- paste0("./analysis/data/geodata/standort_sampling/", i) 
  d <- read.csv2(path, sep = ",", dec = ".") # lies eine Datei ein
  j <- substring(i, 1, nchar(i) - 4) # extrahier den eigentlichen Variablennamen
  
  colnames(d) <- c("id", "obj", "area", "area_p")
  
  vergleichspunkte <- left_join(sampling_sites2, d, by = c("fid" = "id") ) # mit den Geo-Informationen der Fst verbinden
  
  vergleichspunkte <- vergleichspunkte |> 
    group_by(fid, BUNDESLAND, obj) |> # aufsummieren der Teilflächen
    summarise(sum_p_area = sum(area_p)) |>
    ungroup()  |>
    pivot_wider(names_from = obj,                      # pivot wider, um eine Zeile pro Fst zu erreichen
                id_cols = c(fid, BUNDESLAND),
                values_from = sum_p_area, 
                names_prefix = paste0(j, "_") ) # Variablenname vor Ausprägung d. Variablen
  
  assign(paste0("sampl_site_", j), vergleichspunkte) # Variable d. Datensatzes ändern
  
  var_vec <- c(var_vec, j) # gather all variables -> Überblick behalten
  
}

# alle Datensätze verschmelzen:

df_list <- list(sampl_site_AGLI1NNI, sampl_site_AGLI2NNI, sampl_site_AWC_SUB, sampl_site_AWC_TOP, sampl_site_BS_SUB, sampl_site_BS_TOP, sampl_site_DGH, sampl_site_PARMADO, sampl_site_PARMASE, sampl_site_TXSRFDO, sampl_site_TXSRFSE, sampl_site_ZMAX, sampl_site_ZMIN, sampl_site_WRBFU, sampl_site_FAO85FU)

vgl_para <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

vgl_para <- vgl_para |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0) ))


save(vgl_para, file = "./analysis/data/derived_data/vgl_standortpara_2025-01-06.RData")

### daten verschmelzen

load(file = "./analysis/data/derived_data/all_standortparameter_2025-01-07.RData")

load( "./analysis/data/derived_data/vgl_standortpara_2025-01-06.RData")

vgl_para$kultur2 <- "GG/Bundesland"

vgl_para$fid <- vgl_para$fid + 1000 # ACHTUNG! Sonst gleiche fid bei all_standortpara (geht bis 513) -> für späteres Zusammenführen mit sites aber von Bedeutung!

vgl_standort_all <- plyr::rbind.fill(all_standortpara, vgl_para)

library(tidyr)

vgl_standort_all <- vgl_standort_all |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0) ))

vgl_standort_all$kultur2[vgl_standort_all$kultur2 == "SRK?"] <- "SRK" # ein unsicherer Fst, die evtl auch Jordansmühl sein könnte, aber bei Uhyst

vgl_standort_all$kultur2 <- factor(vgl_standort_all$kultur2, ordered = TRUE,
                                   levels = c("Meso", "FBG", "Rös", "Guhrau", "BKK", "SRK", "SBK", "GG/Bundesland") )

vgl_standort_all <- vgl_standort_all |>
  filter(!(BUNDESLAND %in% c("Berlin","Niedersachsen", "Woj. Łódzkie", "Woj. Mazowieckie", "Woj. Podlaskie", "Woj. Świętokrzyskie", "Mecklenburg-Vorpommern", "MVP")) )

vgl_standort_all <- vgl_standort_all |>
  mutate(Bndslnd = case_when(
    BUNDESLAND == "Brandenburg" ~ "Brandenb.",
    BUNDESLAND == "Sachsen" ~ "Sachs.",
    BUNDESLAND == "Sachsen-Anhalt" ~ "Sachs.-Anh.",
    BUNDESLAND == "Thüringen" ~ "Thür.",
    BUNDESLAND == "Woj. Dolnośląskie" ~ "Dolnoślą.",
    BUNDESLAND == "Woj. Kujawsko-Pomorskie" ~ "Kuj.-Pomorsk.",
    BUNDESLAND == "Woj. Lubuskie" ~ "Lubus.",
    BUNDESLAND == "Woj. Małopolskie" ~ "Małopol.",
    BUNDESLAND == "Woj. Opolskie" ~ "Opolsk.",
    BUNDESLAND == "Woj. Pomorskie" ~ "Pomorsk.",
    BUNDESLAND == "Woj. Śląskie" ~ "Śląsk.",
    BUNDESLAND == "Woj. Warmińsko-Mazurskie" ~ "Warm.-Mazur.",
    BUNDESLAND == "Woj. Wielkopolskie" ~ "Wielkopol.",
    BUNDESLAND == "Woj. Zachodniopomorskie" ~ "Zachodniopom."
  ))