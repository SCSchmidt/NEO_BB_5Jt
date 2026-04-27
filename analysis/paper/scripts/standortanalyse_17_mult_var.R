## Multivariate Analyse Brandenburger Fundstellenlagen

source("./analysis/paper/scripts/standortanalyse_01_datenvorbereitung.R")

library(stringr)

fst_bb <- sites
#ls <- list.files("../data/geodata/standortanalyse_500_BB/")

#ls <- ls[ ls != "mesorel.csv"] # muss vorher händisch kategorisiert werden

ls <- list("domlil.csv" , "ertrag_bez.csv", "gefuege.csv" ,"geolog.csv" ,   "gruppe.csv"  , "hauptgru_1.csv" , "hft.csv"  , "layer.csv" ,      "raeumhet.csv", "schwarzerd.csv" ,  "steinob.csv")  

var_vec <- c() 

for (i in ls){
  path <- paste0("./analysis/data/geodata/standortanalyse_500_BB/", i) 
  d <- read.csv2(path, sep = ",", dec = ".") # lies eine Datei ein
  
  j <- substring(i, 1, nchar(i) - 4) # extrahier den eigentlichen Variablennamen
  
  colnames(d) <- c("id", "obj", "area", "area_p")
  d$id <- as.character(d$id)
  
  standortpara_bb <- left_join(fst_bb, d, by = c("FID" = "id") ) # mit den Geo-Informationen der Fst verbinden
  
  bredow_roes <- standortpara_bb |> filter(LFD == 7 & FID == 3007)
  
  bredow_roes$FID <- "3007.2"
  bredow_roes$kultur2 <- "Rös"
  bredow_roes$KULTUR<- "Rössener"
  bredow_roes$Fundart <- ""
  bredow_roes$QUELLE <- "Kirsch 1993 / (c) Denkmaldaten BLDAM 2021"
  
  standortpara_bb <- rbind(standortpara_bb, bredow_roes)
  
  standortpara_bb$kultur2[standortpara_bb$FST == "Potsdam 16"] <- "Rös"
  standortpara_bb$FST[standortpara_bb$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"
  
  
  potsdam_sbk <- standortpara_bb |> filter (FST == "Potsdam 3/13 (3)")
  potsdam_sbk$FID <- "265.2"
  potsdam_sbk$KULTUR <- "Stichbandkeramik"
  potsdam_sbk$kultur2 <- "SBK"
  potsdam_sbk$Fundart <- "Lesefund"
  
  standortpara_bb <- rbind(standortpara_bb, potsdam_sbk)
  
  
  standortpara_bb <- standortpara_bb |> 
    group_by(FID, KREIS, kultur2, obj) |> # aufsummieren der Teilflächen
    summarise(sum_p_area = sum(area_p)) |>
    ungroup()  |>
    pivot_wider(names_from = obj,                      # pivot wider, um eine Zeile pro Fst zu erreichen
                id_cols = c(FID, kultur2, KREIS),
                values_from = sum_p_area, 
                names_prefix = paste0(j, "_") ) |>  # Variablenname vor Ausprägung d. Variablen
    mutate(across(where(is.numeric), ~ replace_na(.x, 0) ))
  
  assign(paste0("site_", j), standortpara_bb) # Variable d. Datensatzes ändern
  
  var_vec <- c(var_vec, j) # gather all variables -> Überblick behalten
  
}

### Zusammenfassung von Mesorelief

path <- paste0("./analysis/data/geodata/standortanalyse_500_BB/mesorel.csv") 
d <- read.csv2(path, sep = ",", dec = ".") # lies eine Datei ein

colnames(d) <- c("id", "obj", "area", "area_p")

d <- d |>
  mutate(obj3 = case_when(
    str_detect(obj, "Aue|aue")  ~ "Niederung / Aue",
    str_detect(obj, "Niederung") ~"Niederung / Aue",
    obj == "Wellige Platte" ~ "Wellige (bis stark) Platte",
    obj == "Geneigte und wellige Platte" ~ "Wellige (bis stark) Platte",
    obj == "ebene Platte" ~  "Eben bis flach wellige Platte",
    obj == "Flach wellige Platte" ~  "Eben bis flach wellige Platte",
    obj == "Schwach wellige Platte" ~  "Eben bis flach wellige Platte",
    obj == "Ebene bis wellige Platte" ~  "Eben bis flach wellige Platte",
    obj == "Ebene bis schwach wellige Platte" ~  "Eben bis flach wellige Platte",
    obj == "Ebene bis flach wellige Platte" ~  "Eben bis flach wellige Platte",
    obj == "Eben bis flache Platte" ~  "Eben bis flach wellige Platte",
    obj == "Ebene Platte mit abfallendem Rand" ~  "Eben bis flach wellige Platte",
    obj == "Flach kuppige Platte" ~  "Eben bis flach wellige Platte",
    obj == "wellige Platte" ~   "Wellige (bis stark) Platte" ,
    obj == "Wellige Platte untergeordnet als s.flache Hügel" ~  "Wellige (bis stark) Platte" ,
    obj ==  "Stark wellige Platte"  ~   "Wellige (bis stark) Platte" ,
    obj == "Wellige bis kuppige Platte"  ~   "Wellige (bis stark) Platte" ,
    obj == "Platte mit Hügel und Senken" ~   "Wellige (bis stark) Platte" ,
    obj == "Stark kuppige Platte" ~   "Wellige (bis stark) Platte" ,
    obj == "Wellige Platte mit Abfall" ~   "Wellige (bis stark) Platte" ,
    obj == "Wellige Platte untergeordnet.s.fl. Hügel<10 m"~   "Wellige (bis stark) Platte" ,
    obj ==  "Ebene" ~  "Ebene",
    obj == "Ebene (schwach geneigt)"~  "Ebene",
    obj ==  "Tal mit Boden"   ~  "Tal",
    str_detect(obj, "Kuppige Platte|kuppige Platte")  ~   "Wellige (bis stark) Platte" ,
    obj == "Wellige Platte mit s.flachem Hang zur Ebene" ~   "Wellige (bis stark) Platte" ,
    obj ==  "Tal ohne Boden"   ~  "Tal ohne Boden / Rinne" ,
    obj == "Tal ohne Boden (Rinne)" ~  "Tal ohne Boden / Rinne" ,
    obj == "Tal ohne Boden (Rinnen)" ~  "Tal ohne Boden / Rinne" ,
    obj == "Tal ohne Boden mit Vollhang" ~  "Tal ohne Boden / Rinne" ,
    obj == "Wellige Platte mit Becken" ~ "Wellige Platte mit Becken / Senken",
    str_detect(obj, "Hügel-Riedel-Komplex") ~ "Hügel-Riedel-Komplex",
    str_detect(obj, "Hügelgruppe") ~  "Hügel(-komplex)",
    obj == "Flach wellige Platte mit Becken" ~  "Eben bis flach wellige Platte mit Becken" ,
    obj == "Ebene Platte mit Becken" ~  "Eben bis flach wellige Platte mit Becken" ,
    obj == "Geneigte Platte mit Becken"  ~ "Geneigte Platte",
    obj == "Geneigte Platte" ~ "Geneigte Platte",
    obj == "Becken" ~ "Mulde / Becken / Senke"    ,
    obj == "Senke" ~ "Mulde / Becken / Senke"    ,
    obj == "Hügel-Mulden-Komplex" ~ "Hügel-Mulden-Komplex",
    obj == "Stufenhang"  ~ "Stufenhang",
    str_detect(obj, "Zerschn") ~ "Zerschnittene Platte / Hang",
    obj == "Kuppige Platte mit Taleinschnitten"~ "Zerschnittene Platte / Hang",
    obj == "Wellige Platte mit Senken" ~ "Wellige Platte mit Becken / Senken",
    obj == "Platte mit Hügel und Senke"~ "Wellige Platte mit Becken / Senken",
    obj == "Kuppige Platte mit Becken" ~ "Kuppige Platte mit Becken",
    obj == "Terrasse"  ~ "Stufenhang",
    obj == "Terrassenebene" ~ "Stufenhang",
    obj == "Terrassenplatte" ~ "Stufenhang",
    str_detect(obj, "Niederungsrand") ~ "Niederung / Aue",
    obj == "Talausweitung"~  "Tal",
    obj == "Talebene" ~ "Tal",
    obj == "Flache Mulde"~  "Mulde / Becken / Senke"    ,
    obj == "Seeterrasse - Mulde"~  "Mulde / Becken / Senke"    ,
    str_detect(obj, "Rinne") ~   "Tal ohne Boden / Rinne",
    obj =="Tal (Entwässerungsrinne)" ~   "Tal ohne Boden / Rinne",
    obj ==  "Terassenebene" ~  "Stufenhang",
    obj ==  "Talsandinsel"  ~ "Talsandinsel" ,
    obj ==  "Terrassenplatte" ~  "Stufenhang",
    obj == "Wellige Platte mit Senken" ~ "Wellige Platte mit Becken / Senken",
    str_detect(obj, "-Mulden-Komplex") ~ "Hügel-Mulden-Komplex",
    str_detect(obj, "Eben|eben") ~ "Eben bis flach wellige Platte",
    str_detect(obj, "Flach wellige|Schwach wellige") ~ "Eben bis flach wellige Platte"  ,
    obj == "Hang" ~ "Hang (gr.)",
    obj == "Vollhang" ~ "Hang (gr.)",
    str_detect(obj, "Streckhang") ~ "Hang (gr.)",
    obj == "Flach abfallender Hang"~ "Hang (gr.)",
    obj == "Gewölbter Hangkomplex"~ "Hang (gr.)",
    obj == "Tal" ~"Tal",
    obj == "Hügel" ~ "Hügel(-komplex)",
    obj == "Hügelrücken" ~ "Hügel(-komplex)",
    obj == "Hügelrücken mit Verebenung" ~ "Hügel(-komplex)",
    obj == "Kuppige Platte - Hügelgruppe"~ "Hügel(-komplex)",
    str_detect(obj, "kupp") ~ "Kuppe",
    str_detect(obj, "Kupp") ~ "Kuppe",
    obj == "-" ~ NA,
    obj == "" ~ NA,
    obj == "NA" ~ NA,
    obj == "Kippe" ~ NA, # Kippe  =  anthropogene Veränderung
    str_detect(obj, "Mulde")  ~ "Mulde / Becken / Senke"    ,
    TRUE ~ obj
  ))              

d$id <- as.character(d$id)

standortpara_bb <- left_join(fst_bb, d, by = c("FID" = "id") ) # mit den Geo-Informationen der Fst verbinden

bredow_roes <- standortpara_bb |> filter(LFD == 7 & FID == 3007)

bredow_roes$FID <- "3007.2"
bredow_roes$kultur2 <- "Rös"
bredow_roes$KULTUR<- "Rössener"
bredow_roes$Fundart <- ""

bredow_roes$QUELLE <- "Kirsch 1993 / (c) Denkmaldaten BLDAM 2021"

standortpara_bb <- rbind(standortpara_bb, bredow_roes)

standortpara_bb$kultur2[standortpara_bb$FST == "Potsdam 16"] <- "Rös"
standortpara_bb$FST[standortpara_bb$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"


potsdam_sbk <- standortpara_bb |> filter (FST == "Potsdam 3/13 (3)")
potsdam_sbk$FID <- "265.2"
potsdam_sbk$KULTUR <- "Stichbandkeramik"
potsdam_sbk$kultur2 <- "SBK"
potsdam_sbk$Fundart <- "Lesefund"

standortpara_bb <- rbind(standortpara_bb, potsdam_sbk)


standortpara_bb <- standortpara_bb |> 
  group_by(FID, KREIS, kultur2, obj3) |> # aufsummieren der Teilflächen
  summarise(sum_p_area = sum(area_p)) |>
  ungroup()  |>
  pivot_wider(names_from = obj3,                      # pivot wider, um eine Zeile pro Fst zu erreichen
              id_cols = c(FID, kultur2, KREIS),
              values_from = sum_p_area, 
              names_prefix = paste0("mesorel_gr", "_") ) |>  # Variablenname vor Ausprägung d. Variablen
  mutate(across(where(is.numeric), ~ replace_na(.x, 0) ))

site_gruppe_gr <- standortpara_bb

## unkorrigierte daten erstmal zusammenführen und speichern
standort_para_merge_mNA <- para <- Reduce(function(x, y) merge(x, y, all=TRUE), list(site_domlil, site_gefuege, site_gruppe, site_gruppe_gr, site_hauptgru_1,  site_raeumhet, site_schwarzerd, site_steinob, site_layer))

save(standort_para_merge_mNA, file = "./analysis/data/derived_data/unkorrigierteBodenparameter.RData")


standort_para_merge_oNA <- standort_para_merge_mNA |>
  select(-c(domlil_, domlil_NA, gefuege_, gefuege_NA, `gruppe_Versiegelungsflächen mit Böden aus Bauschutt führenden Substraten`, `gruppe_Versiegelungsflächen mit Böden aus Bauschutt führenden Substraten`, gruppe_NA, mesorel_gr_NA, hauptgru_1_NA, `hauptgru_1_Böden aus anthropogen abgelagerten Sedimenten`,layer_NA, raeumhet_, raeumhet_NA, schwarzerd_NA, steinob_NA, steinob_, `steinob_--` ))

save(standort_para_merge_oNA, file = "./analysis/data/derived_data/unkorrigierteBodenparameter_ohneNAspalten.RData")


## NA korrektur einzeln

site_mesorel_gr <- site_gruppe_gr |>
  filter(mesorel_gr_NA < 0.2) |>
  mutate(across(where(is.numeric), ~ .x +  mesorel_gr_NA / (ncol(site_gruppe_gr) - 4)  ) ) |>
  select(- mesorel_gr_NA)

##  Datenbereinigung der NAs

# 1. alles mit NAs markieren
# 2. Anteil von NA über 20 % -> rausfiltern
# 3. restliche verteilen (wenn NA unter 20 % -> deren Spalte löschen und die Anteile auf die anderen Kategorien verteilen)

colnames(site_layer) <- c("FID", "kultur2", "KREIS", "Seenfläche", "Festland", "NA_BodenSee")

site_layer$Festland <- 1 - site_layer$Seenfläche

colnames(site_schwarzerd) <- c( "FID", "kultur2", "KREIS", "keineSchwarzerde" , "Schwarzerden", "schwarzerd_NA")

site_schwarzerd <- site_schwarzerd |> select(-schwarzerd_NA)

site_layer <- site_layer |> select(-NA_BodenSee)

# layer, ertrag_bez, site_schwarzerd -> keine NA-Werte

## bei ertrag_bez die überprägungsspalten in der jeweils passenden verrechnen

site_ertrag_bez$`ertrag_bez_Bodenzahlen vorherrschend <30` <- site_ertrag_bez$`ertrag_bez_Bodenzahlen vorherrschend <30` +  site_ertrag_bez$`ertrag_bez_Bodenzahlen überwiegend <30 und verbreitet versiegelt`

site_ertrag_bez$`ertrag_bez_Bodenzahlen vorherrschend <30` <- site_ertrag_bez$`ertrag_bez_Bodenzahlen vorherrschend <30` +  site_ertrag_bez$`ertrag_bez_überwiegend versiegelt und verbreitet Bodenzahlen <30`

site_ertrag_bez$`ertrag_bez_Bodenzahlen vorherrschend 30 - 50` <- site_ertrag_bez$`ertrag_bez_Bodenzahlen vorherrschend 30 - 50` + site_ertrag_bez$`ertrag_bez_überwiegend versiegelt und verbreitet Bodenzahlen 30 - 50`

site_ertrag_bez <- site_ertrag_bez |> select(-c(`ertrag_bez_Bodenzahlen überwiegend <30 und verbreitet versiegelt`, `ertrag_bez_überwiegend versiegelt und verbreitet Bodenzahlen <30`, `ertrag_bez_überwiegend versiegelt und verbreitet Bodenzahlen 30 - 50`, ertrag_bez_NA ))


# geolog, gruppe und mesorel hat mehrere NA-Werte (zusammenrechnen, dann verteilen auf die übrigen columns, dann löschen)
site_geolog$NAwerte <- site_geolog$geolog_ + site_geolog$geolog_NA

site_geolog2 <- site_geolog |>
  filter(NAwerte < 0.2) 

site_gruppe$NAwerte <- site_gruppe$gruppe_NA + site_gruppe$"gruppe_Versiegelungsflächen mit Böden aus Industrie- und Bauschutt führenden Substraten" + site_gruppe$"gruppe_Versiegelungsflächen mit Böden aus Bauschutt führenden Substraten"   + site_gruppe$"gruppe_Böden aus anthropogen abgelagerten natürlichen Substraten"  + site_gruppe$"gruppe_Böden aus Bauschutt führenden und z.T. umgelagerten natürlichen Substraten mit Versiegelungsflächen"                                        


site_gruppe2 <- site_gruppe |>
  filter(NAwerte < 0.2) #|>

site_gruppe2 <- site_gruppe2 |>
  select(-c('gruppe_NA', "gruppe_Versiegelungsflächen mit Böden aus Industrie- und Bauschutt führenden Substraten" , "gruppe_Versiegelungsflächen mit Böden aus Bauschutt führenden Substraten", "gruppe_Böden aus anthropogen abgelagerten natürlichen Substraten", "gruppe_Böden aus Bauschutt führenden und z.T. umgelagerten natürlichen Substraten mit Versiegelungsflächen") )|>   
  mutate(across(where(is.numeric), ~ .x + site_gruppe2$NAwerte / (ncol(site_gruppe) - 9) ) ) |>
  select(- NAwerte) 


#site_mesorel$NAwerte <- site_mesorel$mesorel_ + site_mesorel$mesorel_NA
#site_mesorel <- site_mesorel |>
#   mutate(across(where(is.numeric), ~ .x + site_mesorel$NAwerte / (ncol(site_mesorel) - 6) ) ) |>
#  select(- c(NAwerte, mesorel_, mesorel_NA))

# Hauptgruppe nur ein NA-Spalte

site_hauptgru_1_2 <-site_hauptgru_1|>
  filter(hauptgru_1_NA < 0.2) |>
  mutate(across(where(is.numeric), ~ .x + site_hauptgru_1$hauptgru_1_NA / (ncol(site_hauptgru_1) - 4) ) ) |>
  select(- hauptgru_1_NA)

##  Datenbereinigung MMK -Daten

site_raeumhet$NAwerte <- site_raeumhet$raeumhet_ + site_raeumhet$raeumhet_NA

site_raeumhet2 <- site_raeumhet |>
  filter(NAwerte < 0.2)

site_raeumhet2 <- site_raeumhet2 |>
  mutate(across(where(is.numeric), ~ .x + NAwerte / (ncol(site_raeumhet) - 6) ) ) |>
  select(-c("NAwerte", "raeumhet_NA", "raeumhet_"))


site_steinob$NAwerte <- site_steinob$steinob_ + site_steinob$steinob_-- + site_steinob$steinob_NA

site_steinob2 <- site_steinob |>
  filter(NAwerte < 0.2)

site_steinob2 <- site_steinob2 |>
  mutate(across(where(is.numeric), ~ .x + NAwerte / (ncol(site_steinob) - 7) ) ) |>
  select(-c("NAwerte", "steinob_NA", "steinob_", "steinob_--"))

site_hft$NAwerte <- site_hft$hft_ + site_hft$hft_NA

site_hft2 <- site_hft |>
  filter(NAwerte < 0.2)

site_hft2 <- site_hft2 |>
  mutate(across(where(is.numeric), ~ .x + NAwerte / (ncol(site_hft) - 6) ) ) |>
  select(-c("NAwerte", "hft_NA", "hft_"))

site_domlil2 <- site_domlil |>
  filter(domlil_ + domlil_NA < 0.2)

site_domlil2 <- site_domlil2 |>
  mutate(across(where(is.numeric), ~ (.x + domlil_ + domlil_NA ) / (ncol(site_domlil) - 5) ) ) |>
  select(-c("domlil_", "domlil_NA"))


##  alle Datensätze verschmelzen

df_list <- list(site_ertrag_bez, site_geolog2, site_gruppe2, site_hauptgru_1_2, site_layer, site_mesorel_gr, site_schwarzerd, site_hft2, site_steinob2, site_raeumhet2, site_domlil2 )

para <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)


para <- para |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    kultur2 == "LBK Siedl." & KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam")  ~  "LBK westl. Trad.",
    TRUE ~ kultur2
  ) )


save(para, file =  "./analysis/data/derived_data/comp_data_BB_detail_neueNAremovestrategie.RData")

### 

# umwandeln der Umkreisdaten von 500m in ranked data

load("./analysis/data/derived_data/unkorrigierteBodenparameter_ohneNAspalten.RData")

standort_para_merge_oNA <- standort_para_merge_oNA |> select(-c(`gruppe_Böden aus anthropogen abgelagerten natürlichen Substraten`, `gruppe_Versiegelungsflächen mit Böden aus Industrie- und Bauschutt führenden Substraten`, `gruppe_Versiegelungsflächen mit Böden aus Industrie- und Bauschutt führenden Substraten`))

AZ500 <- read.csv2("./analysis/data/geodata/standortanalyse_500_BB/ertrag_bez.csv", sep = ",", dec = ".")

AZ500 <- AZ500 |>
  group_by(id, obj) |>
  mutate(sum_area = sum(area.)) |>
  ungroup () |>
  select(obj, sum_area, id) |>
  unique()

AZ <- AZ500 |>
  select(obj, sum_area, id) |>
  pivot_wider(names_from = obj,
              values_from = sum_area,
              id_cols = id
  ) |>
  select(-c(starts_with("überwiegend versiegelt")) ) |>
  mutate(`Bodenzahl vorherrschend <30` = `Bodenzahlen vorherrschend <30` + `Bodenzahlen überwiegend <30 und verbreitet versiegelt` ) |>
  select(- `Bodenzahlen überwiegend <30 und verbreitet versiegelt` )

AZ$id <- as.character(AZ$id)

para <- left_join(standort_para_merge_oNA, AZ, by = c("FID" = "id") )

para[is.na(para)] <-0

para[para == 1] <- 5
para[para < 1 & para >= 0.75] <- 4
para[para <  0.75 & para >= 0.5] <- 3
para[para < 0.5 & para >= 0.25] <- 2
para[para < 0.25 & para > 0] <- 1
para[para == 0] <- 0


load("./analysis/data/derived_data/fst_bs_punktabfrage_maxwerte.RData")

fst_bs_500 <- fst_bs |> filter(r == 500)

para <- left_join(para,
                  fst_bs_500)

para_fst <- para


###  rasterdatenwerte laden
library(vegan)
library(dplyr)


load("./analysis/data/derived_data/raster_based_standortpara_BB.RData")

raster <- r_info |>
  select(FID, FST, DGM, EL.soilMoi) |>
  unique()

para_fst_o <- para_fst |> 
  mutate_at(vars(domlil_Lh:`Bodenzahl vorherrschend <30`), ~ factor(.x, levels = c(0,1,2,3,4,5)) )

para_fst_r <- left_join(para_fst_o, raster, by = c("FID") ) # FST nicht immer exakt gleich (facepalm), aber FID

para_fst_r <- para_fst_r |>
  filter(kultur2 != "GG" ) |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2  ))    |>
  select(-(starts_with("gefuege"))) # Gefüge nicht hilfreich


## Datenvorbereitung mit auch DGM und soilMoisture

para_d <- para_fst_r |> select(-c(KREIS, r, FST.x,  FST.y, kultur2, FID
))

para_d <- para_d |>
  select(-c("schwarzerd_bb_admin3", "layer_bb_admin3")) # das sind die Anteile nicht-schwarzerde und nicht-See (können raus)

# einige der ranked data haben nur 0 und 1
# check for variables with only 0 and 1, because they need to be numeric for gowdis 
y <- apply(para_d,2,function(x) { all(na.omit(x) %in% 0:1) }) #zB domlil_sd und Bodenzahl vorherrschend <30 

bin_var <- names(y[y == TRUE]) # Colnames der binären Vars

para_d <- para_d |>
  mutate_at(vars(bin_var), ~ (as.numeric(.x)- 1) ) # hier weil die 1. Level = 0 -> wird dann zur 1 -> -1 für "echte 0 und 1"

bin_numbers <- match(bin_var, colnames(para_d))

# check 
#for (i in bin_numbers) {
#  print(para_d[,i])
#}

library(FD)

dist <- gowdis(para_d, ord = "podani", asym.bin = as.vector(bin_numbers) ) # Problem war: erkennt factor mit 0 und 1 -> als numeric codieren und asymetric binary variables angeben

RDA_all <- dbrda(dist ~ kultur2, data = para_fst_r)

an_rda <- anova(RDA_all) # höchstsignifikant
an_rda_a <- anova(RDA_all, by = "axis") # höchstsignifikante 1. und 2., danach nicht mehr

summary_rda <- summary(RDA_all) # ohne gefuege: 0.05561909
#summary_rda$cont$importance[2, "dbRDA1"]

######## PLOT

library(tidyverse)
library(geomtextpath)
library(ggforce)

palette_LBK_SBK<- c( "#61D04F", "#117733","#CD0BBC",  "#CC6677","red",  "black", "#F5C710", "#2297E6", "darkblue" )

RDA_all <- RDA_all  %>% vegan::scores(tidy = TRUE) %>% 
  split(.[["score"]])

para_fst_r$kultur2 <- factor(para_fst_r$kultur2, levels = c("Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))

library(ggplot2)

t <- RDA_all$sites
t$kultur2 <- para_fst_r$kultur2

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(aes(colour = para_fst_r$kultur2), 
             size = 1,
             alpha = 0.5) +
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- para_fst_r$kultur2),
               aes(label = kultur2, group = kultur2,  color = kultur2), 
               level = 0.95,
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  scale_colour_manual('', values = palette_LBK_SBK) +
  # coord_equal() + # das vllt raus?
  theme_minimal()+
  labs(x = paste("dbRDA1 ", round(summary_rda$cont$importance[2, "dbRDA1"]
                                  , 3)*100, "%"), 
       y = paste("dbRDA2 ", round(summary_rda$cont$importance[2, "dbRDA2"]
                                  , 3)*100, "%"),
       title = "distanzbasierte RDA über Umweltparameter",
       caption = paste("Fundstellen in Brandenburg n = ", length(unique(para_fst_r$FID)) )) +
  theme(legend.position = "right")

ggsave("./analysis/figures/Standortanalyse/gower_dbRDA_alleUmweltpara-ellipse.png", dpi = 300, width = 20, height = 16, units = "cm")

#### Analyse nach Kreis: nur UM , weil dichtest besiedelt


para_fst_rUM <- para_fst_r |>
  filter(kultur2 != "GG" ) |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2  ))    |>
  select(-(starts_with("gefuege"))) |>
  filter(KREIS == "Uckermark")


# Datenvorbereitung mit auch DGM und soilMoisture
para_d <- para_fst_rUM |> select(-c(KREIS, r, FST.x,  FST.y, kultur2, FID
))

# DAtenvorbereitung RDA nur mit BS + Kompositionsdaten
# para_d <- para_fst_o |> select(-c(KREIS, r, FST, kultur2, FID))


para_d <- para_d |>
  select(-c("schwarzerd_bb_admin3", "layer_bb_admin3"))

# einige der ranked data haben nur 0 und 1
# check for variables with only 0 and 1, because they need to be numeric for gowdis 
y <- apply(para_d,2,function(x) { all(na.omit(x) %in% 0:1) }) #zB domlil_sd und Bodenzahl vorherrschend <30 
bin_var <- names(y[y == TRUE])

para_d <- para_d |>
  mutate_at(vars(bin_var), ~ (as.numeric(.x)- 1) ) # hier weil die 1. Level = 0 -> wird dann zur 1 -> -1 für "echte 0 und 1"

bin_numbers <- match(bin_var, colnames(para_d))

# check 
#for (i in bin_numbers) {
#  print(para_d[,i])
#  }

dist <- gowdis(para_d, ord = "podani", asym.bin = as.vector(bin_numbers) ) # Problem war: erkennt factor mit 0 und 1 -> als numeric codieren und asymetric binary variables angeben

RDA_all <- dbrda(dist ~ kultur2, data = para_fst_rUM, comm = para_d)

#anova(RDA_all) # -> höchstsignifikant

summary_rda <- summary(RDA_all) 

RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

para_fst_rUM$kultur2 <- factor(para_fst_rUM$kultur2, levels = c("Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))

palette_UM<- c( "#61D04F", "#CD0BBC",  "#CC6677", "black", "#F5C710", "#2297E6", "darkblue" )

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- para_fst_rUM$kultur2),
               aes(label = kultur2, group = kultur2, color = kultur2), 
               geom = "textpath", hjust = 0.65, vjust = 1.2, linetype = 1) +
  geom_point(aes(colour = para_fst_rUM$kultur2), 
             size = 2) +
  scale_colour_manual('Kultur', values = palette_UM) +
  #  coord_equal() + # das vllt raus?
  theme_minimal()+
  labs(x = paste("dbRDA1 ", round(summary_rda$cont$importance[2, "dbRDA1"]
                                  , 3)*100, "%"), 
       y = paste("dbRDA2 ", round(summary_rda$cont$importance[2, "dbRDA2"]
                                  , 3)*100, "%"),
       title = "distanzbasierte RDA über Umweltparameter",
       caption = paste("Fundstellen in der Uckermark n = ", length(unique(para_fst_rUM$FID)) )) +
  theme(legend.position = "right")

ggsave("./analysis/figures/Standortanalyse/gower_dbRDA_alleUmweltpara_UM-ellipse.png", dpi = 300, width = 20, height = 16, units = "cm")


### nochmal alles OHNE UM



para_fst_rUM <- para_fst_r |>
  filter(kultur2 != "GG" ) |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2  ))    |>
  select(-(starts_with("gefuege"))) |>
  filter(KREIS != "Uckermark")


# Datenvorbereitung mit auch DGM und soilMoisture
para_d <- para_fst_rUM |> select(-c(KREIS, r, FST.x,  FST.y, kultur2, FID
))

# DAtenvorbereitung RDA nur mit BS + Kompositionsdaten
# para_d <- para_fst_o |> select(-c(KREIS, r, FST, kultur2, FID))


para_d <- para_d |>
  select(-c("schwarzerd_bb_admin3", "layer_bb_admin3"))

# einige der ranked data haben nur 0 und 1
# check for variables with only 0 and 1, because they need to be numeric for gowdis 
y <- apply(para_d,2,function(x) { all(na.omit(x) %in% 0:1) }) #zB domlil_sd und Bodenzahl vorherrschend <30 
bin_var <- names(y[y == TRUE])

para_d <- para_d |>
  mutate_at(vars(bin_var), ~ (as.numeric(.x)- 1) ) # hier weil die 1. Level = 0 -> wird dann zur 1 -> -1 für "echte 0 und 1"

## TODO: anderen Weg finden, die binären Variablen in numeric umzuwandeln

bin_numbers <- match(bin_var, colnames(para_d))

# check 
#for (i in bin_numbers) {
#  print(para_d[,i])
#  }

dist <- gowdis(para_d, ord = "podani", asym.bin = as.vector(bin_numbers) ) # Problem war: erkennt factor mit 0 und 1 -> als numeric codieren und asymetric binary variables angeben

RDA_all <- dbrda(dist ~ kultur2, data = para_fst_rUM, comm = para_d)

summary_rda <- summary(RDA_all) #-> 0.08899 ... nooo, nur noch die Hälfte 0.0434... mit DGM + soil moisture nur minimal besser: 0.038 ohne gefuege: 0.05561909

#anova(RDA_all) # -> höchstsignifikant


RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

para_fst_rUM$kultur2 <- factor(para_fst_rUM$kultur2, levels = c("Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))


ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(aes(colour = para_fst_rUM$kultur2), 
             size = 2) +
  scale_colour_manual('', values = palette_LBK_SBK) +
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- para_fst_rUM$kultur2),
               aes(label = kultur2, group = kultur2, color = kultur2), 
               geom = "textpath", hjust = 0.65, vjust = 1.2, linetype = 1) +
  geom_point(aes(colour = para_fst_rUM$kultur2), 
             size = 2) +
  #  scale_colour_manual('Kultur', values = palette_UM) +
  #  coord_equal() + # das vllt raus?
  theme_minimal()+
  labs(x = paste("dbRDA1 ", round(summary_rda$cont$importance[2, "dbRDA1"]
                                  , 3)*100, "%"), 
       y = paste("dbRDA2 ", round(summary_rda$cont$importance[2, "dbRDA2"]
                                  , 3)*100, "%"),
       title = "distanzbasierte RDA über Umweltparameter",
       caption = paste("Fundstellen außerhalb der Uckermark n = ", length(unique(para_fst_rUM$FID)) )) +
  theme(legend.position = "right")

ggsave("./analysis/figures/Standortanalyse/gower_dbRDA_alleUmweltpara_ohneUM-ellipse.png", dpi = 300, width = 20, height = 16, units = "cm")



