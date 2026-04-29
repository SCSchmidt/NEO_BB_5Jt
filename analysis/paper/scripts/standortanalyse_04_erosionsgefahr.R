### Erosionsgefährdung

source("./analysis/paper/scripts/standortanalyse_03_datenvorbereitung_rasterdaten.R")

## Datenvorbereitung

library(tidyr)

r_eros_HG <- r_info_HG |>
  pivot_longer(cols = c("Gefährdun", "Gefährd_1", "Gefährd_2"),
               names_to = "Erosionsart",
               values_to = "eros_pot") 

r_eros_HG <- r_eros_HG |>
  select(kultur2, eros_pot, KREIS, FID, Erosionsart)


r_eros_HG <- r_eros_HG |>
  mutate(Eros_art = case_when(
    Erosionsart == "Gefährdun" ~ "Gefährdung Winderosion",
    Erosionsart == "Gefährd_1" ~ "Gefärdung Wassererosion RxKxSxL",
    Erosionsart == "Gefährd_2" ~ "Gefärdung Wassererosion RxKxS"   
  ))

r_eros_HG$FSt_GG <- NA
r_eros_HG$FSt_GG[r_eros_HG$kultur2 == "GG alle 1500m"] <- "GG"
r_eros_HG$FSt_GG[r_eros_HG$kultur2 != "GG alle 1500m"] <- "Fst"

### Bodenabtrag als Erosionsgefahrindex

#### zuerst: allgemein: Bodenabtrag als Indikator für Ackerfläche (wie viele liegen auf NA, wie viele mit Zahl)

r_info_HG$FSt_GG[r_info_HG$kultur2 == "GG alle 1500m"] <- "GG"
r_info_HG$FSt_GG[r_info_HG$kultur2 != "GG alle 1500m"] <- "Fst"

r_info_bodabtr <- r_info_HG |>
  filter(kultur2 != "GG") |>
  filter(KREIS != "NA") |>
  filter(!is.na(Bodenabtra))

Bodenabtr_NA <- r_info_HG |>
  filter(is.na(Bodenabtra)) |>
  group_by(FSt_GG, KREIS) |>
  summarize(count = n_distinct(FID)) |>
  ungroup()

Bodenabtr_noNA <- r_info_HG |>
  filter(!(is.na(Bodenabtra)) ) |>
  group_by(FSt_GG, KREIS) |>
  summarize(count = n_distinct(FID)) |>
  ungroup()

Bodenabtr_label <- left_join(Bodenabtr_NA, Bodenabtr_noNA, by = c("FSt_GG", "KREIS"))

library(tidyr)

Bodenabtr_label <- Bodenabtr_label |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) 

Bodenabtr_label$l <- paste0(Bodenabtr_label$count.y, ", NA = ", Bodenabtr_label$count.x)

source("./R_functions/farbzuweisungen.R")

r_info_bodabtr |>
  ggplot()+
  geom_jitter( aes(x = KREIS,
                   y = Bodenabtra,
                   col = kultur2),
               alpha = 0.5)+
  scale_color_manual(values = col,
                     na.value = "grey90")+
  geom_boxplot( aes(x = KREIS,
                    y = Bodenabtra),
                fill = "transparent")+
  geom_text( data = Bodenabtr_label,
             aes(x = KREIS,
                 y = 250,
                 label = l),
             hjust = 1,
             col = "grey50") +
  labs(title = "Bodenabtrag durch Wassererosion in Brandenburg",
       col = "Kultur",
       y = "Bodenabtrag (t/ha/a) ") +
  coord_flip() +
  facet_grid( FSt_GG ~ .,
              scales =  "free")+
  theme_bw(base_size = 11)

ggsave("./analysis/figures/Bodenerosion_BB_Kreis_boxplots_2.png", dpi = 300, width = 16, height = 22 , units = "cm")



#### Bodenabtrag zu Lage im Gelände anhand des Mesoreliefs

r_info_bs_nat2$FSt_GG[r_info_bs_nat2$kultur2 == "GG"] <- "GG"
r_info_bs_nat2$FSt_GG[r_info_bs_nat2$kultur2 != "GG"] <- "Fst"

library(ggh4x)

l_per_k <- r_info_bs_nat2 |>
  filter(!is.na(Bodenabtra))|>
  filter(NAME_UNTER %in% r_info_bs_nat2$NAME_UNTER[r_info_bs_nat2$FSt_GG == "Fst"]) |>
  group_by(kreislabel) |>
  mutate(count = n_distinct(NAME_UNTER)) |>
  ungroup()

l_per_k <- data.frame(l_per_k$kreislabel, l_per_k$count) |>  unique()


r_info_bs_nat2 |>
  filter(!is.na(Bodenabtra))|>
  filter(NAME_UNTER %in% r_info_bs_nat2$NAME_UNTER[r_info_bs_nat2$FSt_GG == "Fst"]) |>
  ggplot()+
  geom_boxplot(aes(y = Bodenabtra,
                   x = NAME_UNTER,
                   fill = FSt_GG))+
  coord_flip()+
  facet_grid(kreislabel ~ .,
             scales = "free_y")+
  force_panelsizes(rows = l_per_k$l_per_k.count[order(match(l_per_k$l_per_k.count,l_per_k$l_per_k.kreislabel))])

### Bodenabtrag im Vgl zu mesorelief 250m radius:

mesorel250 <- read.csv2("./analysis/data/geodata/standortanalyse_250_BB/mesorel.csv", sep = ",", dec = ".")

mesorel250$id <- as.character(mesorel250$id)

standort_sites <- left_join(sites, mesorel250, by = c("FID" = "id") )

standort_sites <- standort_sites |> select(LFD, FID, FST, KREIS, kultur2, obj, area, area.) |> st_drop_geometry()

mesorel_gg <- read.csv2("./analysis/data/geodata/gr_gg_250/mesorel.csv", sep = ",", dec = ".")

mesorel_gg$FID <- as.character(mesorel_gg$id)

sites_gg_rel <- left_join(mesorel_gg, HG_for_merge, by ="FID" )

sites_gg_rel <- sites_gg_rel |> select(LFD, FID, FST, KREIS, kultur2, obj, area, area.) #|> st_drop_geometry()

sites_gg_rel$FID2 <- as.numeric(sites_gg_rel$FID)
sites_gg_rel$FID <- as.character(sites_gg_rel$FID2 + 6000)


mesorel_all <- rbind(standort_sites,sites_gg_rel |> select(-FID2) )

mesorel_all <- mesorel_all |>
  mutate(KREIS = case_when(
    KREIS == "Frankfurt am Oder Städte" ~ "Frankfurt (Oder)",
    KREIS == "Brandenburg an der Havel Städte" ~ "Brandenburg (Havel)",
    KREIS == "Potsdam Städte" ~ "Potsdam",
    KREIS == "Cottbus Städte" ~ "Cottbus",
    TRUE ~ KREIS
  )) |>
  filter(kultur2 != "GG")


library(viridis)
library(ggplot2)
library(ggh4x)
library(stringr)

mesorel_all <- mesorel_all |>
  filter(!is.na(KREIS)) |>
  filter(KREIS != "Cottbus")

# 1. Kategorisierungen wie bei Fst-Lagemerkmale

mesorel_all <- mesorel_all |>
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


mesorel_all$obj4 <- factor(mesorel_all$obj3, ordered = TRUE,
                           levels = c(  "Tal",
                                        "Niederung / Aue",
                                        "Talsandinsel",
                                        "Mulde / Becken / Senke"  ,
                                        "Ebene" ,
                                        "Tal ohne Boden / Rinne" ,
                                        "Zerschnittene Platte / Hang" ,
                                        "Stufenhang"  ,
                                        "Geneigte Platte" ,
                                        "Eben bis flach wellige Platte mit Becken" ,
                                        "Wellige Platte mit Becken / Senken" ,
                                        "Hügel-Mulden-Komplex" ,
                                        "Eben bis flach wellige Platte" ,
                                        "Wellige (bis stark) Platte"  ,
                                        "Hang (gr.)" ,
                                        "Hügel-Riedel-Komplex",
                                        "Hügel(-komplex)" ,
                                        "Kuppe") )

## Kategorisierung ob kolluvial oder erodierend

mesorel_all <- mesorel_all |>
  mutate(obj5 = case_when(
    obj3 == "Tal" ~ "eher kolluvial",
    obj3 == "Niederung / Aue" ~ "eher kolluvial",
    obj3 ==  "Talsandinsel"  ~ "wenig erodierend und ablagernd" ,
    obj3 ==  "Mulde / Becken / Senke"   ~ "eher kolluvial",
    obj3 == "Ebene" ~ "wenig erodierend und ablagernd" ,
    obj3 == "Tal ohne Boden / Rinne"  ~ "eher kolluvial",
    obj3 == "Zerschnittene Platte / Hang" ~ "eher erodierend",
    obj3 ==  "Stufenhang"  ~ "eher erodierend",
    obj3 == "Geneigte Platte" ~ "eher erodierend",
    obj3 ==  "Eben bis flach wellige Platte mit Becken" ~ "wenig erodierend und ablagernd" ,
    obj3 ==  "Wellige Platte mit Becken / Senken"  ~ "eher erodierend",
    obj3 == "Hügel-Mulden-Komplex" ~ "eher erodierend",
    obj3 == "Eben bis flach wellige Platte"  ~ "wenig erodierend und ablagernd" ,
    obj3 == "Wellige (bis stark) Platte"  ~ "eher erodierend",
    obj3 == "Hang (gr.)" ~ "eher erodierend",
    obj3 == "Hügel-Riedel-Komplex"~ "eher erodierend",
    obj3 == "Hügel(-komplex)" ~ "eher erodierend",
    obj3 == "Kuppe" ~ "eher erodierend") )

x2 <- mesorel_all |>
  group_by(KREIS, obj5, kultur2) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()


fo_per_kr <- mesorel_all |>
  group_by(KREIS, kultur2) |>
 summarize(count = n_distinct(FST)) |> 
  ungroup() |> unique()

x2 |>
  ggplot()+
  geom_bar(aes(y = sum_p_area,
               x = kultur2,
               fill = obj5),
           position = "fill", stat = "identity")+
  geom_text(data = fo_per_kr,
            aes(x = kultur2,
                y = 0.98,
                label = count),
            colour = "black",
            hjust = 1,
            size = 3) +
  scale_fill_manual(breaks = c( "eher kolluvial",  "wenig erodierend und ablagernd" , "eher erodierend"),
                    labels = c("eher kolluvial (Täler, Senken, Niederungen)",
                               "wenig erodierend und ablagernd (Ebenen)",
                               "eher erodierend (Hänge, Kuppen, Hügel)"),
                    values = c("blue", "orange", "green"))+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 50, 100))+
  labs(title = "Anteil an eher kolluvialen und erodierenden Flächen der Fundstellen",
       subtitle = "kategorisierte Daten anhand des Mesoreliefs",
       x = "",
       y = "Prozent von 250 m Radius",
       fill = "",
       caption = "Datengrundlage: Mittelmaßstäbige Landwirtschaftliche Standortkartierung \nQuelle: © Landesamt für Bergbau, Geologie und Rohstoffe Brandenburg, dl-de/by-2-0")+
  theme_bw(base_size = 11)+
  coord_flip()+
  facet_wrap(KREIS ~ ., scales = "fixed") +
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        axis.text.x = element_text(size = 10))

ggsave("./analysis/figures/Standortanalyse/MMK_mesorel_BB_all_250r_gr_gg_eros_koll.png", dpi = 300, height = 16, width = 25, units = "cm")




################ Exkurs: Erosion im Arbeitsgebiet (diskutiert in Kap. Quellenkritik, aber nicht visualisiert)

### Bodenabtrag KS-Test: Fst gegen nicht Fundstellen nach Kreis

source("./R_functions/get_ks.R")

df = r_info_HG

vars = "Bodenabtra"

kult = "FSt_GG"

gruppe = "KREIS" 

df2 <- data.frame(kultur = df[[kult]],       # select rel cols
                  variable = df[[vars]],
                  group = df[[gruppe]])

ls <- list()

kulturen <- unique(df2$kultur) # in diesem Fall Fst vs GG

gr <- unique(df2$group)

gr <-  gr[!gr %in% c("Frankfurt (Oder)", "Cottbus", "Potsdam","Brandenburg (Havel)", NA, "Barnim", "Oberspreewald-Lausitz") ] # zu wenige 

for (l in c(1:length(gr) ) ) { 
  
  res_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("k", "k1", "k2", "n_note", "p.value", "gr", "method"))
  res_df$k <- as.character(res_df$k)
  res_df$k1 <- as.character(res_df$k1)
  res_df$k2 <- as.character(res_df$k2)
  res_df$n_note <- as.character(res_df$n_note)
  res_df$p.value <- as.numeric(res_df$p.value)
  res_df$gr <- as.character(res_df$gr)
  res_df$method <- as.character(res_df$method)
  
  
  varname <- paste(gr[l])
  
  data <- df2 |>
    dplyr::filter(group == varname)
  
  for (i in 1:(length(kulturen)-1 ) ) {
    
    k1 <- kulturen[i] 
    
    for (j in 2:(length(kulturen) ) ) {
      
      k2 <- kulturen[j]
      
      data2 <- data |>
        filter(kultur %in% c(k1, k2) ) |> # filter for two cultures |>
        filter(!is.na(variable))
      
      # check if there are enough infos: 
      
      if (nrow(data2 |> dplyr::filter(kultur == k1)) < 2) {
        print(paste("Datensatz für ", k1, " unter 2 in ", varname) )
      } 
      else if (nrow(data2 |> dplyr::filter(kultur == k2)) < 2) {
        print( paste("Datensatz für ", k2, " unter 2 in ", varname))
      }  
      else if (length(unique(data2$variable)) < 2) {
        print(paste("Variable ", varname, " unter 2 in ", k1, k2))
      }
      else if (length(unique(data2$kultur)) < 2) {
        print(paste("gleiche Kultur", k1) ) } #
      
      else if (paste0(k2, " - ", k1) %in% res_df$k) {
        print(paste("Paar", k1, k2, "schon besprochen") ) } #  
      else {
        
        # alle Voraussetzungen erfüllt:
        d1 <- data2 |> 
          dplyr::filter(kultur == k1)
        
        d2 <- data2 |> 
          dplyr::filter(kultur == k2)
        
        res <- ks.test(d1$variable, d2$variable )
        
        res$gr <- varname
        res$K1 <- k1
        res$K2 <- k2
        res$k <- paste0(k1, " - ", k2)
        res$K1n <- nrow(data2 |> dplyr::filter(kultur == k1)  ) # n soll fstanzahl entsprechen
        res$K2n <- nrow(data2 |> dplyr::filter(kultur == k2)  )
        res$n_note <- paste0(k1, " n = ", res$K1n, ", \n", k2, " n = ", res$K2n)
        
        res_zusammen <- data.frame("k" = res$k, 
                                   "k1" = res$K1, 
                                   "k2" = res$K2, 
                                   "n_note" = res$n_note, 
                                   "p.value" = res$p.value, 
                                   "gr" = varname,
                                   "method" = res$method )  
        
        
        
        res_df <-rbind(res_df, res_zusammen)
        
      }
      
    }
  }
  ls[[varname]] <- res_df
  
  
}


bodenabtrag_ks <- adjust_p(ls)

### Bodenabtrag alle Fst - GG über ganz BB KS-Test

df = r_info_HG

vars = "Bodenabtra"

kult = "FSt_GG"


df2 <- data.frame(kultur = df[[kult]],       # select rel cols
                  variable = df[[vars]])

ls <- list()

kulturen <- unique(df2$kultur)


res_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("k", "k1", "k2", "n_note", "p.value", "gr", "method"))
res_df$k <- as.character(res_df$k)
res_df$k1 <- as.character(res_df$k1)
res_df$k2 <- as.character(res_df$k2)
res_df$n_note <- as.character(res_df$n_note)
res_df$p.value <- as.numeric(res_df$p.value)
res_df$gr <- as.character(res_df$gr)
res_df$method <- as.character(res_df$method)


varname <- "ganz BB"

data <- df2

for (i in 1:(length(kulturen)-1 ) ) {
  
  k1 <- kulturen[i] 
  
  for (j in 2:(length(kulturen) ) ) {
    
    k2 <- kulturen[j]
    
    data2 <- data |>
      filter(kultur %in% c(k1, k2) ) |> # filter for two cultures |>
      filter(!is.na(variable))
    
    # check if there are enough infos: 
    
    if (nrow(data2 |> dplyr::filter(kultur == k1)) < 2) {
      print(paste("Datensatz für ", k1, " unter 2 in ", varname) )
    } 
    else if (nrow(data2 |> dplyr::filter(kultur == k2)) < 2) {
      print( paste("Datensatz für ", k2, " unter 2 in ", varname))
    }  
    else if (length(unique(data2$variable)) < 2) {
      print(paste("Variable ", varname, " unter 2 in ", k1, k2))
    }
    else if (length(unique(data2$kultur)) < 2) {
      print(paste("gleiche Kultur", k1) ) } #
    
    else if (paste0(k2, " - ", k1) %in% res_df$k) {
      print(paste("Paar", k1, k2, "schon besprochen") ) } #  
    else {
      
      # alle Voraussetzungen erfüllt:
      d1 <- data2 |> 
        dplyr::filter(kultur == k1)
      
      d2 <- data2 |> 
        dplyr::filter(kultur == k2)
      
      res <- ks.test(d1$variable, d2$variable )
      
      res$gr <- "ganz BB"
      res$K1 <- k1
      res$K2 <- k2
      res$k <- paste0(k1, " - ", k2)
      res$K1n <- nrow(data2 |> dplyr::filter(kultur == k1)  ) # n soll gefäßanzahl entsprechen
      res$K2n <- nrow(data2 |> dplyr::filter(kultur == k2)  )
      res$n_note <- paste0(k1, " n = ", res$K1n, ", \n", k2, " n = ", res$K2n)
      
      res_zusammen <- data.frame("k" = res$k, 
                                 "k1" = res$K1, 
                                 "k2" = res$K2, 
                                 "n_note" = res$n_note, 
                                 "p.value" = res$p.value, 
                                 "gr" = varname,
                                 "method" = res$method )  
      
      
      
      res_df <-rbind(res_df, res_zusammen)
      
    }
    
  }
}

p <- round(res_df$p.value, 5)


df = r_info_HG |> filter(FSt_GG == "GG")

vars = "Bodenabtra"

kult = "kreislabel"

df2 <- data.frame(kultur = df[[kult]],       # select rel cols
                  variable = df[[vars]])

ls <- list()

kulturen <- unique(df2$kultur)

res_df <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("k", "k1", "k2", "n_note", "p.value", "method"))
res_df$k <- as.character(res_df$k)
res_df$k1 <- as.character(res_df$k1)
res_df$k2 <- as.character(res_df$k2)
res_df$n_note <- as.character(res_df$n_note)
res_df$p.value <- as.numeric(res_df$p.value)
res_df$method <- as.character(res_df$method)


data <- df2

for (i in 1:(length(kulturen)-1 ) ) {
  
  k1 <- kulturen[i] 
  
  for (j in 2:(length(kulturen) ) ) {
    
    k2 <- kulturen[j]
    
    data2 <- data |>
      filter(kultur %in% c(k1, k2) ) |> # filter for two cultures |>
      filter(!is.na(variable))
    
    # check if there are enough infos: 
    
    if (nrow(data2 |> dplyr::filter(kultur == k1)) < 2) {
      print(paste("Datensatz für ", k1, " unter 2 in ") )
    } 
    else if (nrow(data2 |> dplyr::filter(kultur == k2)) < 2) {
      print( paste("Datensatz für ", k2, " unter 2 in "))
    }  
    else if (length(unique(data2$variable)) < 2) {
      print(paste("Variable ", " unter 2 in ", k1, k2))
    }
    else if (length(unique(data2$kultur)) < 2) {
      print(paste("gleiche Kultur", k1) ) } #
    
    else if (paste0(k2, " - ", k1) %in% res_df$k) {
      print(paste("Paar", k1, k2, "schon besprochen") ) } #  
    else {
      
      # alle Voraussetzungen erfüllt:
      d1 <- data2 |> 
        dplyr::filter(kultur == k1)
      
      d2 <- data2 |> 
        dplyr::filter(kultur == k2)
      
      res <- ks.test(d1$variable, d2$variable )
      
      res$K1 <- k1
      res$K2 <- k2
      res$k <- paste0(k1, " - ", k2)
      res$K1n <- nrow(data2 |> dplyr::filter(kultur == k1)  ) # n soll gefäßanzahl entsprechen
      res$K2n <- nrow(data2 |> dplyr::filter(kultur == k2)  )
      res$n_note <- paste0(k1, " n = ", res$K1n, ", \n", k2, " n = ", res$K2n)
      
      res_zusammen <- data.frame("k" = res$k, 
                                 "k1" = res$K1, 
                                 "k2" = res$K2, 
                                 "n_note" = res$n_note, 
                                 "p.value" = res$p.value, 
                                 "method" = res$method )  
      
      
      
      res_df <-rbind(res_df, res_zusammen)
      
    }
    
  }
}




bodenabtrag_ks_gg <- res_df

bodenabtrag_ks_gg$p.adj <- p.adjust(bodenabtrag_ks_gg$p.value)


library(tidyr)

farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "grey70","grey60","grey50")

bodenabtrag_ks_gg |>
  filter(k1 != "P") |>
  filter(k2 != "P") |>
  filter(k1 != "BRB") |>
  filter(k2 != "BRB") |>
  ggplot() +
  geom_tile(aes(x = k1,
                y = k2,
                fill = p.adj)) +
  geom_text(aes(x = k1,
                y = k2,
                label = n_note),
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
  geom_text(aes(x = k1,
                y = k2,
                label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  geom_tile(aes(x = k2,
                y = k1,
                fill = p.adj)) +
  geom_text(aes(x = k2,
                y = k1,
                label = n_note),
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
  geom_text(aes(x = k2,
                y = k1,
                label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  coord_flip() +
  scale_fill_gradientn(colours = farben ,
                       values =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       breaks =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       labels = c("0.00 - höchstsign.", "0.01 - hoch sign.", "0.05 - signif.", "0.1 - nicht sign.", "0.2","0.8", "1"),
                       guide = "colorbar", limits = c(0,1),
                       na.value = "white")+
  guides(fill = guide_legend(nrow = 1)  ) +
  labs(x = "",
       y = "",
       fill = "p-Werte",
       title = "P-Werte für Unterschiede im Bodenabtrag der Kreise",
       caption = "anhand regelmäßiger Punkte über das Arbeitsgebiet im Abstand von 1500m, \n p-Werte eines zweiseiten Kolmogoroff-Smirnoff-Test, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995)\nPotsdam entfernt, da p-Werte stets 1\n Brandenburg entfernt, da nur zur UM signifikanter Unterschird (p = 0.03) " )+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 12))

# ggsave("../figures/Bodenabtrag_KS-Test_Kreise_2.png", dpi = 300, width = 50, height = 25, units = "cm") # nicht benutzt



#### GG Kreise summary-Werte für Erosion

tab_Bodenabtra_kreis <- r_info_HG |>
  filter(!is.na(Bodenabtra)) |>
  filter(!is.na(KREIS) ) |>
  group_by(KREIS, FSt_GG) |>
  summarise(mittelw = mean(Bodenabtra),
            sd = sd(Bodenabtra),
            median = median(Bodenabtra),
            min = min(Bodenabtra),
            Q1 = quantile(Bodenabtra, probs = 0.25),
            Q3 = quantile(Bodenabtra, probs = 0.75),
            max = max(Bodenabtra))|>
  ungroup()

save(tab_Bodenabtra_kreis, file = "./analysis/data/derived_data/tab_Bodenabtrag_Kreis.RData")

tab_Bodenabtra_kreis |>
  ggplot()+
  geom_col(aes(x = KREIS,
               fill = FSt_GG,
               y = median),
           position = "dodge")+
  coord_flip()+
  theme_bw()


