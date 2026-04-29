# 1. Einladen Daten Mesorelief

# 2. Verteilung dastellen

#3. Distanzberechnung und ADONIStest


#### 1. Daten: 
load(file = "./analysis/data/derived_data/sites.RData")

mesorel <- read.csv2("./analysis/data/geodata/standortanalyse_2000_BB/mesorel.csv", sep = ",", dec = ".")

mesorel500 <- read.csv2("./analysis/data/geodata/standortanalyse_500_BB/mesorel.csv", sep = ",", dec = ".")

mesorel250 <- read.csv2("./analysis/data/geodata/standortanalyse_250_BB/mesorel.csv", sep = ",", dec = ".")

mesorel1000 <- read.csv2("./analysis/data/geodata/standortanalyse_1000_BB/mesorel.csv", sep = ",", dec = ".")


mesorel$r <- 2000

mesorel500$r <- 500

mesorel250$r <- 250

mesorel1000$r <- 1000

mesorel_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(mesorel, mesorel1000, mesorel250, mesorel500))

## Beilfundstellen zuweisen

mesorel_all$id <- as.character(mesorel_all$id)

standort_sites <- left_join(sites, mesorel_all, by = c("FID" = "id") )


standort_sites <-standort_sites |>
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
  filter(FST != "Zeestow 14/4 (14)")

standort_sites$kultur2 <- factor(standort_sites$kultur2, ordered = TRUE,
                                 levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "Guhrau", "Rös", "Rös. Beil/Axt"))


## Korrekturen von mehrperiodigen Fundstellen:
bredow_roes <- standort_sites |> filter(FID == "3007")

bredow_roes$FID <- "3007.2"
bredow_roes$kultur2 <- "Rös"

standort_sites <- rbind(standort_sites, bredow_roes)

standort_sites$kultur2[standort_sites$FST == "Potsdam 16"] <- "Rös"
standort_sites$FST[standort_sites$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"


potsdam_sbk <-standort_sites |> filter (FST == "Potsdam 3/13 (3)")
potsdam_sbk$FID <- "265.2"
potsdam_sbk$KULTUR <- "Stichbandkeramik"
potsdam_sbk$kultur2 <- "SBK"
potsdam_sbk$Fundart <- "Lesefund"

standort_sites <- rbind(standort_sites, potsdam_sbk)




library(viridis)
library(ggplot2)
library(ggh4x)
library(stringr)

standort_sites <- standort_sites |>
  filter(!is.na(KREIS)) |>
  filter(kreislabel != "CB")


# Kategorisierung:


## ansonsten einfach viel zu viele Kategorien, scheinen auch nicht einheitlich benannt worden zu sein

standort_sites <- standort_sites |>
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


standort_sites$obj3 <- factor(standort_sites$obj3, ordered = TRUE,
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

#### 2. Verteilung darstellen


standort_sites <- standort_sites |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2  ))

standort_sites$kultur2 <- factor(standort_sites$kultur2, levels = c("GG", "Meso", "FBG", "LBK Siedl.", "LBK Dechsel", "SBK", "SRK", "Guhrau", "Rös","Rös. Beil/Axt"))

fo_per_kr <- standort_sites |>    
  group_by(kultur2, r) |>
  summarize(count = n_distinct(FST))|>
  filter(!is.na(r)) |>
  filter(r != "NA")

k_per_kr <- standort_sites |>    
  group_by(r) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup() |>
  filter(r == 250)

x <- standort_sites |>
  group_by(obj3, kultur2, r) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()

farbskala <- c("#440154FF", "#26828EFF", "yellow", "green",  "brown", "black", "#3B528BFF","orange","darkorange", "darkolivegreen", "#5DC863FF","darkblue" , "pink", "violet", "#AADC32FF", "#D5E21AFF", "#FDE725FF", "cornsilk")

x |>
  filter(!is.na(r)) |>
  filter(r != "NA") |>
  ggplot()+
  geom_bar(aes(y = sum_p_area,
               x = kultur2,
               fill = obj3),
           position = "fill", stat = "identity")+
  geom_text(data = fo_per_kr,
            aes(x = kultur2,
                y = 0.99,
                label = count),
            colour = "white",
            hjust = 1,
            size = 3) +
  labs(title = "Mesorelief",
       subtitle = "kategorisierte Daten",
       x = "",
       y = "Anteil von Radius",
       fill = "",
       caption = "Datengrundlage: Mittelmaßstäbige Landwirtschaftliche Standortkartierung \nQuelle: © Landesamt für Bergbau, Geologie und Rohstoffe Brandenburg, dl-de/by-2-0")+
  scale_fill_manual(na.value = "grey70",
                    values = farbskala )+
  coord_flip()+
  facet_grid(. ~ as.factor(r), scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kr$count)+
  theme_bw()+
  guides(fill = guide_legend(ncol = 4 ))+
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10))


ggsave("./analysis/figures/Standortanalyse/MMK_mesorel_BB_250-2000.png", dpi = 300, height = 20, width = 30, units = "cm")


## nach kreisen

fo_per_kr <- standort_sites |>    
  group_by(kreislabel, kultur2, r) |>
  summarize(count = n_distinct(FST))|>
  filter(!is.na(r)) |>
  filter(r != "NA")

k_per_kr <- standort_sites |>    
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2))

x <- standort_sites |>
  group_by(kreislabel, obj3, kultur2, r) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()


x |>
  filter(!is.na(r)) |>
  filter(r != "NA") |>
  ggplot(na.rm = TRUE)+
  geom_bar(aes(y = sum_p_area,
               x = kultur2,
               fill = obj3),
           position = "fill", stat = "identity")+
  geom_text(data = fo_per_kr,
            aes(x = kultur2,
                y = 0.99,
                label = count),
            colour = "white",
            hjust = 1,
            size = 3) +
  labs(title = "Mesorelief",
       subtitle = "kategorisierte Daten",
       x = "",
       y = "Anteil von Radius",
       fill = "",
       caption = "Datengrundlage: Mittelmaßstäbige Landwirtschaftliche Standortkartierung \nQuelle: © Landesamt für Bergbau, Geologie und Rohstoffe Brandenburg, dl-de/by-2-0")+
  scale_fill_manual(na.value = "grey70",
                    values = farbskala )+
  coord_flip()+
  facet_grid(kreislabel ~ as.factor(r), scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kr$count)+
  theme_bw()+
  guides(fill = guide_legend(ncol = 1 ))+
  theme(text = element_text(size = 12),
        legend.position = "right",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10))

ggsave("./analysis/figures/Standortanalyse/MMK_mesorel_BB_all_250-2000_facet.png", dpi = 300, height = 35, width = 35, units = "cm")


#3. Distanzberechnung und ADONIStest

# Datenvorbereitung

standort_sites <- standort_sites |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2 ))

mesorelief500 <- standort_sites |>
  filter(r == 500) |>
  group_by(FID, obj3, kultur2, r) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()

mesorelief500_og <- st_drop_geometry(mesorelief500)

meso500w <- mesorelief500_og |>
  pivot_wider(names_from = obj3,
              values_from = sum_p_area,
              id_cols = c(FID, kultur2 )) 


meso500w <- meso500w |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0.00001) )) 

meso500w <- meso500w |>
  mutate(across(where(is.numeric), ~ .x + meso500w$'NA' / (ncol(meso500w) - 2) ) ) |>
  select(-c('NA')) # NA-WErte gleichmäßig auf die anderen Gruppen verteilt


### Adonistest mesorel
### adonis für alle gegen alle kulturen:


bb_para_n0 <- meso500w |>
  select(-c(FID, kultur2))   

bb_para_comps <- vegdist(bb_para_n0, "aitchison") # aitchison distance

# jetzt in adonis

meso500_adonis <- adonis2(bb_para_comps ~ kultur2, data = meso500w, permutations = 9999)

## -> ergebnis R2 0.0485 und p = 0.0001 -> schwach

## adonis für verschiedene Kulturen gegeneinander


kulturen <- unique(meso500w$kultur2)


adonis_meso500w <- data.frame(k1 = as.character(),
                              k2 = as.character(),
                              p = as.numeric(),
                              R2 = as.numeric())


for (i in c(1:(length(kulturen)-1) ) ) {
  
  k1 <- kulturen[i]
  
  for (j in c(2:length(kulturen) ) ) {
    
    k2 <- kulturen[j]
    
    if (k1 != k2) {
      
      bb_para_n0 <- meso500w |> filter(kultur2 %in% c(k1, k2) ) |>
        select(-c(FID, kultur2))   
      
      bb_para_n0 <- bb_para_n0 |>
        mutate(across(where(is.numeric), ~ replace_na(.x, 0.00001) ))
      
      bb_para_comps <- vegdist(bb_para_n0, "aitchison") # airchison distance
      
      # jetzt in adonis
      
      meso500w_adonis <- adonis2(bb_para_comps ~ kultur2, data = meso500w[meso500w$kultur2 %in% c(k1, k2) ,], permutations = 9999)
      
      res <- data.frame(k1 = k1,
                        k2 = k2,
                        p = meso500w_adonis$`Pr(>F)`[1],
                        R2 = meso500w_adonis$R2[1])
      
      adonis_meso500w <- rbind(adonis_meso500w,
                               res)
      
    }
  }
}

adonis_meso500w$k1 <- factor(adonis_meso500w$k1, levels = c("GG", "Meso", "FBG", "LBK Siedl.", "LBK Dechsel", "SBK", "SRK", "Guhrau", "Rös","Rös. Beil/Axt"))

adonis_meso500w$k2 <- factor(adonis_meso500w$k2, levels = levels(adonis_meso500w$k1))


#### plot testergebnisse

adonis_meso500w$p_adj <- p.adjust(adonis_meso500w$p, method = "fdr")

farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "gray80" , "grey70","grey60","grey50")

adonis_meso500w |>
  ggplot() +
  geom_tile(aes(x = k1,
                y = k2,
                fill = p_adj)) +
  geom_text(aes(x = k1,
                y = k2,
                label = format(round(R2, 2), nsmall = 2) ),
            col = "black") +
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
       title = "R² für Zusammenhang zwischen Anteilen an Mesorelief und Kultur",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n R² und p-Werte eines ADONIS-Tests (9999 Permutationen) über die Aitchison-Distanz, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995)")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))

ggsave("../figures/Standortanalyse/Meso500_R2_p_adonis.png", dpi = 300, width = 30, height = 15, units = "cm")
