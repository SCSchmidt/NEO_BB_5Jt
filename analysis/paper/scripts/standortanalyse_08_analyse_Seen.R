# Bezug zu offfenen Gewässern (Seen)

# 1. Daten laden
# 2. plotten
# 3. Distanzberechnung und ADONIS


## Daten laden

load(file = "./analysis/data/derived_data/sites.RData")

ofl_wasser <- read.csv2("/analysis/data/geodata/standortanalyse_BB_2025-02-03/layer.csv", sep = ",", dec = ".")

ofl_wasser1000 <- read.csv2("/analysis/data/geodata/standortanalyse_1000_BB/layer.csv", sep = ",", dec = ".")

ofl_wasser2000 <- read.csv2("/analysis/data/geodata/standortanalyse_2000_BB/layer.csv", sep = ",", dec = ".")

ofl_wasser250 <- read.csv2("/analysis/data/geodata/standortanalyse_250_BB/layer.csv", sep = ",", dec = ".")

ofl_wasser500 <- read.csv2("/analysis/data/geodata/standortanalyse_500_BB/layer.csv", sep = ",", dec = ".")


ofl_wasser1000$obj <- paste0(ofl_wasser1000$obj , "_r1000")

ofl_wasser2000$obj <- paste0(ofl_wasser2000$obj , "_r2000")

ofl_wasser250$obj <- paste0(ofl_wasser250$obj , "_r250")

ofl_wasser500$obj <- paste0(ofl_wasser500$obj , "_r500")

ofl_wasser_all <- rbind(ofl_wasser, ofl_wasser1000, ofl_wasser2000, ofl_wasser500, ofl_wasser250)


ofl_wasser_all$id <- as.character(ofl_wasser_all$id)

standort_sites <- left_join(sites, ofl_wasser_all, by = c("FID" = "id") )

library(viridis)
library(ggplot2)
library(ggh4x)
library(stringr)

standort_sites <- standort_sites |>
  filter(!is.na(KREIS)) |>
  filter(kreislabel != "CB")


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


fo_per_kr <- standort_sites |>    
  group_by(kreislabel, kultur2) |>
  summarize(count = n_distinct(FST))

fo_per_kr <- fo_per_kr |> ungroup()


k_per_kr <- standort_sites |>    
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()

x <- standort_sites |>
  group_by(kreislabel, obj, kultur2) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()

#### Plotten


x2 <- left_join(x, fo_per_kr, by = c("kreislabel", "kultur2" ))

x2 <- x2 |>
  mutate(obj2 = case_when(
    obj == "SEEN25_r250" ~ 250,
    obj == "SEEN25_r500" ~ 500,
    obj == "SEEN25_r1000" ~ 1000,
    obj == "SEEN25_r2000" ~ 2000,
    obj == "SEEN25" ~ 5000
  ))

x2 |>
  filter(!is.na(obj2) ) |>
  mutate(proz = sum_p_area / count * 100) |>
  ggplot()+
  geom_col(aes(y = proz,
               x = kultur2,
               fill = as.factor(obj2) ),
           position = "dodge") +
  geom_text(data = fo_per_kr,
            aes(x = kultur2,
                y = 60.5,
                label = count),
            colour = "black",
            hjust = 1,
            size = 3) +
  labs(title = "Bezug zu Oberflächenwasser",
       subtitle = "Anteile an Seeflächen im Umkreis von 250 bis 5000 m",
       x = "",
       y = "% von Fläche im Umkreis",
       fill = "Radius d. Site Catchment",
       caption = "Datengrundlage: ATKIS BasisDLM \nQuelle: © LfU dl-by-de/2.0 \n Flüsse und Seen unter 100 qm Fläche ausgespart")+
  scale_fill_manual(breaks = c(250, 500, 1000, 2000, 5000),
                    values = c("black", "darkblue","cyan4",  "blue", "lightblue"),
                    labels = c("250 m", "500 m", "1 km", "2 km", "5 km")) +
  coord_flip()+
  facet_wrap(kreislabel ~ ., scales = "fixed", drop = FALSE) +
  force_panelsizes(rows = rep(8, 17))+
  theme_bw()+
  theme(legend.position = "bottom" )+
  guides(fill=guide_legend(nrow=1))


ggsave("./analysis/figures/Standortanalyse/LGB_see_Kreis-Kultur_rad250-5000.png", dpi = 300, height = 25, width = 25, units = "cm")

# "../figures/Standortanalyse/LGB_see_Kreis-Kultur_rad250-5000_v2.png" händisch so geändert, dass die Legende besser liegt


### adonis Seenanteil, 500m Radius

seen500 <- standort_sites |>
  filter(obj == "SEEN25_r500")

seen500$proz_festland <- 1 - seen500$area.

seen500 <- seen500 |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2
  ))

## mal nur Uckermark alle Kulturen gegeneinander:

seen_um <- seen500 |> filter(kreislabel == "UM")


bb_para_n0 <- seen_um[,c("area.", "proz_festland")] |> st_drop_geometry()

bb_para_n0[bb_para_n0 == 0] <- 0.0001

bb_para_n0 <- bb_para_n0 |>
  mutate(area. = case_when(
    proz_festland == 0.0001 ~ area. -  0.0001,
    TRUE ~ area.),
    proz_festland = case_when(
      area. == 0.0001 ~ proz_festland - 0.0001,
      TRUE ~ proz_festland
    ) )


#bb_para_n0 <- bb_para_n0 |>
#   mutate(across(where(is.numeric), ~ replace_na(.x, 0.0001) ))

bb_para_comps <- vegdist(bb_para_n0, "aitchison") # aitchison distance

# jetzt in adonis

seen_um_adonis <- adonis2(bb_para_comps ~ kultur2, data = seen_um, permutations = 9999)
## nicht signifikant, sehr kleines R


library(compositions)
library(sf)
library(vegan)

adonis_seen <- data.frame(k1 = as.character(),
                          k2 = as.character(),
                          p = as.numeric(),
                          R2 = as.numeric())

kulturen <- unique(seen500$kultur2)

for (i in c(1:(length(kulturen)-1) ) ) {
  
  k1 <- kulturen[i]
  
  for (j in c(2:length(kulturen) ) ) {
    
    k2 <- kulturen[j]
    
    if (k1 != k2) {
      
      bb_para_n0 <- seen500[seen500$kultur2 %in% c(k1, k2) ,c("area.", "proz_festland")] |> st_drop_geometry()
      
      bb_para_n0[bb_para_n0 == 0] <- 0.0001
      
      bb_para_n0 <- bb_para_n0 |>
        mutate(area. = case_when(
          proz_festland == 0.0001 ~ area. -  0.0001,
          TRUE ~ area.),
          proz_festland = case_when(
            area. == 0.0001 ~ proz_festland - 0.0001,
            TRUE ~ proz_festland
          ) )
      
      
      #bb_para_n0 <- bb_para_n0 |>
      #   mutate(across(where(is.numeric), ~ replace_na(.x, 0.0001) ))
      
      bb_para_comps <- vegdist(bb_para_n0, "aitchison") # airchison distance
      
      # jetzt in adonis
      
      seen500_adonis <- adonis2(bb_para_comps ~ kultur2, data = seen500[seen500$kultur2 %in% c(k1, k2) ,], permutations = 9999)
      
      res <- data.frame(k1 = k1,
                        k2 = k2,
                        p = seen500_adonis$`Pr(>F)`[1],
                        R2 = seen500_adonis$R2[1])
      
      adonis_seen <- rbind(adonis_seen,
                           res)
      
    }
  }
}

### plot adonis test 
adonis_seen$p_adj <- p.adjust(adonis_seen$p, method = "fdr")

farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "grey80", "grey70","grey60","grey50")

adonis_seen$k1 <- factor(adonis_seen$k1, levels = c("GG", "Meso", "FBG",   "LBK Dechsel","LBK Siedl." ,  "SBK"  , "SRK", "Guhrau" ,       "Rös",           "Rös. Beil/Axt"))

adonis_seen$k2 <- factor(adonis_seen$k2, levels = levels(adonis_seen$k1))

adonis_seen |>
  filter(p_adj < 0.15) |>
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
       title = "R²-Werte für Varianzunterschiede zwischen Anteil an Seenflächen",
       subtitle = "im Radius von 500m um die Fundstelle",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n R² und p-Werte eines ADONIS-Tests (9999 Permutationen) über die Aitchison-Distanz, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995)\n gefiltert auf p-Werte < 0,15")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 10))

ggsave("./analysis/figures/Standortanalyse/Anteile_Seenflächen_ADONIS_R2_p-werte.png", dpi = 300, width = 22, height = 10, units = "cm")

