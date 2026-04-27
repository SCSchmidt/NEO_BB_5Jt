
# Analyse der Daten der Bodenübersichtskarte 300

load(file = "./analysis/data/derived_data/sites.RData")

## Hautpgruppe 250m - 2000 m Radius 

# Daten einladen und korrigieren
# Daten wurden per QGIS-Pythonscript gewonnen

bük_hg <- read.csv2("./analysis/data/geodata/standortanalyse_2000_BB/hauptgru_1.csv", sep = ",", dec = ".")

bük_hg250 <- read.csv2("./analysis/data/geodata/standortanalyse_250_BB/hauptgru_1.csv", sep = ",", dec = ".")

bük_hg500 <- read.csv2("./analysis/data/geodata/standortanalyse_500_BB/hauptgru_1.csv", sep = ",", dec = ".")

bük_hg1000 <- read.csv2("./analysis/data/geodata/standortanalyse_1000_BB/hauptgru_1.csv", sep = ",", dec = ".")

# Zuweisung der Radien
bük_hg$r <- 2000

bük_hg250$r <- 250

bük_hg500$r <- 500

bük_hg1000$r <- 1000

# in einen dataframe
bük_hg <-  Reduce(function(x, y) merge(x, y, all=TRUE), list(bük_hg, bük_hg1000, bük_hg250, bük_hg500) )

bük_hg$id <- as.character(bük_hg$id)

bük_hg_sites <- left_join(bük_hg, sites, by = c("id" = "FID"))

# Datenkorrektur, siehe oben 

bük_hg_sites <- bük_hg_sites |>
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
        "Bornim 8")~ "Rös. Beil/Axt",
    TRUE ~ kultur2
  )) |>
  filter(FST != "Zeestow 14/8 (14)")

bük_hg_sites$kultur2 <- factor(bük_hg_sites$kultur2, ordered = TRUE,
                               levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "Guhrau", "Rös", "Rös. Beil/Axt"))


## Korrekturen von mehrperiodigen Fundstellen:
bredow_roes <- bük_hg_sites |> filter(id == 3007)

bredow_roes$id <- "3007.2"
bredow_roes$kultur2 <- "Rös"

bük_hg_sites <- rbind(bük_hg_sites, bredow_roes)

bük_hg_sites$kultur2[bük_hg_sites$FST == "Potsdam 16"] <- "Rös"
bük_hg_sites$FST[bük_hg_sites$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"


potsdam_sbk <- bük_hg_sites |> filter (FST == "Potsdam 3/13 (3)")
potsdam_sbk$id <- "265.2"
potsdam_sbk$KULTUR <- "Stichbandkeramik"
potsdam_sbk$kultur2 <- "SBK"
potsdam_sbk$Fundart <- "Lesefund"

bük_hg_sites <- rbind(bük_hg_sites, potsdam_sbk)

## PLOTS

## plot BÜK Hauptgruppen

source("analysis/paper/scripts/standortanalyse_03_datenvorbereitung_analyseBUEK.R")

standort_BB <- bük_hg_sites

standort_BB <- standort_BB |>
  filter(!is.na(KREIS)) |> 
  filter(kreislabel != "CB") # Cottbus: keine Funde


# in dieser Analyse: SRK von SBK trennen

standort_BB <- standort_BB |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" , 
    TRUE ~ kultur2  ))

standort_BB$kultur2 <- factor(standort_BB$kultur2, ordered = TRUE,
                              levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))


fo_per_kr <- standort_BB |>    
  group_by(kultur2, r) |>
  summarize(count = n_distinct(FST)) |>
  ungroup() |>
  filter(r == 250)

k_per_kreis <-  standort_BB |>    
  group_by(r) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()

x <- standort_BB |>
  group_by(obj, kultur2, r) |>
  summarise(sum_p_area = sum(area.) )
ggplot()+
  geom_bar(data = x, aes(y = sum_p_area,
                         x = kultur2,
                         fill = obj),
           position = "fill", stat = "identity")+
  labs(title = "Verteilung der Kulturgruppen in Brandenburg",
       subtitle = "auf die BÜK 300 Hauptgruppen",
       caption = "Datengrundlage: Bodenübersichtskarte 1:300.000 \n Quelle: © Landesamt für Bergbau, Geologie und Rohstoffe Brandenburg, dl-de/by-2-0 \n Label in Facette r = 250 gibt Anzahl Fundstellen der Kulturgruppe",
       x = "Kulturgruppe",
       y = "Anteil von Radius",
       fill = "")+
  geom_text(data = fo_per_kr,
            aes(x = kultur2,
                y = 0.11,
                label = count),
            colour = "white",
            hjust = 1) +
  scale_fill_manual(values = c("grey", "lightblue" , "#5DC863FF",  "brown",  "darkblue" , "cornsilk" ),
                    breaks = c( "Böden aus anthropogen abgelagerten Sedimenten" ,
                                "Böden aus glazialen Sedimenten einschließlich ihrer periglaziären Überprägungen",
                                "Böden aus Auensedimenten",
                                "Böden aus organogenen Sedimenten",
                                "Böden aus Fluss- und Seesedimenten einschließlich Urstromtalsedimenten",
                                "Böden aus äolischen Sedimenten"  ),
                    labels = c( "Böden aus anthropogen abgelagerten Sedimenten" ,
                                "Böden aus glazialen Sedimenten \neinschließlich ihrer periglaziären Überprägungen",
                                "Böden aus Auensedimenten",
                                "Böden aus organogenen Sedimenten",
                                "Böden aus Fluss- und Seesedimenten \neinschließlich Urstromtalsedimenten",
                                "Böden aus äolischen Sedimenten"))+
  coord_flip()+
  facet_grid(. ~ r, scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kreis$count)+
  theme_bw()+
  guides(fill = guide_legend(ncol = 2 ))+
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12)  )  

ggsave("./analysis/figures/Standortanalyse/BÜK_HG_BB-radius-kultur.png", dpi = 300,  height = 15, width = 30, units = "cm")



fo_per_kr <- standort_BB |>    
  group_by(kreislabel, kultur2, r) |>
  summarize(count = n_distinct(FST)) |>
  ungroup() |>
  filter(r == 250)

k_per_kreis <-  standort_BB |>    
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()

x <- standort_BB |>
  group_by(kreislabel, obj, kultur2, r) |>
  summarise(sum_p_area = sum(area.) )




ggplot()+
  geom_bar(data = x, aes(y = sum_p_area,
                         x = kultur2,
                         fill = obj),
           position = "fill", stat = "identity")+
  labs(title = "Verteilung der Kulturgruppen in Brandenburg",
       subtitle = "auf die BÜK 300 Hauptgruppen",
       caption = "Datengrundlage: Bodenübersichtskarte 1:300.000 \n Quelle: © Landesamt für Bergbau, Geologie und Rohstoffe Brandenburg, dl-de/by-2-0",
       x = "Kulturgruppe",
       y = "Anteil von Radius",
       fill = "")+
  geom_text(data = fo_per_kr,
            aes(x = kultur2,
                y = -0.1,
                label = count),
            colour = "black",
            hjust = 0) +
  scale_fill_manual(values = c("grey", "lightblue" , "#5DC863FF",  "brown",  "darkblue" , "cornsilk" ),
                    breaks = c( "Böden aus anthropogen abgelagerten Sedimenten" ,
                                "Böden aus glazialen Sedimenten einschließlich ihrer periglaziären Überprägungen",
                                "Böden aus Auensedimenten",
                                "Böden aus organogenen Sedimenten",
                                "Böden aus Fluss- und Seesedimenten einschließlich Urstromtalsedimenten",
                                "Böden aus äolischen Sedimenten"  ),
                    labels = c( "Böden aus anthropogen abgelagerten Sedimenten" ,
                                "Böden aus glazialen Sedimenten \neinschließlich ihrer periglaziären Überprägungen",
                                "Böden aus Auensedimenten",
                                "Böden aus organogenen Sedimenten",
                                "Böden aus Fluss- und Seesedimenten \neinschließlich Urstromtalsedimenten",
                                "Böden aus äolischen Sedimenten"))+
  coord_flip()+
  facet_grid(kreislabel ~ r, scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kreis$count)+
  theme_bw()+
  guides(fill = guide_legend(ncol = 2 ))+
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12)  )  

ggsave("./analysis/figures/Standortanalyse/BB_2000/BÜK_HG_250-2000m_BB-kultur.png", dpi = 300,  height = 45, width = 30, units = "cm")


## ADONIS test für BÜK Hauptgruppen

library(compositions) # für die Aitchinson-Distanz
library(vegan)


bük_hg_sites <- bük_hg_sites |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2  ))

## Radius von 500m als Analysegrundlage

bük_hg_500 <- bük_hg_sites |>
  filter(r == 500, obj != "Böden aus anthropogen abgelagerten Sedimenten") ## sind nicht relevant -> müssen dann aber aufgeteilt werden

bük_hg_500 <- bük_hg_500 |> select(c(id, kultur2, obj, area.) )|>
  unique()

bük_hg_500 <- bük_hg_500 |>
  group_by(obj, kultur2, id) |>
  summarise(sum_p_area = sum(area.) )


bük_hg_500w <- bük_hg_500 |>
  pivot_wider(names_from = obj,
              values_from = sum_p_area,
              id_cols = c(id, kultur2)
  )  |>
  ungroup()


### adonis für alle gegen alle kulturen:

## Achtung: läuft eine Weile! 

bb_para_n0 <- bük_hg_500w |>
  select(starts_with("Böden") ) 

bb_para_n0 <- bb_para_n0 |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0.00001) )) # Aitchinson geht nicht mit 0

bb_para_comps <- vegdist(bb_para_n0, "aitchison") # aitchison distance

# jetzt in adonis (vegan)

bük_hg_500_adonis <- adonis2(bb_para_comps ~ kultur2, data = bük_hg_500w, permutations = 9999)

## -> ergebnis R2 0.78 und p = 0.0001 -> hohe Korrelation allgemein

## adonis für verschiedene Kulturen gegeneinander

kulturen <- unique(bük_hg_500$kultur2)

# Ergebnis dataframe
adonis_BÜK_hg <- data.frame(k1 = as.character(),
                            k2 = as.character(),
                            p = as.numeric(),
                            R2 = as.numeric())


for (i in c(1:(length(kulturen)-1) ) ) {
  
  k1 <- kulturen[i]
  
  for (j in c(2:length(kulturen) ) ) {
    
    k2 <- kulturen[j]
    
    if (k1 != k2) {
      
      bb_para_n0 <- bük_hg_500w |> filter(kultur2 %in% c(k1, k2) ) |>
        select(starts_with("Böden") ) 
      
      bb_para_n0 <- bb_para_n0 |>
        mutate(across(where(is.numeric), ~ replace_na(.x, 0.00001) ))
      
      bb_para_comps <- vegdist(bb_para_n0, "aitchison") # aitchison distance
      
      # jetzt in adonis
      
      bük_hg_500_adonis <- adonis2(bb_para_comps ~ kultur2, data = bük_hg_500w[bük_hg_500w$kultur2 %in% c(k1, k2) ,], permutations = 9999)
      
      res <- data.frame(k1 = k1,
                        k2 = k2,
                        p = bük_hg_500_adonis$`Pr(>F)`[1],
                        R2 = bük_hg_500_adonis$R2[1])
      
      adonis_BÜK_hg <- rbind(adonis_BÜK_hg,
                             res)
      
    }
  }
}

adonis_BÜK_hg$k1 <- factor(adonis_BÜK_hg$k1, levels = c("GG", "Meso", "FBG", "LBK Siedl.", "LBK Dechsel", "SBK", "SRK", "Guhrau", "Rös","Rös. Beil/Axt"))

adonis_BÜK_hg$k2 <- factor(adonis_BÜK_hg$k2, levels = levels(adonis_BÜK_hg$k1))

## Korrektur für häufiges testen:
adonis_BÜK_hg$p_adj <- p.adjust(adonis_BÜK_hg$p, method = "fdr")




## plot das Ergebnis des ADONIS-tests


farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "gray80" , "grey70","grey60","grey50")

adonis_BÜK_hg |>
  ggplot() +
  geom_tile(aes(x = k1,
                y = k2,
                fill = p_adj)) +
  geom_text(aes(x = k1,
                y = k2,
                label = format(round(R2, 2), nsmall = 2) ),
            col = "white", size = 1) +
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
       title = "R² für Zusammenhang zwischen Anteilen an Bodenhauptgruppen und Kultur",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n R² und p-Werte eines ADONIS-Tests (9999 Permutationen) über die Aitchison-Distanz, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995)")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))

ggsave("../figures/Standortanalyse/Anteile_BÜK-hg_ADONIS_R2_p-werte.png", dpi = 300, width = 26, height = 18, units = "cm")

