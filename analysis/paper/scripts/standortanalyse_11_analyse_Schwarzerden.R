## Schwarzerdenanteile in Umgebung der Fundstellen

# 1. Daten laden
# 2. Daten plotten
# 3. Distanzberechnung und ADONIS

load(file = "./analysis/data/derived_data/sites.RData")


ofl_schwarzerd5000 <- read.csv2("/analysis/data/geodata/standortanalyse_BB_2025-02-03/schwarzerd.csv", sep = ",", dec = ".")

ofl_schwarzerd1000 <- read.csv2("/analysis/data/geodata/standortanalyse_1000_BB/schwarzerd.csv", sep = ",", dec = ".")

ofl_schwarzerd2000 <- read.csv2("/analysis/data/geodata/standortanalyse_2000_BB/schwarzerd.csv", sep = ",", dec = ".")

ofl_schwarzerd250 <- read.csv2("/analysis/data/geodata/standortanalyse_250_BB/schwarzerd.csv", sep = ",", dec = ".")

ofl_schwarzerd500 <- read.csv2("/analysis/data/geodata/standortanalyse_500_BB/schwarzerd.csv", sep = ",", dec = ".")


ofl_schwarzerd5000$obj  <- paste0(ofl_schwarzerd5000$obj , "_r5000")

ofl_schwarzerd1000$obj <- paste0(ofl_schwarzerd1000$obj , "_r1000")

ofl_schwarzerd2000$obj <- paste0(ofl_schwarzerd2000$obj , "_r2000")

ofl_schwarzerd250$obj <- paste0(ofl_schwarzerd250$obj , "_r250")

ofl_schwarzerd500$obj <- paste0(ofl_schwarzerd500$obj , "_r500")

ofl_schwarzerd_all <- rbind(ofl_schwarzerd1000, ofl_schwarzerd2000, ofl_schwarzerd500, ofl_schwarzerd250, ofl_schwarzerd5000) #ofl_schwarzerd5000 - 5000 dazu, falls nötig



ofl_schwarzerd_all$id <- as.character(ofl_schwarzerd_all$id)

standort_sites <- left_join(sites, ofl_schwarzerd_all, by = c("FID" = "id") )


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
  filter(!(str_detect(obj, "bb_admin")) ) |>
  group_by(kreislabel, kultur2) |>
  summarize(count = n_distinct(FST))

fo_per_kr <- st_drop_geometry(fo_per_kr) |> ungroup()


k_per_kr <- standort_sites |>    
  filter(!(str_detect(obj, "bb_admin")) ) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()

x <- standort_sites |>
  group_by(kreislabel, obj, kultur2) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()


### plot

x2 <- left_join(x, fo_per_kr, by = c("kreislabel", "kultur2" ))

x2 <- x2 |>
  mutate(obj2 = case_when(
    obj == "Schwarzerden_r250" ~ 250,
    obj == "Schwarzerden_r500" ~ 500,
    obj == "Schwarzerden_r1000" ~ 1000,
    obj == "Schwarzerden_r2000" ~ 2000 ,
    obj == "Schwarzerden_r5000" ~ 5000
  ))

x2 |>
  filter(!is.na(obj2) ) |>
  mutate(proz = sum_p_area / count * 100) |>
  ggplot()+
  geom_col(aes(y = proz,
               x = kultur2,
               fill = as.factor(obj2) ),
           position = "dodge") +
  geom_text(data = fo_per_kr ,
            aes(x = kultur2,
                y = 60.5,
                label = count),
            colour = "black",
            hjust = 1,
            size = 3) +
  labs(title = "Bezug zu Schwarzerden",
       subtitle = "Anteile an Schwarzerden im Umkreis von 250 bis 5000 m",
       x = "",
       y = "% von Fläche im Umkreis",
       fill = "Radius d. Site Catchment",
       caption = "Datengrundlage: Bodenschätzung \nQuelle: © LGB dl-by-de/2.0 \n Flüsse und Seen unter 100 qm Fläche ausgespart")+
  scale_fill_manual(breaks = c(250, 500, 1000, 2000, 5000),
                    values = c("black", "brown", "red","orange", "yellow"),
                    labels = c("250 m", "500 m", "1 km", "2 km", "5 km")) +
  coord_flip()+
  facet_wrap(kreislabel ~ ., scales = "free_y", drop = TRUE) +
  force_panelsizes(rows = rep(8, 17))+
  theme_bw()+
  theme(legend.position = "bottom" )+
  guides(fill=guide_legend(nrow=1))


ggsave("./analysis/figures/Standortanalyse/schwarzerden_Kreis-Kultur_rad250-5000.png", dpi = 300, height = 25, width = 25, units = "cm")





#### Adonistest Schwarzerden

schw_500 <- standort_sites |>
  filter(obj == "Schwarzerden_r500")


schw_500 <- schw_500 |>
  group_by(FID, kultur2) |>
  mutate(sum_p_area = sum(area.)) |>
  ungroup()

schw_500 <- schw_500 |> select(FID, sum_p_area, kultur2, kreislabel)

### fehlen Fundstellen?
ohne_schw <- sites |>
  filter(!(FID %in%  schw_500$FID))

ohne_schw$sum_p_area <- 0

ohne_schw <- ohne_schw |> select(FID, kultur2, sum_p_area, kreislabel)

schw_500 <- rbind(schw_500, ohne_schw)


schw_500 <- schw_500 |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(kreislabel %in% c("TF", "HVL", "PM", "P") ) ~ "SRK" ,
    TRUE ~ kultur2  )) |>
  st_drop_geometry()


schw_500$nicht_schw <- 1 - schw_500$sum_p_area

## alle gegen alle


bb_para_n0 <- schw_500 |>
  select(sum_p_area, nicht_schw) 

bb_para_n0[bb_para_n0 == 0] <- 0.0001

bb_para_n0 <- bb_para_n0 |>
  mutate(sum_p_area = case_when(
    nicht_schw == 0.0001 ~ sum_p_area -  0.0001,
    TRUE ~ sum_p_area),
    nicht_schw = case_when(
      sum_p_area == 0.0001 ~ nicht_schw - 0.0001,
      TRUE ~ nicht_schw
    ) )


bb_para_comps <- vegdist(bb_para_n0, "aitchison") # airchison distance

# jetzt in adonis

adonis2(bb_para_comps ~ kultur2, data = schw_500, permutations = 9999)

## R2 = 0,19, p = 0,0071, wenn nur die 110 Fst mit schwarzerde dabei sind

## R2 = 0.255, p = 1e-04 wenn alle anderen dazugenommen -> klar, besser  

#pairwise.wilcox.test(schw_500$sum_p_area, schw_500$kultur2)

###
kulturen <- unique(schw_500$kultur2)


adonis_schw_500 <- data.frame(k1 = as.character(),
                              k2 = as.character(),
                              p = as.numeric(),
                              R2 = as.numeric())


for (i in c(1:(length(kulturen)-1) ) ) {
  
  k1 <- kulturen[i]
  
  for (j in c(2:length(kulturen) ) ) {
    
    k2 <- kulturen[j]
    
    if (k1 != k2) {
      
      bb_para_n0 <- schw_500 |> filter(kultur2 %in% c(k1, k2) ) |>
        select(sum_p_area, nicht_schw) 
      
      bb_para_n0[bb_para_n0 == 0] <- 0.0001
      
      bb_para_n0 <- bb_para_n0 |>
        mutate(sum_p_area = case_when(
          nicht_schw == 0.0001 ~ sum_p_area -  0.0001,
          TRUE ~ sum_p_area),
          nicht_schw = case_when(
            sum_p_area == 0.0001 ~ nicht_schw - 0.0001,
            TRUE ~ nicht_schw
          ) )
      
      
      bb_para_comps <- vegdist(bb_para_n0, "aitchison") # airchison distance
      
      # jetzt in adonis
      
      schw_500_adonis <- adonis2(bb_para_comps ~ kultur2, data = schw_500[schw_500$kultur2 %in% c(k1, k2) ,], permutations = 9999)
      
      res <- data.frame(k1 = k1,
                        k2 = k2,
                        p = schw_500_adonis$`Pr(>F)`[1],
                        R2 = schw_500_adonis$R2[1])
      
      adonis_schw_500 <- rbind(adonis_schw_500,
                               res)
      
    }
  }
}

adonis_schw_500$k1 <- factor(adonis_schw_500$k1, levels = c("GG", "Meso", "FBG", "LBK Siedl.", "LBK Dechsel", "SBK", "SRK", "Guhrau", "Rös","Rös. Beil/Axt"))

adonis_schw_500$k2 <- factor(adonis_schw_500$k2, levels = levels(adonis_schw_500$k1))

## plot test
adonis_schw_500$p_adj <- p.adjust(adonis_schw_500$p, method = "fdr")

farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF",  "grey70","grey60","grey50")

adonis_schw_500 |>
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
       title = "R² und P-Werte für Korrelation zwischen Kultur und Anteilen an Schwarzerden",
       subtitle = "im Umkreis von 500 m der Fundstellen",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n R² und p-Werte eines ADONIS-Tests (9999 Permutationen) über die Aitchison-Distanz, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995)")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))

ggsave("./analysis/figures/Standortanalyse/schwarzerd_ADONIS_R2_p-werte.png", dpi = 300, width = 26, height = 18, units = "cm")


