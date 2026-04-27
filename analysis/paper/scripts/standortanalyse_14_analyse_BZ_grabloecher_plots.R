
## plotten

## falls noch nie laufgen gelassen, benötigt es: source("standortanalyse_11_analyse_BZ_grabloecher_vorb.R")
# source("analysis/paper/scripts/standortanalyse_01_datenvorbereitung.R")

load("./analysis/data/derived_data/fst_bs_punktabfrage_maxwerte.RData")

load(file = "./analysis/data/derived_data/sites.RData")


sites_bs <- left_join(fst_bs, sites, by = c("FST", "FID"))


sites_bs <- left_join(fst_bs, sites, by = c("FST", "FID"))

sites_bs <- sites_bs |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & kreislabel %in% c("UM","SPN", "OPR", "FF", "DSW", "OSL" ,"EE", "MOL", "LOS", "PR",  "OH",  "BAR") ~ "SRK",
    TRUE ~ kultur2) )

sites_bs$kultur2 <- factor(sites_bs$kultur2, 
                           levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt") )

library(ggplot2)

palette_LBK_SBK <- c("grey", "#61D04F", "#117733", "#CC6677",  "#882255",  "#CD0BBC","black",  "#F5C710", "#2297E6", "darkblue" )


sample_size <- as.data.frame(table(sites_bs$kultur2, sites_bs$r))

colnames(sample_size) <- c("kultur2", "r", "n")

sites_bs |>
  ggplot()+
  geom_jitter(aes(x = kultur2,
                  y = BS),
              alpha = 0.5)+
  geom_boxplot(aes(x = kultur2,
                   y = BS,
                   col = kultur2),
               fill = NA)+
  ylim(0,80)+
  geom_text(data = sample_size,
            aes(x = kultur2,
                y = 0,
                label = paste("n = ", n),
                hjust = 0))+
  scale_colour_manual(values = palette_LBK_SBK)+
  theme_bw()+
  labs(title = "Bodengüte im Umkreis der Fundstellen",
       subtitle = "anhand der Bohrlöcher",
       caption = "im Umkreis von 250, 500, 1000 und 2000m",
       y = "max. Wert d. Bodenzahl um eine Fundstelle herum",
       x = "",
       fill = "",
       colour = "")+
  coord_flip()+
  facet_grid( ~ r, scales = "free")+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(nrow = 1, byrow = TRUE))

ggsave("./analysis/figures/Standortanalyse/BS_max-wert_radius_kultur2.png", dpi = 300, width = 23, unit = "cm")


library(ggplot2)
library(ggh4x)

palette_LBK_SBK<- c( "grey", "#61D04F", "#117733","#CD0BBC",  "#CC6677",  "black", "#F5C710", "#2297E6", "darkblue" )

sample_size <- as.data.frame(table(sites_bs$kultur2, sites_bs$r, sites_bs$kreislabel))
colnames(sample_size) <- c("kultur2", "r", "kreislabel", "n")
sample_size <- sample_size[sample_size$n > 0,]
sample_size <- sample_size  |> filter(kreislabel != "OSL") 

sites_bs$kreislabel <- factor(sites_bs$kreislabel, ordered  = T,
                              levels = c("TF", "EE", "HVL", "BAR", "DSW", "SPN", "LOS", "MOL", "UM", "BRB", "FF", "OH", "OSL", "OPR", "P", "PM", "PR") )


l_per_h <- sites_bs |>
  filter(kreislabel != "OSL") |>
  group_by(kreislabel) |>
  mutate(count = n_distinct(kultur2)) |>
  select(kreislabel, count) |>
  unique() |>
  as.data.frame()

l_per_h$kreislabel <- factor(l_per_h$kreislabel, ordered  = T,
                             levels = c("TF", "EE", "HVL", "BAR", "DSW", "SPN", "LOS", "MOL", "UM", "BRB", "FF", "OH", "OSL", "OPR", "P", "PM", "PR") )


sites_bs |>
  filter(kreislabel != "OSL") |>
  ggplot()+
  geom_jitter(aes(x = kultur2,
                  y = BS),
              alpha = 0.5)+
  geom_boxplot(aes(x = kultur2,
                   y = BS,
                   col = kultur2),
               fill = NA)+
  ylim(0,80)+
  geom_text(data = sample_size,
            aes(x = kultur2,
                y = 0,
                label = paste("n = ", n),
                hjust = 0),
            size = 3)+
  scale_colour_manual(values = palette_LBK_SBK)+
  theme_bw()+
  labs(title = "Bodengüte im Umkreis der Fundstellen",
       subtitle = "anhand der Bohrlöcher",
       caption = "im Umkreis von 250, 500, 1000 und 2000m",
       y = "max. Wert d. Bodenzahl um eine Fundstelle herum",
       x = "",
       fill = "",
       colour = "")+
  coord_flip()+
  facet_grid(kreislabel ~ r, 
             scales = "free",
             drop = T,
             shrink = T)+
  force_panelsizes(rows = l_per_h$count[order(l_per_h$kreislabel)])+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 1, byrow = TRUE))

ggsave("./analysis/figures/Standortanalyse/BS_max-wert_radius-kreis_kultur.png", dpi = 300, width = 23, height = 30, unit = "cm")


## plot reduzieren auf nur neol. Kulturen + GG


sites_bs_o_mes <- sites_bs |>
  filter(!kultur2 %in% c("FBG", "Meso")) |>
  filter(!kreislabel %in% c("OSL", "PR", "OH", "BAR", "EE", "MOL", "OPR", "P", "TF") )


sample_size <- as.data.frame(table(sites_bs_o_mes$kultur2, sites_bs_o_mes$r, sites_bs_o_mes$kreislabel))
colnames(sample_size) <- c("kultur2", "r", "kreislabel", "n")
sample_size <- sample_size[sample_size$n > 0,]

sites_bs_o_mes$kreislabel <- factor(sites_bs_o_mes$kreislabel, ordered  = T,
                                    levels = c("TF", "EE", "HVL", "BAR", "DSW", "SPN", "LOS", "MOL", "UM", "BRB", "FF", "OH", "OSL", "OPR", "P", "PM", "PR") )


l_per_h <- sites_bs_o_mes |>
  filter(!kreislabel %in% c("OSL", "PR", "OHV") ) |>
  group_by(kreislabel) |>
  mutate(count = n_distinct(kultur2)) |>
  select(kreislabel, count) |>
  unique() |>
  as.data.frame()

l_per_h$kreislabel <- factor(l_per_h$kreislabel, ordered  = T,
                             levels = c("TF", "EE", "HVL", "BAR", "DSW", "SPN", "LOS", "MOL", "UM", "BRB", "FF", "OH", "OSL", "OPR", "P", "PM", "PR") )

source("../../R_functions/farbzuweisungen.R")

sites_bs_o_mes |>
  ggplot()+
  geom_jitter(aes(x = kultur2,
                  y = BS),
              alpha = 0.5)+
  geom_boxplot(aes(x = kultur2,
                   y = BS,
                   col = kultur2),
               fill = NA)+
  ylim(0,100)+
  geom_text(data = sample_size,
            aes(x = kultur2,
                y = 0,
                label = paste("n = ", n),
                hjust = 0),
            size = 3)+
  scale_colour_manual(values = col)+
  theme_bw()+
  labs(title = "Bodengüte im Umkreis der Fundstellen",
       subtitle = "anhand der Bohrlöcher",
       caption = "im Umkreis von 250, 500, 1000 und 2000m",
       y = "max. Wert d. Bodenzahl um eine Fundstelle herum",
       x = "",
       fill = "",
       colour = "")+
  coord_flip()+
  facet_grid(kreislabel ~ r, 
             scales = "free",
             drop = T,
             shrink = T)+
  force_panelsizes(rows = l_per_h$count[order(l_per_h$kreislabel)])+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 1, byrow = TRUE))

ggsave("./analysis/figures/Standortanalyse/BS_max-wert_radius-kreis_kultur_o_meso.png", dpi = 300, width = 23, height = 30, unit = "cm")


#| label: tabellarisches Bodenzahl-Median auslesen
#sites_bs_o_mes |>
#  group_by(kreislabel, r, kultur2) |>
#  mutate(median = median(BS)) |>
#  select(kreislabel, r, kultur2, median)|> 
#  unique() |>
#  filter(r == 500 & kreislabel == "UM")

#sites_bs |>
#  group_by(r, kultur2) |>
#  mutate(median = median(BS)) |>
#  select(median, r, kultur2) |>
#  unique()


## Aufteilung SBK-SRK und LBK-LBK-westl

sites_bs_SRK <- sites_bs |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & kreislabel %in% c("UM","SPN", "OPR", "FF", "DSW", "OSL" ,"EE", "MOL", "LOS", "PR",  "OH",  "BAR") ~ "SRK",
    kultur2 == "LBK Siedl." & kreislabel %in% c("PM" , "BRB" , "HVL" , "P" ) ~ "LBK westl. Trad.",
    TRUE ~ kultur2
  ))

sites_bs_SRK$kultur2 <- factor(sites_bs_SRK$kultur2, ordered = T,
                               levels =  c( "GG", "FBG", "Meso", "LBK Dechsel", "LBK Siedl.", "LBK westl. Trad.","SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt") )

palette_mit_LBK <- c("grey", "#61D04F", "#117733", "#CC6677",  "#882255", "red", "black","#CD0BBC",  "#F5C710", "#2297E6", "darkblue" )

k_breaks <- c("GG", "FBG", "Meso", "LBK Dechsel", "LBK Siedl.", "LBK westl. Trad.","SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt")


sample_size <- as.data.frame(table(sites_bs_SRK$kultur2, sites_bs_SRK$r))
colnames(sample_size) <- c("kultur2", "r", "n")

sites_bs_SRK |>
  ggplot()+
  geom_jitter(aes(x = kultur2,
                  y = BS),
              alpha = 0.5)+
  geom_boxplot(aes(x = kultur2,
                   y = BS,
                   col = kultur2),
               fill = NA)+
  ylim(0,80)+
  geom_text(data = sample_size,
            aes(x = kultur2,
                y = 0,
                label = paste("n = ", n),
                hjust = 0))+
  scale_colour_manual(values = palette_mit_LBK
  )+
  theme_bw()+
  labs(title = "Bodengüte im Umkreis der Fundstellen",
       subtitle = "anhand der Bohrlöcher",
       caption = "im Umkreis von 250, 500, 1000 und 2000m",
       y = "max. Wert d. Bodenzahl um eine Fundstelle herum",
       x = "",
       fill = "",
       colour = "")+
  coord_flip()+
  facet_grid( ~ r, scales = "free")+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(nrow = 2, byrow = TRUE))

ggsave("./analysis/figures/Standortanalyse/BS_max-wert_radius_SRK_LBKwest.png", dpi = 300, width = 23, unit = "cm")

#### Bodenzahl KS-Test

load("../data/derived_data/fst_bs_punktabfrage_maxwerte.RData")

fst_bs_500 <- fst_bs |> filter(r == 500)

fst_bs_2000 <- fst_bs |> filter(r == 2000)

fst_bs_2000 <-left_join(fst_bs_2000, sites, by = "FID")

fst_bs_2000 <- fst_bs_2000 |>
  mutate(kultur2 = case_when(
    kultur2 == "SBK" & !(KREIS %in% c("Teltow-Fläming", "Havelland", "Potsdam-Mittelmark", "Potsdam") ) ~ "SRK" ,
    TRUE ~ kultur2  ))

n_rows <- as.data.frame(table(fst_bs_2000$kultur2))

# remotes::install_github("happyrabbit/DataScienceR")

library(DataScienceR)

ks_test_bs_BB <- as.data.frame( pairwise_ks_test(fst_bs_2000$BS, fst_bs_2000$kultur2, n_min = 5, warning = 0,
                                                 alternative = "two.sided") )

ks_test_bs_BB$k1 <- rownames(ks_test_bs_BB)

BS_p <- ks_test_bs_BB |>
  pivot_longer(names_to = "k2",
               cols = c(`LBK Dechsel`:Meso),
               values_to = "pvalues")


BS_p$k1 <- factor(BS_p$k1 , levels = c("GG", "Meso", "FBG", "LBK Siedl.", "LBK Dechsel", "SBK", "SRK", "Guhrau", "Rös", "Rös. Beil/Axt"))

BS_p$k2 <- factor(BS_p$k2 , levels = levels(BS_p$k1))

BS_p <- left_join(BS_p, n_rows, by = c("k1" = "Var1"))
BS_p <- left_join(BS_p, n_rows, by = c("k2" = "Var1"))

BS_p$n_note <- paste(BS_p$k1, "n =", BS_p$Freq.x, "\n", BS_p$k2, "n =", BS_p$Freq.y )

BS_p$n_note <- stringr::str_replace_all(BS_p$n_note , pattern = "LBK Dechsel n =", replacement = "LBK Beil n =")
BS_p$n_note <- stringr::str_replace_all(BS_p$n_note , pattern = "Rös. Beil/Axt n =", replacement = "Rös. Axt n =")

BS_p$p.adj <- p.adjust(BS_p$pvalues, method = "fdr")


BS_p$p.adj[BS_p$k1 == BS_p$k2] <- NA

BS_p |>
  filter(!is.na(p.adj) ) |>
  ggplot(aes(x = k1,
             y = k2) ) +
  geom_tile(aes(fill = round(p.adj, 2 )),
            color = "white" )+
  geom_text(aes(x = k2,
                y = k1,
                label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  geom_text(aes(x = k1,
                y = k2,
                label = n_note),
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
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
       title = "P-Werte für Gruppenunterschiede der Fundstellenlage auf Bodengüte",
       subtitle = "größter Wert der Bodenzahl im Umkreis von 2000 m",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n Guhrauer Fundstellen alle nicht signifikant, deshalb nicht dargestellt, \n p-Werte mit dem KS-Test erhalten und angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995).")+
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom")

ggsave("./analysis/figures/Standortanalyse/BS_Bohrlöcher_200m_p-Werte_KS-test.png", dpi = 300, width = 44, height = 20, units = "cm")
