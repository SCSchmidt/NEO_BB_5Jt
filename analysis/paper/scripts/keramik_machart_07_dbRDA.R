### Keramik Machart: distance based RDA

load("./analysis/data/derived_data/scherben_gef_magerung.RData")

magerung <- scherben_gef_magerung |> # war vorgefiltert
  filter(magerungsel_gr != "NA") |>
  mutate(rn = 1) 

magerung <- magerung |>
  select(c(gefäßnummer, kultur2, FO, magerungsel_gr, rn)) |>
  unique() |>
  pivot_wider(id_cols = c(gefäßnummer, kultur2, FO),
              names_from = magerungsel_gr,
              values_from = rn,
              values_fill = 0) 

magerung$organik <- magerung$organik + magerung$'organik?' # zusammenführen der unsicheren und sicheren
magerung$organik[magerung$organik > 1] <- 1
magerung <- magerung |>
  select(-c('organik?'))

load(file = "./analysis/data/derived_data/s_ofl.RData")

ofl <- s_ofl |>
  ungroup() |>
  select(gefäßnummer, ofl) |>
  group_by(gefäßnummer) |>
  slice_max(ofl) |>
  mutate(ofl = as.character(ofl) ) |>
  unique()

mag_menge <- scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  group_by(gefäßnummer) |>          # now required with changes to dplyr::count()
  slice_max(as.numeric(magerungsmenge)) |>  #nimmt größte der Werte aus der Gruppierung (egal, müssten alle gleich sein, aber so wird ein Wert ausgewählt)
  filter(!is.na(magerungsmenge)) |>
  ungroup() |>
  select(gefäßnummer, magerungsmenge) |>
  unique()


mag_gr <- scherben |>
  filter(as.integer(bruchpotential) < 7) |>
  group_by(gefäßnummer) |>          # now required with changes to dplyr::count()
  slice_max(as.numeric(magerungsgr)) |> #nimmt größte der Werte aus der Gruppierung (egal, müssten alle gleich sein, aber so wird ein Wert ausgewählt)
  filter(!is.na(magerungsgr)) |>
  ungroup() |>
  select(gefäßnummer, magerungsgr) |>
  unique()

load(file = "./analysis/data/derived_data/scherben_bf.RData")


bruchfarbe <- scherben_bf |>  # war vorgefiltert
  select(gefäßnummer, bruchfarbe_gr) |>
  unique() |>
  mutate(bruchfarbe_gr = as.character(bruchfarbe_gr) ) # not an ordered factor (war nur füs plotten!)


wandungsdicke <- scherben |>
  select(gefäßnummer, kultur2, min_dicke, max_dicke) |>
  group_by(gefäßnummer) |>
  slice_max(max_dicke) |>
  slice_min(min_dicke) |>
  unique()

gef <- unique(scherben |> select(gefäßnummer, FO) )

machart <- left_join(gef, magerung)
machart <- left_join(machart, ofl)
machart <- left_join(machart, bruchfarbe)
machart <- left_join(machart, wandungsdicke)
machart <- left_join(machart, mag_menge)
machart <- left_join(machart, mag_gr)


#machart$gefäßnummer[duplicated(machart$gefäßnummer)] # keine Dopplungen 

machart <- machart |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) # verändere alle Spalten, die numeric sind und wende Funktion replace_na auf Spalte (.x) an 

machart <- machart |>
  mutate(min_dicke = case_when(
    min_dicke == 0 ~ NA,
    TRUE ~ min_dicke),
    max_dicke = case_when(
      max_dicke == 0 ~ NA,
      TRUE ~ max_dicke    ) )

save(machart, file = "./analysis/data/derived_data/machart_zusammengefasst.RData")


### Gower distance alle Gefäße

library(FD)

machart2 <- machart |>
  filter(!is.na(kultur2) ) 

machart_og <- machart2 |>
  select(-c(gefäßnummer, FO, kultur2))

data_gower <- gowdis(machart_og, ord = "podani", asym.bin = c(1:8)) 

## dbrda über gowdis machart alle Gefäße

RDA_all <- dbrda(data_gower ~ kultur2, data = machart2)

summary_rda <- summary(RDA_all) #

summary_rda$cont$importance[2, "dbRDA1"]
summary_rda$cont$importance[2, "dbRDA2"]

## plot dbrda über gowdis machart: alle

library(tidyverse)
library(geomtextpath)
library(ggforce)

RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])


source("./R_functions/farbzuweisungen.R")

machart2$kultur2[machart2$kultur2 == "Rössen"] <- "Rös"

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "distanzbasierte Redundanzanalyse über Machart-Parameter",
       caption = "mit der Gower-Distanz über Magerungselemente, -größe, -menge, \nOberflächengestaltung, Wandungsdicke und Bruchfarbe mit \nKulturgruppe als erklärenden Faktor",
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ) )+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- machart2$kultur2),
               aes(label = kultur2, group = kultur2, color = kultur2), 
               geom = "textpath", hjust = 0.65, vjust = 1.2, linetype = 2) +
  geom_point(aes(colour = machart2$kultur2), 
             size = 2) +
  scale_color_manual("", 
                     breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col) +
  #coord_equal() + # das vllt raus?
  theme_minimal()

ggsave("./analysis/figures/Magerung/machart_rda_all.png", dpi = 300, width = 16, height = 16, units = "cm")


### nur verzierte Gefäße

load("./analysis/data/derived_data/machart_zusammengefasst.RData")
load("./analysis/data/derived_data/gef_infos.RData")


machart$bruchfarbe_gr <- factor(machart$bruchfarbe_gr )

machart2 <- machart |>
  filter(!is.na(kultur2) ) 

machart_v <- left_join(machart2, gef_infos)

machart_v <- machart_v |>
  filter(verz_n == "verz" ) |>
  select(-c(verz_n, Bef) ) |>
  unique()

machart_og <- machart_v |>
  select(-c(gefäßnummer, FO, kultur2, kreis, kreislabel, gemarkung, land, stufe)) 

data_gower <- gowdis(machart_og, ord = "podani", asym.bin = c(1:8)) ## Binary variables should be of class 'numeric' , drauf achten, dass character allesamt factor sind!

RDA_all <- dbrda(data_gower ~ kultur2, data = machart_v)

summary_rda <- summary(RDA_all) 

#summary_rda$cont$importance[2, "dbRDA1"]
#summary_rda$cont$importance[2, "dbRDA2"]


RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Merkmale der Machart",
       subtitle = "verzierte Gefäße",
       caption = paste("mit der Gower-Distanz über Magerungselemente, -größe, -menge, \nOberflächengestaltung, Wandungsdicke und Bruchfarbe mit \nKulturgruppe als erklärenden Faktor\n verzierte Gefäße, n = ", length(unique(machart_v$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- machart_v$kultur2),
               aes(label = kultur2, group = kultur2, colour = kultur2), 
               geom = "textpath", hjust = "ymax", vjust = 1.2) +
  geom_point(aes(colour = machart_v$kultur2,
                 shape = machart_v$kultur2), 
             size = 2) +
  scale_color_manual("", 
                     breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col ) +
  theme_minimal()

ggsave("./analysis/figures/Magerung/machart_rda_verz_all_kultur2_mit_ellipse.png", dpi = 300, width = 21, height = 16, units = "cm")

### plot RDA nur verz. Gef. Farbe nach Kreis

data_gower <- gowdis(machart_og, ord = "podani", asym.bin = c(1:8)) ## Binary variables should be of class 'numeric' , drauf achten, dass character allesamt factor sind!

RDA_all <- dbrda(data_gower ~ kultur2 + kreislabel, data = machart_v)

summary_rda <- summary(RDA_all) #

summary_rda$cont$importance[2, "dbRDA1"]
summary_rda$cont$importance[2, "dbRDA2"]

RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Merkmale der Machart",
       subtitle = "verzierte Gefäße",
       caption = paste("mit der Gower-Distanz über Magerungselemente, -größe, -menge, \nOberflächengestaltung, Wandungsdicke und Bruchfarbe mit \nKulturgruppe und Bundesland als erklärenden Faktor\n verzierte Gefäße, n = ", length(unique(machart_v$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- machart_v$kultur2),
               aes(label = kultur2, group = kultur2, linetype = kultur2), 
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  geom_point(aes(colour = machart_v$kreislabel,
                 shape = machart_v$kultur2), 
             size = 2) +
  scale_linetype_discrete(guide="none") +
  theme_minimal()

ggsave("./analysis/figures/Magerung/machart_rda_verz_all_kultur2-kreis_mit_ellipse.png", dpi = 300, width = 21, height = 16, units = "cm")
