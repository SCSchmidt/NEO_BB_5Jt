### dbRDA der Verzierungen

library(stringr)
library(dplyr)
library(ggh4x)
library(ggplot2)
library(FD)
library(ade4)
library(vegan)

load("./analysis/data/derived_data/verzierungen_ornamente.RData")
load("./analysis/data/derived_data/verzierungen_waagerechteBaender.RData")
load("./analysis/data/derived_data/verzierungen.RData")
load("./analysis/data/derived_data/scherben.RData")

gef_infos <- scherben |>
  select(gefäßnummer, FO, kreis, kultur2, Bef) |>
  unique() |>
  group_by(gefäßnummer) |>
  slice_max(Bef) # größere der beiden

# Muster zusammenführen und pivotieren
verz_muster_red <- verz_muster |>
  select(c(gefäßnummer, technik, Muster_neu_gr, "Nur.waagerechte.Bänder":"Mehrfache.Winkelbänder"))

verz_muster_red <- verz_muster_red |> distinct()

verz_muster_red_wide <- verz_muster_red |>
  mutate(rn = row_number()) #|>

verz_muster_red_wide <- verz_muster_red_wide |>
  pivot_wider(id_cols = c(gefäßnummer, Muster_neu_gr), names_from = technik, values_from = "rn", values_fn = list(rn = length))

verz_muster_red_wide <- verz_muster_red_wide |>
  mutate(rn = row_number()) #|>

verz_muster_red_wide <- verz_muster_red_wide |> filter(Muster_neu_gr != "")

verz_muster_red_wide <- verz_muster_red_wide |>
  pivot_wider(id_cols = gefäßnummer, names_from = Muster_neu_gr, values_from = "rn", values_fn = list(rn = length)) 



verz_muster_red_wide <- verz_muster_red_wide |>
  group_by(gefäßnummer)|>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) |> # verändere alle Spalten, die numeric sind und wende Funktion replace_na auf Spalte (.x) an 
  mutate(across(where(is.numeric),  ~ifelse(. > 0, 1, 0) ) ) # verändere alle Spalten, die numeric sind und wende ifelse auf werte (.) an. wenn über 0 -> 1, alles andere 0.


verz_muster$rn <- rownames(verz_muster)

technik <- verz_muster |>
  pivot_wider(id_cols = gefäßnummer, names_from = technik, values_from = "rn", values_fn =  list(rn = length)) |>
  select(-c("einzelstich o. rössener doppelstich", "durchstich von hinten zugepropft", "freigelassene fläche") )

ausf <- verz_muster |>
  pivot_wider(id_cols = gefäßnummer, names_from = ausfuehrung, values_from = "rn", values_fn =  list(rn = length), names_prefix = "ausf_") |>
  select(-c("ausf_NA") )

gr <-  verz_muster |>
  pivot_wider(id_cols = gefäßnummer, names_from = groeße, values_from = "rn", values_fn =  list(rn = length), names_prefix = "gr_") |>
  select(-c("gr_NA") )

zinkenzahl <-  verz_muster |>
  pivot_wider(id_cols = gefäßnummer, names_from = zinkenzahl, values_from = "rn", values_fn =  list(rn = length), names_prefix = "zinkenzahl_") |>
  select(-c("zinkenzahl_NA") )

element <- verz_muster |> filter(element != "")

element <-  element |>
  pivot_wider(id_cols = gefäßnummer, names_from = element, values_from = "rn", values_fn =  list(rn = length))

technik_element <- left_join(gef_infos, technik)
technik_element <- left_join(technik_element, ausf)
technik_element <- left_join(technik_element, gr)
technik_element <- left_join(technik_element, zinkenzahl)
technik_element <- left_join(technik_element, element)


technik_element <- technik_element |>
  ungroup() |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) |> # verändere alle Spalten, die numeric sind und wende Funktion replace_na auf Spalte (.x) an 
  mutate(across(where(is.numeric),  ~ifelse(. > 0, 1, 0) ) )  # verändere alle Spalten, die numeric sind und wende ifelse auf werte (.) an. wenn über 0 -> 1 

technik_element <- technik_element |>
  filter(!is.na(kultur2)) 

technik_element$rs <- rowSums(technik_element[,c(6:87)])

technik_element <- technik_element |>
  filter(rs > 0)


technik_element_og <- technik_element |>
  select(-c(gefäßnummer, FO, kreis, kultur2, Bef) )

##### RDA Technik und Elemente -> erklärt am meisten!

data_gower <- gowdis(technik_element_og, ord = "podani", asym.bin = c(1:82)) ## Binary variables should be of class 'numeric' , drauf achten, dass character allesamt factor sind!

save(data_gower, file = "./analysis/data/derived_data/gower_distanz_technik-element.RData")
save(technik_element, file = "./analysis/data/derived_data/technik-element_grundlage.RData")

RDA_all <- dbrda(data_gower ~ kultur2, data = technik_element)

summary_rda <- summary(RDA_all) #

summary_rda$cont$importance[2, "dbRDA1"]
summary_rda$cont$importance[2, "dbRDA2"]


RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

library(geomtextpath)

source("./R_functions/farbzuweisungen.R")

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Verzierungs-Parameter",
       caption = paste("mit der Gower-Distanz über Verzierungstechniken, -größe, -ausführung, -elemente mit \nKulturgruppe als erklärenden Faktor\n verzierte Gefäße, n = ", length(unique(technik_element$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- technik_element$kultur2),
               aes(label = kultur2, group = kultur2, colour = kultur2), 
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  geom_point(aes(colour = technik_element$kultur2,
                 shape = technik_element$kultur2), 
             size = 2) +
  scale_color_manual("", 
                     breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col) +
  scale_linetype_discrete(guide="none") +
  #coord_equal() + # das vllt raus?
  theme_minimal()


ggsave("./analysis/figures/Muster/dbRDA_Technik_Element_all-kultur2.png", dpi = 300, width = 21, height = 16, units = "cm")

### nach Kreisen

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Verzierungsparameter",
       caption = paste("mit der Ochiai-Distanz über Verzierungsmuster, -elemente, -technik und Ornamentik \nKulturgruppe als erklärenden Faktor\n verzierte Gefäße, n = ", length(unique(technik_element$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  geom_point(aes(colour = technik_element$kreis,
                 shape = technik_element$kultur2), 
             alpha = 0.5,
             size = 2) +
  stat_ellipse(data = within(RDA_all$sites, kreis <- technik_element$kreis),
               aes(label = kreis, group = kreis, colour = kreis), 
               geom = "textpath", hjust = 0.5, vjust = 1.2) +
  scale_linetype_discrete(guide="none") +
  theme_minimal()

ggsave("./analysis/figures/Muster/dbRDA_Technik_Element_all-kreis.png", dpi = 300, width = 21, height = 16, units = "cm")

### Technik - Element - MUSTER -> Kultur erklärt weniger, Muster wie "Winkel" sind Kulturübergreifend

verz_muster_red_wide <- left_join(verz_muster_red_wide, gef_infos) 

verz_muster_red_wide <- verz_muster_red_wide |>
  filter(!is.na(kultur2))

all_verz <- left_join(technik_element, verz_muster_red_wide)

all_verz <- all_verz |> select(-c("rs", "Rauten über Gefäßkörper ?" ) )|>
  group_by(gefäßnummer)|>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) |> # verändere alle Spalten, die numeric sind und wende Funktion replace_na auf Spalte (.x) an 
  mutate(across(where(is.numeric),  ~ifelse(. > 0, 1, 0) ) ) # verändere alle Spalten, die numeric sind und wende ifelse auf werte (.) an. wenn über 0 -> 1, alles andere 0.

all_verz_og <- all_verz |>
  ungroup() |>
  select(-c(gefäßnummer, FO, kreis, kultur2, Bef) ) 


data_gower <- dist.binary(all_verz_og, method = 7) ## Ochiai Distanz gewichtet nach Anzahl der Beziehungen

RDA_all <- dbrda(data_gower ~ kultur2, data = all_verz)

summary_rda <- summary(RDA_all) #

summary_rda$cont$importance[2, "dbRDA1"]
summary_rda$cont$importance[2, "dbRDA2"]


RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Verzierungsparameter",
       caption = paste("mit der Ochiai-Distanz über Verzierungsmuster, -elemente, -technik \nKulturgruppe als erklärenden Faktor\n verzierte Gefäße, n = ", length(unique(verz_muster_red_wide$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- all_verz$kultur2),
               aes(label = kultur2, group = kultur2, colour = kultur2), 
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  geom_point(aes(colour = all_verz$kultur2,
                 shape = all_verz$kultur2), 
             size = 2) +
  scale_color_manual("", 
                     breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col ) +
  scale_linetype_discrete(guide="none") +
  #coord_equal() + # das vllt raus?
  theme_minimal()


ggsave("./analysis/figures/Muster/dbRDA_all_verz_muster-el-technik-kultur2.png", dpi = 300, width = 21, height = 16, units = "cm")

### Technik - Element - Muster und ORNAMENT -> auch nicht so gut im erklären (keine große Veränderung zu dem vorher)


ornament <- verz_muster |>
  select(gefäßnummer, kreis, c('Nur.waagerechte.Bänder':'Auflösungserscheinungen')) |>
  unique() |>
  group_by(gefäßnummer, kreis) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) |> # NA zu 0
  summarise(across(everything(), sum)) |>  
  mutate(across(where(is.numeric),  ~ifelse(. > 0, 1, 0) ) ) # wo über 1 zu 1, alles andere 0
#irgendwie war da was duplicated

all_verz_orn <- left_join(all_verz, ornament)

all_verz_orn <- all_verz_orn |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0) ) ) |> # NA zu 0
  filter(!is.na(kultur2)) |>
  ungroup()


all_verz_orn_og <- all_verz_orn |>
  select(-c(gefäßnummer, FO, kreis, kultur2, Bef) ) 

data_gower <- dist.binary(all_verz_orn_og, method = 7) ## Ochiai Distanz gewichtet nach Anzahl der Beziehungen

RDA_all <- dbrda(data_gower ~ kultur2, data = all_verz_orn)

summary_rda <- summary(RDA_all) #

summary_rda$cont$importance[2, "dbRDA1"]
summary_rda$cont$importance[2, "dbRDA2"]


RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Verzierungsparameter",
       caption = paste("mit der Ochiai-Distanz über Verzierungsmuster, -elemente, -technik und Ornamentik \nKulturgruppe als erklärenden Faktor\n verzierte Gefäße, n = ", length(unique(verz_muster_red_wide$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- all_verz$kultur2),
               aes(label = kultur2, group = kultur2, colour = kultur2), 
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  geom_point(aes(colour = all_verz$kultur2,
                 shape = all_verz$kultur2), 
             size = 2) +
  scale_color_manual("", 
                     breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col ) +
  scale_linetype_discrete(guide="none") +
  #coord_equal() + # das vllt raus?
  theme_minimal()

ggsave("./analysis/figures/Muster/dbRDA_all_verz_all-kultur2.png", dpi = 300, width = 21, height = 16, units = "cm")


ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Verzierungsparameter",
       caption = paste("mit der Ochiai-Distanz über Verzierungsmuster, -elemente, -technik und Ornamentik \nKulturgruppe als erklärenden Faktor\n verzierte Gefäße, n = ", length(unique(verz_muster_red_wide$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kreis <- all_verz$kreis),
               aes(label = kreis, group = kreis, colour = kreis), 
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  geom_point(aes(colour = all_verz$kreis,
                 shape = all_verz$kultur2), 
             size = 2) +
  scale_linetype_discrete(guide="none") +
  #coord_equal() + # das vllt raus?
  theme_minimal()
