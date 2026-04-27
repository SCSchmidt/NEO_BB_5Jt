### RDA der Gefäßformmerkmale

library(dplyr)
library(vegan)
library(forcats)
library(FD)
library(ggplot2)
library(geomtextpath)
library(ggforce)

load("./analysis/data/derived_data/gef_umbruch.RData")

gef_umbruch <- gef_umbruch2

gef_umbruch$erh_s <- as.numeric(gef_umbruch$erh_s)
gef_umbruch$erh_r <- as.numeric(gef_umbruch$erh_r)
gef_umbruch$randneigung <- as.numeric(gef_umbruch$randneigung)
gef_umbruch$bodenwinkel <- as.numeric(gef_umbruch$bodenwinkel)
gef_umbruch$erh_b <- as.numeric(gef_umbruch$erh_b)

gef_umbruch$randabschluss_gr <- as.factor(gef_umbruch$randabschluss_gr)
gef_umbruch$randabschluss_f <- as.factor(gef_umbruch$randabschluss_f)
gef_umbruch$boden <- as.factor(gef_umbruch$boden)
gef_umbruch$Hals <- as.factor(gef_umbruch$Hals)

# filtern auf die vollständigen Datensätze

oberteile <- gef_umbruch |>
  filter(erh_r > 4)

oberteile2 <- oberteile |>
  mutate(h_hals = case_when(
    erh_s < 1 ~ NA,
    TRUE ~ h_hals)) |>
  mutate(durchm_r = case_when(
    erh_r < 5 ~ NA,
    TRUE ~ durchm_r)) |>
  mutate(randneigung = case_when(
    erh_r < 5 ~ NA,
    TRUE ~ randneigung) )|>
  mutate(across(c(`Hals-Schulter-Umbruch`:`Schulter-Bauch-Boden`, randabschluss_gr, randabschluss_f), as_factor)) |>
  mutate(across(randform_nonek:bauchform_nonek, as_factor)) |> 
  select(-c(erh_r, erh_s, erh_b, erh_bod, sicherheit_orient, rand_halsband, x_scherben, n_anspassungen, gef_typ, gef_typ_gr, gef_untertyp, bem, vgl, durchm_b:durchm_bod) )

# Merkmalsauswahl

oberteile3 <- oberteile2 |>
  select(-c(h_insg, h_unterteil, bauchform_nonek, `Schulter-Bauch-Umbruch`, `Bauch-Boden`:Fuß, `Schulter-Bauch-Boden`, verh_durchm_r_h_insg, verh_durchm_r_b, verh_durchm_s_b, verh_durchm_r_bod, verh_durchm_b_h_insg, verh_o_u) ) # alles Infos, die bei Randscherben selten sind -> nur noch Infos zum Thema Rand und Schulter behalten

oberteile2_og <- oberteile3 |>
  select(-c(gefäßnummer, FO:kultur_gr) )

oberteile2_og <- oberteile2_og[,colSums(is.na(oberteile2_og)) < nrow(oberteile2_og)] # find column that is empty ->  doch nix raus ?

data_gower <- gowdis(oberteile2_og, ord = "podani") 

data_gower_m <- as.matrix(data_gower)

rownames(data_gower_m) <- colnames(data_gower_m) <- oberteile2$gefäßnummer

dist_new <- data_gower_m[rowSums(is.na(data_gower_m)) == 0, colSums(is.na(data_gower_m)) == 0, drop = FALSE] # alle Spalten und Zeilen raus, die NAs haben

gower_df <- as.data.frame(dist_new)
gower_df$gefäßnummer <- rownames(gower_df)

dist_new <- as.dist(dist_new)

gower_df_infos <- left_join(gower_df, oberteile2) # connect den reduzierten DF mit den Infos -> hmmmjh, aber es bleibt bei 206?

RDA_all <- dbrda(dist_new ~ kultur2, data = gower_df_infos) 


summary_rda <- summary(RDA_all) #

summary_rda$cont$importance[2, "dbRDA1"]
summary_rda$cont$importance[2, "dbRDA2"]


RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

source("./R_functions/farbzuweisungen.R")

ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Merkmale der Gefäßform",
       subtitle = "Randscherben",
       caption = paste("mit der Gower-Distanz über Gefäßoberteilform (Hals - Schulter) mit \nKulturgruppe als erklärenden Faktor\n Gefäße mit > 4 % erhaltenem Rand, n = ", length(unique(gower_df_infos$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- gower_df_infos$kultur2),
               aes(label = kultur2, group = kultur2, colour = kultur2), 
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  geom_point(aes(colour = gower_df_infos$kultur2,
                 shape = gower_df_infos$kultur2), 
             size = 2) +
  scale_color_manual("", 
                     breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col)  +
  scale_linetype_discrete(guide="none") +
  theme_minimal()

ggsave("./analysis/figures/Gef_Form/dbRDA_Raender-kultur2.png", dpi = 300, width = 21, height = 16, units = "cm")