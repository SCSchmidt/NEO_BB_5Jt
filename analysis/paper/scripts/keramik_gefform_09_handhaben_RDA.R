### RDA der Handhaben


load("./analysis/data/derived_data/handhaben.RData")

hh_2 <- handhaben |>
  select(gefäßnummer, technikform, 
         #   ausfuehrung, größe, # sonst Dopplung mit länge in mm 
         ausr.zu.rand, Handhabentyp, 
         Form_grundflaeche, 
         profil_tiefe,
         profil_seitlich_abschluss, 
         seitlich_richtung, 
         geteilt, 
         gelocht, 
         `in.Muster.einbezogen`,
         ort_k, länge_mm, tiefe_mm, kultur2, kreislabel) |>
  unique()

hh_2$tiefe_mm <- as.numeric(hh_2$tiefe_mm)
hh_2$ausr.zu.rand <- as.numeric(hh_2$ausr.zu.rand)
hh_2$profil_tiefe <- factor(hh_2$profil_tiefe,  ordered = T, levels = c("flach", "mittel", "groß", "sehr groß", "nach oben negativ") )

hh_2 <- hh_2 |>
  mutate(across(technikform:ort_k, as_factor))|>
  group_by(gefäßnummer) |> slice_head() |> ungroup() # aus irgendeinem grund ein Gef mit zwei gleichen Knubben doppelt

#hh_2$ausfuehrung <- factor(hh_2$ausfuehrung, ordered = T, levels = c("flach", "klein", "mittel", "groß", "sehr groß") )
#hh_2$größe <- factor(hh_2$größe,  ordered = T, levels = c("klein", "mittel", "groß", "sehr groß") )



hh_3 <- hh_2 |>
  select(-c(gefäßnummer, kreislabel, kultur2))

data_gower <- gowdis(hh_3, ord = "podani") #

RDA_all <- dbrda(data_gower ~ kultur2 + kreislabel, data = hh_2) 


summary_rda <- summary(RDA_all) #

summary_rda$cont$importance[2, "dbRDA1"]
summary_rda$cont$importance[2, "dbRDA2"]


RDA_all <- RDA_all  %>% scores(tidy = TRUE) %>% 
  split(.[["score"]])

source("./R_functions/farbzuweisungen.R")


ggplot(RDA_all$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  labs(title = "Distanzbasierte Redundanzanalyse über Merkmale der Handhaben",
       caption = paste("mit der Gower-Distanz über Handhaben mit \nKulturgruppe und Kreis als erklärende Faktoren\n Gefäße n = ", length(unique(hh_2$gefäßnummer)) ),
       x = paste("dbRDA1", round(summary_rda$cont$importance[2, "dbRDA1"] *100, 1), "%" ),
       y =  paste("dbRDA2",  round(summary_rda$cont$importance[2, "dbRDA2"]*100,1 ), "%" ),
       colour = "",
       shape = "")+
  stat_ellipse(data = within(RDA_all$sites, kultur2 <- hh_2$kultur2),
               aes(label = kultur2, group = kultur2, colour = kultur2), 
               geom = "textpath", hjust = "ymin", vjust = 1.2) +
  geom_point(aes(colour = hh_2$kultur2,
                 shape = hh_2$kultur2), 
             size = 2) +
  scale_color_manual("", 
                     breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col ) +
  scale_linetype_discrete(guide="none") +
  theme_minimal()

ggsave("./analysis/figures/Gef_Form/dbRDA_Handhaben-kultur2.png", dpi = 300, width = 21, height = 16, units = "cm")