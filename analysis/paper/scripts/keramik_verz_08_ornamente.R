### Verzierungsanalyse: Ornamentik

## Daten

ornamente <- read.csv2("./analysis/data/raw_data/ornamente.csv", sep = "|")

load("./analysis/data/derived_data/verzierungen.RData")

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggh4x)
 
ornamente <- ornamente |>
  mutate_at(c(3:35), ~ replace_na(.,0)) # NAs zu 0 !

verzierungen$ornament.id[verzierungen$ornament.id == "NA"] <- NA

#verz <- verzierungen[!is.na(verzierungen$ornament.id),] # verzierungen ohne ornamente raus???

verz_muster <- left_join(verzierungen, ornamente, join_by(ornament.id == Ornament.ID) )

save(verz_muster, file = "./analysis/data/derived_data/verzierungen_ornamente.RData")


## plot 



verz_muster |>
  pivot_longer(cols = c(`Nur.waagerechte.Bänder`:Auflösungserscheinungen),
               names_to = "ornament",
               values_to = "value",
               values_drop_na = TRUE) |>
  filter(value > 0) |>
  group_by(gefäßnummer) |>
  ggplot()+
  geom_bar(aes(x = ornament,
               fill = kultur_gr) )+
  theme(legend.position = "bottom")+
  labs(x = "Ornamentbeschreibung",
       y = "Anzahl Gefäße",
       fill = "Kultur",
       title = "Vergleich der Verteilung von Ornamenttypen",
       caption = paste0("n Gefäße = ", length(unique(verz_muster$gefäßnummer))))+
  coord_flip()+
  facet_grid(. ~kreislabel)


verz_ornamente_long <- verz_muster |>
  pivot_longer(cols = c("Nur.waagerechte.Bänder":"Auflösungserscheinungen"),
               names_to = "ornament",
               values_to = "value",
               values_drop_na = TRUE) |>
  filter(value > 0) 

gef_per_mustergr <- verz_ornamente_long |>   
  group_by(ornament) |>
  summarize(count = n_distinct(gefäßnummer))

ornam <- unique(verz_ornamente_long$ornament)
ornam_label <- gsub(ornam, pattern = '.', replacement = " ",      fixed = TRUE  )

source("./R_functions/farbzuweisungen.R")

verz_ornamente_long |>
  count(gefäßnummer, kultur2, ornament) |>
  group_by(gefäßnummer, kultur2, ornament) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = ornament,
               fill = kultur2,
               y = prop),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_mustergr, 
            aes(x = ornament, y = 1, 
                label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9) )  +
  scale_fill_manual(breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"), 
                    values = col )+
  scale_x_discrete(breaks = ornam,
                   labels = ornam_label)+
  theme(legend.position = "bottom")+
  labs(x = "",
       y = "Anteil Gefäße",
       fill = "Kultur",
       title = "Vergleich der Verteilung von Ornamenttypen",
       caption = paste0("n Gefäße, die eine Ornamentrekonstruktion zulassen = ", length(unique(verz_ornamente_long$gefäßnummer))))+
  theme_bw(base_size = 11)+
  theme(text = element_text(size = 11),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))+
  coord_flip() 

ggsave("./analysis/figures/Muster/2024-10-23_Ornamente_Kultur-Kreis.png", dpi = 300, width = 20, height = 30, units = "cm")

