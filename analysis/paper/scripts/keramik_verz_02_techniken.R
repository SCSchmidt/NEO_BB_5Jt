## Verzierungsanalyse: Techniken

load(file = "./analysis/data/derived_data/verzierungen.RData")

library(stringr)
library(dplyr)
library(ggh4x)
library(ggplot2)


safe_colorblind_palette13 <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888","black")

# verzierungstyp "freigelassene Fläche" nicht konsistent genug aufgenommen

fo_per_kr <- verzierungen |>
  filter(technik_gr != "freigelassene fläche") |>
  filter(technik_gr != "NA") |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- verzierungen |>
  filter(technik_gr != "freigelassene fläche") |>
  filter(technik_gr != "NA") |>
  group_by(kultur_gr, fundort, kreislabel) |>
  summarise(count = n_distinct(gefäßnummer)) 

verzierungen |>
  filter(technik_gr != "freigelassene fläche") |>
  filter(technik_gr != "NA") |>
  filter(!is.na(technik_gr)) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, technik_gr) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, technik_gr) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = technik_gr),
           position = "fill", stat = "identity")+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 50, 100) )+
  geom_text(data = gef_per_fo,
            aes(x = fundort, y = 1, 
                label  = count ), 
            size = 4,
            hjust = 1, 
            angle = 0, 
            colour = "black",
            position = position_fill(vjust = .95)  )  +
  labs(x = "",
       y = "Prozent der verzierten Gefäße",
       title = "Verzierungstechniken",
       fill = "Technik",
       caption = paste0( "verzierte Gefäße, n = ", length(unique(verzierungen$gefäßnummer)), "\nmehrere Techniken pro Gefäß möglich") )+
  coord_flip()+
  scale_fill_manual(values = safe_colorblind_palette13)+ 
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 11),
        legend.position = "bottom")

ggsave("./analysis/figures/Muster/2024-10-23_Techniken_FBG-SBK-BKK-Rös_Guhrau--Kreis2.jpg", height = 21, width = 16, units = "cm")  

## für Chi-Vorbereitung


verz_technik <- verzierungen |>
  select(gefäßnummer, technik_gr, kultur2) |>
  distinct(gefäßnummer, technik_gr, kultur2) |>
  mutate(kultur2 = as.character(kultur2)) |> 
  filter(technik_gr != "Einzelstich o. Rössener Doppelstich") |> # zu selten
  mutate(technik_gr = case_when(
    technik_gr == "geritzt" | technik_gr == "geschnitten" ~ "geritzt o. geschnitten", 
    technik_gr == "Fünffachstich" | technik_gr == "Vierfachstich" ~ "4-5fachstich",
    TRUE ~ technik_gr
  )) |>
  filter(technik_gr != "Inkrustation")  |> # erhaltungsbedingt
  filter(technik_gr != "Meißelstich, tremolierend") |> # zu selten
  filter(technik_gr != "plastisch verziert") # eher Grobkeramik -> weniger im Datensatz


### Größe der Stiche



gef_per_kr <- verzierungen |> 
  filter(technik_gr != "NA") |>
  filter(!is.na(groeße)) |>
  group_by(kreis, kultur_gr) |>
  summarise(count = n_distinct(gefäßnummer))

verzierungen |> 
  filter(technik_gr != "NA") |>
  filter(!is.na(groeße)) |>
  group_by(gefäßnummer, groeße, kreis, kultur_gr) |>
  count(gefäßnummer, groeße, kreis, kultur_gr) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = kreis,
               fill = groeße),
           position = "fill", stat = "identity") +
  scale_x_discrete(limits = rev(levels(verzierungen$kreis))) +  # Reihenfolge x-Achse wieder drehen so dass Harz und Leipzig oben
  geom_text(data = gef_per_kr , aes(x = kreis, 
                                    y = 1, 
                                    label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9) )+    
  labs(x = "Kreis",
       y = "Prozent",
       title = "Größe der Stichelemente nach Kultur und Landkreis",
       fill = "",
       caption = paste0("n Gefäße = ", sum(gef_per_kr$count)) ) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 50, 100) )+
  scale_fill_viridis(discrete = TRUE,
                     direction = 1)+
  theme_bw(base_size = 11)+
  coord_flip()+
  facet_grid(. ~ kultur_gr,
             scales = "free")+
  theme(legend.position = "bottom")

ggsave("./analysis/figures/Muster/2024-10-23_Größe_Stiche_Kreis-Kultur2.png", dpi = 300, height = 15, width = 16, units = "cm")



### Tiefe der Stiche


gef_per_kr <- verzierungen |> 
  filter(technik_gr != "NA") |>
  filter(!is.na(ausfuehrung)) |>
  group_by(kreis, kultur_gr) |>
  summarise(count = n_distinct(gefäßnummer))

verzierungen |> 
  filter(ausfuehrung != "NA") |>
  filter(!is.na(ausfuehrung)) |>
  group_by(gefäßnummer, technik_gr, ausfuehrung, kreis, kultur_gr) |>
  count(gefäßnummer, technik_gr, ausfuehrung, kreis, kultur_gr) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = kreis,
               fill = ausfuehrung),
           position = "fill", stat = "identity") +
  scale_x_discrete(limits = rev(levels(verzierungen$kreis))) +  # Reihenfolge x-Achse wieder drehen so dass Harz und Leipzig oben
  geom_text(data = gef_per_kr , aes(x = kreis, 
                                    y = 1, 
                                    label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9) )+    
  labs(x = "Kreis",
       y = "Prozent",
       title = "Ausführung der Stichelemente nach Kultur und Kreis",
       fill = "",
       caption = paste0("n Gefäße = ", sum(gef_per_kr$count)) ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = -1)+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))+
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom")+
  coord_flip()+
  facet_grid(. ~ kultur_gr,
             scales = "free")


ggsave("./analysis/figures/Muster/2024-10-23_Tiefe_Stiche_Kreis-Kultur.png", dpi = 300, height = 15, width = 16, unit = "cm")



