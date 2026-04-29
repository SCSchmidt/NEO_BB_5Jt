### Gefäßformanalyse: Symmetrien

## 1. Halssymmetrie
## 2. Schulter
## 3. Bauch
## 4. Schärfe der Umbrüche

## 5. Offene vs geschlossene Formen



load("./analysis/data/derived_data/gef_umbruch.RData")

gef_umbruch <- gef_umbruch2


library(viridis)
library(ggplot2)
library(dplyr)
library(ggh4x)


### Hals


fo_per_kr <- gef_umbruch |>    filter(!is.na(randform_nonek)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_umbruch |>    filter(!is.na(randform_nonek)) |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch |>
  filter(!is.na(randform_nonek),
         randform_nonek != "") |>
  count(gefäßnummer, kreislabel, kultur_gr, randform_nonek, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, randform_nonek, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = randform_nonek),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra , aes(x = fundort, y = 1, 
                                    label  = paste0(count)  ), 
            size = 3,
            hjust = 1, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9) )  +
  labs(title = "Richtungswechsel Hals",
       x = "",
       y = "Prozent",
       fill = "",
       caption = paste0( "erhaltene Ränder und Schultern, n = ", nrow(unique(gef_umbruch |>  filter(!is.na(randform_nonek),  randform_nonek != "") |> select(gefäßnummer)))) ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = 1,
                     na.value = "grey")+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  scale_y_continuous( labels = c(0,50,100),
                      breaks = c(0, 0.5, 1) )+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.key.size = unit(0.25, "cm"))+
  guides(fill = guide_legend(nrow = 2) )+
  theme(strip.text.y = element_text(angle = 0) )  # drehen der Facettierungsbeschriftung


ggsave("./analysis/figures/Gef_Form/2025-09-25_Richtungswechsel_Hals.png", dpi = 300, width = 18, height = 27, units = "cm")



#### Schulter


fo_per_kr <- gef_umbruch |>    filter(!is.na(schulterform_nonek), schulterform_nonek != "") |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_umbruch |>    filter(!is.na(schulterform_nonek), schulterform_nonek != "") |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch |>
  filter(!is.na(schulterform_nonek),
         schulterform_nonek != "") |>
  count(gefäßnummer, kreislabel, kultur_gr, schulterform_nonek, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, schulterform_nonek, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = schulterform_nonek),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra , aes(x = fundort, y = 1, 
                                    label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9))  +
  labs(title = "Richtungswechsel Schulter",
       x = "Fundort",
       y = "Prozent",
       fill = "",
       caption = paste0( "erhaltene Schultern, n = ", nrow(unique(gef_umbruch |>  filter(!is.na(schulterform_nonek), schulterform_nonek != "") |> select(gefäßnummer)))) ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = 1,
                     na.value = "grey")+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  scale_y_continuous( labels = c(0,50,100),
                      breaks = c(0, 0.5, 1) )+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.key.size = unit(0.25, "cm"))+
  guides(fill = guide_legend(nrow = 2) )+
  theme(strip.text.y = element_text(angle = 0) ) 


ggsave("./analysis/figures/Gef_Form/2025-09-25_Richtungswechsel_schulter.png", dpi = 300, width = 20, height = 24, units = "cm")



#### Bauch



fo_per_kr <- gef_umbruch |>    filter(!is.na(bauchform_nonek), bauchform_nonek != "") |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_umbruch |>    filter(!is.na(bauchform_nonek), bauchform_nonek != "") |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch |>
  filter(!is.na(bauchform_nonek), bauchform_nonek != "") |>
  count(gefäßnummer, kreislabel, kultur_gr, bauchform_nonek, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, bauchform_nonek, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = bauchform_nonek),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra , aes(x = fundort, y = 1, 
                                    label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            position = position_fill(vjust = .9),
            col = "white" )  +
  labs(title = "Richtungswechsel Bauch",
       x = "",
       y = "Anteil",
       fill = "",
       caption = paste0( "erhaltene Bäuche, n = ", nrow(unique(gef_umbruch |>  filter(!is.na(bauchform_nonek), bauchform_nonek != "") |> select(gefäßnummer)))) ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = 1,
                     na.value = "grey")+
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c(0,0.5,1))+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        legend.key.size = unit(0.25, "cm"))+
  guides(fill = guide_legend(nrow = 1) )+
  theme(strip.text.y = element_text(angle = 0) ) 


ggsave("./analysis/figures/Gef_Form/2025-09-25_Richtungswechsel_bauch.png", dpi = 300, width = 16, height = 21, units = "cm")



### Umbruchsschärfe



gef_umbruch$Hals.schulter.umbruch <- factor(gef_umbruch$Hals.schulter.umbruch, ordered = TRUE,
                                            levels = c("keine Wölbung","schwache Wölbung" ,"mittlere Wölbung", "starke Wölbung" , "scharfer Umbruch"  ))

fo_per_kr <- gef_umbruch |>    filter(!is.na(Hals.schulter.umbruch)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_umbruch |>    filter(!is.na(Hals.schulter.umbruch)) |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))


gef_umbruch |>
  filter(!is.na(Hals.schulter.umbruch)) |>
  count(gefäßnummer, kreislabel, kultur_gr, Hals.schulter.umbruch, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, Hals.schulter.umbruch, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = Hals.schulter.umbruch),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra , aes(x = fundort, y = 1, 
                                    label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9))  +
  labs(title = "Schärfe des Hals-Schulter-Umbruchs",
       x = "",
       y = "Prozent",
       fill = "",
       caption = paste0( "erhaltene Hals und Schulter, n = ", sum(gef_per_ra$count)) ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = -1,
                     na.value = "grey")+
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c(0,50,100)) +
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1 ))+
  theme(strip.text.y = element_text( size = 10),
        legend.key.size = unit(0.25, "cm"))+
  theme(strip.text.y = element_text(angle = 0) ) 


ggsave("./analysis/figures/Gef_Form/2024-10-25_Schärfe_Hals-Schulter.png", dpi = 300, width = 24, height = 16, units = "cm")


### Offen oder geschlossene Form

gef_off <- gef_umbruch |>
  mutate(off = case_when(
    verh_durchm_r_b >= 1 ~ "offene Form",
    verh_durchm_r_b < 1 ~ "geschlossene Form",
    verh_durchm_r_s > 1 ~ "Rand weiter als Schulter",
    verh_durchm_r_s < 1 ~ "geschlossene Form",
    randneigung == 90 ~ "gerader Hals",
    randneigung < 90 ~ "ausstehender Hals",
    randneigung > 90 ~ "geschlossene Form",
    TRUE ~ "unbekannt"
  )) 

gef_off$off <- factor(gef_off$off, ordered = TRUE,
                      levels = c("offene Form",  "Rand weiter als Schulter", "ausstehender Hals", "gerader Hals", "geschlossene Form", "unbekannt"))

fo_per_kr <- gef_off |>    filter(off != "unbekannt") |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_off |>    filter(off != "unbekannt") |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))

gef_off |>
  filter(off != "unbekannt") |>
  count(gefäßnummer, kreislabel, kultur_gr, off, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, off, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = off),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra,
            aes(x = fundort, y = 1, 
                label  = count ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  labs(title = "Offene zu geschlossene Formen",
       caption = paste0("n = ", sum(gef_per_ra$count), "\noffene Form: Rand weiter als Bauch \ngeschlossene Form: Rand enger als Schulter oder Bauch, oder einziehender Hals" ),
       x = "",
       y = "Anteil",
       fill = "")+
  scale_fill_viridis(discrete = TRUE,
                     direction = -1,
                     na.value = "grey") +
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c(0,0.5,1)) +
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 2 ))+
  theme(strip.text.y = element_text( size = 10),
        legend.key.size = unit(0.25, "cm"))+
  theme(strip.text.y = element_text(angle = 0) ) 

ggsave("./analysis/figures/Gef_Form/2024-10-25_Offenheit_der_Formen.png", dpi = 300, width = 18, height = 24, units = "cm")

