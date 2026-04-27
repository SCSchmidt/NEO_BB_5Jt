## Verzierungsanalyse: Verzierungselemente

library(stringr)
library(dplyr)
library(ggh4x)
library(ggplot2)

load(file = "./analysis/data/derived_data/verzierungen.RData")

gef_verz <- verzierungen |> 
  select(gefäßnummer, kultur_gr, kreislabel, fundort) |>
  group_by(kultur_gr, kreislabel, fundort) |>
  mutate(count = n_distinct(gefäßnummer)) |>
  ungroup() |>
  select(-gefäßnummer) |>
  unique()

## 1. Furchenstich
## 2. Reihe Einzelstiche
## 3. Reihe Doppelstiche
## 3. Mehrfachstiche
## 4. Sonderformen

### Furchenstiche

verzierungen$element <- tolower(verzierungen$element)



verzierungen |>
  filter(str_detect(element, "furchenstich") ) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

fo_per_kr <- verzierungen |>
  filter(str_detect(element, "furchenstich") ) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen |>
  filter(str_detect(element, "furchenstich") ) |>
  group_by(kreislabel, fundort, kultur_gr) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))



verzierungen |>
  filter(str_detect(element, "furchenstich") ) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, element) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, element) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fundort,
               fill = element,
               y = prop),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort, 
                y = 1, 
                label  = count ), 
            size = 4,
            hjust = 0.5, 
            angle = 0, 
            colour = "black",
            position = position_fill(vjust = .9)  )  +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  labs(x = "Fundorte",
       y = "Prozent d. mit Furchenstich verzierten Gefäße",
       fill = "",
       caption = paste0(sum(gef_per_fo$count), " von ", verzierungen |> summarize(count = n_distinct(gefäßnummer)), " verzierten Gefäßen" )) +
  theme_bw()+
  coord_flip()+
  scale_fill_manual(values = c("beige", "aquamarine4", "darkblue", "grey", "black"),
                    labels = c("Durchgezogene Furchenstichreihe",
                               "Furchenstichreihe",
                               "kurze Furchenstichreihe",
                               "Lockere Furchenstichreihe",
                               "Unterbrochene Furchenstichreihe")) + 
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size = 14))+
  guides(fill = guide_legend(nrow = 2))


ggsave("./analysis/figures/Muster/2024-10-23_Furchenstiche-Kultur2.png", dpi = 300, height = 10, width = 12)



# Einzelstichreihen

kult_per_kr <- verzierungen |>
  filter(!str_detect(element, "furchenstich") ) |>
  filter(str_detect(element, "einzelstiche") ) |>
  filter(!str_detect(muster, "randstiche")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- verzierungen |>
  filter(!str_detect(element, "furchenstich") ) |>
  filter(str_detect(element, "einzelstiche") ) |>
  filter(!str_detect(muster, "randstiche")) |>
  group_by(kreislabel, fundort, kultur_gr) |> summarize(count = n_distinct(gefäßnummer))|>
  ungroup()


vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  group_by(kultur_gr, fundort, kreislabel) |>
  mutate(count = paste0(count.x, "/", count.y)) |>
  group_by(kultur_gr, fundort, kreislabel) |>
  ungroup()


library(stringr)

verzierungen |>
  filter(!str_detect(element, "furchenstich") ) |>
  filter(str_detect(element, "einzelstiche") ) |>
  filter(!str_detect(muster, "randstiche")) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, element) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, element) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fundort,
               fill = element,
               y = prop),
           position = "fill", stat = "identity")+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort, 
                y = 1, 
                label  = count ), 
            size = 4,
            hjust = 0.5, 
            angle = 0, 
            colour = "black",
            position = position_fill(vjust = .9)  )  +
  labs(x = "Fundorte",
       y = "Prozent der mit Einzelstichen verzierten Gefäße",
       fill = "",
       caption = paste0(sum(gef_per_fo$count), " von ", verzierungen |> summarize(count = n_distinct(gefäßnummer)), " verzierten Gefäßen" )) +
  scale_fill_manual(labels= c("Dreieck aus drei Einzelstichen",
                              "Gebogene Reihe Einzelstiche",
                              "Gefülltes Dreieck aus Einzelstichen",
                              "Kurze Reihe Einzelstiche hintereinander",
                              "Kurze Reihe Einzelstiche parallel",
                              "Kurze Reihe schräg gesetzte Einzelstiche",
                              "Reihe Einzelstiche hintereinander",
                              "Reihe Einzelstiche parallel"                               ),
                    values = c("brown", "orange", "yellow", "beige","grey", "darkblue","blue", "lightblue", "black") )+
  theme_bw()+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = kult_per_kr$count)+
  theme(text = element_text(size = 18),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "cm"))+
  ggtitle("Verteilung des Elements Reihe Einzelstiche")+
  guides(fill = guide_legend(nrow = 3))

ggsave("./analysis/figures/Muster/2024-10-23_Einzelstiche-Kultur-Kreis2.png", dpi = 300, height = 15, width = 15)


### Doppelstichreihen



kult_per_kr <- verzierungen |>
  filter(!str_detect(element, "furchenstich") ) |>
  filter(!str_detect(element, "einzelstiche") ) |>
  filter(str_detect(element, "doppel") ) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen |>
  filter(!str_detect(element, "furchenstich") ) |>
  filter(!str_detect(element, "einzelstiche") ) |>
  filter(str_detect(element, "doppel") ) |>
  group_by(fundort, kreislabel, kultur_gr) |> summarize(count = n_distinct(gefäßnummer))



vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y)) |>
  ungroup()



verzierungen |>
  filter(!str_detect(element, "furchenstich") ) |>
  filter(!str_detect(element, "einzelstiche") ) |>
  filter(str_detect(element, "doppel") ) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, element) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, element) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fundort,
               fill = element,
               y = prop),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort, 
                y = 1, 
                label  = count ), 
            size = 4,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  labs(title = "Verteilung des Elements Reihe Doppelstiche",
       x = "Fundorte",
       y = "Prozent d. mit Doppelstich verzierten Gefäße",
       fill = "",
       caption = paste0(sum(gef_per_fo$count), " von ", verzierungen |> summarize(count = n_distinct(gefäßnummer)), " verzierten Gefäßen" )) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  scale_fill_manual(breaks = c("reihe versetzter doppelstiche",
                               "unterbrochene reihe versetzter doppelstiche" ,
                               "kurze reihe versetzter doppelstiche",
                               "gebogene reihe versetzter doppelstiche",
                               "reihe einzelner doppelstiche",
                               "reihe doppelstiche parallel",
                               "reihe einzelner doppelstiche schräg",
                               "kurze reihe einzelner doppelstiche schräg",
                               "kurze reihe einzelner doppelstiche parallel" ),
                    labels = c("Reihe versetzter Doppelstiche",
                               "Unterbrochene Reihe versetzter Doppelstiche",
                               "Kurze Reihe versetzter Doppelstiche",
                               "Gebogene Reihe versetzter Doppelstiche",
                               "Reihe einzelner Doppelstiche",
                               "Reihe einzelner Doppelstiche parallel",
                               "Reihe einzelner Doppelstiche schräg",
                               "Kurze Reihe einzelner Doppelstiche \n schräg",
                               "Kurze Reihe einzelner Doppelstiche \n parallel"),
                    values =c("#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                              "#44AA99", "#999933", "#882255", "#661100", "#6699CC") )+
  theme_bw()+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = kult_per_kr$count)+
  theme(text = element_text(size = 18),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "cm"))+
  guides(fill = guide_legend(nrow = 3))


ggsave("./analysis/figures/Muster/2024-10-23_2-Stiche-Kultur-Kreis2.png", dpi = 300, height = 18, width = 16)



#### MEhrfachstiche



kult_per_kr <- verzierungen |>
  filter(str_detect(element, "drei|vier|fünf") ) |>
  filter(!str_detect(element, "einzelstich") ) |>
  group_by(kreis) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen |>
  filter(str_detect(element, "drei|vier|fünf") ) |>
  filter(!str_detect(element, "einzelstich") ) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))


vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y)) |>
  ungroup()


library(stringr)

library(scales)

# Verschönerungsfunktionen, um die Brüche im Plot aufzuhübschen./analysis. 

mylimits <- function(x) range(scales::breaks_extended()(x))

mybreaks <- function(n = 3) {
  function(x) {
    breaks <- mylimits(x)
    seq(breaks[1], breaks[2], length.out = n)  
  }
}


verzierungen |>
  filter(str_detect(element, "drei|vier|fünf") ) |>
  filter(!str_detect(element, "einzelstich") ) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, element) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, element) |>          # now required with changes to dplyr::count()  
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fundort,
               fill = element,
               y = prop),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort, 
                y = 1, 
                label  = count ), 
            size = 4,
            hjust = 0.5, 
            angle = 0, 
            colour = "grey30",
            position = position_fill(vjust = .9)  )  +
  labs(x = "Fundorte",
       y = "Prozent der mit Mehrfachstich verzierten Gefäße",
       fill = "",
       title = "Verteilung des Elements Reihe mit 3-5-fachem Stich",
       caption = paste0(sum(gef_per_fo$count), " von ", verzierungen |> summarize(count = n_distinct(gefäßnummer)), " verzierten Gefäßen" )) +
  scale_fill_manual(breaks = c("reihe dreifachstiche",
                               "reihe dreifachstiche, mittlerer stich einzeln",
                               "reihe dreifachstiche, mittelfurche mit absätzen",
                               "reihe dreifachstiche, mittelfurche durchgezogen",
                               "kurze reihe dreifachstiche, mittlerer stich einzeln",
                               "reihe vierfachstiche, alle stiche einzeln",
                               "gebogene reihe vierfachstiche, alle stiche einzeln",
                               "reihe vierfachstiche, eine mittelfurche mit absätzen",
                               "reihe fünfzinkiger stiche"),
                    labels = c("Reihe Dreifachstiche",
                               "Reihe Dreifachstiche, mittlerer Stich einzeln",
                               "Reihe Dreifachstiche, Mittelfurche mit Absätzen",
                               "Reihe Dreifachstiche, Mittelfurche durchgezogen",
                               "kurze Reihe Dreifachstiche, mittlerer Stich einzeln",
                               "Reihe Vierfachstiche, alle Stiche einzeln",
                               "Gebogene Reihe Vierfachstiche, alle Stiche einzeln",
                               "Reihe Vierfachstiche, eine Mittelfurche mit Absätzen",
                               "Reihe Fünffachstiche"
                    ),
                    values = c("brown", "orange", "yellow", "beige", "darkblue","blue", "lightblue","grey", "black") )+
  theme_bw()+
  #scale_y_continuous(labels = scales::label_number(accuracy = 1))+
  # scale_y_continuous(breaks = mybreaks(n = 3), limits = mylimits,
  #                   labels = scales::label_number(accuracy = 2) ) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = kult_per_kr$count,
                   cols = c(4,18))+
  theme(text = element_text(size = 15),
        legend.key.size = unit(0.25, "cm"))+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))+ 
  guides(fill = guide_legend(nrow = 4))



ggsave("./analysis/figures/Muster/2024-10-23_Mehrfachstiche-Kultur-Kreis2.png", dpi = 300, height = 10, width = 15)


#### Sonderformen



safe_colorblind_palette16 <- c("grey", "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288","blue",
                               "#44AA99", "#999933", "grey50" , "#882255", "#661100", "#6699CC",  "#AA4499", "black","green") 

kult_per_kr <- verzierungen |>
  filter(str_detect(element, "schnitt|schraffur|linie|geritzt|kreuz aus|dreieck aus|inkrust|einzelelement|delle|leiste|meißel") ) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))



gef_per_fo <- verzierungen |>
  filter(str_detect(element, "schnitt|schraffur|linie|geritzt|kreuz aus|dreieck aus|inkrust|einzelelement|delle|leiste|meißel") ) |>
  group_by(fundort, kreislabel, kultur_gr) |> summarize(count = n_distinct(gefäßnummer))


vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y)) |>
  ungroup()

sonderelemente <- verzierungen |>
  filter(str_detect(element, "schnitt|schraffur|linie|geritzt|kreuz aus|dreieck aus|inkrust|einzelelement|delle|leiste|meißel") ) |>    
  select(element) |>
  unique()

library(stringr)

verzierungen |>
  filter(str_detect(element, "schnitt|schraffur|linie|geritzt|kreuz aus|dreieck aus|inkrust|einzelelement|delle|leiste|meißel") ) |>    
  count(gefäßnummer, kreislabel, fundort, kultur_gr, element) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, element) |>          # now required with changes to dplyr::count()
  # mutate(n = ifelse(n > 0, 1, 0) ) |> # nicht nötig
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fundort,
               fill = element,
               y = prop),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort, 
                y = 1, 
                label  = count ), 
            size = 4,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  scale_fill_manual(breaks = sonderelemente$element,
                    labels = str_to_title(sonderelemente$element),
                    values = safe_colorblind_palette16) +
  labs(x = "Fundorte",
       y = "Prozente d. Gefäße mit selten vorkommenden Verzierungselementen",
       fill = "",
       title = "Verteilung von seltenen Elementen",
       caption = paste0(sum(gef_per_fo$count), " von ", verzierungen |> summarize(count = n_distinct(gefäßnummer)), " verzierten Gefäßen" )) +
  theme_bw()+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = kult_per_kr$count)+
  theme(text = element_text(size = 15),
        legend.key.size = unit(0.25, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 14))


ggsave("./analysis/figures/Muster/2024-10-23_Sonderelemente-Kultur-Kreis2.png", dpi = 300, height = 15, width = 15)