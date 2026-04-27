### Keramik Machart: Bruchfarbe und -aufbau

## Daten laden und vorbereiten

load("./analysis/data/derived_data/scherben.RData")

library(viridis)
library(ggh4x)
library(dplyr)
library(stringr)
library(ggplot2)

scherben$bruchfarbe <- tolower(scherben$bruchfarbe)

# uneinheitlich aufgenommenes vereinheitlichen:
scherben_bf <- scherben |>
  filter(as.numeric(bruchpotential) < 7) |>
  filter(as.numeric(erh_ofl_k ) < 3) |>
  mutate(bruchfarbe_gr = case_when(
    str_detect(bruchfarbe, "\\/") ~ "gemischt",
    bruchfarbe == "dunkelgrau" ~ "dunkel",
    bruchfarbe == "gelb bis grau" ~ "hell-dunkel",
    bruchfarbe == "dunkelgrau bis rot" ~ "dunkel-hell",
    bruchfarbe == "grau" ~ "dunkel",
    bruchfarbe == "NA"  ~ NA,
    TRUE ~ bruchfarbe
  ))

scherben_bf <- scherben_bf |>
  select(gefäßnummer, kreislabel, kultur_gr, kultur2, fundort, bruchfarbe_gr) |>
  unique()

dubs <- scherben_bf$gefäßnummer[duplicated(scherben_bf$gefäßnummer)]

scherben_bf <- scherben_bf |>
  mutate(bruchfarbe_gr = case_when(
    gefäßnummer %in% dubs ~ "gemischt",
    TRUE ~ bruchfarbe_gr
  ) ) |>
  unique() |>
  mutate(bruchfarbe_gr = factor(bruchfarbe_gr, ordered = T,
                                levels = c("dunkel",
                                           "dunkel-hell",
                                           "dunkel-hell-dunkel",
                                           "dunkel-hell-dunkel-hell",
                                           "dunkel-hell-dunkel-hell-dunkel",
                                           "gemischt",
                                           "hell-dunkel-hell-dunkel",
                                           "hell-dunkel-hell",
                                           "hell-dunkel",
                                           "hell"))) 


## plot bruchfarbe

gef_per_ra <- scherben_bf |>  
  filter(!is.na(bruchfarbe_gr)) |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))


fo_per_kr <- scherben_bf |>  
  filter(!is.na(bruchfarbe_gr)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

scherben_bf |>
  filter(!is.na(bruchfarbe_gr)) |>
  count(gefäßnummer, kreislabel, kultur_gr, fundort, bruchfarbe_gr) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, fundort, bruchfarbe_gr) |>
  mutate(n = case_when( # wenn n vorher über 0 -> egal wie viele scherben pro gefäß -> 1
    n > 0 ~ 1,
    TRUE ~ 0
  )) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = bruchfarbe_gr ),
           position = "fill", stat = "identity",
           na.rm = TRUE)+
  geom_text(data = gef_per_ra,
            aes(x = fundort, y = 1, 
                label  = count ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  labs(title = "Bruchfarbe",
       caption = paste0("hell = oxidierend, dunkel = reduzierend gebrannt, \n Gefäße mit guter Erhaltung d. Oberfläche und Sichtbarkeit des Bruchs, n = ", sum(gef_per_ra$count) ),
       x = "",
       y = "Prozent",
       fill = "" ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = 1,
                     na.value = "grey")+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     label = c(0,50,100))+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 11),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm")) 

ggsave("./analysis/figures/Magerung/bruchfarbe.png", dpi = 300, width = 17, height = 23, units = "cm")

save(scherben_bf, file = "./analysis/data/derived_data/scherben_bf.RData")

### Bruchaufbau


scherben <- scherben |>
  mutate(bruchaufb_gr = case_when(
    bruchaufbau == "N/S" ~ "N/S",
    bruchaufbau == "S/N?" ~ "N/S?",
    bruchaufbau == "S/N ?" ~ "N/S?",
    bruchaufbau == "S/N" ~ "N/S",
    bruchaufbau == "NA" ~ NA,
    bruchaufbau == "" ~ NA,
    bruchaufbau == "nicht erkennbar" ~ NA,
    bruchaufbau == "„dachförmig“ (Rand noch im Nachgang aufgesetzt)" ~ "U",
    #  str_detect(bruchaufbau, "\\?") ~ NA, #hier könnte man auch nochmal differenzieren./analysis. aber wird vmtl kein anderes Bild geben
    TRUE ~ bruchaufbau
  ))

gef_per_ra <- scherben |>  
  filter(!is.na(bruchaufb_gr)) |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))


fo_per_kr <- scherben |>  
  filter(!is.na(bruchaufb_gr)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))


scherben |> 
  filter(!is.na(bruchaufb_gr)) |>
  count(gefäßnummer, kreislabel, kultur_gr, fundort, bruchaufb_gr) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, fundort, bruchaufb_gr) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = bruchaufb_gr ),
           position = "fill", stat = "identity",
           na.rm = TRUE)+
  geom_text(data = gef_per_ra,
            aes(x = fundort, y = 1, 
                label  = count ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  labs(title = "Aufbautechnik der Gefäße",
       caption = paste0( "n Gefäße mit erkennbarem Aufbau = ", sum(gef_per_ra$count) ),
       x = "Fundort",
       y = "Anteil",
       fill = "")+
  scale_fill_manual(breaks = c("N", "N?", "N/S?", "N/S", "S?", "S", "U"),
                    values = c("black", "darkgrey",  "#F5C710", "#DF536B",  "#28E2E5", "#2297E6", "darkgreen"),
                    na.value = "white")+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 0.5, 1))+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom"  ) +
  guides(fill = guide_legend(nrow = 1))


ggsave("./analysis/figures/Magerung/bruchaufbau_2024-01-10.png", dpi = 300, width = 16, height = 21, unit = "cm")
