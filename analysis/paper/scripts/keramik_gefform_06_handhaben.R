#### Analyse der Handhaben

library(dplyr)
library(ggplot2)
library(viridis)
library(ggh4x)

## Daten einladen

hh <- read.csv2("./analysis/data/raw_data/handhaben.csv", sep = "|")
gef_umbruch <- load("./analysis/data/derived_data/gef_umbruch.RData")

source("./R_functions/farbzuweisungen.R")

gef_umbruch <- gef_umbruch2


handhaben <- left_join(hh, gef_umbruch, by = "gefäßnummer")
gef_hh <- right_join(hh, gef_umbruch, by = "gefäßnummer")


## Handhabentypen nach Kultur und Kreis


gef_per_ra <- gef_hh |>  
  group_by(kreislabel, kultur_gr, fundort.y) |>
  summarize(count = n_distinct(gefäßnummer))


fo_per_kr <- gef_hh |>  
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort.y))

gef_hh |>
  count(gefäßnummer, kreislabel, kultur_gr, fundort.y, Handhabentyp) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, fundort.y, Handhabentyp) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort.y,
               fill = Handhabentyp),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra,
            aes(x = fundort.y,
                y = 1, 
                label  = count ), 
            size = 2.5,
            hjust = 1, 
            angle = 0, 
            colour = "black",
            # position = position_fill(vjust = .9)  
  )  +
  labs(title = "Plastische Verzierungen",
       caption = paste0("n = ", sum(gef_per_ra$count) ),
       x = "Fundort",
       y = "Prozent",
       fill = "")+
  scale_fill_viridis(discrete = TRUE,
                     direction = -1,
                     na.value = "grey",
                     labels = function(breaks) {breaks[is.na(breaks)] <- "keine"; breaks},)+
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c(0,50,100))+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 11),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "cm") )+
  guides(fill = guide_legend(nrow = 1) ) +
  theme(strip.text.y = element_text(angle = 0)) # drehen der Facettierungsbeschriftung

ggsave("./analysis/figures/Gef_Form/2024-10-25_Handhaben_Kultur_Kreis.png", dpi = 300, width = 16, height = 24, units = "cm")



## Knubbenformen



gef_per_ra <- gef_hh |>  
  filter(Handhabentyp == "Knubbe")|>
  group_by(kreislabel, kultur_gr, fundort.y) |>
  summarize(count = n_distinct(gefäßnummer))


fo_per_kr <- gef_hh |>  
  filter(Handhabentyp == "Knubbe")|>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort.y))

gef_hh <- gef_hh |>
  mutate(form_loch = paste(Form_grundflaeche, gelocht)) |>
  mutate(form_loch = case_when(
    form_loch ==  "rund NA" ~ "rund evtl.",
    form_loch == "rund nein" ~ "rund",
    form_loch ==  "oval nein" ~ "oval",
    form_loch ==  "oval NA" ~ "oval evtl.",
    form_loch ==  "oval quer große Öffnung" ~ "oval quergelocht",
    form_loch ==   "Eckig-oval nein" ~  "Eckig-oval",
    form_loch == "Leiste mit Anhang nein" ~ "Leiste mit Anhang" ,
    form_loch ==  "Leiste ohne Anhang nein"  ~  "Leiste ohne Anhang",  
    form_loch ==  "oval senkrechte gelocht" ~ "oval senkrecht gelocht",
    form_loch == "flache Kannelur nein" ~ "flache Kannelur" ,
    form_loch == "NA NA" ~ NA,
    TRUE ~ form_loch )) |>
  mutate(form_loch = factor(form_loch, levels = c("rund", "rund evtl.", "oval", "oval evtl.", "Eckig-oval", "Leiste mit Anhang" , "Leiste ohne Anhang" , "oval quergelocht", "oval senkrecht gelocht"))) 


gef_hh |>
  filter(Handhabentyp == "Knubbe")|>
  count(gefäßnummer, kreislabel, kultur_gr, fundort.y, form_loch) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, fundort.y, form_loch) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort.y,
               fill = form_loch),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra,
            aes(x = fundort.y, y = 1, 
                label  = count ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "grey10",
            position = position_fill(vjust = .9)  )  +
  labs(title = "Knubben: Form und Ösenrichtung",
       caption = paste0("n = ", sum(gef_per_ra$count) ),
       x = "Fundort",
       y = "Anteil",
       fill = "")+
  scale_fill_viridis(discrete = TRUE,
                     direction = -1,
                     na.value = "grey" )+
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c(0,0.5,1) )+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom"         )+
  guides(fill = guide_legend(nrow = 1) )+
  theme(strip.text.y = element_text(angle = 0),
        legend.key.size = unit(0.25, "cm")) 

ggsave("./analysis/figures/Gef_Form/Knubben_Form_Lochung.png", dpi = 300, width = 21, height = 16, units = "cm")


### Ausrichtung zum Rand


gef_hh$ausr.zu.rand <- as.numeric(gef_hh$ausr.zu.rand)


gef_hh |>
  # filter(!is.na(ausr.zu.rand)) |>
  filter(kultur2 != "FBG") |>
  ggplot()+
  geom_boxplot(aes(y = ausr.zu.rand,
                   x = kultur2,
                   fill = kultur2))+
  geom_jitter(aes(y = ausr.zu.rand,
                  x = kultur2), 
              colour = "grey")  +
  scale_fill_manual(breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK"),
                    values = col[names(col) != "FBG"] )+
  labs(title = "Ausrichtung der Handhaben zum Rand",
       caption = paste0("n = ", sum(gef_per_ra$count) ),
       x = "Kulturgruppe",
       y = "Grad zum Rand",
       fill = "")+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 12),
        legend.position = "bottom"         ) #+
#  guides(fill = guide_legend(nrow = 1) )


ggsave("./analysis/figures/Gef_Form/Ausrichtung_Handhaben.png", dpi = 300, width = 16, height = 10, unit = "cm")

### Lage der Knubben am Gefäßkörper



gef_per_ra <- handhaben |>  
  filter(Handhabentyp != "flache Kannelur") |>
  group_by(kreislabel, kultur_gr, fundort.y) |>
  summarize(count = n_distinct(gefäßnummer))

fo_per_kr <- handhaben |>  
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort.y))

handhaben |>
  filter(Handhabentyp != "flache Kannelur") |>
  count(gefäßnummer, kreislabel, kultur_gr, fundort.y, ort_k, Handhabentyp) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, fundort.y, ort_k, Handhabentyp) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort.y,
               fill = ort_k,
               # col = Handhabentyp
  ),
  position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra,
            aes(x = fundort.y, y = 1, 
                label  = count ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  labs(title = "Ort der Handhaben am Gefäßkörper",
       caption = paste0("n = ", sum(gef_per_ra$count) ),
       x = "",
       y = "Prozent",
       fill = "")+
  scale_fill_manual(values = c(rev(viridis(11)), "grey"),
                    breaks = c("oben auf dem rand", 
                               "randständig",
                               "unter dem rand, hals",
                               "hals",
                               "hals-schulter-umbruch",
                               "hals und schulter",
                               "hals, schulter, bauch",
                               "schulter",
                               "schulter oder bauch",
                               "schulter-bauch-umbruch"),
                    labels = c("oben auf dem Rand", 
                               "randständig",
                               "unter dem Rand, Hals",
                               "Hals",
                               "Hals-Schulter-Umbruch",
                               "Hals und Schulter",
                               "Hals, Schulter, Bauch",
                               "Schulter",
                               "Schulter oder Bauch",
                               "Schulter-Bauch-Umbr."))+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 50, 100))+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom"         ) +
  guides(fill = guide_legend(nrow = 3) )+
  theme(strip.text.y = element_text(angle = 0),  # drehen der Facettierungsbeschriftung
        legend.key.size = unit(0.25, "cm"))

ggsave("./analysis/figures/Gef_Form/Handhaben_Ort.png", dpi = 300, width = 18, height = 21, units = "cm")


## Handhabengrößen

handhaben$länge_mm <- as.numeric(handhaben$länge_mm)
handhaben$tiefe_mm <- as.numeric(handhaben$tiefe_mm)



safe_colorblind_palette13 <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888","black")

gef_per_ra <- handhaben |>  
  summarize(count = n_distinct(gefäßnummer))

handhaben |>
  filter(Handhabentyp != "flache Kannelur") |>
  count(gefäßnummer, kreislabel, kultur2, fundort.y, länge_mm, tiefe_mm, Handhabentyp) |>
  group_by(gefäßnummer, kreislabel, kultur2, fundort.y,  länge_mm, tiefe_mm, Handhabentyp) |>
  ggplot()+
  geom_point(aes(x = tiefe_mm,
                 y = länge_mm,
                 col = kultur2,
                 shape = Handhabentyp),
             size = 2)+
  scale_color_manual(breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                     values = col[names(col) != "FBG"]  )+
  labs(title = "Größe der Handhaben",
       caption = paste0("n = ", sum(gef_per_ra$count) ),
       x = "Tiefe der Handhaben (mm)",
       y = "Länge der Handhaben (mm)",
       col = "",
       shape = "")+
  geom_vline(xintercept = 21, linetype="dotted", 
             color = "grey", size=1)+
  theme_bw()+
  theme(text = element_text(size = 15),
        legend.position = "bottom"         ) #+


ggsave("./analysis/figures/Gef_Form/Größe_Handhaben.png", dpi = 300, width = 10, height = 8, unit = "in")


##### Länge zu Tiefe


gef_hh |>
  filter(!is.na(länge_zu_tiefe)) |>
  ggplot()+
  geom_boxplot(aes(y = log(länge_zu_tiefe),
                   x = kultur2,
                   fill = kultur2))+
  geom_jitter(aes(y =  log(länge_zu_tiefe),
                  x = kultur2), 
              colour = "grey")  +
  scale_fill_manual(breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                    values = col[names(col) != "FBG"] )+
  labs(title = "Länge zu Tiefe der Handhaben",
       caption = paste0("n = ", sum(gef_per_ra$count) ),
       x = "Kulturgruppe",
       y = "log(Länge zu Tiefe)",
       fill = "")+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 15),
        legend.position = "bottom"     )


ggsave("./analysis/figures/Gef_Form/Länge_Tiefe_Handhaben.png", dpi = 300, width = 17, height = 12, unit = "cm")


### Knubbentypologie von Czerniak erweitert


library(stringr)
handhaben <- handhaben |>
  mutate(sys_gr = case_when(
    str_detect(Sys_Czerniak1980_Abb7, "1a") ~ "1a",
    str_detect(Sys_Czerniak1980_Abb7, "L2a") ~ "L2a",
    str_detect(Sys_Czerniak1980_Abb7, "L2b") ~ "L2b",
    str_detect(Sys_Czerniak1980_Abb7, "L1b") ~ "L1b",
    str_detect(Sys_Czerniak1980_Abb7, "1b") ~ "1b",
    str_detect(Sys_Czerniak1980_Abb7, "1n") ~ "1n",
    str_detect(Sys_Czerniak1980_Abb7, "1s") ~ "1s",
    str_detect(Sys_Czerniak1980_Abb7, "3p") ~ "3p",
    Sys_Czerniak1980_Abb7 == "2b3" ~ "3p", # wegen der Lochung
    str_detect(Sys_Czerniak1980_Abb7, "2b") ~ "2b",
    str_detect(Sys_Czerniak1980_Abb7, "3n") ~ "3n",
    str_detect(Sys_Czerniak1980_Abb7, "4n") ~ "4n",
    str_detect(Sys_Czerniak1980_Abb7, "7n") ~ "9n",
    str_detect(Sys_Czerniak1980_Abb7, "10a") ~ "1a",
    Sys_Czerniak1980_Abb7 == "7a2" ~ "3a2", # wegen dr Doppling
    str_detect(Sys_Czerniak1980_Abb7, "7a") ~ "7a",
    str_detect(Sys_Czerniak1980_Abb7, "1c") ~ "1c",
    str_detect(Sys_Czerniak1980_Abb7, "9n") ~ "9n",
    str_detect(Sys_Czerniak1980_Abb7, "7p") ~ "2cr",
    str_detect(Sys_Czerniak1980_Abb7, "2cr") ~ "2cr",
    str_detect(Sys_Czerniak1980_Abb7, "3cr") ~ "2cr",
    str_detect(Sys_Czerniak1980_Abb7, "4cr") ~ "2cr",
    str_detect(Sys_Czerniak1980_Abb7, "8b") ~ "8b",
    str_detect(Sys_Czerniak1980_Abb7, "8n") ~ "8n",
    str_detect(Sys_Czerniak1980_Abb7, "1d") ~ "2d",
    str_detect(Sys_Czerniak1980_Abb7, "4p") ~ "1p",
    str_detect(Sys_Czerniak1980_Abb7, "8a") ~ "1a",
    str_detect(Sys_Czerniak1980_Abb7, "9b") ~ "1p",
    str_detect(Sys_Czerniak1980_Abb7, "9a") ~ "3a2",
    str_detect(Sys_Czerniak1980_Abb7, "10a") ~ "7a",
    Sys_Czerniak1980_Abb7 == "11a" ~ "7a",
    str_detect(Sys_Czerniak1980_Abb7, "9b") ~ "2b",
    str_detect(Sys_Czerniak1980_Abb7, "10b") ~ "1a",
    str_detect(Sys_Czerniak1980_Abb7, "5c") ~ "5c",
    str_detect(Sys_Czerniak1980_Abb7, "6n") ~ "5n",
    str_detect(Sys_Czerniak1980_Abb7, "10n") ~ "10n",
    str_detect(Sys_Czerniak1980_Abb7, "4a2") ~ "1s",
    str_detect(Sys_Czerniak1980_Abb7, "6b") ~ "6b",
    str_detect(Sys_Czerniak1980_Abb7, "4b") ~ "4b",
    str_detect(Sys_Czerniak1980_Abb7, "5s") ~ "5s",
    str_detect(Sys_Czerniak1980_Abb7, "gebrochen") ~ NA,
    TRUE ~ Sys_Czerniak1980_Abb7
  ) )

save(handhaben, file = "./analysis/data/derived_data/handhaben.RData")

load(file = "./analysis/data/derived_data/handhaben.RData")


library(forcats)

gef_per_ra <- handhaben |>  
  filter(sys_gr != "gebrochen") |>
  filter(Handhabentyp != "flache Kannelur") |>
  group_by(sys_gr, Handhabentyp, kultur2) |>
  summarize(count = n_distinct(gefäßnummer))

typ_anzahl <- gef_per_ra |>
  group_by(sys_gr) |>
  mutate(n = sum(count)) |>
  ungroup()

typ_mehr1 <- typ_anzahl |>
  filter(n > 1)

gef_per_ra <- gef_per_ra |>
  filter(sys_gr %in% typ_mehr1$sys_gr)

fo_per_kr <- handhaben |>  
  filter(sys_gr != "gebrochen") |>
  filter(sys_gr %in% typ_mehr1$sys_gr) |>
  filter(Handhabentyp != "flache Kannelur") |>
  group_by(Handhabentyp) |>
  summarize(count = n_distinct(sys_gr))

handhaben |>
  filter(Handhabentyp != "flache Kannelur") |>
  filter(sys_gr != "gebrochen") |>
  filter(sys_gr %in% typ_mehr1$sys_gr) |>
  count(gefäßnummer, kultur2, sys_gr, Handhabentyp) |>
  group_by(gefäßnummer, kultur2, sys_gr, Handhabentyp) |>
  #  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(x = fct_infreq(sys_gr),
               fill = kultur2 )
  ) +
  geom_text(data = gef_per_ra,
            aes(x = sys_gr, y = 1, 
                label  = count ), 
            size = 4,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9)  )  +
  scale_fill_manual(values = col)+
  ylim(0,8)+
  labs(title = "Handhabentypen (gruppierte Feintypologie)",
       caption = paste0("n = ", sum(gef_per_ra$count), ", gefiltert auf Typ mehr als 1x vorhanden" ),
       x = "",
       y = "Anzahl",
       fill = "")+
  coord_flip()+
  facet_grid(Handhabentyp ~ kultur2, scales = "free_y", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom" ,
        legend.key.size = unit(0.5, "cm")) 


ggsave("./analysis/figures/Gef_Form/Knubbentypen2.png", dpi = 300, width = 24, height = 42, units = "cm")

