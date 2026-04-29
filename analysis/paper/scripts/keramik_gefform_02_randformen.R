#### Gefäßformanalyse Rand

## 1. Randform grob
## 2. Randform fein (Randlippe)

## 3. Randwinkel

load("./analysis/data/derived_data/gef_umbruch.RData")

gef_umbruch <- gef_umbruch2


library(viridis)
library(ggplot2)
library(dplyr)
library(ggh4x)


fo_per_kr <- gef_umbruch |>    filter(!is.na(randabschluss_gr)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_umbruch |>    filter(!is.na(randabschluss_gr)) |>
  group_by(fundort, kultur_gr, kreislabel) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch |>
  filter(!is.na(randabschluss_gr)) |>
  filter(randabschluss_gr != "") |>
  count(gefäßnummer, kreislabel, kultur_gr, randabschluss_gr, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, randabschluss_gr, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = randabschluss_gr),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra , aes(x = fundort, y = 1, 
                                    label  = paste0(count)  ), 
            size = 2,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9))  +
  labs(title = "Randausformungen",
       x = "",
       y = "Prozent",
       fill = " ",
       caption = paste0( "erhaltene Ränder, n = ", nrow(unique(gef_umbruch |>  filter(!is.na(randabschluss_gr)) |> select(gefäßnummer)))) ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = 1,
                     na.value = "grey")+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50, 100) )+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10))+
  guides(fill = guide_legend(nrow = 4) )+
  theme(strip.text.y = element_text(angle = 0),  # drehen der Facettierungsbeschriftung
        legend.key.size = unit(0.25, "cm"))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave("./analysis/figures/Gef_Form/2024-10-25_Randausformungen.jpg", dpi = 300, width = 18, height = 26, units = "cm")


library(scales)

gef_umbruch$randabschluss_f <- tolower(gef_umbruch$randabschluss_f)

fo_per_kr <- gef_umbruch |>    filter(!is.na(randabschluss_f)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_umbruch |>    filter(!is.na(randabschluss_f)) |>
  group_by(fundort, kultur_gr, kreislabel) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch$randabschluss_f <- factor(gef_umbruch$randabschluss_f, ordered = T,
                                      levels = c("gerade abgestrichen", "rund, nach außen verdickt", "horizontal abgeschlossen, gerundet",  "spitz-gerundet" , "diagonal von innen nach außen", "diagonal von außen nach innen", "spitz" ))

gef_umbruch |>
  filter(!is.na(randabschluss_f)) |>
  count(gefäßnummer, kreislabel, kultur_gr, randabschluss_f, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, randabschluss_f, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = randabschluss_f),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra , aes(x = fundort, y = 1, 
                                    label  = paste0(count)  ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9))  +
  labs(title = "Randabschluss fein (Randlippe)",
       x = "",
       y = "Prozent",
       fill = "",
       caption = paste0("n erhaltene Ränder = ", nrow(unique(gef_umbruch |>  filter(!is.na(randabschluss_f)) |> select(gefäßnummer)))) ) +
  scale_fill_viridis(discrete = TRUE,
                     direction = 1,
                     na.value = "grey")+
  scale_y_continuous( labels = c(0,50,100),
                      breaks = c(0, 0.5, 1) )+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.key.size = unit(0.25, "cm"))+
  guides(fill = guide_legend(nrow = 4) )+
  theme(strip.text.y = element_text(angle = 0) )  # drehen der Facettierungsbeschriftung


ggsave("./analysis/figures/Gef_Form/2024-10-25_Randlippe.jpg", dpi = 300, width = 18, height = 27, units = "cm")

gef_umbruch$randabschluss_f <- factor(gef_umbruch$randabschluss_f , ordered = F)


## Randwinkel

source("./R_functions/farbzuweisungen.R")

gef_umbruch$randneigung <- as.numeric(gef_umbruch$randneigung)

gef_per_k2 <- gef_umbruch |>    filter(!is.na(randneigung)) |>
  group_by(kultur2) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch |>
  filter(!is.na(randneigung)) |>
  ggplot()+
  geom_jitter(aes(x = kultur2,
                  y = randneigung,
                  col = kultur2),
              width = 0.3,
              height = 0.3,
              shape = 4) +
  geom_boxplot(aes(x = kultur2,
                   y = randneigung),
               alpha = 0.5)+
  geom_text(data = gef_per_k2, 
            aes(x = kultur2,
                y = 160,
                label = paste0("n = ", count) ) ) +
  geom_hline(yintercept = 90,
             linetype = "dashed",
             size = 0.5,
             colour = "grey") +
  labs(title = "Randwinkel",
       x = "",
       y = "Randneigung (Grad)",
       caption = paste0( "erhaltene Ränder, n = ", nrow(unique(gef_umbruch |>  filter(!is.na(randneigung),  randneigung != "") |> select(gefäßnummer))), ", \n Zittern der Punkte um 0.3, um Überlappung zu reduzieren") )  +
  scale_colour_manual(name = "",
                      breaks = c("SBK", "SRK","Rös", "Guhrau", "BKK", "FBG"),
                      values = col )+
  scale_y_continuous(breaks = c(30, 50, 70, 90, 110, 130, 150))+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom")+
  guides(colour = guide_legend(nrow = 1 ))

ggsave("./analysis/figures/Gef_Form/2024-10-25_Randneigung.png", dpi = 300, width = 16, height = 8, units = "cm")
