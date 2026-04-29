### Verzierungsanalyse Keramik -- Drei - und Vierecke

library(stringr)
library(dplyr)
library(ggh4x)
library(ggplot2)

load(file = "./analysis/data/derived_data/verzierungen.RData")

verzierungen$muster <- tolower(verzierungen$muster)

fo_per_kr <- verzierungen |>
  filter(str_detect(muster, "dreieck")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen |>
  filter(str_detect(muster, "dreieck")) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))


verz_dreiecke <- verzierungen |>
  filter(str_detect(muster, "dreieck") | muster.zusatz == "Zwickelfüllung") |>
  mutate(Muster_neu_gr = case_when(                    # neu zusammenfassen: hängend, stehend + Grad zusammengefasst: 60 "60" und 45-50 (Schmal) 
    Muster_neu == "Stehendes 45 Grad Dreieck" ~ "schmales stehendes Dreieck (20-45°)",
    Muster_neu == "mit Schraffur gefülltes hängendes Dreieck auf dem Bauch" ~ "hängendes Dreieck gefüllt mit Schraffur",
    Muster_neu == "mit Schraffur gefülltes stehendes Dreieck auf dem Bauch" ~ "stehendes Dreieck gefüllt mit Schraffur",
    Muster_neu == "Reihe von Dreiecken auf Bauchumbruch" ~ "Reihe kleiner Dreiecke",
    Muster_neu == "Stehendes 40 Grad Dreieck auf der Schulter" ~ "schmales stehendes Dreieck (20-45°)",
    Muster_neu == "Stehendes 20 Grad Dreieck" ~ "schmales stehendes Dreieck (20-45°)",
    Muster_neu == "Stehendes 30 Grad Dreieck auf der Schulter" ~ "schmales stehendes Dreieck (20-45°)",
    Muster_neu == "Stehendes 45 Grad Dreieck auf der Schulter" ~ "schmales stehendes Dreieck (20-45°)",
    Muster_neu == "verschachteltes stehendes 50 Grad Dreieck auf der Schulter" ~ "schmales stehendes Dreieck (20-45°)",
    Muster_neu == "Stehendes 60 Grad Dreieck auf der Schulter" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "Stehendes 60 Grad Dreieck auf hohem Fuß" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "stehendes 60 Grad Dreieck" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "Stehendes 60 Grad Dreieck auf Schulter" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "Hängendes 60 Grad Dreieck auf der Schulter" ~ "hängendes Dreieck (50-60°)",
    Muster_neu == "Hängendes 60 Grad Dreieck" ~ "hängendes Dreieck (50-60°)",
    Muster_neu == "Hängendes 60 Grad Dreieck von Schulter bis Bauch" ~ "hängendes Dreieck (50-60°)",
    Muster_neu == "verschachteltes hängendes 50 Grad Dreieck über gesamten Körper" ~ "hängendes Dreieck (50-60°)",
    Muster_neu == "Zwickelfüllung aus unregelmäßigen Einzelstichen" ~ "hängendes Dreieck aus Einzelstichen",
    Muster_neu == "Hängendes 40 Grad Dreieck auf der Schulter" ~ "schmales hängendes Dreieck (20-45°)",
    Muster_neu == "Hängendes 45 Grad Dreieck auf der Schulter" ~ "schmales hängendes Dreieck (20-45°)",
    Muster_neu == "Hängendes 20 Grad Dreieck auf der Schulter" ~ "schmales hängendes Dreieck (20-45°)",
    Muster_neu == "Hängendes 30 Grad Dreieck auf der Schulter" ~ "schmales hängendes Dreieck (20-45°)",
    Muster_neu == "Stehendes 50 Grad Dreieck auf der Schulter" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "Band aus kleinen Dreiecken auf der Schulter"  ~ "Reihe kleiner Dreiecke",
    Muster_neu == "Stehendes 60 Grad Dreieck auf Schulter und Bauch" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "Stehendes 60 Grad Dreieck auf Schulter" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "stehendes 60 Grad Dreieck auf Schulter" ~ "stehendes Dreieck (50-60°)",
    Muster_neu == "Hängendes 60 Grad Dreieck auf Schulter und Bauch" ~ "hängendes Dreieck (50-60°)",  
    is.na(Muster_neu) ~ "gebrochenes Muster",
    TRUE ~ Muster_neu ))


# plot dreiecke

verz_dreiecke |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, Bandbreite, Muster_neu_gr) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, Bandbreite, Muster_neu_gr) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = Muster_neu_gr),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort,
                y = 1,
                label = count),
            col = "white",
            size = 3,
            hjust = 0.5,
            position = position_fill(vjust = .8))+  
  scale_fill_manual(breaks = c("gebrochenes Muster",
                               "hängendes Dreieck (50-60°)",
                               "stehendes Dreieck (50-60°)",
                               "schmales hängendes Dreieck (20-45°)",
                               "schmales stehendes Dreieck (20-45°)",
                               "hängendes Dreieck gefüllt mit Einzelstichen",
                               "hängendes Dreieck gefüllt mit Schraffur",
                               "stehendes Dreieck gefüllt mit Schraffur",
                               "hängendes Dreieck aus Einzelstichen",
                               "Reihe kleiner Dreiecke"
  ),
  values = c("grey", viridis(10)) )+
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels =c(0,50,100))+
  coord_flip()+
  labs(fill = "",
       x = "Fundort",
       y = "Prozent der mit Dreiecken verzierten Gefäße",
       caption = paste0("n = ", sum(vgl_gef_per_fo$count.x), " von ", n_distinct(verzierungen$gefäßnummer), " verzierten Gefäßen"))+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count,
                   cols = c(3,3,6,6))+
  ggtitle("Übersicht über Dreiecksmuster")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14) )+
  guides(fill = guide_legend(ncol = 2 ))

ggsave("./analysis/figures/Muster/2024-10-23_dreiecke-Kultur--Kreis2.jpg", height = 10, width = 10)  



## plot Vierecke


fo_per_kr <- verzierungen |>
  filter(str_detect(muster, "viereck")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- verzierungen |>
  filter(str_detect(muster, "viereck")) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))

verzierungen |>
  filter(str_detect(muster, "viereck")) |>
  mutate(Muster_neu_gr2 = case_when(
    str_detect(Muster_neu_gr, "nach unten offen") ~  "nach unten offenes Rechteck",
    str_detect(Muster_neu_gr, "raute auf schulter") ~  "Rauten über Gefäßkörper",
    str_detect(Muster_neu_gr, "Raute auf Schulter") ~  "Rauten über Gefäßkörper",
    str_detect(Muster_neu_gr, "Rauten über Gefäßkörper ?") ~  "Rauten über Gefäßkörper",
    str_detect(Muster_neu_gr, "rauten über gefäßkörper") ~  "Rauten über Gefäßkörper",
    str_detect(Muster_neu_gr, "rauten-band auf schulter-bauch-umbruch") ~  "Rautenband auf Bauchumbruch",
    Muster_neu_gr == "aufgelöstes schachbrettmuster" ~  "aufgelöstes Schachbrettmuster",
    Muster_neu_gr == "gefülltes karo" ~  "Raute als Winkelfüllung",
    Muster_neu_gr == "schachbrettmuster" ~  "Schachbrettmuster",
    TRUE ~ Muster_neu_gr  )) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, Bandbreite, Muster_neu_gr2) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, Bandbreite, Muster_neu_gr2) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = Muster_neu_gr2),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort,
                y = 1,
                label = count),
            col = "white",
            size = 3,
            hjust = 0.5,
            position = position_fill(vjust = .9))+  
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels =c(0,50,100))+
  coord_flip()+
  labs(fill = "",
       x = "Fundort",
       y = "Prozent d. mit Vierecksmustern verz. Gefäße")+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  ggtitle("Vierecksmuster")+
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 9))+
  guides(fill=guide_legend(nrow = 3))


ggsave("./analysis/figures/Muster/2024-10-23_vierecke-Kultur--Kreis.jpg", height = 10, width = 16, units = "cm")  