### Verzierungsanalyse Muster: Winkel diverser Art

load(file = "./analysis/data/derived_data/verzierungen.RData")

library(ggplot2)
library(dplyr)
library(stringr)
library(ggh4x)

## Winkelbreite gruppieren

verzierungen <- verzierungen |>
  mutate(winkelwert = case_when(
    muster.zusatz == "20 Grad Winkel" ~ 20,
    muster.zusatz == "30 Grad Winkel" ~ 30,
    muster.zusatz == "40 Grad Winkel" ~ 40,
    muster.zusatz == "45 Grad Winkel" ~ 45,
    muster.zusatz == "50 Grad Winkel" ~ 50,
    muster.zusatz == "60 Grad Winkel" ~ 60,
    muster.zusatz == "70 Grad Winkel" ~ 70,
    muster.zusatz == "75 Grad Winkel" ~ 75,
    muster.zusatz == "80 Grad Winkel" ~ 80,
    muster.zusatz == "90 Grad Winkel" ~ 90,
    muster.zusatz == "100 Grad Winkel" ~ 100,
    muster.zusatz == "120 Grad Winkel" ~ 120,
    muster.zusatz == "130 Grad Winkel" ~ 130,
  )) |>
  mutate(winkel_gr = case_when(
    winkelwert < 50 ~ "20-45",
    winkelwert > 45 & winkelwert < 80  ~ "50-75",
    winkelwert > 75 & winkelwert < 120 ~ "80-100",
    winkelwert > 100  ~ "120-130"
  )) |>
  mutate(winkel_gr2 = case_when(
    winkelwert < 40 ~ "20-30",
    winkelwert > 35 & winkelwert < 60  ~ "40-55",
    winkelwert == 60 ~ "60",
    winkelwert > 60 & winkelwert <= 80  ~ "70-80",
    winkelwert == 90 ~ "90", 
    winkelwert > 95  ~ "100-130"
  ))

verzierungen$winkel_gr2 <- factor(verzierungen$winkel_gr2,
                                  ordered = TRUE,
                                  levels = c("20-30", "40-55", "60", "70-80", "90", "100-130"))


verzierungen_ww <- verzierungen |> select(gefäßnummer, winkelwert, winkel_gr, winkel_gr2, kreislabel, kultur2, kultur_gr)

save(verzierungen_ww, file = "./analysis/data/derived_data/verzierungen_ww")

## Verhältnis Gef mit Winkelband zu verz Gef

load("./analysis/data/derived_data/gef_umbruch.RData")

gef_umbruch <- gef_umbruch2

## Gefäße, die verziert sind

gef_s_v <- gef_umbruch |>
  filter(gefäßnummer %in% verzierungen$gefäßnummer)

verz_winkel <- verzierungen |>
  filter(str_detect(muster, "winkel"))

n_winkel <- verz_winkel |>
  filter(!is.na(Muster_neu_gr)) |>
  group_by(kultur2, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer)) |>
  ungroup()

n_gef_s_v <- gef_s_v |> 
  group_by(kultur2, kultur_gr, fundort, kreislabel) |>
  summarize(count = n_distinct(gefäßnummer)) |>
  ungroup()

comp <- left_join(n_gef_s_v, n_winkel, by = c("fundort", "kultur2", "kultur_gr") )

colnames(comp) <- c("kultur2", "kultur_gr", "fundort","kreislabel", "count_verzGef", "count_winkel")

##### wo kommen diesen focking NAs in kreislabel her?!?!?!

comp$count_verzGef[is.na(comp$count_verzGef)] <- 0
comp$count_winkel[is.na(comp$count_winkel)] <- 0

comp$verhältnis <- paste0(comp$count_winkel, "/", comp$count_verzGef)

comp <- comp |>
  filter(count_winkel > 0)


### Übersicht Dreiecke

verzierungen$muster <- tolower(verzierungen$muster)

verz_dreieckig <- verzierungen |>
  mutate(gr_muster = case_when(
    muster == "winkel mit anhängsel" ~ "Y-Anhänge",
    str_detect(muster, "winkel") & str_detect(muster, "gestapelt") ~ "mehrfaches Winkelband",
    str_detect(muster, "winkel") & gebrochen == 0 ~ "einfaches Winkelband",
    str_detect(muster, "winkel") & gebrochen == 1 ~ "min. einfaches Winkelband",
    str_detect(muster, "gefüllt") & str_detect(muster, "dreieck") ~ "gefüllte Dreiecke",
    str_detect(muster, "y") ~ "Y-Anhänge",
    TRUE ~ NA
  ))

save(verz_dreieckig, file = "./analysis/data/derived_data/verz_dreieckig.RData")

fo_per_kr <- verz_dreieckig |>
  filter(!is.na(gr_muster)) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- verz_dreieckig |>
  filter(!is.na(gr_muster)) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))



fo_per_kr <- verz_dreieckig |>
  filter(!is.na(gr_muster) ) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

verz_dreieckig |>  
  filter(!is.na(gr_muster)) |>
  count(gefäßnummer, gr_muster, fundort, kultur_gr, kreislabel) |>
  group_by(gefäßnummer, gr_muster, fundort, kultur_gr, kreislabel) |>
  mutate(prop = prop.table(n)) |>
  ggplot() +
  geom_bar(aes(y = prop,
               x = fundort,
               fill = gr_muster),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort,
                y = 1,
                label = count),
            col = "white",
            size = 3,
            hjust = 1,
            position = position_fill(vjust = .9))+
  scale_fill_manual(values = palette("R4"),
                    breaks = c("gefüllte Dreiecke", "einfaches Winkelband", "min. einfaches Winkelband", "mehrfaches Winkelband", "Y-Anhänge"))+
  coord_flip()+
  labs(fill = "",
       x = "Fundort",
       y = "Prozent der winkelverzierten Gefäße",
       caption = paste0("n = ", sum(comp$count_winkel), " von ", n_distinct(verzierungen$gefäßnummer), " verzierten Gefäßen"))+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  ggtitle("Überblick über Winkel- und Dreiecksverzierungen")+
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0, size = 8),
        legend.key.size = unit(0.5, "cm"))+
  guides(fill=guide_legend(nrow = 2))

ggsave("./analysis/figures/Muster/2024-10-23_Winkel-Dreiecke-Kultur--Kreis.jpg", dpi = 300, width = 16, height = 23, units = "cm")


####  winkelgrad von winkeln 

library(ggh4x)

fo_per_kr <- verz_dreieckig |>
  filter(!is.na(winkel_gr2)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))


gef_per_fo <- verz_dreieckig |>
  filter(!is.na(winkel_gr2)) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))

verz_dreieckig |>  
  filter(!is.na(winkel_gr2)) |>
  count(gefäßnummer, winkel_gr2, fundort, kultur_gr, kreislabel) |>
  group_by(gefäßnummer, winkel_gr2, fundort, kultur_gr, kreislabel) |>
  mutate(prop = prop.table(n)) |>
  ggplot() +
  geom_bar(aes(y = prop,
               x = fundort,
               fill = winkel_gr2),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort,
                y = 1,
                label = count),
            col = "white",
            size = 3,
            hjust = 0.5,
            position = position_fill(vjust = .9))+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  coord_flip()+
  labs(fill = "Gradzahl",
       x = "Fundort",
       y = "Prozent d. winkelverzierten Gefäße",
       caption = paste0("n = ", sum(vgl_gef_per_fo$count.x), " von ", n_distinct(verzierungen$gefäßnummer), " verzierten Gefäßen"))+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme(text = element_text(size = 20))+
  ggtitle("Größe der Winkel in Winkelbändern und Dreiecken")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 1))

ggsave("./analysis/figures/Muster/2024-10-23_Winkelbreite-Kultur--Kreis.jpg", dpi = 300, width = 8, height = 10)


### 
library(ggh4x)

verzierungen$Muster_neu_gr <- tolower(verzierungen$Muster_neu_gr)

safe_colorblind_palette15 <- c("grey", "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288","blue",
                               "#44AA99", "#999933", "#882255", "#661100", "#6699CC",  "#AA4499", "black","green") 

fo_per_kr <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "breites") & str_detect(Muster_neu_gr, "winkel")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "breites") & str_detect(Muster_neu_gr, "winkel")) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))

library(stringr)

winkelb_breit <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "breites") & str_detect(Muster_neu_gr, "winkel"))  |>
  mutate(Muster_neu_gr2 = case_when(
    str_detect(Muster_neu_gr, "fuß") ~ "breites Winkelband auf dem Fuß",
    str_detect(Muster_neu_gr, "zwickelfüllung") &   str_detect(Muster_neu_gr, "schulter") ~ "breites Winkelband mit Zwickelfüllung auf d. Schulter",
    str_detect(Muster_neu_gr, "schulter") & str_detect(Muster_neu_gr, "mehrfaches")  ~ "mehrfaches breites Winkelband auf d. Schulter",
    str_detect(Muster_neu_gr, "zwickelfüllung") &   str_detect(Muster_neu_gr, "bauch") ~ "breites Winkelband mit Zwickelfüllung auf d. Bauch",
    str_detect(Muster_neu_gr, "unterem bauch") ~ "breites Winkelband auf dem unterem Bauch",
    Muster_neu_gr == "aufgefächertes breites winkelband auf schulter" ~ "mehrfaches breites Winkelband auf d. Schulter",
    Muster_neu_gr == "aufgefächertes breites winkelband auf dem hals" ~ "mehrfaches breites Winkelband auf dem Hals",
    str_detect(Muster_neu_gr, "schulter und bauch") & str_detect(Muster_neu_gr, "mehrfaches") ~ "mehrfaches breites Winkelband über Schulter und Bauch",
    str_detect(Muster_neu_gr, "schulter und bauch") ~ "breites Winkelband über Schulter und Bauch",
    str_detect(Muster_neu_gr, "mehrfaches") ~ "mehrfaches breites Winkelband",
    str_detect(Muster_neu_gr, "schulter") ~ "breites Winkelband auf d. Schulter",
    str_detect(Muster_neu_gr, "bauch") ~ "breites Winkelband auf d. Bauch",
    Muster_neu_gr == "mehrfaches breites winkelband auf gefäßkörper" ~ "mehrfaches breites Winkelband auf Gefäßkörper",
    str_detect(Muster_neu_gr, "gefäßkörper") ~ "breites Winkelband auf Gefäßkörper",
    Muster_neu_gr == "breites winkelband"  ~ "breites Winkelband" ,
    Muster_neu_gr ==  "breites winkelband auf dem hals" ~ "breites Winkelband auf dem Hals"   ,
    Muster_neu_gr ==   "breites winkelband mit zwickelfüllungen" ~   "breites Winkelband mit Zwickelfüllungen",
    Muster_neu_gr == "sehr breites winkelband mit zwickelfüllung" ~ "sehr breites Winkelband mit Zwickelfüllung"
  )) |>
  mutate(Muster_neu_gr2 = factor(Muster_neu_gr2,
                                 ordered = TRUE,
                                 levels =c("breites Winkelband",
                                           "breites Winkelband auf dem Hals",
                                           "breites Winkelband auf d. Schulter",
                                           "breites Winkelband über Schulter und Bauch",
                                           "breites Winkelband auf d. Bauch",
                                           "breites Winkelband auf dem unterem Bauch",
                                           "breites Winkelband auf Gefäßkörper",
                                           "breites Winkelband auf dem Fuß",
                                           "mehrfaches breites Winkelband",
                                           "mehrfaches breites Winkelband auf dem Hals",
                                           "mehrfaches breites Winkelband auf Hals und Schulter",
                                           "mehrfaches breites Winkelband auf d. Schulter",
                                           "mehrfaches breites Winkelband über Schulter und Bauch",
                                           "breites Winkelband mit Zwickelfüllungen",
                                           "breites Winkelband mit Zwickelfüllung auf d. Schulter",
                                           "breites Winkelband mit Zwickelfüllung auf d. Bauch",
                                           "sehr breites Winkelband mit Zwickelfüllung") ) )


save(winkelb_breit, file = "./analysis/data/derived_data/winkelb_breit.RData") 

winkelb_breit |>
  count(gefäßnummer, kreislabel, fundort, kultur2, kultur_gr, Muster_neu_gr2) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur2, kreislabel,  kultur_gr,  Muster_neu_gr2) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = Muster_neu_gr2),
           position = "fill", stat = "identity")+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort,
                y = 1,
                label = count),
            col = "white",
            size = 3,
            hjust = 0.5,
            position = position_fill(vjust = .9))+  
  coord_flip()+
  labs(fill = "",
       x = "Fundort",
       y = "Prozent d. mit breiten Bändern verz. Gefäße",
       caption = paste0("n = ", sum(vgl_gef_per_fo$count.x), " von ", n_distinct(verzierungen$gefäßnummer), " verzierten Gefäßen"))+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  scale_fill_manual(values = safe_colorblind_palette15) +
  force_panelsizes(rows = fo_per_kr$count)+
  ggtitle("Breite Winkelbänder")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14)) +
  guides(fill = guide_legend(ncol = 2 ))

ggsave("./analysis/figures/Muster/2024-10-23_breiteWinkelbänder-Kultur--Kreis.jpg", height = 11, width = 10)  

### schmale winkelbänder

library(ggh4x)

verzierungen$Muster_neu_gr <- tolower(verzierungen$Muster_neu_gr)

fo_per_kr <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "schmales") & str_detect(Muster_neu_gr, "winkel")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "schmales") & str_detect(Muster_neu_gr, "winkel")) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))


winkelb_schmal <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "schmales") & str_detect(Muster_neu_gr, "winkel")) |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, Bandbreite, Muster_neu_gr, kultur2) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, Bandbreite, Muster_neu_gr, kultur2) |>          # now required with changes to dplyr::count()
  filter(!is.na(Muster_neu_gr)) |>
  mutate(prop = prop.table(n)) |>
  mutate(Muster_neu_gr2 = case_when(
    Muster_neu_gr == tolower("schmales lockeres Winkelband über Hals und Schulter") ~ "einfaches schmales Winkelband auf Hals und Schulter",
    Muster_neu_gr == tolower("schmales lockeres verschachteltes Winkelband über Gefäßkörper") ~ "einfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("Mehrfaches schmales stumpfes Winkelband über Gefäßkörper") ~ "mehrfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("Mehrfaches schmales stumpfes Winkelband über den gesamten Gefäßkörper") ~ "mehrfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("schmales lockeres Winkelband über Gefäßkörper") ~ "einfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("mehrfaches schmales stumpfes Winkelband auf dem Fuß") ~ "mehrfaches schmales Winkelband auf dem Fuß",
    Muster_neu_gr == tolower("mehrfaches schmales stumpfes Winkelband über den gesamten Gefäßkörper") ~ "mehrfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("mehrfaches schmales stumpfes Winkelband über Gefäßkörper") ~ "mehrfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("mehrfaches schmales stumpfes Winkelband auf schulter"    ) ~ "mehrfaches schmales Winkelband auf der Schulter",
    Muster_neu_gr == tolower("mehrfaches schmales winkelband über schulter und bauch") ~ "mehrfaches schmales Winkelband über Schulter und Bauch",
    Muster_neu_gr == tolower("einfaches schmales Winkelband") ~ "einfaches schmales Winkelband",
    Muster_neu_gr == tolower("einfaches schmales Winkelband auf Hals und Schulter") ~ "einfaches schmales Winkelband auf Hals und Schulter",
    Muster_neu_gr == tolower("schmales lockeres Winkelband über Hals und Schulter") ~ "einfaches schmales Winkelband auf Hals und Schulter",
    Muster_neu_gr == tolower("schmales lockeres Winkelband auf der Schulter") ~ "einfaches schmales Winkelband auf der Schulter",
    Muster_neu_gr == tolower("einfaches schmales Winkelband auf Schulter und Bauch") ~  "einfaches schmales Winkelband auf Schulter und Bauch",
    Muster_neu_gr == tolower("einfaches schmales Winkelband auf dem Bauch") ~ "einfaches schmales Winkelband auf dem Bauch",
    Muster_neu_gr == tolower("einfaches schmales Winkelband über Gefäßkörper") ~  "einfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("einfaches schmales Winkelband auf dem Fuß") ~   "einfaches schmales Winkelband auf dem Fuß",
    Muster_neu_gr == tolower("mehrfaches schmales Winkelband auf Schulter") ~   "mehrfaches schmales Winkelband auf Schulter",
    Muster_neu_gr == tolower("mehrfaches schmales Winkelband über Schulter und Bauch") ~ "mehrfaches schmales Winkelband über Schulter und Bauch",
    Muster_neu_gr == tolower("mehrfaches schmales Winkelband über Gefäßkörper") ~   "mehrfaches schmales Winkelband über Gefäßkörper",
    Muster_neu_gr == tolower("einfaches schmales Winkelband auf dem Hals") ~    "einfaches schmales Winkelband auf dem Hals",
    Muster_neu_gr == tolower("einfaches schmales Winkelband auf der schulter") ~  "einfaches schmales Winkelband auf der Schulter",
    Muster_neu_gr == tolower( "mehrfaches schmales Winkelband auf dem Fuß") ~ "mehrfaches schmales Winkelband auf dem Fuß" ))  |>
  mutate(Muster_neu_gr2 = factor(Muster_neu_gr2,
                                 ordered = TRUE,
                                 levels =  c("einfaches schmales Winkelband",
                                             "einfaches schmales Winkelband auf dem Hals",
                                             "einfaches schmales Winkelband auf Hals und Schulter",
                                             "einfaches schmales Winkelband auf der Schulter",
                                             "einfaches schmales Winkelband auf Schulter und Bauch",
                                             "einfaches schmales Winkelband auf dem Bauch",
                                             "einfaches schmales Winkelband über Gefäßkörper",
                                             "einfaches schmales Winkelband auf dem Fuß",
                                             "mehrfaches schmales Winkelband auf der Schulter",
                                             "mehrfaches schmales Winkelband über Schulter und Bauch",
                                             "mehrfaches schmales Winkelband über Gefäßkörper",
                                             "mehrfaches schmales Winkelband auf dem Fuß"))) 

save(winkelb_schmal, file = "analysis/data/derived_data/winkelb_schmal.RData")

winkelb_schmal |>
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
  scale_fill_manual(values = safe_colorblind_palette13 ) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  coord_flip()+
  labs(fill = "",
       x = "Fundort",
       y = "Prozent d. Gef. mit schmalen Winkelbändern",
       caption = paste0("n = ", sum(vgl_gef_per_fo$count.x), " von ", n_distinct(verzierungen$gefäßnummer), " verzierten Gefäßen"))+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  ggtitle("Schmale Winkelbänder")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14) ) +
  guides(fill = guide_legend(ncol = 2 ))

ggsave("./analysis/figures/Muster/2024-10-23_schmaleWinkelbänder-Kultur--Kreis.jpg", height = 10, width = 12)  



### besondere winkelbänder

library(ggh4x)


fo_per_kr <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "stumpf|verschachtelt|lockeres|aufgefächert")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))


gef_per_fo <- verzierungen |>
  filter(str_detect(Muster_neu_gr, "stumpf|verschachtelt|lockeres|aufgefächert")) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))


verzierungen |>
  filter(str_detect(Muster_neu_gr, "stumpf|verschachtelt|lockeres|aufgefächert")) |>
  mutate(Muster_neu_gr2 = case_when(
    str_detect(Muster_neu_gr, "verschachtelt") & str_detect(Muster_neu_gr, "breit") ~ "breites verschachteltes Winkelband",
    str_detect(Muster_neu_gr, "verschachtelt") ~ "schmales verschachteltes Winkelband",
    str_detect(Muster_neu_gr, "stumpf") & str_detect(Muster_neu_gr, "breit") ~ "breites stumpfes Winkelband",
    str_detect(Muster_neu_gr, "aufgefächert") & str_detect(Muster_neu_gr, "breit") ~ "breites aufgefächertes Winkelband",
    str_detect(Muster_neu_gr, "mehrfaches") & str_detect(Muster_neu_gr, "schmal") &  str_detect(Muster_neu_gr, "stumpf") ~ "mehrfaches schmales stumpfes Winkelband",
    str_detect(Muster_neu_gr, "stumpf") & str_detect(Muster_neu_gr, "schmal") ~ "schmales stumpfes Winkelband",
    str_detect(Muster_neu_gr, "locker") & str_detect(Muster_neu_gr, "schmal") ~ "schmales lockeres Winkelband",
    str_detect(Muster_neu_gr, "locker") & str_detect(Muster_neu_gr, "breit") ~ "breites lockeres Winkelband",
    TRUE ~ Muster_neu_gr  )) |>
  count(gefäßnummer, kreislabel, fundort, Bandbreite, Muster_neu_gr2) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort,kreislabel, Bandbreite, Muster_neu_gr2) |>          # now required with changes to dplyr::count()
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
  coord_flip()+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0,50,100))  +
  labs(fill = "",
       x = "Fundort",
       y = "Prozent d. Gefäße mit Winkelbändern",
       caption = paste0("n = ", sum(vgl_gef_per_fo$count.x), " von ", n_distinct(verzierungen$gefäßnummer), " verzierten Gefäßen"))+
  facet_grid(kreislabel ~ ., scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  ggtitle("Sonderformen von Winkelbändern in der Stichbandkeramik")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 14))+
  guides(fill = guide_legend(ncol = 2 ))

ggsave("./analysis/figures/Muster/2024-10-23_bes_Winkelbänder-SBK--Kreis.jpg", height = 6, width = 10)  