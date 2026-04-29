### Ähnlichkeiten zwischen Fundstellen aufgrund der Gefäßmachart
library(dplyr)
library(sf)

### Magerung

## 1. Distanzmaße berechnen

load("./analysis/data/derived_data/machart_zusammengefasst.RData")
load( "./analysis/data/derived_data/gef_infos.RData")

sites_all <- read.csv2("./analysis/data/geodata/sites_fst-analyse2_tab.csv")

# 3 Fst ohne Koordinaten? quick workaround:
sites_all <- sites_all |> filter(!is.na(X))

sites_all <- st_as_sf(sites_all, coords = c("X", "Y"), crs = 25833)

machart$bruchfarbe_gr <- factor(machart$bruchfarbe_gr )

machart2 <- machart |>
  filter(!is.na(FO) ) 

machart_infos <- left_join(machart2, gef_infos)

machart_v <- machart_infos |>
  filter(verz_n == "verz" ) |>
  filter(kultur2 %in% c("SRK", "SBK")) |>
  select(-c(verz_n, Bef) ) |>
  unique()

machart_og <- machart_v |>
  select(-c(gefäßnummer, FO, kultur2, kreis, kreislabel, gemarkung, land, stufe)) 

machart_v$rn <- as.character(seq.int(nrow((machart_v)) )) # rownumber ging so nich.

machart_og <- machart_v |>
  select(-c(gefäßnummer, FO, kultur2, kreis, kreislabel, gemarkung, land, stufe, rn)) 

machart_v2 <- machart_v |>
  select(gefäßnummer, FO, kreis, kreislabel, gemarkung, land, rn)

library(FD)

data_gower <- gowdis(machart_og, ord = "podani", asym.bin = c(1:8)) ## Binary variables should be of class 'numeric' , drauf achten, dass character allesamt factor sind!

library(igraph)

t <- graph_from_adjacency_matrix(as.matrix(data_gower), weighted=TRUE) # aus distanzmatrix adjecency matrix für igraph
machart_df <- get.data.frame(t) # daraus dataframe 

# 1. Verknüpfen mit Fundort
# 2. Fundorte aufsummieren nach beiden FO
# 3. plotten von Linien ?

machart_site <- left_join(machart_df, machart_v2, by = c("from" = "rn"))

colnames(machart_site) <- c("from","to","weight","gefäßnummer_from", "FO_from", "kreis_from","kreislabel_from" , "gemarkung_from" ,  "land_from")

machart_site <- left_join(machart_site, machart_v2, by = c("to" = "rn"))

machart_site_s <- machart_site |>
  group_by(FO_from, FO) |>
  mutate(sum_w = sum(weight) ) |>
  filter(sum_w > 0) |>
  group_by(FO_from , FO, sum_w) |>
  summarise(n = n() ) |>
  filter(n > 0) |>
  mutate(sum_w_s = sum_w/n) # aufsummieren der Beziehung von einem FPL zum anderen nach Kultur und teilen durch Anzahl d Beziehungen != Anzahl Gefäße!

machart_site_s2 <- machart_site_s |>
  select(FO_from, FO, sum_w_s ) |>
  unique()|>
  ungroup() |>
  unique()

### nur SBK und SRK


sites_SBK_SRK <- sites_all |>
  filter(KULTUR %in% c("SBK", "SRK", "SBK?", "SRK?") )

sites_SBK_SRK <- sites_SBK_SRK |> select(FST, geometry) |> unique()


machart_site3 <- left_join(machart_site_s2, sites_SBK_SRK, by = c("FO_from" = "FST"))

machart_site3 <- machart_site3 |>
  select(FO_from, FO, sum_w_s, geometry_from = geometry)

machart_site3 <- left_join(machart_site3, sites_SBK_SRK, by = c("FO" = "FST"))

machart_site3$rn <- rownames(machart_site3)

xy_from <- as.data.frame(st_coordinates(machart_site3$geometry_from))
xy_from$rn <- rownames(xy_from)

machart_site3_xy <- left_join(machart_site3, xy_from, by = "rn")

xy_to <- as.data.frame(st_coordinates(machart_site3$geometry))
xy_to$rn <- rownames(xy_to)

machart_site3_xy <- left_join(machart_site3_xy, xy_to, by = "rn")

machart_site3_xy <- machart_site3_xy |>
  filter(sum_w_s > 0, !is.na(sum_w_s))

machart_site3_xy$S <- 1- machart_site3_xy$sum_w_s

save(machart_site3_xy, file =  "./analysis/data/derived_data/machart_distance_site_to_site_xy.RData")

label <- unique(machart_site3_xy[,c("FO", "geometry")])
label2 <- unique(machart_site3_xy[,c("FO_from", "geometry_from")])

label2 <- label2 |>
  group_by(FO_from) |> slice_sample()

label2 <- st_as_sf(label2)

### plot Karte

load("./analysis/data/geodata/admin.RData")
load("./analysis/data/geodata/water3.RData")

library(ggplot2)
library(ggspatial)
library(lwgeom)

plot_HG_SBK <- ggplot() +
  theme_bw()+
  geom_sf(data = admin_2, colour = "darkgrey", fill = "white")+
  geom_sf(data = water3, size = 2, colour = "lightblue3" ) +
  geom_sf(data = sites_SBK_SRK, size = 1, shape = 18, colour = "grey70")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

machart_site3_xy <- machart_site3_xy |>
  select(S, X.x, Y.x, X.y, Y.y, FO_from, FO)|>
  unique() # hmh. wieso dopplungen??

plot_HG_SBK  +
  geom_spatial_segment(data = machart_site3_xy, 
                       aes(x=X.x,
                           y=Y.x,
                           xend=X.y,
                           yend=Y.y,
                           color = S,
                           alpha = S),
                       crs = st_crs(sites_SBK_SRK) ) +
  ggrepel::geom_label_repel(data = label2, 
                            aes(label = FO_from, 
                                geometry = geometry_from),
                            fill = alpha(c("white"),0.5),
                            size = 2,
                            label.padding = 0.1,
                            max.overlaps = 20,
                            stat = "sf_coordinates",
                            col = "black" )+
  scale_colour_gradient(name = "mittlere \nÄhnlichkeit",
                        low = "grey",
                        high = "red")+
  scale_alpha_continuous(name = "mittlere \nÄhnlichkeit")+
  labs(title = "Gemittelte Ähnlichkeit zwischen den Fundstellen",
       subtitle = "anhand der Machart der verzierten Gefäße",
       caption = paste("Ähnlichkeit als 1 - mittlere Gowerdistanz (Summe der Distanzmaße zwischen den Gefäßen der Fundstellen geteilt durch Anzahl Verbindungen)\n anhand n =", length(unique(machart_v2$gefäßnummer) ), "Gefäßen von", unique(length(label$FO)), "Fundstellen"),
       x = "",
       y = "")+
  theme(legend.key = element_rect(fill = "white",
                                  colour = "white",
                                  linewidth = 0.5, linetype = "solid"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linewidth = 0.5, linetype = "solid")) +
  coord_sf(xlim = c(10,20), ylim = c(50.5, 53.5))

ggsave("./analysis/figures/Magerung/Karte_mittlere_gowerähnlichkeit_machart_label_v2.png", dpi = 300, width = 24, height = 16, units = "cm")


# zum Überblick gewinnen:
machart_site3_xy |>
  filter(FO_from == FO) |>
  ggplot()+
  geom_density(aes(x = S))

machart_site3_xy |>
  filter(FO_from != FO) |>
  filter(!is.na(S)) |>
  ggplot()+
  geom_density(aes(x = S))


machart_site3_xy |>
  filter(S > 0.55) |>
  select(FO_from, FO, S) |>
  unique()


######### # 2. art der darstellung: heatmap

machart_site3_xy$FO_from <- factor(machart_site3_xy$FO_from, ordered = T,
                                   levels = c( "Eythra" ,"Quedlinburg KGA 1", "Quedlinburg KGA 2", "Jüterbog 31" ,   "Rohrbeck 14"  , "Bochow 15", "Bochow 2" , "Bochow 16",  "Bochow 6/7", "Schiaß 1"   ,
                                               "Heinersbrück 42"  ,  "Hohenbrück 3",
                                               "Wierzchno/Wirchenblatt",
                                               "Neuendorf 23",
                                               "Klein-Rietz" ,
                                               "Seelow 20"  ,
                                               "Platkow 13"  ,
                                               "Niederjesar 3" ,
                                               "Flemsdorf 10" ,
                                               "Gartz ?",
                                               "Nowe Objezierze 7" ,
                                               "Karsko/Schöningsburg" ,
                                               "Niederguhren/Kije",
                                               "Pyritz – Weinberg" ,
                                               "Iwno",
                                               "Racot 18" ,
                                               "Kruszynek 6"  ))


machart_site3_xy$FO <- factor(machart_site3_xy$FO, ordered = T,
                              levels = levels(machart_site3_xy$FO_from))

machart_site3_xy |>
  filter(!(is.na(FO_from)) ) |> # workaround, 1 Fs als NA, aber eigentlich alle da?
  filter(!(is.na(FO)) )|>
  ggplot()+
  geom_tile(aes(x = FO_from,
                y = FO,
                fill = S ))+
  geom_text(aes(x = FO_from,
                y = FO,
                label = round(S, 2) ), 
            size = 3,
            col = "white")+
  scale_fill_gradient(low = "grey",
                      high = "red")+
  labs(fill = "Ähnlichkeit",
       x = " ",
       y = " ",
       caption = " ")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./analysis/figures/Magerung/mittlere_Gowerähnlichkeit_Machart_heatmap.png", dpi = 300, width = 24, height = 16, units = "cm")


################ Rössen Guhrau BKK + SRK


machart_v <- machart_infos |>
  filter(verz_n == "verz" ) |>
  filter(kultur2 %in% c("SRK", "BKK", "Guhrau", "Rössen")) |>
  select(-c(verz_n, Bef) ) |>
  unique()

machart_v$FST_k <- paste(machart_v$FO, machart_v$kultur2)

machart_v$rn <- as.character(seq.int(nrow((machart_v)) ))

machart_og <- machart_v |>
  select(-c(gefäßnummer, FO, kultur2, kreis, kreislabel, gemarkung, land, stufe, rn,FST_k)) 

machart_v2 <- machart_v |>
  select(gefäßnummer, FO, kreis, kreislabel, gemarkung, land, rn, FST_k)

library(FD)

data_gower <- gowdis(machart_og, ord = "podani", asym.bin = c(1:8)) ## Binary variables should be of class 'numeric' , drauf achten, dass character allesamt factor sind!

library(igraph)

t <- graph_from_adjacency_matrix(as.matrix(data_gower), weighted=TRUE) # aus distanzmatrix adjecency matrix für igraph
machart_df <- get.data.frame(t) # daraus dataframe 

# 1. Verknüpfen mit Fundort
# 2. Fundorte aufsummieren nach beiden FO
# 3. plotten von Linien ?

machart_site <- left_join(machart_df, machart_v2, by = c("from" = "rn"))

colnames(machart_site) <- c("from","to","weight","gefäßnummer_from", "FO_from", "kreis_from","kreislabel_from" , "gemarkung_from" ,  "land_from", "FST_k_from")

machart_site <- left_join(machart_site, machart_v2, by = c("to" = "rn"))

machart_site_s <- machart_site |>
  group_by(FST_k_from, FST_k, FO_from, FO) |>
  filter(!is.na(weight) ) |>
  mutate(sum_w = sum(weight) ) |>
  group_by(FST_k_from, FST_k, sum_w, FO_from, FO) |>
  summarise(n = n() ) |>
  filter(n > 0) |>
  mutate(sum_w_s = sum_w/n) # aufsummieren der Beziehung von einem FPL zum anderen nach Kultur und teilen durch Anzahl d Beziehungen != Anzahl Gefäße!

machart_site_s2 <- machart_site_s |>
  group_by(FST_k_from, FO_from, FO, FST_k, sum_w) |>
  unique()|>
  ungroup() |>
  unique()

sites_RGB <- sites_all |>
  filter(KULTUR %in% c("BKK", "Guhrau", "Rössen", "Rössener", "SRK", "SRK?") )

machart_site3 <- left_join(machart_site_s2, sites_all, by = c("FO_from" = "FST"))

machart_site3 <- machart_site3 |>
  select(FO_from, FO, sum_w_s, FST_k_from, FST_k, geometry_from = geometry)

machart_site3 <- left_join(machart_site3, sites_RGB, by = c("FO" = "FST"))

machart_site3$rn <- rownames(machart_site3)

xy_from <- as.data.frame(st_coordinates(machart_site3$geometry_from))
xy_from$rn <- rownames(xy_from)

machart_site3_xy <- left_join(machart_site3, xy_from, by = "rn")

xy_to <- as.data.frame(st_coordinates(machart_site3$geometry))
xy_to$rn <- rownames(xy_to)

machart_site3_xy <- left_join(machart_site3_xy, xy_to, by = "rn")

machart_site3_xy <- machart_site3_xy |>
  filter(sum_w_s > 0, !is.na(sum_w_s))

machart_site3_xy$S <- 1- machart_site3_xy$sum_w_s


machart_site3_xy <- machart_site3_xy |>
  mutate(k_from = case_when(
    str_detect(FST_k_from, "SRK") ~ "SRK",
    str_detect(FST_k_from, "BKK") ~ "BKK",
    str_detect(FST_k_from, "Rössen") ~ "Rössen",
    str_detect(FST_k_from, "Guhrau") ~ "Guhrau")) |>
  mutate(k = case_when(
    str_detect(FST_k, "SRK") ~ "SRK",
    str_detect(FST_k, "BKK") ~ "BKK",
    str_detect(FST_k, "Rössen") ~ "Rössen",
    str_detect(FST_k, "Guhrau") ~ "Guhrau"))


label <- unique(machart_site3_xy[,c("FO", "geometry")])
label2 <- unique(machart_site3_xy[,c("FO_from", "geometry_from", "k_from")])

label2 <- label2 |>
  group_by(FO_from) |> slice_sample()

label2 <- st_as_sf(label2)


machart_site3_xy <- machart_site3_xy |>
  select(S, X.x, Y.x, X.y, Y.y, FO_from, FO, FST_k_from, FST_k, k_from, k)|>
  unique() # hmh. wieso dopplungen??

#--> Kartendarstellung lohnt nicht: zu viele Überlappungen durch mehrere Phasen an eine FO

machart_site_s2$S <- 1 - machart_site_s2$sum_w_s

save(machart_site_s2, file = "./analysis/data/derived_data/machart_distance_site_to_site_RGBxy.RData")


library(stringr)
machart_site_s2 <-machart_site_s2  |>
  mutate(k_from = case_when(
    str_detect(FST_k_from, "SRK") ~ "SRK",
    str_detect(FST_k_from, "BKK") ~ "BKK",
    str_detect(FST_k_from, "Rössen") ~ "Rössen",
    str_detect(FST_k_from, "Guhrau") ~ "Guhrau")) |>
  mutate(k = case_when(
    str_detect(FST_k, "SRK") ~ "SRK",
    str_detect(FST_k, "BKK") ~ "BKK",
    str_detect(FST_k, "Rössen") ~ "Rössen",
    str_detect(FST_k, "Guhrau") ~ "Guhrau"))

fst_per_k <- machart_site_s2 |>
  group_by(k) |> summarize(count = n_distinct(FST_k))

fst_per_k_from <- machart_site_s2 |>
  group_by(k_from) |> summarize(count = n_distinct(FST_k_from))


machart_site_s2 |>
  ggplot()+
  geom_tile(aes(x = FST_k_from,
                y = FST_k,
                fill = S ))+
  geom_text(aes(x = FST_k_from,
                y = FST_k,
                label = round(S, 2) ), 
            size = 2.2,
            col = "white")+
  scale_fill_gradient(low = "grey",
                      high = "red")+
  labs(fill = "Ähnlichkeit",
       x = " ",
       y = " ",
       caption = " ")+
  theme_bw()+
  facet_grid(k ~ k_from, scales = "free") +
  ggh4x::force_panelsizes(rows = fst_per_k$count+2,
                          cols = fst_per_k_from$count+2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./analysis/figures/Magerung/mittlere_Gowerähnlichkeit_RGB_machart_heatmap.png", dpi = 300, width = 26, height = 18, units = "cm")
