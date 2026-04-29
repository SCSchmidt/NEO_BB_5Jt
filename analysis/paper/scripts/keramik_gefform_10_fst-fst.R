### Fst zu Fst - Beziehung
### Gefäßform

## TODO try https://github.com/nevrome/ggpointgrid/tree/futhark -> points at the same point!
## geom_pointgrid braucht x und y nich geometry


library(dplyr)
library(vegan)
library(forcats)
library(FD)
library(ggplot2)
library(geomtextpath)
library(ggforce)


sites_all <- read.csv2("./analysis/data/geodata/sites_fst-analyse2_tab.csv")

load("./analysis/data/geodata/admin.RData")
load("./analysis/data/geodata/water3.RData")

# 3 Fst ohne Koordinaten? quick workaround:
sites_all <- sites_all |> filter(!is.na(X))

sites_all <- st_as_sf(sites_all, coords = c("X", "Y"), crs = 25833)

load("./analysis/data/derived_data/gef_umbruch.RData")
gef_umbruch <- gef_umbruch2

gef_umbruch$erh_s <- as.numeric(gef_umbruch$erh_s)
gef_umbruch$erh_r <- as.numeric(gef_umbruch$erh_r)
gef_umbruch$randneigung <- as.numeric(gef_umbruch$randneigung)
gef_umbruch$bodenwinkel <- as.numeric(gef_umbruch$bodenwinkel)
gef_umbruch$erh_b <- as.numeric(gef_umbruch$erh_b)

gef_umbruch$randabschluss_gr <- as.factor(gef_umbruch$randabschluss_gr)
gef_umbruch$randabschluss_f <- as.factor(gef_umbruch$randabschluss_f)
gef_umbruch$boden <- as.factor(gef_umbruch$boden)
gef_umbruch$Hals <- as.factor(gef_umbruch$Hals)

oberteile <- gef_umbruch |>
  filter(erh_r > 4) |>
  filter(kultur2 %in% c("SBK", "SRK"))

# check oberteilinfos nur dann, wenn rand/schulter tatsächlich vorhanden
oberteile2 <- oberteile |>
  mutate(h_hals = case_when(
    erh_s < 1 ~ NA,
    TRUE ~ h_hals)) |>
  mutate(durchm_r = case_when(
    erh_r < 5 ~ NA,
    TRUE ~ durchm_r)) |>
  mutate(randneigung = case_when(
    erh_r < 5 ~ NA,
    TRUE ~ randneigung) )|>
  mutate(across(c(`Hals-Schulter-Umbruch`:`Schulter-Bauch-Boden`, randabschluss_gr, randabschluss_f), as_factor)) |>
  mutate(across(randform_nonek:bauchform_nonek, as_factor)) |> 
  select(-c(erh_r, erh_s, erh_b, erh_bod, sicherheit_orient, rand_halsband, x_scherben, n_anspassungen, gef_typ, gef_typ_gr, gef_untertyp, bem, vgl, durchm_b:durchm_bod) )

oberteile3 <- oberteile2 |>
  select(-c(h_insg, h_unterteil, bauchform_nonek, `Schulter-Bauch-Umbruch`, `Bauch-Boden`:Fuß, `Schulter-Bauch-Boden`, verh_durchm_r_h_insg, verh_durchm_r_b, verh_durchm_s_b, verh_durchm_r_bod, verh_durchm_b_h_insg, verh_o_u) ) # alles Infos, die bei Randscherben selten sind -> nur noch Infos zum Thema Rand und Schulter behalten

oberteile2_og <- oberteile3 |>
  select(-c(gefäßnummer, FO:kultur_gr) )

gefform_info <- oberteile3 |>
  select(gefäßnummer, FO, kreis, kreislabel, gemarkung, land)

gefform_info$rn <- as.character(rownames(gefform_info))

oberteile2_og <- oberteile2_og[,colSums(is.na(oberteile2_og)) < nrow(oberteile2_og)] # find column that is empty ->  doch nix raus ?

data_gower <- gowdis(oberteile2_og, ord = "podani") ## Problem: kreiiert NAs! 

library(igraph)

t <- graph.adjacency(as.matrix(data_gower), weighted=TRUE) # aus distanzmatrix adjecency matrix für igraph

gefform_df <- get.data.frame(t) # daraus dataframe 

# 1. Verknüpfen mit Fundort
# 2. Fundorte aufsummieren nach beiden FO
# 3. plotten von Linien ?

gefform_site <- left_join(gefform_df, gefform_info, by = c("from" = "rn"))

colnames(gefform_site) <- c("from","to","weight","gefäßnummer_from", "FO_from", "kreis_from","kreislabel_from" , "gemarkung_from" ,  "land_from")

gefform_site <- left_join(gefform_site, gefform_info, by = c("to" = "rn"))

gefform_site_s <- gefform_site |>
  filter(!is.na(weight)) |>
  group_by(FO_from, FO) |>
  mutate(sum_w = sum(weight) ) |>
  group_by(FO_from , FO, sum_w) |>
  summarise(n = n() ) |>
  filter(n > 0) |>
  mutate(sum_w_s = sum_w/n) # aufsummieren der Beziehung von einem FPL zum anderen nach Kultur und teilen durch Anzahl d Beziehungen

gefform_site_s2 <- gefform_site_s |>
  select(FO_from, FO, sum_w_s ) |>
  unique()|>
  ungroup() |>
  unique()

#### nur SBK und SRK

sites_SBK_SRK <- sites_all |>
  filter(KULTUR %in% c("SBK", "SRK", "SBK?", "SRK?") )

gefform_site3 <- left_join(gefform_site_s2, sites_all, by = c("FO_from" = "FST"))

gefform_site3 <- gefform_site3 |>
  select(FO_from, FO, sum_w_s, geometry_from = geometry)

gefform_site3 <- left_join(gefform_site3, sites_SBK_SRK, by = c("FO" = "FST"))
gefform_site3$rn <- rownames(gefform_site3)

## rumgegurke die x und y aus den Koordiaten zu ziehen für die Striche zwischen den Punkten. Scheint die einzige Möglichkeit zu sein? Bin konsterniert...
xy_from <- as.data.frame(st_coordinates(gefform_site3$geometry_from))
xy_from$rn <- rownames(xy_from)

gefform_site3_xy <- left_join(gefform_site3, xy_from)

xy_to <- as.data.frame(st_coordinates(gefform_site3$geometry))
xy_to$rn <- rownames(xy_to)

gefform_site3_xy <- left_join(gefform_site3_xy, xy_to, by = "rn")


gefform_site3_xy <- gefform_site3_xy |>
  filter(!is.na(sum_w_s))

gefform_site3_xy$S <- 1 - gefform_site3_xy$sum_w_s

save(gefform_site3_xy, file = "./analysis/data/derived_data/gefform_distance_site_to_site_xy.RData")

label <- unique(gefform_site3_xy[,c("FO", "geometry")])
label2 <- unique(gefform_site3_xy[,c("FO_from", "geometry_from")])

label2 <- label2 |>
  group_by(FO_from) |>
  slice_sample()

label2 <- st_as_sf(label2)


ggplot() +
  theme_bw()+
  geom_sf(data = admin_2, colour = "darkgrey", fill = "white")+
  geom_sf(data = water3, size = 2, colour = "lightblue3" ) +
  geom_sf(data = sites_SBK_SRK, size = 1, shape = 18, colour = "grey70")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_spatial_segment(data = gefform_site3_xy ,
                       aes(x=X.x,
                           y=Y.x,
                           xend=X.y,
                           yend=Y.y,
                           color = S,
                           alpha = S),
                       crs = st_crs(sites_SBK_SRK) ) +
  scale_alpha_continuous(name = "mittlere \nÄhnlichkeit")+
  scale_colour_gradient(name = "mittlere \nÄhnlichkeit",
                        low = "grey",
                        high = "red")+
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  ggrepel::geom_label_repel(data = label2, 
                            aes(label = FO_from, 
                                geometry = geometry_from),
                            fill = alpha(c("white"),0.5),
                            size = 1.5,
                            max.overlaps = 20,
                            label.padding = 0.1,
                            stat = "sf_coordinates",
                            col = "black" )+
  labs(title = "Gemittelte Ähnlichkeit zwischen den Fundstellen",
       subtitle = "anhand der Form der Gefäße",
       caption =paste("Ähnlichkeit = 1 - mittlere Gowerdistanz (Summe der Distanzmaße zwischen den Gefäßen der Fundstellen geteilt durch Anzahl Vergleiche)\n anhand n =", nrow(oberteile2_og), "Gefäßen von", unique(length(label$FO)), "Fundstellen"),
       x = "",
       y = "")+
  theme(legend.key = element_rect(fill = "white",
                                  colour = "white",
                                  linewidth = 0.5, linetype = "solid"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linewidth = 0.5, linetype = "solid")) +
  coord_sf(xlim = c(10.5,20), ylim = c(50.5, 53.5))

ggsave("./analysis/figures/Gef_Form/Karte_mittlere_Gowerähnlichkeit_Gefoberteil_label.png", dpi = 300, width = 24, height = 16, units = "cm")


# 2. art der darstellung: heatmap

gefform_site3_xy$FO_from <- factor(gefform_site3_xy$FO_from, ordered = T,
                                   levels = c( "Eythra" ,"Quedlinburg KGA 1", "Quedlinburg KGA 2",  "Prettin 6", "Bochow 15", "Bochow 2" ,  "Bochow 6/7",  "Kaden 4" ,  "Zöllmersdorf" , "Uhyst 13",  "Uhyst 19"  , "Uhyst 20",
                                               "Schöpsdorf 37",
                                               "Heinersbrück 42"  ,
                                               "Wierzchno/Wirchenblatt",
                                               "Neuendorf 23",
                                               "Klein-Rietz" ,
                                               "Seelow 20"  ,
                                               "Niederjesar 3" ,
                                               "Nowe Objezierze 7" ,
                                               "Karsko/Schöningsburg" ,
                                               "Niederguhren/Kije",
                                               "Pyritz – Weinberg" ,
                                               "Iwno",
                                               "Racot 18" ,
                                               "Kruszynek 6"  ))

gefform_site3_xy$FO <- factor(gefform_site3_xy$FO, ordered = T,
                              levels = levels(gefform_site3_xy$FO_from))

gefform_site3_xy |>
  ggplot()+
  geom_tile(aes(x = FO_from,
                y = FO,
                fill = S ))+
  geom_text(aes(x = FO_from,
                y = FO,
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./analysis/figures/Gef_Form/mittlere_Gowerähnlichkeit_Gefoberteil_heatmap.png", dpi = 300, width = 24, height = 16, units = "cm")


gefform_site3_xy |>
  ggplot()+
  geom_boxplot(aes(x = S) )



########################## Rössen, Guhrau, BKK und SRK


oberteile <- gef_umbruch |>
  filter(erh_r > 4) |>
  filter(kultur2 %in% c("SRK", "BKK", "Guhrau", "Rös")) |>
  select(-c(Bef) ) |>
  unique()


# check oberteilinfos nur dann, wenn rand/schulter tatsächlich vorhanden
oberteile2 <- oberteile |>
  mutate(h_hals = case_when(
    erh_s < 1 ~ NA,
    TRUE ~ h_hals)) |>
  mutate(durchm_r = case_when(
    erh_r < 5 ~ NA,
    TRUE ~ durchm_r)) |>
  mutate(randneigung = case_when(
    erh_r < 5 ~ NA,
    TRUE ~ randneigung) )|>
  mutate(across(c(`Hals-Schulter-Umbruch`:`Schulter-Bauch-Boden`, randabschluss_gr, randabschluss_f), as_factor)) |>
  mutate(across(randform_nonek:bauchform_nonek, as_factor)) |> 
  select(-c(erh_r, erh_s, erh_b, erh_bod, sicherheit_orient, rand_halsband, x_scherben, n_anspassungen, gef_typ, gef_typ_gr, gef_untertyp, bem, vgl, durchm_b:durchm_bod) )

oberteile3 <- oberteile2 |>
  select(-c(h_insg, h_unterteil, bauchform_nonek, `Schulter-Bauch-Umbruch`, `Bauch-Boden`:Fuß, `Schulter-Bauch-Boden`, verh_durchm_r_h_insg, verh_durchm_r_b, verh_durchm_s_b, verh_durchm_r_bod, verh_durchm_b_h_insg, verh_o_u) ) # alles Infos, die bei Randscherben selten sind -> nur noch Infos zum Thema Rand und Schulter behalten

oberteile2_og <- oberteile3 |>
  select(-c(gefäßnummer, FO:kultur_gr) )

gefform_info <- oberteile3 |>
  select(gefäßnummer, FO, kreis, kreislabel, gemarkung, land, kultur2)

gefform_info$rn <- as.character(rownames(gefform_info))

oberteile2_og <- oberteile2_og[,colSums(is.na(oberteile2_og)) < nrow(oberteile2_og)] # find column that is empty ->  doch nix raus ?

data_gower <- gowdis(oberteile2_og, ord = "podani") ## Problem: kreiiert NAs! 

library(igraph)

t <- graph.adjacency(as.matrix(data_gower), weighted=TRUE) # aus distanzmatrix adjecency matrix für igraph

gefform_df <- get.data.frame(t) # daraus dataframe 

# 1. Verknüpfen mit Fundort
# 2. Fundorte aufsummieren nach beiden FO
# 3. plotten von Linien ?

gefform_site <- left_join(gefform_df, gefform_info, by = c("from" = "rn"))

colnames(gefform_site) <- c("from","to","weight","gefäßnummer_from", "FO_from", "kreis_from","kreislabel_from" , "gemarkung_from" ,  "land_from", "kultur2_from")

gefform_site <- left_join(gefform_site, gefform_info, by = c("to" = "rn"))

gefform_site_s <- gefform_site |>
  filter(!is.na(weight)) |>
  group_by(FO_from, FO, kultur2_from, kultur2) |>
  mutate(sum_w = sum(weight) ) |>
  group_by(FO_from , FO, sum_w, kultur2_from, kultur2) |>
  summarise(n = n() ) |>
  filter(n > 0) |>
  mutate(sum_w_s = sum_w/n) # aufsummieren der Beziehung von einem FPL zum anderen nach Kultur und teilen durch Anzahl d Beziehungen

gefform_site_s2 <- gefform_site_s |>
  select(FO_from, FO, sum_w_s,  kultur2_from, kultur2 ) |>
  unique()|>
  ungroup() |>
  unique()

gefform_site_s2$S <- 1 - gefform_site_s2$sum_w_s

save(gefform_site_s2, file = "./analysis/data/derived_data/gefform_distance_site_to_site_RGBxy.RData")


#mean(gefform_site_s2$S)

# 2. art der darstellung: heatmap

fst_per_k <- gefform_site_s2 |>
  group_by(kultur2) |> summarize(count = n_distinct(FO))

fst_per_k_from <- gefform_site_s2 |>
  group_by(kultur2_from) |> summarize(count = n_distinct(FO_from))


gefform_site_s2 |>
  ggplot()+
  geom_tile(aes(x = FO_from,
                y = FO,
                fill = S ))+
  geom_text(aes(x = FO_from,
                y = FO,
                label = round(S, 2) ), 
            size = 2.1,
            col = "white")+
  scale_fill_gradient(low = "grey",
                      high = "red")+
  labs(fill = "Ähnlichkeit",
       x = " ",
       y = " ",
       caption = " ")+
  theme_bw()+
  facet_grid(kultur2 ~ kultur2_from, scales = "free") +
  ggh4x::force_panelsizes(rows = fst_per_k$count,
                          cols = fst_per_k_from$count)+
  theme(axis.text.x = element_text(angle = 325 , hjust = 0))

ggsave("./analysis/figures/Gef_Form/mittlere_Gowerähnlichkeit_GBR_Gefoberteil_heatmap.png", dpi = 300, width = 26, height = 16, units = "cm")


