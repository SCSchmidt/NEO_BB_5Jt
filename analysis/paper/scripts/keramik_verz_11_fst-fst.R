### Verzierung zwischen Fst - mittlere Ochiai


### Verzierung

library(igraph)
library(dplyr)
library(sf)

sites_all <- read.csv2("./analysis/data/geodata/sites_fst-analyse2_tab.csv")


load("./analysis/data/geodata/admin.RData")
load("./analysis/data/geodata/water3.RData")

# 3 Fst ohne Koordinaten? quick workaround:
sites_all <- sites_all |> filter(!is.na(X))

sites_all <- st_as_sf(sites_all, coords = c("X", "Y"), crs = 25833)


#Gower-Distanz für Technik Element alle geladen
load("./analysis/data/derived_data/gower_distanz_technik-element.RData")

load( "./analysis/data/derived_data/technik-element_grundlage.RData")

technik_element$rn <- as.character(row_number(technik_element))

technik_element_info <- technik_element |> 
  select(gefäßnummer, FO, kreis, kultur2, rn)

t <- graph.adjacency(as.matrix(data_gower), weighted=TRUE) # aus distanzmatrix adjecency matrix für igraph

verz_df <- get.data.frame(t) # daraus dataframe 

# 1. Verknüpfen mit Fundort
# 2. Fundorte aufsummieren nach beiden FO
# 3. plotten von Linien ?

verz_site <- left_join(verz_df, technik_element_info, by = c("from" = "rn"))

colnames(verz_site) <- c("from","to","weight","gefäßnummer_from", "FO_from", "kreis_from", "kultur2_from")

verz_site <- left_join(verz_site, technik_element_info, by = c("to" = "rn"))

verz_site_s <- verz_site |>
  filter(kultur2_from %in% c("SBK", "SRK"),
         kultur2 %in% c("SBK", "SRK"),
         !is.na(weight)) |>
  group_by(FO_from, FO) |>
  mutate(sum_w = sum(weight) ) |>
  filter(sum_w > 0) |>
  group_by(FO_from , FO, sum_w) |>
  summarise(n = n() ) |>
  filter(n > 0) |>
  mutate(sum_w_s = sum_w/n) # aufsummieren der Beziehung von einem FPL zum anderen nach Kultur und teilen durch Anzahl d Beziehungen

verz_site_s2 <- verz_site_s |>
  select(FO_from, FO, sum_w_s ) |>
  unique()|>
  ungroup() |>
  unique()

#### SBK und SRK

sites_SBK_SRK <- sites_all |>
  filter(KULTUR %in% c("SBK", "SRK", "SBK?", "SRK?") )# was is mit("Flemsdorf 10", "Kruszynek 6", "Bochow 2") 

verz_site3 <- left_join(verz_site_s2, sites_all, by = c("FO_from" = "FST"))

verz_site3 <- verz_site3 |>
  select(FO_from, FO, sum_w_s, geometry_from = geometry)

verz_site3 <- left_join(verz_site3, sites_all, by = c("FO" = "FST"))
verz_site3$rn <- rownames(verz_site3)

# irgendwas mehrmals?

## rumgegurke die x und y aus den Koordiaten zu ziehen für die Striche zwischen den Punkten. Scheint die einzige Möglichkeit zu sein? 
xy_from <- as.data.frame(st_coordinates(verz_site3$geometry_from))
xy_from$rn <- rownames(xy_from)

verz_site3_xy <- left_join(verz_site3, xy_from)

xy_to <- as.data.frame(st_coordinates(verz_site3$geometry))
xy_to$rn <- rownames(xy_to)

verz_site3_xy <- left_join(verz_site3_xy, xy_to, by = "rn")

### üöpt Karte
verz_site3_xy <- verz_site3_xy |>
  filter(!is.na(sum_w_s))

verz_site3_xy$S <- 1- verz_site3_xy$sum_w_s # Umwandlung in Ähnlichkeitsmaß!

save(verz_site3_xy, file = "./analysis/data/derived_data/verz_distance_site_to_site_xy.RData")


label <- unique(verz_site3_xy[,c("FO", "geometry")])
label2 <- unique(verz_site3_xy[,c("FO_from", "geometry_from")])

label2 <- st_as_sf(label2)

label2 <- label2 |> 
  group_by(FO_from) |> slice_sample() # hässliche dopplung raus (wiesoauchimmerdiedrinist)

#library(ggpointgrid)

# gib mal dichteverteilung der S similarity -Werte um zu gucken, wo filtern
# -> weil zu viel dargestellt und geringe Werte sagen nix aus

b <- mean(verz_site3_xy$S, na.rm = T)/2
## -> filtert unter 0,27 r

ggplot() +
  theme_bw()+
  geom_sf(data = admin_2, colour = "darkgrey", fill = "white")+
  geom_sf(data = water3, size = 2, colour = "lightblue3" ) +
  geom_sf(data = sites_all, size = 1, shape = 18, colour = "grey")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_spatial_segment(data = verz_site3_xy |> 
                         filter(S > b), 
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
                            max.overlaps = 20,
                            label.padding = 0.1,
                            stat = "sf_coordinates",
                            col = "black" )+
  scale_colour_gradient(name = "mittlere \nÄhnlichkeit",
                        low = "grey80",
                        high = "red")+
  scale_alpha_continuous(name = "mittlere \nÄhnlichkeit")+
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Gemittelte Ähnlichkeit zwischen den Fundstellen",
       subtitle = "anhand der Verzierungstechniken und -elemente der Gefäße",
       caption = paste("Ähnlichkeit als 1 - mittlere Ochiaidistanz (Summe der Distanzmaße zwischen den Gefäßen der Fundstellen\n geteilt durch Anzahl Verbindungen), gefiltert auf Ähnlichkeiten >", round(b,2), ", anhand n =", length(unique(technik_element_info$gefäßnummer) ), "Gefäßen von", unique(length(label$FO)), "Fundstellen"),
       x = "",
       y = "")+
  theme(legend.key = element_rect(fill = "white",
                                  colour = "white",
                                  linewidth = 0.5, linetype = "solid"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linewidth = 0.5, linetype = "solid")) +
  coord_sf(xlim = c(10.5,20), ylim = c(50.5, 53.5))

ggsave("./analysis/figures/Muster/Karte_mittlere_Ochiaiähnlichkeit_Verzierung-label.png", dpi = 300, width = 21, height = 16, units = "cm")



verz_site3_xy |>
  ggplot()+
  geom_boxplot(aes(x = S))


## mean(verz_site3_xy$S, na.rm = T)


verz_site3_xy$FO_from <- factor(verz_site3_xy$FO_from, ordered = T,
                                levels = c( "Eythra" ,"Quedlinburg KGA 1", "Quedlinburg KGA 2",  "Prettin 6", "Drebkau 2", "Rohrbeck 14"  , "Bochow 15", "Bochow 16", "Bochow 2" ,  "Bochow 6/7", "Jüterbog 31" ,   "Schiaß 1"  , "Kaden 4" ,  "Zöllmersdorf" , "Uhyst 13",  "Uhyst 19"  , "Uhyst 20",
                                            "Schöpsdorf 37",
                                            "Heinersbrück 42"  ,
                                            "Wierzchno/Wirchenblatt",
                                            "Neuendorf 23",
                                            "Klein-Rietz" ,
                                            "Seelow 20",
                                            "Platkow 13", 
                                            "Flemsdorf 10", "Gartz ?",
                                            "Berkholz zu Berkholz-Meyenburg 5", "Gramzow 23", "Hohenbrück 3",
                                            "Niederjesar 3" ,
                                            "Nowe Objezierze 7" ,
                                            "Karsko/Schöningsburg" ,
                                            "Niederguhren/Kije",
                                            "Pyritz – Weinberg" ,
                                            "Iwno",
                                            "Racot 18" ,
                                            "Kruszynek 6"  ))

verz_site3_xy$FO <- factor(verz_site3_xy$FO, ordered = T,
                           levels = levels(verz_site3_xy$FO_from))

verz_site3_xy |>
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

ggsave("./analysis/figures/Muster/mittlere_Gowerähnlichkeit_technik-element_heatmap.png", dpi = 300, width = 26, height = 16, units = "cm")




################# Rössen, Guhrau, BKK und SRK


verz_site_s <- verz_site |>
  filter(kultur2_from %in% c("BKK", "SRK", "Rössen", "Guhrau"),
         kultur2 %in% c("BKK", "SRK", "Rössen", "Guhrau"),
         !is.na(weight)) |>
  group_by(FO_from, FO, kultur2_from, kultur2) |>
  mutate(sum_w = sum(weight) ) |>
  group_by(FO_from , FO, sum_w, kultur2_from, kultur2) |>
  summarise(n = n() ) |>
  filter(n > 0) |>
  mutate(sum_w_s = sum_w/n) # aufsummieren der Beziehung von einem FPL zum anderen nach Kultur und teilen durch Anzahl d Beziehungen

verz_site_s2 <- verz_site_s |>
  select(FO_from, FO, sum_w_s, kultur2, kultur2_from ) |>
  unique()|>
  ungroup() |>
  unique()

verz_site_s2$S <- 1 - verz_site_s2$sum_w_s

save(verz_site_s2, file = "./analysis/data/derived_data/verz_distance_site_to_site_RGBxy.RData")


mean(verz_site_s2$S)

fst_per_k <- verz_site_s2 |>
  group_by(kultur2) |> summarize(count = n_distinct(FO))

fst_per_k_from <- verz_site_s2 |>
  group_by(kultur2_from) |> summarize(count = n_distinct(FO_from))


verz_site_s2 |>
  ggplot()+
  geom_tile(aes(x = FO_from,
                y = FO,
                fill = S ))+
  geom_text(aes(x = FO_from,
                y = FO,
                label = round(S, 2) ), 
            size = 2,
            col = "white")+
  scale_fill_gradient(low = "grey",
                      high = "red")+
  labs(fill = "S",
       x = " ",
       y = " ",
       caption = "S als Ähnlichkeit 1 - mittlere Gowerdistanz der Verzierungstechniken und -elemente")+
  theme_bw()+
  facet_grid(kultur2 ~ kultur2_from, scales = "free") +
  ggh4x::force_panelsizes(rows = fst_per_k$count,
                          cols = fst_per_k_from$count)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./analysis/figures/Muster/mittlere_Gowerähnlichkeit_RGB_technik_element_heatmap.png", dpi = 300, width = 34, height = 40, units = "cm")
