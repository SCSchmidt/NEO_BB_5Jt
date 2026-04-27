### Keramik Machart: Wandungsstärken

## Wandungsstärke data wrangling
library(reshape2)

scherben_d <- melt(scherben, id.vars = c('fotonr', 'gefäßnummer', 'kultur2', 'typ', "verz_n")  , measure.vars = c('max_dicke', 'min_dicke'), value.name = c('dicke')) 

scherben_d <- scherben_d |>
  mutate(typ_gr = case_when(
    typ == "fast komplettes Profil" ~ "Rand bis Boden",
    typ == "komplettes Profil" ~ "Rand bis Boden",
    typ == "rekonstruiertes Gefäß" ~ "Rand bis Boden",
    typ == "rekonstriertes Gefäß" ~ "Rand bis Boden",
    typ == "Rand- bis Bodenscherbe" ~ "Rand bis Boden",
    typ == "Boden- und Wandscherbe" ~ "Unterteilscherbe",
    typ == "Boden- oder Wandscherbe" ~ "Unterteilscherbe",
    typ == "Boden-oder Wandscherbe" ~ "Unterteilscherbe",
    typ == "Wand- und Bodenscherbe" ~ "Unterteilscherbe",
    typ == "Wand- oder Bodenscherbe" ~ "Unterteilscherbe",
    typ == "Wand-Bodenscherbe" ~ "Unterteilscherbe",
    typ == "Rand- und Wandscherbe" ~ "Oberteilscherbe",
    typ == "Rand- oder Wandscherbe" ~ "Oberteilscherbe",
    typ == "Henkelscherbe" ~ "Wandscherbe",
    typ == "NA" ~ NA,
    TRUE ~ typ
  )) |>
  mutate(typ_grgr = case_when(
    str_detect(typ_gr, "Boden") ~ "Boden",
    str_detect(typ, "Gefäß|Profil") ~ "gesamtes Gefäß",
    TRUE ~ "Wandung")  )

dicke.labs <- c("max. Dicke", "min. Dicke")
names(dicke.labs) <- c("max_dicke", "min_dicke")            


# gruppieren nach Gefäßnummer und variable -> max oder min dicke
# da wo min dicke -> min wert
# da wo max dicke -> max wert

scherben_d_max <- scherben_d |>
  filter(variable == "max_dicke") |>
  group_by(gefäßnummer, typ_grgr) |>
  slice_max(dicke) |>
  select(gefäßnummer, typ_grgr, verz_n, dicke, kultur2, variable) |>
  ungroup() |>
  unique()

scherben_d_min <- scherben_d |>
  filter(variable == "min_dicke") |>
  group_by(gefäßnummer, typ_grgr) |>
  slice_min(dicke) |>
  select(gefäßnummer, typ_grgr, verz_n, dicke, kultur2, variable) |>
  ungroup() |>
  unique()

scherben_gef_d <- rbind(scherben_d_max, scherben_d_min)

## Wandungsstärke plotten

scherben_gef_d |>
  filter(dicke > 0) |>
  unique() |>
  ggplot(aes(x = kultur2,
             y = dicke,
             fill = verz_n))+
  geom_jitter(aes(col = typ_grgr,
                  shape = verz_n),
              size = 2,
              alpha = 0.5,
              height = 0.2,
              width = 0.3)+
  geom_boxplot(alpha = 0.7)+
  scale_fill_manual(values = c("white", "lightgrey"),
                    labels = c("unverziertes Gef.", "verziertes Gef" ) )+
  scale_shape_manual( breaks = c("unverz", "verz"), 
                      labels =  c("unverziertes Gef.", "verziertes Gef" ),
                      values = c(19, 4))+
  scale_colour_manual(values = c(19, 4, 2)) +
  coord_flip()+
  theme_bw()+
  labs(x = "",
       fill = "",
       y = "Scherbenstärke (mm)",
       col = "",
       shape = "",
       title = "Wandungsstärke",
       caption = paste("Scherben von", length(unique(scherben_gef_d$gefäßnummer)), "Gefäßen")) +
  facet_grid(.~variable,
             labeller = as_labeller(dicke.labs))

ggsave("./analysis/figures/Magerung/Wandungsstärke.png", dpi = 300, width = 16, height = 16, units = "cm")


### Variabilität der Wandungsdicke an einem Gefäß


scherben_d3 <- scherben |>
  filter(! gefäßnummer %in% scherben$gefäßnummer[is.na(scherben$min_dicke)])  |>
  filter(! gefäßnummer %in% scherben$gefäßnummer[is.na(scherben$max_dicke)] ) |>
  select(fotonr, gefäßnummer, kultur2, typ, verz_n, min_dicke, max_dicke, max_l) |>
  mutate(typ_gr = case_when(
    typ == "fast komplettes Profil" ~ "Rand bis Boden",
    typ == "komplettes Profil" ~ "Rand bis Boden",
    typ == "rekonstruiertes Gefäß" ~ "Rand bis Boden",
    typ == "rekonstriertes Gefäß" ~ "Rand bis Boden",
    typ == "Rand- bis Bodenscherbe" ~ "Rand bis Boden",
    typ == "Boden- und Wandscherbe" ~ "Unterteilscherbe",
    typ == "Boden- oder Wandscherbe" ~ "Unterteilscherbe",
    typ == "Boden-oder Wandscherbe" ~ "Unterteilscherbe",
    typ == "Wand- und Bodenscherbe" ~ "Unterteilscherbe",
    typ == "Wand- oder Bodenscherbe" ~ "Unterteilscherbe",
    typ == "Wand-Bodenscherbe" ~ "Unterteilscherbe",
    typ == "Rand- und Wandscherbe" ~ "Oberteilscherbe",
    typ == "Rand- oder Wandscherbe" ~ "Oberteilscherbe",
    typ == "Henkelscherbe" ~ "Wandscherbe",
    typ == "NA" ~ NA,
    TRUE ~ typ )) |>
  mutate(typ_grgr = case_when(
    str_detect(typ_gr, "Boden") ~ "Boden",
    str_detect(typ, "Gefäß|Profil") ~ "gesamtes Gefäß",
    TRUE ~ "Wandung")  ) |>
  filter(typ_grgr == "Wandung") |>
  group_by(gefäßnummer) |>
  slice_max(max_dicke) |>
  slice_min(min_dicke) |>
  unique() |> 
  mutate(diff_dicke = abs(max_dicke - min_dicke) ) # Problem schon, je nachdem wie groß die Scherbe, mehr Chance für Variabilität, aber durch Scherbengröße teilen ist schwierig

## plot Variabilität der Wandungsdicke

scherben_d3 |>
  ggplot(aes(x = kultur2,
             y = diff_dicke))+
  geom_jitter(aes(shape = verz_n),
              size = 2,
              alpha = 0.3,
              height = 0.2,
              width = 0.3)+
  geom_boxplot(aes(fill = verz_n),
               alpha = 0.7)+
  scale_fill_manual(values = c("white", "lightgrey"),
                    labels = c("unverziertes Gef.", "verziertes Gef." ) )+
  scale_shape_manual( breaks = c("unverz", "verz"), 
                      labels =  c("unverziertes Gef.", "verziertes Gef." ),
                      values = c(19, 4))+
  scale_colour_manual(values = c(19, 4, 2)) +
  coord_flip()+
  theme_bw()+
  labs(x = "",
       fill = "",
       y = "Unterschied der Dicke innerhalb der Scherbe (mm)",
       col = "",
       shape = "",
       title = "Variabilität der Wandungsdicken",
       caption = paste("Scherben von", length(unique(scherben_d3$gefäßnummer)), "Gefäßen, ohne Boden"))

#ggsave("./analysis/figures/Magerung/Wandungsdicke_Variabilität.png")