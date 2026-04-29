#### Analyse der dominanten Lithologie

# 1. Daten
# 2. plotten

# Adonistest lohnt nicht

### daten laden

load(file = "./analysis/data/derived_data/sites.RData")


domlil500 <- read.csv2("./analysis/data/geodata/standortanalyse_500_BB/domlil.csv", sep = ",", dec = ".")

domlil250 <- read.csv2("./analysis/data/geodata/standortanalyse_250_BB/domlil.csv", sep = ",", dec = ".")

domlil250$r <- 250

domlil500$r <- 500

domlil1000 <- read.csv2("./analysis/data/geodata/standortanalyse_1000_BB/domlil.csv", sep = ",", dec = ".")

domlil1000$r <- 1000

domlil2000 <- read.csv2("./analysis/data/geodata/standortanalyse_2000_BB/domlil.csv", sep = ",", dec = ".")

domlil2000$r <- 2000


domlil <- rbind(domlil250, domlil500, domlil1000, domlil2000)

unique(domlil$obj)

domlil$id <- as.character(domlil$id)

standort_sites <- left_join(domlil, sites, by = c("id" = "FID") )

bredow_roes <- standort_sites |> filter(id == "3007")

bredow_roes$id <- "3007.2"
bredow_roes$kultur2 <- "Rös"
bredow_roes$KULTUR<- "Rössener"
bredow_roes$Fundart <- ""
bredow_roes$QUELLE <- "Kirsch 1993 / (c) Denkmaldaten BLDAM 2021"

standort_sites <- rbind(standort_sites, bredow_roes)

standort_sites$kultur2[standort_sites$FST == "Potsdam 16"] <- "Rös"
standort_sites$FST[standort_sites$FST == "Potsdam 16"] <- "Potsdam 3/58 (16)"


potsdam_sbk <- standort_sites |> filter (FST == "Potsdam 3/13 (3)")
potsdam_sbk$id <- "265.2"
potsdam_sbk$KULTUR <- "Stichbandkeramik"
potsdam_sbk$kultur2 <- "SBK"
potsdam_sbk$Fundart <- "Lesefund"

standort_sites <- rbind(standort_sites, potsdam_sbk)

### 2. plotten
# gefiltert auf die, wo Analyse sich lohnt

fo_per_kr <- standort_sites |> 
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |>
  # filter(kultur2 %in% c("SBK", "Rös", "LBK Siedl.")) |>
  filter(!is.na(kultur2)) |>
  group_by(kreislabel, kultur2, r) |>
  summarize(count = n_distinct(FST)) |>
  ungroup()

k_per_kreis <-  standort_sites |>    
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |>
  #  filter(kultur2 %in% c("SBK", "Rös", "LBK Siedl.")) |>
  filter(!is.na(kultur2)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()


x <- standort_sites |>
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |>
  #  filter(kultur2 %in% c("SBK", "Rös", "LBK Siedl.")) |>
  filter(!is.na(FST)) |>
  filter(kultur2 != "<NA>")  |>
  group_by(kreislabel, obj, kultur2, r) |>
  summarise(sum_p_area = sum(area.) ) |>
  ungroup()



x |>
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |> # nur die, wo lohnt
  ggplot(na.rm = TRUE)+
  geom_bar(aes(y = sum_p_area,
               x = kultur2,
               fill = obj),
           position = "fill", stat = "identity")+
  labs(title = "Verteilung der Fundstellen auf Klassen dominanter Sedimente",
       x = "",
       y = "Anteil von 500 m Radius",
       fill = "",
       caption = "Datengrundlage: Mittelmaßstäbige Landwirtschaftliche Standortkartierung \nQuelle: © Landesamt für Bergbau, Geologie und Rohstoffe Brandenburg, dl-de/by-2-0")+
  geom_text(data = fo_per_kr |> filter(r == 250),
            aes(x = kultur2,
                y = 0,
                label = count),
            colour = "black",
            hjust = 1,
            size = 3) +
  coord_flip()+
  scale_fill_manual(na.value = "grey80",
                    values = c("Lh" = "coral4",
                               "Ls" = "yellow",
                               "Lm" = "lightgreen",
                               "Lt" = "brown",
                               "Ll" = "darkorange",
                               "su" = "darkblue",
                               "Lu" = "slateblue2",
                               "sö" = "seagreen",
                               "sd" = "lightblue",
                               "Lf" = "black"),
                    labels = c( "Lh" = "Torf",
                                "Ls" = "Sand",
                                "Lm" = "Mergel (auch Geschiebemergel",
                                "Lt" = "Ton",
                                "Ll" = "Lehm",
                                "su" = "schluffiger (lehmiger) Treibsand",
                                "Lu" = "Schluff",
                                "sö" = "Sandlöss",
                                "sd" = "Dünensand",
                                "Lf" = "Mudde") )+
  facet_grid(kreislabel ~ r, scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kreis$count ) +
  theme_bw()+
  guides(fill = guide_legend(nrow = 2 ))+
  theme(text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12))

ggsave("./analysis/figures/Standortanalyse/MMK_domlil_250-2000_ausw_kreis.png", dpi = 300, width = 15, height = 10)
