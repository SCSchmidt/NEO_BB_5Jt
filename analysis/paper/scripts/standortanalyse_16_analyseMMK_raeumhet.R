## Räumliche Heterogenität

raeumhet <- read.csv2("/analysis/data/geodata/standortanalyse_500_BB/raeumhet.csv", sep = ",", dec = ".")

load(file = "./analysis/data/derived_data/sites.RData")


## Korrektur Nachtrag
raeumhet$id <- as.character(raeumhet$id)

standort_sites <- left_join(raeumhet, sites, by = c("id" = "FID") )

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


### plot!

fo_per_kr <- standort_sites |> 
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |>
  filter(!is.na(kultur2)) |>
  group_by(kreislabel, kultur2) |>
  summarize(count = n_distinct(FST)) |>
  ungroup()

k_per_kreis <-  standort_sites |>
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |>
  filter(!is.na(kultur2)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()


x <- standort_sites |>
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |>
  filter(!is.na(FST)) |>
  filter(kultur2 != "<NA>")  |>
  group_by(kreislabel, obj, kultur2) |>
  summarise(sum_p_area = sum(area.) )

x$obj[x$obj == ""] <- NA

x$obj <- factor(x$obj, ordered = T,
                levels = c("k", "m", "g"))

x |>
  filter(kreislabel %in% c("TF", "HVL", "MOL", "UM", "EE", "DSW", "LOS", "OH", "OPR" )) |>
  ggplot(na.rm = TRUE)+
  geom_bar(aes(y = sum_p_area,
               x = kultur2,
               fill = as.factor(obj) ),
           position = "fill", stat = "identity")+
  labs(title = "Verteilung der Fundstellen auf räumliche Heterogenitätsklassen",
       x = "",
       y = "Anteil von 500 m Radius",
       fill = "",
       caption = "Datengrundlage: Mittelmaßstäbige Landwirtschaftliche Standortkartierung \nQuelle: © Landesamt für Bergbau, Geologie und Rohstoffe Brandenburg, dl-de/by-2-0")+
  geom_text(data = fo_per_kr,
            aes(x = kultur2,
                y = 0,
                label = count),
            colour = "black",
            hjust = 1,
            size = 3) +
  scale_fill_manual(labels = c( "k" = "kleinflächige Verteilung",
                                "m" = "mittelflächige Verteilung",
                                "g" = "großflächige Verteilung") ,
                    values = viridis(3),
                    na.value = "grey") +
  coord_flip()+
  facet_grid(kreislabel ~ ., scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kreis$count)+
  theme_bw()+
  guides(fill = guide_legend(nrow = 4 ))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14))

ggsave("./analysis/figures/Standortanalyse/MMK_räuml_hetero_500_BB_auswahl.png", dpi = 300, height = 20, width = 30, units = "cm")