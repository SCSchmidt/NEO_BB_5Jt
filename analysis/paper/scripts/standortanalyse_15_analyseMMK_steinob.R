# Steinigkeit des Oberbodens (Daten MMK)

# steinigkeit daten laden und bereinigen
steinob <- read.csv2("./analysis/data/geodata/standortanalyse_500_BB/steinob.csv", sep = ",", dec = ".")

load(file = "./analysis/data/derived_data/sites.RData")

# unique(steinob$obj)

steinob$id <- as.character(steinob$id)

standort_sites <- left_join(steinob, sites, by = c("id" = "FID") )

standort_sites$obj[standort_sites$obj == ""] <- NA
standort_sites$obj[standort_sites$obj == "--"] <- NA
standort_sites$obj[standort_sites$obj == " "] <- NA


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

fo_per_kr <- standort_sites |> 
  filter(kreislabel %in% c("TF", "MOL", "UM","DSW", "LOS", "OH", "PM", "PR")) |>
  filter(!is.na(kultur2)) |>
  group_by(kreislabel, kultur2) |>
  summarize(count = n_distinct(FST)) |>
  ungroup()

k_per_kreis <-  standort_sites |>    
  filter(kreislabel %in% c("TF", "MOL", "UM","DSW", "LOS", "OH", "PM", "PR")) |>
  filter(!is.na(kultur2)) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(kultur2)) |>
  ungroup()

standort_sites$obj <- factor(standort_sites$obj, ordered = TRUE,
                             levels = c("0-",
                                        "01",
                                        "1-" ,
                                        "10",
                                        "-1" ,
                                        "12",
                                        "13" ,
                                        "2-" ,
                                        "-2",
                                        "21" ,
                                        "23",
                                        "24",
                                        "3-",
                                        "-3",
                                        "31",
                                        "32",
                                        "34",
                                        "45" ) )

# plot steinob

x <- standort_sites |>
  filter(kreislabel %in% c("TF", "MOL", "UM","DSW", "LOS", "OH", "PM", "PR")) |>
  filter(!is.na(FST)) |>
  filter(kultur2 != "<NA>")  |>
  group_by(kreislabel, obj, kultur2) |>
  summarise(sum_p_area = sum(area.) )


x |>
  ggplot(na.rm = TRUE)+
  geom_bar(aes(y = sum_p_area,
               x = kultur2,
               fill = obj),
           position = "fill", stat = "identity")+
  labs(title = "Verteilung der Fundstellen auf Steinigkeit d. Bodens",
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
  coord_flip()+
  scale_fill_manual(na.value = "grey80",
                    values = rev(viridis(19)),
                    labels = c( "0-" = "keine Steine",
                                "01" = "keine Steine / steinarm",
                                "1-" = "steinarm",
                                "10" = "steinarm / keine Steine",
                                "-1" = "<40% steinarm",
                                "12" =  "steinarm/mäßig steinig",
                                "13" = "steinarm / steinig",
                                "2-" = "mäßig steinig",
                                "-2" = "<40% mäßig steinig",
                                "21" = "mäßig steinig / steinarm",
                                "23" = "mäßig steinig / steinig",
                                "24" = "mäßig steinig / stark steinig",
                                "3-" = "steinig",
                                "-3" = "<40% steinig",
                                "31" = "steinig / steinarm",
                                "32" = "steinig / mäßig steinig",
                                "34" = "steinig / stark steinig",
                                "45" = "stark steinig/ sehr stark steinig") )+
  facet_grid(kreislabel ~ ., scales = "free", drop = TRUE) +
  force_panelsizes(rows = k_per_kreis$count)+
  theme_bw()+
  guides(fill = guide_legend(ncol = 1 ))+
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12))

ggsave("./analysis/figures/Standortanalyse/MMK_steinob_500_BB_ausw_kreise.png", dpi = 300, height = 25, width = 25, units = "cm")

