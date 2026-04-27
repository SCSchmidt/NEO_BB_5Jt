## Analyse der Fundstellendichten nach Kreis und Naturraum

source("analysis/paper/scripts/standortanalyse_01_datenvorbereitung.R")

## 1. Fst nach Kreis
## 2. Fst nach Naturraum
## 3. Fst nach Dauer der Periode

# zur Vergleichbarkeit der Besiedlungsdichte der Kreise, Größe recherchiert und hier händisch eingetragen in km2

kreis_gr <- data.frame(kreis = c("Teltow-Fläming", "Elbe-Elster", "Havelland", "Barnim", "Dahme-Spreewald", "Spree-Neiße", "Oder-Spree", "Märkisch-Oderland", "Uckermark", "Brandenburg (Havel)", "Frankfurt (Oder)", "Oberhavel", "Oberspreewald-Lausitz", "Ostprignitz-Ruppin", "Potsdam", "Potsdam-Mittelmark", "Prignitz"),
                       kreislabel = c("TF", "EE", "HVL", "BAR", "DSW", "SPN", "LOS", "MOL", "UM", "BRB", "FF", "OH", "OSL", "OPR", "P", "PM", "PR"),
                       gr = c(2104.2, 1899.18, 1727.31, 1479.58, 2274.5, 1657, 2256.76, 2158.66, 3077.02, 229.73, 147.85, 1808.18, 1223.48, 2526.46, 188.24, 2592.03, 2138.58) )


fst_kreis <- as.data.frame(table(sites$kreislabel, sites$kultur2))
colnames(fst_kreis) <- c("kreislabel", "kultur", "anz_fst")

vgl_kreis_fst <- left_join(fst_kreis, kreis_gr, by = "kreislabel")

vgl_kreis_fst$index <- vgl_kreis_fst$anz_fst / vgl_kreis_fst$gr

## plotten


# Rasterpunkte entfernen
vgl_kreis_fst <- vgl_kreis_fst |>
  filter(kultur != "GG") 

# Anordnung der Kreise
vgl_kreis_fst$kreislabel <- factor(vgl_kreis_fst$kreislabel, ordered = TRUE,
                                   levels = c("PM", "BRB", "HVL", "P", "TF", "EE", "OSL", "DSW", "SPN", "LOS",  "FF", "MOL", "BAR", "UM", "OH",  "OPR", "PR") )


# Plot für Heatmap mit Balkendiagrammen der Häufigkeit oben und rechts davon

## nach: https://stackoverflow.com/questions/42315829/how-to-plot-histograms-of-raw-data-on-the-margins-of-a-plot-of-interpolated-data

p1 <- vgl_kreis_fst |>  
  ggplot()+
  geom_tile(aes(x = kultur,
                y = kreislabel,
                fill = index ))+
  geom_text(aes(x = kultur,
                y = kreislabel,
                label = anz_fst ), # format forces not to use eulersche zahl
            col = "white")+
  scale_fill_gradientn(colors = viridis(100))+
  labs(fill = "Anz. Fst / qkm Kreis",
       x = "Kulturgruppe",
       y = "Kreis",
       caption = "Label ist absolute Anzahl Fst.")+
  theme_bw()

#             label = format(round(index,4), scientific = FALSE) ), # format forces not to use eulersche zahl

thm = list(theme_classic(),
           guides(fill=FALSE),
           theme(plot.margin = unit(rep(0.5,4), "lines")))


p2 <- vgl_kreis_fst |>  # p2 anzahl nach kreis -> nach rechts
  ggplot()+
  geom_col(aes(x = kreislabel,
               y = anz_fst))+
  labs(x = "",
       y = "Anzahl Fst.")+
  coord_flip()+
  thm

p3 <- vgl_kreis_fst |>  # p3 kulturen anzahl -> nach oben
  ggplot()+
  geom_col(aes(x = kultur,
               y = anz_fst))+
  labs(x = "",
       y = "Anzahl Fst.")+
  thm


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend) }

leg = g_legend(p1) # muss vor p1 + thm

p1 <- p1 + thm


library(cowplot)

plot_grid(
  plot_grid(plotlist=list(p3, ggplot(), p1, p2), ncol=2, 
            rel_widths=c(3,1), rel_heights=c(1,3), align="hv", scale=1),
  leg, rel_widths=c(4,1))

ggsave("./analysis/figures/verteilung_fst_auf_kreise_BB_v3.png", dpi = 300, width = 32, height = 20, units = "cm")


### Fundstellendichte nach Naturraum:

# pro Fundstelle abgefragt, in welchem Naturraum sie liegt (point sampling tool in qGIS)
sites_naturraum <- read.csv2("./analysis/data/geodata/sites_naturraum_tab.csv")

# Naturrauminformationen von Scholz (v.a. Größe wichtig)
naturraum <- read.csv2("./analysis/data/geodata/naturraum_tab.csv")

## Zuweisung nur Axtfunde:
sites_naturraum <- sites_naturraum |>
  mutate(kultur2 = case_when(
    KULTUR == "Mesolithikum" ~ "Meso",
    KULTUR == "Rössener" ~ "Rös",
    KULTUR == "LBK Siedlungen" ~ "LBK Siedl.",
    KULTUR == "SBK?" ~ "SBK",
    TRUE ~ KULTUR
  )) |>
  mutate(kultur2 = case_when(
    FST %in% c("Frankfurt 108",
               "Wernsdorf 6",
               "Beeskow 5",
               "Kossenblatt 3",
               "Bad Freienwalde 8",
               "Blankenburg 26",
               "Blankenburg 8",
               "Brüssow 67",
               "Niederlandin 12/5 (13)",
               "Prenzlau 23",
               "Ribbeck 9/21 (9)") ~ "Rös. Beil/Axt",
    TRUE ~ kultur2
  ))

sites_naturraum$kultur2 <- factor(sites_naturraum$kultur2, ordered = TRUE,
                                  levels = c("GG", "Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "Guhrau", "Rös", "Rös. Beil/Axt"))


sites_naturraum_anzahl <- sites_naturraum |>
  group_by(NAME_UNTER, kultur2) |>
  mutate(n = n_distinct(FID)) |>
  ungroup() |>
  select(NAME_HAUPT, NAME_UNTER, kultur2, n) |>
  unique() |>
  filter(kultur2 != "GG")

naturraum_gr <- naturraum |>
  group_by(NAME_UNTER) |>
  mutate(groeße = sum(AREA)) |>
  ungroup() |>
  select(NAME_UNTER, NAME_HAUPT, groeße) |>
  unique()

vgl <- left_join(sites_naturraum_anzahl, naturraum_gr)

vgl$index <- (vgl$n/vgl$groeße)*1000000 # von quadratmeter in quadratkilometer!


library(ggh4x)

vgl <- vgl |>
  mutate(label = case_when(
    NAME_HAUPT ==   "MITTELBRANDENBURGISCHE PLATTEN UND NIEDERUNGEN" ~ "Mittelbrandenburg. Platten \n und Niederungen",
    NAME_HAUPT ==   "RÜCKLAND DER MECKLENBURGISCHEN SEENPLATTE"    ~ "Rückland d. mecklenburg. \n Seenplatte" ,
    NAME_HAUPT ==   "LAUSITZER BECKEN UND HEIDELAND"   ~ "Lausitzer Becken \n u. Heideland",
    NAME_HAUPT ==   "LUCHLAND" ~ "Luchland",
    NAME_HAUPT ==   "OSTBRANDENBURGISCHE PLATTE" ~ "Ostbrandenburg. Platte",
    NAME_HAUPT ==   "OSTBRANDENBURGISCHES HEIDE UND SEENGEBIET" ~ "Ostbrandenburg. Heide \n und Seengebiet",
    NAME_HAUPT ==   "ODERTAL" ~ "Odertal",
    NAME_HAUPT ==   "ELBTALNIEDERUNG" ~ "Elbtalniederung",
    NAME_HAUPT ==   "OBERLAUSITZER HEIDELAND"  ~ "Oberlausitzer Heideland",
    NAME_HAUPT ==   "ELBE-MULDE-TIEFLAND"  ~ "Elbe-Mulde-Tiefland",
    NAME_HAUPT ==   "FLÄMING" ~ "Fläming",
    NAME_HAUPT ==   "MECKLENBURGISCHE SEENPLATTE"   ~ "Mecklenburg. Seenplatte",
    NAME_HAUPT ==  "SPREEWALD" ~ "Spreewald",
    NAME_HAUPT ==   "NORDBRANDENBURGISCHES PLATTEN UND HÜGELLAND" ~ "Nordbrandenburg. Platten\n und Hügelland"
  ))

#vgl$label <- factor(vgl$label, ordered = TRUE,
#                    levels = c("Rückland d. mecklenburg. \n Seenplatte" ,"Mecklenburg. Seenplatte","Nordbrandenburg. Platten\n und Hügelland", "Elbeniederung", "Luchland", "Ostbrandenburg. Platte", "Odertal", "Ostbrandenburg. Heide und Seengebiet", "Mittelbrandenburg. Platten \n und Niederungen", "Fläming", "Elbe-Mulde-Tiefland", "Oberlausitzer Heideland", "Lausitzer Becken \n u. Heideland")


u_per_h <- vgl |>
  select(label, NAME_UNTER)  |>
  unique() |>
  group_by(label) |>
  mutate(n = n_distinct(NAME_UNTER)) |>
  ungroup() |>
  select(label, n) |>
  unique()

u_per_h2 <- u_per_h[order(u_per_h$label),]


vgl |>  
  ggplot()+
  geom_tile(aes(x = kultur2,
                y = NAME_UNTER,
                fill =  index) ) +
  geom_text(aes(x = kultur2,
                y = NAME_UNTER,
                label = n ), # format forces not to use eulersche zahl
            col = "white")+
  scale_fill_gradient(low = (viridis(1)), high = (viridis(100)),
                      breaks=round(c(min(vgl$index),max(vgl$index)), 3),
                      limits = c(0,0.04)) +
  labs(fill = "Anz. Fst / qkm",
       x = "",
       y = "",
       caption = "Label ist absolute Anzahl Fst.")+
  theme_classic()+
  facet_grid(label ~ .,
             scales = "free")+
  force_panelsizes(rows = u_per_h2$n +1 )+
  theme(
    strip.text.y = element_text(
      size = 10,
      angle = 0) )

ggsave("./analysis/figures/verteilung_fst_auf_naturräume_BB.png", dpi = 300, width = 30, height = 35, units = "cm") #


## 3. Anzahl Fundstellen pro Dauer der Kulturgruppe

periodendauer <- data.frame(kultur = c("Meso", "FBG", "LBK Dechsel", "LBK Siedl.", "SBK", "Rös", "Rös. Beil/Axt", "Guhrau"),
                            dauer = c(5500, 600, 500, 500, 300, 300, 300, 400))

# LBK etwa 5300 bis 4800 (Daten Neurosow), SBK 48-4300 (wir haben ja nur die späteren Phasen), Rössen: 47-4400, Guhrau, geraten 44-4000, FBG 4600–4000/3800 nach Wetzel

vgl_kreis_fst_d <- left_join(vgl_kreis_fst, periodendauer)

vgl_kreis_fst_d$fst_per_j <- vgl_kreis_fst_d$anz_fst / vgl_kreis_fst_d$dauer

vgl_kreis_fst_d <- vgl_kreis_fst_d |>
  group_by(kultur) |>
  mutate(sum_fst_j_index = sum(fst_per_j)) |>
  mutate(sum_fst_pro_k = sum(anz_fst)) |>
  ungroup()


vgl_kreis_fst_d$kultur <- factor(vgl_kreis_fst_d$kultur, ordered = TRUE,
                                 levels = c("Meso", "LBK Dechsel", "LBK Siedl.", "SBK",  "Rös", "Rös. Beil/Axt", "FBG", "Guhrau"))

vgl_kreis_fst_d |>
  filter(kultur != "GG") |>
  ggplot()+
  geom_col(aes(x = kultur,
               y = fst_per_j ))+
  geom_text(aes(x = kultur,
                y = 0.01,
                label = sum_fst_pro_k ), # format forces not to use eulersche zahl
            col = "white")+
  labs(y = "Anz. Fst / Jahr",
       x = "Kulturgruppe",
       caption = "Label ist absolute Anzahl Fst.")+
  theme_bw()

ggsave("./analysis/figures/BB_Fst_per_Jahr_Kultur_2.png", dpi = 300)
