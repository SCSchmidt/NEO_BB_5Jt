## Bodenfeuchteindex

source("./analysis/paper/scripts/standortanalyse_13_datenvorbereitung_rasterdaten.R")

source("./R_functions/farbzuweisungen.R")

r_info_HG |>
  filter(kr_group != "NA") |>
  ggplot()+
  geom_boxplot(aes(x = kultur2,
                   y = EL.soilMoi,
                   col = kultur2) )+
  geom_jitter(data = r_info_HG |> filter(kultur2 != "GG alle 1500m"),
              aes(x = kultur2 ,
                  y = EL.soilMoi),
              alpha = 0.25)+
  scale_color_manual(values = col) +
  labs(title = "Verteilung der Fundstellen auf Bodenfeuchtigkseitsindex",
       subtitle = "in ganz Brandenburg",
       y = "Bodenfeuchteindex",
       x = "",
       col = "",
       caption = "Daten als Punktabfrage der Fundstellen auf das 5x5 Raster \nQuelle: Landesvermessung und Geobasisinformation Brandenburg (LGB)") +
  theme_bw() # + 
#facet_grid(kr_group ~ .)

ggsave("../figures/Standortanalyse/soilMoist_BB_boxplot.png", dpi =300, width = 22, height = 10, units = "cm")


### Wilcoxon-test soilmoi ganz BB

source("./R_functions/get_p_wilcoxon.R")

soilmoi_wilcox <- get_p_wilcoxon(r_info_HG, vars = "EL.soilMoi", kult = "kultur2", p.adjust.method = "none")

soilmoi_wilcox <- adjust_p(soilmoi_wilcox)

### gleiche nochmal mit KS ->  gibt da schon tests...
library(DataScienceR)

ks_test_soilMoi_BB <- as.data.frame( pairwise_ks_test(r_info_HG$EL.soilMoi, r_info_HG$kultur2, n_min = 10, warning = 0,
                                                      alternative = "two.sided") )

ks_test_soilMoi_BB$k1 <- rownames(ks_test_soilMoi_BB)

soilMoi_p <- ks_test_soilMoi_BB |>
  pivot_longer(names_to = "k2",
               cols = c(`GG alle 1500m`:Meso),
               values_to = "pvalues")

soilMoi_p$p.adj <- p.adjust(soilMoi_p$pvalues, method = "fdr")

soilMoi_p2 <- left_join(soilMoi_p, soilmoi_wilcox, by = c("k1", "k2"))

soilMoi_p2$k1 <- factor(soilMoi_p2$k1 , levels = levels(r_info_HG$kultur2))
soilMoi_p2$k2 <- factor(soilMoi_p2$k2 , levels = levels(r_info_HG$kultur2))

soilMoi_p2 <- soilMoi_p2 |>
  mutate(n_note = gsub(pattern = "GG alle 1500m", "GG", n_note),
         n_note =  gsub(pattern = "LBK Dechsel", "LBK D.", n_note),
         n_note =  gsub(pattern = "LBK Siedl.", "LBK S.", n_note)
         )

soilMoi_p2$p.adj.x[soilMoi_p2$k1 == soilMoi_p2$k2] <- NA

soilMoi_p2 |>
  filter(!is.na(p.adj.x) ) |>
  filter(k1 != "Rös. Beil/Axt") |>
  filter(k2 != "Rös. Beil/Axt") |>
  ggplot(aes(x = k1,
             y = k2) ) +
  geom_tile(aes(fill = round(p.adj.x, 2 )),
            color = "white", linewidth = 1.5 )+
 # geom_tile(aes(x = k2,
#                y = k1,
 #               fill = round(p.adj.x, 2 )),
 #           color = "white", linewidth = 1.5 )+
  geom_text(aes(x = k2,
                y = k1,
                label = format(round(p.adj.x, 2), nsmall = 2) ),  # p-Wert-label
            col = "black",
            hjust = 2) +
  geom_text(aes(x = k1,
                y = k2,
                label = n_note), # Anzahl Fst label
            size = 3,
            col = "black",
            hjust = 0.2,
            vjust = 0.5) +
  scale_fill_gradientn(colours = farben ,
                       values =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       breaks =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       labels = c("0.00 - höchstsign.", "0.01 - hoch sign.", "0.05 - signif.", "0.1 - nicht sign.", "0.2","0.8", "1"),
                       guide = "colorbar", limits = c(0,1),
                       na.value = "white")+
  guides(fill = guide_legend(nrow = 1)  ) +
  labs(x = "",
       y = "",
       fill = "p-Werte",
       title = "P-Werte für Gruppenunterschiede im Feuchteindex d. Fundstellenlage",
       caption = "GG = Grundgesamtheit über Arbeitsgebiet, \n Rös. Beil/Axt - Fundstellen alle nicht signifikant, deshalb nicht dargestellt, \n p-Werte mit dem KS-Test erhalten, \n alle p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995).")+
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom",
        text = element_text(size = 10))

ggsave("./analysis/figures/Standortanalyse/KS_soilMoi_BB.png", dpi = 300, width = 36, height = 16, units = "cm")

