#### Testen der Handhaben

source("analysis/paper/scripts/keramik_gefform_06_handhaben.R")


## Chi-Quadrat für nominales
source("./analysis/./analysis/R_functions/get_p-adj_chiperm.R")

gef_hh_chi <- get_p_chi_perm(df = gef_hh, vars = c("Handhabentyp", "ort_k", "form_loch"), id = "gefäßnummer", kult = "kultur2")

## Wilcoxon für ordinale Variablen


source("./analysis/./analysis/R_functions/get_p_wilcoxon.R")

gef_hh$länge_mm <- as.numeric(gef_hh$länge_mm)
gef_hh$tiefe_mm <- as.numeric(gef_hh$tiefe_mm)

hh_wilcoxon <- get_p_wilcoxon(gef_hh, vars = c("ausr.zu.rand",  "länge_mm", "tiefe_mm", "länge_zu_tiefe"), kult = "kultur2", p.adjust.method = "none")

hh_p_adj_wilcox <- adjust_p(hh_wilcoxon) 


## plot p-Werte


farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "grey70","grey60","grey50")

p_values_chi <- adjust_p(gef_hh_chi)

df_list <- list(p_values_chi, hh_p_adj_wilcox)

p_values <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

save(p_values, file = "./analysis/data/derived_data/p_values_all_handhaben.RData")

### zusammenführen der P-WErte

p_values$label <- paste0(p_values$variable, "\n", p_values$method) 

p_values$n_note <- gsub(" \n ", "\n", p_values$n_note)

p_values <- p_values |>
  mutate(k = case_when(
    k == "Rös - SRK" ~ "SRK - Rös" ,
    k == "Rös - SBK" ~ "SBK - Rös" ,
    k == "Guhrau - Rös" ~ "Rös - Guhrau" ,
    k == "Guhrau - SBK" ~ "SBK - Guhrau" ,
    k == "FBG - SBK" ~ "SBK - FBG" ,
    k == "FBG - Rös" ~ "Rös - FBG" ,
    k == "FBG - Guhrau" ~ "Guhrau - FBG" ,
    k == "BKK - Rös" ~ "Rös - BKK",
    k == "Guhrau - BKK" ~ "BKK - Guhrau",
    k == "Guhrau - SRK" ~ "SRK - Guhrau",
    k == "BKK - SBK" ~ "SBK - BKK",
    k == "BKK - SRK" ~ "SRK - BKK",
    TRUE ~ k))




p_values$label <- factor(p_values$label, ordered = TRUE,
                         levels = c("Handhabentyp\nChi-Quadrat, Permutationstest",
                                    "ort_k\nChi-Quadrat, Permutationstest"   ,
                                    "form_loch\nChi-Quadrat, Permutationstest" ,
                                    "ausr.zu.rand\npaarweiser Wilcoxon"  ,
                                    "profil\nChi-Quadrat, Permutationstest",
                                    "länge_mm\npaarweiser Wilcoxon"   ,
                                    "tiefe_mm\npaarweiser Wilcoxon" ,
                                    "länge_zu_tiefe\npaarweiser Wilcoxon"        
                         ) )


#p_values_chi$method <- "Chi-Quadrat Permutationstest"

p_values |>
  filter(variable != "profil") |>
  ggplot(aes(x = variable,
             y = k) ) +
  geom_tile(aes(fill = round(p.adj, 2 )),
            color = "white", linewidth = 1 )+
  geom_text(aes(label = format(round(p.adj, 2), nsmall = 2) ),
            col = "white",
            hjust = 2) +
  geom_text(aes(label = n_note),
            size = 3,
            col = "white",
            hjust = 0.3,
            vjust = 0.5) +
  scale_fill_gradientn(colours = farben ,
                       values =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       breaks =  c(0.00, 0.01, 0.05, 0.1, 0.2, 0.8, 1),
                       labels = c("0.00 - höchstsign.", "0.01 - hoch sign.", "0.05 - signif.", "0.1 - nicht sign.", "0.2","0.8", "1"),
                       guide = "colorbar", limits = c(0,1),
                       na.value = "lightgrey")+
  scale_x_discrete(labels = c("Handhabentyp\nChi-Quadrat,\n Permutationstest",
                              "Lage am Gef.\nChi-Quadrat,\n Permutationstest"   ,
                              "Grundform\nChi-Quadrat,\n Permutationstest" ,
                              "Ausrichtung zum Rand\npaarweiser\n Wilcoxon"  ,
                              #           "Profilform\nChi-Quadrat,\n Permutationstest",
                              "Länge\npaarweiser\n Wilcoxon"   ,
                              "Tiefe\npaarweiser\n Wilcoxon" ,
                              "Länge zu Tiefe\npaarweiser\n Wilcoxon"     ))+
  guides(fill = guide_legend(nrow = 1)  ) +
  labs(x = "",
       y = "",
       fill = "p-Werte",
       title = "P-Werte für Gruppenunterschiede in den Handhaben",
       caption = "n variiert je nach Erhaltungszustand und Belegung der Kreuztabellen und ist als label angegeben \n p-Werte mit dem Chi-Quadrat Permutationstest mit 9999 Wiederholungen erhalten, \n p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995) .")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 12))

ggsave("./analysis/figures/Gef_Form/Vgl_p_Handhaben2.png", dpi = 300, width = 30, height = 15, units = "cm")