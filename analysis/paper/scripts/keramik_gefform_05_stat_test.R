### Testen

## Chi-Quadrat für nominale Daten als Permutationstest mit 9999 Wiederholungen -> Achtung braucht!

source("./R_functions/get_p-adj_chiperm.R")

gef_form_chi <- get_p_chi_perm(df = gef_umbruch, kult = "kultur2", vars = c("randabschluss_gr", "randabschluss_f", "randform_nonek", "schulterform_nonek", "bauchform_nonek", "boden"), id = "gefäßnummer")

p_values_chi <- adjust_p(gef_form_chi)


save(p_values_chi, file = "./analysis/data/derived_data/gefform_p_values_chi-permuted.RData")

## pairwise wilcoxon -> deutlich schneller

source("./R_functions/get_p_wilcoxon.R")

# note: k-s-test ergibt in etwa gleiche ergebnisse


gefform_wilcoxon <- get_p_wilcoxon(df = gef_umbruch, vars = c("randneigung", "Hals.schulter.umbruch", "Schulter.bauch.umbruch", "bodenwinkel"), kult = "kultur2",  p.adjust.method = "none")
# when "none", dann danach die p.adj-Geschichte nötig:

p_values_wilcox <- adjust_p(gefform_wilcoxon)


## gef-form_p_zusammenführen


df_list <- list(p_values_chi, p_values_wilcox)

p_values <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

### plotten der P-WErte

p_values$label <- paste0(p_values$variable, "\n", p_values$method) 

p_values$n_note <- gsub(" \n ", "\n", p_values$n_note)

farben <- c("#FDE725FF", "#7AD151FF", "#22A884FF", "grey70","grey60","grey50")


p_values$label <- factor(p_values$label,
                         ordered = TRUE,
                         levels = c("randneigung\npaarweiser Wilcoxon",
                                    "randabschluss_gr\nChi-Quadrat, Permutationstest" ,
                                    "randabschluss_f\nChi-Quadrat, Permutationstest",
                                    "randform_nonek\nChi-Quadrat, Permutationstest"  ,
                                    "Hals.schulter.umbruch\npaarweiser Wilcoxon"    ,
                                    "schulterform_nonek\nChi-Quadrat, Permutationstest", 
                                    "Schulter.bauch.umbruch\npaarweiser Wilcoxon"         ,
                                    "bauchform_nonek\nChi-Quadrat, Permutationstest"   ,
                                    "boden\nChi-Quadrat, Permutationstest",
                                    "bodenwinkel\npaarweiser Wilcoxon"    ) )


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
    TRUE ~ k))

#p_values$label

save(p_values, file = "./analysis/data/derived_data/p_values_gefform_chi_wilcox.RData")

p_values |>
  filter( variable != "Schulter.bauch.umbruch") |> # nur ein unsignifikanter SBK-SRK-p-Wert berechenbar
  ggplot(aes(x = label,
             y = k) ) +
  geom_tile(aes(fill = round(p.adj, 2 )),
            color = "white", size = 1)+
  geom_text(aes(label = format(round(p.adj, 2), nsmall = 2) ),
            col = "black",
            hjust = 2) +
  geom_text(aes(label = n_note),
            size = 3,
            col = "black",
            hjust = 0.1,
            vjust = 0.5) +
  scale_x_discrete(labels = c("Randneigung\npaarweiser Wilcoxon",
                              "Randabschluss grob\nChi-Quadrat \n Permutationstest" ,
                              "Randabschluss fein\nChi-Quadrat  \nPermutationstest",
                              "Halsform NoNeK\nChi-Quadrat  \nPermutationstest"  ,
                              "Hals-Schulter-Umbruch\npaarweiser  \nWilcoxon",
                              "Schulterform NoNeK\nChi-Quadrat  \nPermutationstest", 
                              "Schulter-Bauch-Umbruch\npaarweiser  \nWilcoxon"         ,
                              "Bauchform NoNeK\nChi-Quadrat \n Permutationstest"   ,
                              "Boden\nChi-Quadrat \nPermutationstest",
                              "Bodenwinkel\npaarweiser  \nWilcoxon"   ))+
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
       title = "P-Werte für Gruppenunterschiede in der Gefäßform",
       caption = "n variiert je nach Erhaltungszustand und Belegung der Kreuztabellen und ist als label angegeben \n p-Werte mit dem Chi-Quadrat Permutationstest mit 9999 Wiederholungen erhalten, \n p-Werte angepasst an mehrfaches Testen mit dem Algorithmus von Benjamini & Hochberg (1995) .")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 12))

ggsave("./analysis/figures/Gef_Form//alle_p-Werte_Gefform_neu_v4.png", dpi = 300, width = 38, height = 25, units = "cm")
