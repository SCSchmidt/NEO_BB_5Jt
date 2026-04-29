### Standortanalyse Polen, Mitteldeutschland, BB

### plotten der Balkendiagramme

library(viridis)
library(ggplot2)
library(dplyr)
library(ggh4x)


vgl_WRBFU <- vgl_standort_all |>
  select(fid, FST, BUNDESLAND, Bndslnd, kultur2, starts_with("WRB")) |>
  pivot_longer(cols = c(WRBFU_CHha:WRBFU_LVab),
               names_to = "var",
               values_to = "proz_fl",
               names_prefix = "WRBFU_")

vgl_WRBFU <- vgl_WRBFU |>
  mutate(var2 = case_when(
    var == "" ~ NA,
    var == "NA" ~ NA,
    var ==  "1 1 1" ~ NA, 
    var == "2 2 22 2" ~ NA,
    var == "3 3 33 3" ~ NA,
    TRUE ~ var
  ))


library(stringr)

vgl_WRBFU$var2 <- substr(vgl_WRBFU$var2, 1, 2)


fo_per_BL <- vgl_WRBFU |>     
  filter(Bndslnd != "NA") |>
  group_by(Bndslnd, kultur2) |>
  summarize(count = n_distinct(fid))

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888", "black")

vgl_WRBFU |>
  group_by(kultur2, var) |>
  #filter(Bndslnd != "NA") |>
  ggplot()+
  geom_bar(aes(y = proz_fl,
               x = kultur2,
               fill = var2),
           position = "fill", stat = "identity")+
  scale_fill_manual(values = safe_colorblind_palette,
                    na.value = "grey")+
  scale_y_continuous(breaks = c(0, 0.5, 1) )+
  coord_flip()+
  labs(title = "Vergleich der Bodenreferenzgruppen (WRB)",
       subtitle = "der Kulturgruppen im Arbeitsgebiet",
       caption = "Daten aus 5 km - Radius, \nDatenquelle: SGDBE",
       fill = "",
       x = "",
       y = "Anteil") +
  guides(fill = guide_legend(nrow = 2)) + 
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        axis.text.y = element_text(size = 10)
  )

ggsave("./analysis/figures/Standortanalyse/WRBFU_balkendiagramm2_zusammengef.png", dpi = 300, width = 20, units = "cm")


vgl_WRBFU |>
  group_by(kultur2, var, Bndslnd) |>
  filter(Bndslnd != "NA") |>
  ggplot()+
  geom_bar(aes(y = proz_fl,
               x = kultur2,
               fill = var2),
           position = "fill", stat = "identity")+
  geom_text(data = fo_per_BL , aes(x = kultur2, y = 1, 
                                   label  = paste0(count)  ), 
            size = 3,
            hjust = 1, 
            angle = 0, 
            colour = "white",
            position = position_fill(vjust = .9))  +
  scale_fill_manual(values = safe_colorblind_palette,
                    na.value = "grey")+
  scale_y_continuous(breaks = c(0, 0.5, 1) )+
  coord_flip()+
  facet_grid(. ~ Bndslnd, scales = "free", drop = TRUE) +
  labs(title = "Vergleich der Bodenreferenzgruppen (WRB)",
       subtitle = "der Kulturgruppen auf Bundesländerebene",
       caption = "Label gibt Anzahl der Fundstellen/Stichprobenpunkte, Daten aus 5 km - Radius, \nDatenquelle: SGDBE",
       fill = "",
       x = "",
       y = "Anteil") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_bw()+
  theme(text = element_text(size = 10),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 8) )

ggsave("./analysis/figures/Standortanalyse/WRBFU_balkendiagramm2.png", dpi = 300, width = 35, units = "cm")



######### Schleife über die anderen Eigenschaften

# reduzierte Variablen, die genutzt werden: "AGLI1NNI", "AGLI2NNI", "AWC_SUB",  "AWC_TOP" , "PARMADO" , "PARMASE", "TXSRFDO",  "TXSRFSE"

var_vec_red <- c("BS_TOP", "DGH", "AGLI1NNI", "AGLI2NNI", "AWC_SUB",  "AWC_TOP" , "PARMADO" , "PARMASE", "TXSRFDO",  "TXSRFSE")

for (i in var_vec_red) {
  
  if (i == "AGLI1NNI") {
    name <- "ersten Einschränkung für ackerbauliche Nutzung" 
  } else if (i == "AGLI2NNI") {
    name <- "sekundären Einschränkung für ackerbauliche Nutzung"
  }else if (i == "AWC_SUB") {
    name <- "Wasserkapazität d. Unterbodens"
  }else if (i == "AWC_TOP") {
    name <- "Wasserkapazität d. Oberbodens"
  }else if (i == "ERODI") {
    name <- "Erosionsklassen"
  }else if (i == "PARMADO") {
    name <- "dominantem Ausgangsgestein"
  }else if (i == "PARMASE") {
    name <- "sekundärem Ausgangsgestein"
  }else if (i == "SLOPEDO") {
    name <- "Neigungklassen"
  }else if (i == "TXSRFDO") {
    name <- "dominanten Bodentextur"
  }else if (i == "TXSRFSE") {
    name <- "sekundären Bodentextur"
  }else if (i == "BS_TOP") {
    name <- "Basensättigung d. Oberbodens"
  }else if (i == "DGH") {
    name <- "Tiefe bis Gleyhorizont"
  }
  
  
  vgl_var <- vgl_standort_all |>
    select(fid, FST, BUNDESLAND, Bndslnd, kultur2, starts_with(i)) |>
    pivot_longer(cols = c(starts_with(i)),
                 names_to = "var",
                 values_to = "proz_fl",
                 names_prefix = paste0(i, "_"))
  
  vgl_var$var[vgl_var$var == "NA"] <- NA
  
  if (i %in% c("AWC_TOP", "AWC_SUB") ) {
    vgl_var$var <- factor(vgl_var$var, ordered = TRUE,
                          levels = c("VL", "L", "M", "H", "VH"))
  } else if (i %in% c("TXRSFDO", "TXRSFSE")) {
    vgl_var$var <- factor(vgl_var$var, ordered = TRUE,
                          levels = c("0", "1", "2", "3", "4", "5"))
    vgl_var$var[vgl_var$var == "9"] <- NA 
  }  else if (i %in% c("PARMADO" , "PARMASE") ) {
    vgl_var$var[vgl_var$var == "0"] <- NA
  }
  
  v_u <- unique(vgl_var$var)
  
  if (length(v_u) < 8) {
    legend_rows <- 1
  } else if (16 > length(v_u) & length(v_u) > 7) {
    legend_rows <- 2
  } else {
    legend_rows <- 3
  }
  
  fo_per_BL <- vgl_var |>     
    filter(Bndslnd != "NA") |>
    group_by(Bndslnd, kultur2) |>
    summarize(count = n_distinct(fid))
  
  vgl_var |>
    # group_by(kultur2, var) |>
    ggplot()+ # na.rm = TRUE
    geom_bar(aes(y = proz_fl,
                 x = kultur2,
                 fill = var),
             position = "fill", stat = "identity")+
    scale_fill_viridis(discrete = TRUE,
                       direction = 1,
                       na.value = "grey")+
    scale_y_continuous(breaks = c(0, 0.5, 1) )+
    coord_flip()+
    labs(title = paste("Vergleich d. ", name),
         subtitle = "der Kulturgruppen über das gesamte Arbeitsgebiet",
         caption = "Label gibt Anzahl der Fundstellen/Stichprobenpunkte, Daten aus 5 km - Radius \n Datenquelle: SGDBE",
         fill = "",
         x = "",
         y = "Anteil") +
    guides(fill = guide_legend(nrow = legend_rows)) + 
    theme_bw()+
    theme(text = element_text(size = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.key.size = unit(0.25, "cm"),
          axis.text.y = element_text(size = 10)
    )
  
  filename1 <- paste0("./analysis/figures/Standortanalyse/", i, "_balkendiagramm_zusammengef.png")
  
  ggsave(filename1, dpi = 300, width = 20, units = "cm")
  
  
  vgl_var |>
    group_by(kultur2, var, Bndslnd) |>
    filter(Bndslnd != "NA") |>
    ggplot()+
    geom_bar(aes(y = proz_fl,
                 x = kultur2,
                 fill = var),
             position = "fill", stat = "identity")+
    geom_text(data = fo_per_BL , aes(x = kultur2, y = 1, 
                                     label  = paste0(count)  ), 
              size = 3,
              hjust = 1, 
              angle = 0, 
              colour = "white",
              position = position_fill(vjust = .9))  +
    scale_fill_viridis(discrete = TRUE,
                       direction = 1,
                       na.value = "grey")+
    scale_y_continuous(breaks = c(0, 0.5, 1) )+
    coord_flip()+
    facet_grid(. ~ Bndslnd, scales = "free", drop = TRUE) +
    labs(title = paste("Vergleich d.", name),
         subtitle = "der Kulturgruppen auf Bundesländerebene",
         caption = "Label gibt Anzahl der Fundstellen/Stichprobenpunkte, Daten aus 5 km - Radius \nDatenquelle: SGDBE",
         fill = "",
         x = "",
         y = "Anteil") +
    guides(fill = guide_legend(nrow = legend_rows)) + 
    theme_bw()+
    theme(text = element_text(size = 10),
          legend.position = "bottom",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.key.size = unit(0.25, "cm"),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 8) )
  
  filename2 <- paste0("./analysis/figures/Standortanalyse/", i, "_balkendiagramm.png")
  
  
  ggsave(filename2, dpi = 300, width = 35, units = "cm")
  
}

