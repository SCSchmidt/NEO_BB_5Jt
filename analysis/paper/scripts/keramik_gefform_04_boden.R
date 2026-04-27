### Gefäßformanalyse: Boden


load("./analysis/data/derived_data/gef_umbruch.RData")

gef_umbruch <- gef_umbruch2




library(viridis)
library(ggplot2)
library(dplyr)
library(ggh4x)

fo_per_kr <- gef_umbruch |>    filter(erh_bod != 0) |>
  group_by(kreislabel) |>
  summarize(count = n_distinct(fundort))

gef_per_ra <- gef_umbruch |>   filter(erh_bod != 0) |>
  group_by(kreislabel, kultur_gr, fundort) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch$boden <- factor(gef_umbruch$boden, ordered = TRUE,
                            levels = c("Rundboden", "Flach-gerundet", "Flachboden", "Niedriger Standfuß", "Standfuß"))


gef_umbruch |>
  filter(erh_bod != 0) |>
  count(gefäßnummer, kreislabel, kultur_gr, boden, bodenwinkel, fundort) |>
  group_by(gefäßnummer, kreislabel, kultur_gr, boden, bodenwinkel, fundort) |>
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = boden),
           position = "fill", stat = "identity")+
  geom_text(data = gef_per_ra,
            aes(x = fundort, y = 1, 
                label  = count ), 
            size = 3,
            hjust = 0.5, 
            angle = 0, 
            colour = "black",
            position = position_fill(vjust = .9)  )  +
  scale_y_continuous(breaks = c(0, 0.5,1),
                     labels = c(0, 50, 100))+
  labs(title = "Bodenform",
       caption = paste0("n = ", sum(gef_per_ra$count) ),
       x = "Fundort",
       y = "Prozent",
       fill = "")+
  scale_fill_viridis(discrete = TRUE,
                     direction = -1,
                     na.value = "grey")+
  coord_flip()+
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw()+
  theme(text = element_text(size = 11),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1 ))+
  theme(strip.text.y = element_text( size = 10,
                                     angle = 0))


ggsave("./analysis/figures/Gef_Form/2024-10-25_Bodenform.png", dpi = 300, width = 20, height = 20, unit = "cm")



gef_umbruch$kultur2 <- factor(gef_umbruch$kultur2, ordered = T,
                              levels = c("SBK", "SRK", "Guhrau", "Rös", "BKK", "FBG"))

gef_umbruch$bodenwinkel <- as.numeric(gef_umbruch$bodenwinkel)

gef_per_ra <- gef_umbruch |>    filter(erh_bod != 0) |>
  summarize(count = n_distinct(gefäßnummer))

gef_umbruch |>
  filter(erh_bod != 0) |>
  # filter(kultur2 != "FBG") |>
  #      kultur2 != "Guhrau") |>
  ggplot()+
  geom_boxplot(aes(x = kultur2,
                   y = bodenwinkel),
               col = "grey80")+
  geom_jitter(aes(x = kultur2,
                  y = bodenwinkel,
                  col = boden),
              #               shape = bauchform_nonek),
              size = 3)+
  
  labs(title = "Winkel im Übergang Bauch - Boden",
       caption = paste0("n = ", sum(gef_per_ra$count),"\n Punkte zittern um Überlappung zu vermeiden" ),
       y = "Bodenwinkel",
       col = "",
       x = "")+
  scale_colour_viridis(discrete = TRUE,
                       direction = -1,
                       na.value = "grey")+
  scale_shape_discrete(na.value = 1)+
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size = 25),
        legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1 ))+
  theme(strip.text.y = element_text( size = 15))

ggsave("./analysis/figures/Gef_Form/2024-10-25_Boden-Bauch.png", dpi = 300, width = 15, height = 10)