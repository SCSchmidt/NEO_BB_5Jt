
load(file = "./analysis/data/derived_data/scherben.RData")


library(dplyr)
library(stringr)
library(ggplot2)
library(ggh4x)

kult_per_kr <- scherben |>
  filter(as.numeric(erh_ofl_k) < 3) |>
  filter(!is.na(ofl))  |>
  group_by(kreislabel) |> summarize(count = n_distinct(kultur_gr))


s_ofl <- scherben |>
  filter(as.numeric(erh_ofl_k) < 3) |>
  filter(!is.na(ofl)) |>
  select(gefäßnummer, kreislabel, fundort, kultur_gr, kultur2, ofl, verz_n) |>
  group_by(gefäßnummer) |>
  slice_min(ofl) |> # nimm die "ordentlichste" Oberfläche per Gefäß
  ungroup() |>
  unique()

save(s_ofl, file = "./analysis/data/derived_data/s_ofl.RData")

s_ofl |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, ofl, verz_n) |>  # group by gefäßnummer vergessen! aber sieht genau gleich aus....
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, ofl, verz_n) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot() + 
  geom_bar(aes(x = kultur_gr,
               fill = ofl)) + 
  coord_flip()+
  theme_bw(base_size = 11)+ # sets size for all "normal text" to 11
  labs(x = "Kultur",
       y = "Prozent",
       title = "Äußere Oberflächenbehandung nach Kultur und Region",
       fill = "",
       caption = paste0("n Gefäße = ",length(unique(s_ofl$gefäßnummer) ) ) ) +
  guides(fill=guide_legend(ncol = 4)) +
  theme(legend.position = "bottom",
        legend.title = element_text(""))+
  facet_grid(kreislabel  ~ verz_n,
             scales = "free")+
  force_panelsizes(rows = kult_per_kr$count+2) 

ggsave("./analysis/figures/Magerung/2024-09-20_Ofl-Beh_nachKreis.png", dpi = 300, height = 21, width = 16, units = "cm")