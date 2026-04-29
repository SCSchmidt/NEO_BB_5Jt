### Verzierungsanalyse Keramik: Randstiche


library(stringr)
library(dplyr)
library(ggh4x)
library(ggplot2)

load(file = "./analysis/data/derived_data/verzierungen.RData")

gef_verz <- verzierungen |> 
  select(gefäßnummer, kultur_gr, kreislabel, fundort) |>
  group_by(kultur_gr, kreislabel, fundort) |>
  mutate(count = n_distinct(gefäßnummer)) |>
  ungroup() |>
  select(-gefäßnummer) |>
  unique()

fo_per_kr <- verzierungen |>
  filter(str_detect(muster, "randstiche")) |>
  group_by(kreislabel) |> summarize(count = n_distinct(fundort))

gef_per_fo <- verzierungen |>
  filter(str_detect(muster, "randstiche")) |>
  group_by(fundort, kultur_gr, kreislabel) |> summarize(count = n_distinct(gefäßnummer))

vgl_gef_per_fo <- left_join(gef_per_fo, gef_verz, by = c("kultur_gr", "kreislabel", "fundort"))

vgl_gef_per_fo <- vgl_gef_per_fo |>
  mutate(count = paste0(count.x, "/", count.y))


verz_rs <- verzierungen |>
  filter(str_detect(muster, "randstiche")) |>
  mutate(muster.zusatz = case_when(
    muster.zusatz == "leicht schräg" ~ "leicht schräg zum Randdurchmesser",
    TRUE ~ muster.zusatz ) ) |>
  filter(muster.zusatz != "geht über Knubbe hinweg") |>
  mutate(muster.zusatz = factor(muster.zusatz, ordered = TRUE,
                                levels = c("senkrecht zum Randdurchmesser",
                                           "senkrecht zum Randdurchmesser, gruppiert",
                                           "leicht schräg zum Randdurchmesser",
                                           "leicht schräg zum Randdurchmesser, gruppiert",
                                           "schräg zum Randdurchmesser", 
                                           "parallel zum Randdurchmesser" )
  ) )

save(verz_rs, file = "./analysis/data/derived_data/verz_rs.RData")

verz_rs |>
  count(gefäßnummer, kreislabel, fundort, kultur_gr, muster.zusatz ) |>  # group by gefäßnummer 
  group_by(gefäßnummer, fundort, kultur_gr, kreislabel, muster.zusatz) |>          # now required with changes to dplyr::count()
  mutate(prop = prop.table(n)) |>
  ggplot()+
  geom_bar(aes(y = prop,
               x = fundort,
               fill = muster.zusatz),
           position = "fill", stat = "identity")+
  geom_text(data = vgl_gef_per_fo,
            aes(x = fundort,
                y = 1,
                label = count),
            col = "white",
            size = 3,
            hjust = 1,
            position = position_fill(vjust = .9))+
  scale_fill_viridis(discrete = TRUE,
                     na.value = "grey")+
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     labels = c(0, 50, 100)) +
  coord_flip()+
  labs(x = "Fundort", 
       y = "Prozent d. Gefäße mit Randstichen",
       fill  ="",
       caption = paste0(sum(gef_per_fo$count), " von ", verzierungen |> summarize(count = n_distinct(gefäßnummer)), " verzierten Gefäßen" )) +
  facet_grid(kreislabel ~ kultur_gr, scales = "free", drop = TRUE) +
  force_panelsizes(rows = fo_per_kr$count)+
  theme_bw(base_size = 11)+
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))+
  guides(fill = guide_legend(nrow = 3))+
  ggtitle("Ausrichtung der Randstiche")

ggsave("./analysis/figures/Muster/2024-10-23_ausr_randstiche_FBG-SBK-Rös--Kreis_prop.jpg", height = 27, width = 22, units = "cm")