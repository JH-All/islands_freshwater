# Packages --------------------------
library(tidyverse)
library(readxl)
library(stringr)
library(maps)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(units)
library(pageviews)
library(geodata)
library(MASS)
library(car)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggdist)
library(tidyr)
library(forcats)

# Preparing data -----------------------------
papers = read_excel("data.xlsx", sheet = "papers")

papers$study_topic = as.factor(papers$study_topic)
levels(papers$study_topic)

islands = read_excel("data.xlsx", sheet = "islands")
str(islands)
islands$area_km2 = as.numeric(islands$area_km2)

# Study topic -----------------------------------
papers$study_topic <- dplyr::recode(
  papers$study_topic,

  "Bioinvasão" = "Bioinvasion",
  
  "comportamento" = "Behavior",
  "Comportamento" = "Behavior",
  
  "Distribuição" = "Distribution",
  
  "Ecologia de comunidade" = "Community ecology",
  
  "Ecologia trófica" = "Trophic ecology",
  
  "Ecotoxicologia" = "Ecotoxicology",
  
  "eDNA" = "eDNA",
  
  "Evolução/Biogeografia" = "Evolution and biogeography",
  
  "fisiologia" = "Physiology",
  "Fisiologia" = "Physiology",
  
  "Genética de população" = "Population genetics",
  
  "História de vida" = "Life history",
  
  "Morfologia" = "Morphology",
  
  "Movimento" = "Movement",
  
  "Parasitologia/Microbiologia" = "Parasitology and diseases",
  
  "Paleontologia" = "Paleontology",
  
  "Taxonomia" = "Taxonomy"
)


papers$study_topic <- droplevels(as.factor(papers$study_topic))

levels(papers$study_topic)

papers <- papers %>%
  mutate(
    study_topic = fct_relevel(
      study_topic,
      sort(levels(study_topic))
    )
  )

papers_year <- papers %>%
  count(year, study_topic) %>%
  complete(
    year = full_seq(year, 1),
    study_topic,
    fill = list(n = 0)
  ) %>%
  arrange(study_topic, year) %>%
  group_by(study_topic) %>%
  mutate(cumulative_studies = cumsum(n)) %>%
  ungroup()

## Figure 1 -------------------------------
fig1 = ggplot(papers_year,
              aes(x = year,
                  y = cumulative_studies,
                  group = 1)) +
  
  geom_line(linewidth = 1, color = "cyan4") +
  
  facet_wrap(~ study_topic) +
  
  scale_x_continuous(
    limits = c(1985, 2025),
    breaks = seq(1985, 2025, by = 10)
  ) +
  
  labs(
    x = "Year",
    y = "Cumulative number of studies"
  ) +
  
  theme_bw(base_size = 16) +
  
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    # remover grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig1

ggsave("Figure_1.jpg", fig1, width = 12, height = 9)


# Environments  -------------------------------
head(papers$environment)

environment_count <- papers %>%
  mutate(article_id = row_number()) %>%
  
  separate_rows(environment, sep = ",") %>%
  
  mutate(
    environment = str_trim(environment),
    
    environment = str_to_title(environment),
    
    environment = ifelse(environment == "Na",
                         "Not available",
                         environment)
  ) %>%
  
  distinct(article_id, environment) %>%
  count(environment, sort = TRUE)

environment_count


# Figure 2 ----------------------------
dms_to_decimal <- function(dms) {
  
  parts <- stringr::str_match(
    dms,
    "(\\d+)°(\\d+)'([0-9.]+)\"([NSEW])"
  )
  
  degrees <- as.numeric(parts[,2])
  minutes <- as.numeric(parts[,3])
  seconds <- as.numeric(parts[,4])
  direction <- parts[,5]
  
  decimal <- degrees + minutes/60 + seconds/3600
  
  
  decimal[direction %in% c("S", "W")] <-
    -decimal[direction %in% c("S", "W")]
  
  return(decimal)
}


islands <- islands %>%
  mutate(
    lat = dms_to_decimal(latitude),
    lon = dms_to_decimal(longitude)
  )


world <- map_data("world")

fig2 = ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "gray90",
    color = "gray70",
    linewidth = 0.2
  ) +
  geom_point(
    data = islands,
    aes(x = lon,
        y = lat,
        size = n_studies),
    color = "cyan4",
    alpha = 0.8
  ) +
  
  scale_size_continuous(
    name = "Number of studies",
    range = c(1, 10),   
    breaks = c(1, 5, 10, 20, 30)
  )+
  
  scale_x_continuous(
    breaks = seq(-100, 100, by = 100),
    
    labels = function(x) {
      paste0(abs(x), "°",
             ifelse(x < 0, "W",
                    ifelse(x > 0, "E", "")))
    }
  )+
  scale_y_continuous(
    labels = function(y) {
      paste0(abs(y), "°",
             ifelse(y < 0, "S",
                    ifelse(y > 0, "N", "")))
    }
  ) +
  coord_fixed(1.3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

fig2

ggsave("Figure_2.jpg", fig2)

# Measuring mainland distance -------------------------------
islands_sf <- islands %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )


world_sf <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)


land_parts <- world_sf %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  mutate(area_km2 = set_units(st_area(.), km^2)) %>%
  filter(as.numeric(area_km2) > 50000) 

mainland <- land_parts %>%
  st_union()

islands$distance_mainland_km <- as.numeric(
  st_distance(islands_sf, mainland)
) / 1000

head(islands$distance_mainland_km)

ggplot() +
  geom_sf(data = mainland)

summary(islands$distance_mainland_km)

# Measuring societal interest -----------------------------
islands$island_wiki <- paste0(
  gsub(" ", "_", islands$island),
  "_Island"
)

get_views <- function(island_name) {
  
  tryCatch({
    
    views <- article_pageviews(
      project = "en.wikipedia",
      article = island_name,
      start = as.Date("2016-01-01"),
      end = as.Date("2025-12-31")
    )
    
    sum(views$views, na.rm = TRUE)
    
  }, error = function(e) {
    
    NA
    
  })
}

islands$wiki_views <- map_dbl(
  islands$island_wiki,
  get_views
)

# Measuring Human footprint ---------------------------
hii = footprint(year=2009, path = "/Users/joaobiosmac/Desktop/R" ) 
class(hii)

coords <- islands %>%
  select(lon, lat)

hii_values <- terra::extract(
  hii,
  coords
)

islands$HII <- hii_values[,2]

head(islands$HII)

# Bayesian model: number of studies ---------------------
na_count <- islands %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_NA"
  ) %>%
  arrange(desc(n_NA))

na_count

islands_model <- islands %>%
  mutate(
    population = log10(population + 1),
    area_km2 = log10(area_km2 + 1),
    distance_to_university_km = log10(distance_to_university_km + 1),
    wiki_views = log10(wiki_views + 1),
    distance_mainland_km = log10(distance_mainland_km + 1),
    HII = log10(HII + 1)
  ) %>%
  mutate(
    across(
      c(
        population,
        area_km2,
        distance_to_university_km,
        wiki_views,
        distance_mainland_km,
        HII,
        lat
      ),
      ~ as.numeric(scale(.))
    )
  )


mod_bayes <- brm(
  n_studies ~
    population +
    area_km2 +
    island_type +
    distance_to_university_km +
    wiki_views +
    distance_mainland_km +
    HII +
    lat,
  
  data = islands_model,
  
  family = negbinomial(),
  
  chains = 4,
  cores = 4,
  iter = 4000,
  
  seed = 123
)

summary(mod_bayes)

posterior <- spread_draws(
  mod_bayes,
  b_population,
  b_area_km2,
  b_distance_to_university_km,
  b_wiki_views,
  b_distance_mainland_km,
  b_HII,
  b_island_typeoceanic,
  b_lat
)

posterior_long <- posterior %>%
  pivot_longer(
    cols = starts_with("b_"),
    names_to = "term",
    values_to = "estimate"
  ) %>%
  mutate(
    term = dplyr::recode(
      term,
      "b_population" = "Population",
      "b_area_km2" = "Island area",
      "b_island_typeoceanic" = "Oceanic island",
      "b_distance_to_university_km" = "Distance to university",
      "b_wiki_views" = "Societal interest",
      "b_distance_mainland_km" = "Distance to mainland",
      "b_HII" = "Human footprint",
      "b_lat" = "Latitude"
    )
  )

posterior_summary <- posterior_long %>%
  group_by(term) %>%
  summarise(
    mean = mean(estimate),
    lower = quantile(estimate, 0.025),
    upper = quantile(estimate, 0.975),
    .groups = "drop"
  )

posterior_summary <- posterior_summary %>%
  mutate(
    term = forcats::fct_reorder(term, mean)
  )

posterior_long <- posterior_long %>%
  mutate(
    term = factor(term,
                  levels = levels(posterior_summary$term))
  )


relevant_effects <- posterior_summary %>%
  mutate(
    supported_effect = lower > 0 | upper < 0
  ) %>%
  filter(supported_effect) %>%
  arrange(desc(abs(mean)))

relevant_effects

relevant_effects_text <- relevant_effects %>%
  mutate(
    mean = round(mean, 2),
    lower = round(lower, 2),
    upper = round(upper, 2)
  )

relevant_effects_text

## Figure 3 --------------------------------
fig3 = ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray40"
  ) +
  stat_halfeye(
    data = posterior_long,
    aes(
      x = estimate,
      y = term
    ),
    fill = "cyan4",
    alpha = 0.2,
    color = "cyan4",
    slab_color = NA,
    point_interval = "mean_qi",
    .width = 0.95
  ) +
  geom_segment(
    data = posterior_summary,
    aes(
      x = lower,
      xend = upper,
      y = term,
      yend = term
    ),
    linewidth = 1.2,
    color = "black"
  ) +
  geom_point(
    data = posterior_summary,
    aes(
      x = mean,
      y = term
    ),
    shape = 21,
    size = 4,
    fill = "cyan4",
    color = "black"
  ) +
  
  labs(
    x = "Posterior estimate",
    y = NULL
  ) +
  
  theme_bw(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

fig3

ggsave("Figure_3.jpg", fig3)

# Bayesian model: number of studied species ---------------------
mod_bayes_species <- brm(
  number_of_studies_species ~
    population +
    area_km2 +
    island_type +
    distance_to_university_km +
    wiki_views +
    distance_mainland_km +
    HII +
    lat,
  
  data = islands_model,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 4000,
  seed = 123
)

summary(mod_bayes_species)


posterior_species <- spread_draws(
  mod_bayes_species,
  b_population,
  b_area_km2,
  b_distance_to_university_km,
  b_wiki_views,
  b_distance_mainland_km,
  b_HII,
  b_island_typeoceanic,
  b_lat
)

posterior_species_long <- posterior_species %>%
  pivot_longer(
    cols = starts_with("b_"),
    names_to = "term",
    values_to = "estimate"
  ) %>%
  mutate(
    term = dplyr::recode(
      term,
      "b_population" = "Population",
      "b_area_km2" = "Island area",
      "b_island_typeoceanic" = "Oceanic island",
      "b_distance_to_university_km" = "Distance to university",
      "b_wiki_views" = "Societal interest",
      "b_distance_mainland_km" = "Distance to mainland",
      "b_HII" = "Human footprint",
      "b_lat" = "Latitude"
    )
  )

term_order <- rev(c(
  "Island area",
  "Distance to mainland",
  "Societal interest",
  "Population",
  "Latitude",
  "Human footprint",
  "Oceanic island",
  "Distance to university"
))

posterior_species_summary <- posterior_species_long %>%
  group_by(term) %>%
  summarise(
    mean = mean(estimate),
    lower = quantile(estimate, 0.025),
    upper = quantile(estimate, 0.975),
    .groups = "drop"
  ) %>%
  mutate(
    term = factor(term, levels = term_order)
  )

posterior_species_long <- posterior_species_long %>%
  mutate(
    term = factor(term, levels = term_order)
  )

relevant_effects_species <- posterior_species_summary %>%
  mutate(
    supported_effect = lower > 0 | upper < 0
  ) %>%
  filter(supported_effect) %>%
  arrange(desc(abs(mean)))

posterior_species_summary

relevant_effects_text_sp <- posterior_species_summary %>%
  mutate(
    mean = round(mean, 2),
    lower = round(lower, 2),
    upper = round(upper, 2)
  )

relevant_effects_text_sp

## Figure 4 ----------------------------
fig4 = ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray40"
  ) +
  stat_halfeye(
    data = posterior_species_long,
    aes(x = estimate, y = term),
    fill = "cyan4",
    alpha = 0.2,
    color = "cyan4",
    slab_color = NA,
    point_interval = "mean_qi",
    .width = 0.95
  ) +
  geom_segment(
    data = posterior_species_summary,
    aes(x = lower, xend = upper, y = term, yend = term),
    linewidth = 1.2,
    color = "black"
  ) +
  geom_point(
    data = posterior_species_summary,
    aes(x = mean, y = term),
    shape = 21,
    size = 4,
    fill = "cyan4",
    color = "black"
  ) +
  labs(
    x = "Posterior estimate",
    y = NULL
  ) +
  theme_bw(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

fig4

ggsave("Figure_4.jpg", fig4)