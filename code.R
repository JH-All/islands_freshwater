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
library(patchwork)
library(CoordinateCleaner)
library(terra)
library(geosphere)
library(car)
library(posterior)
library(WDI)

# Preparing data -----------------------------
papers = read_excel("data.xlsx", sheet = "papers")

papers$study_topic = as.factor(papers$study_topic)
levels(papers$study_topic)

islands = read_excel("data.xlsx", sheet = "islands")
str(islands)

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

ggsave("Figure_1.tiff", fig1, width = 12, height = 9, dpi = 300)


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

fig2 <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "gray90",
    color = "gray70",
    linewidth = 0.2
  ) +
  geom_point(
    data = islands,
    aes(
      x = lon,
      y = lat,
      fill = n_studies
    ),
    shape = 21,
    size = 3.5,
    color = "gray20",
    stroke = 0.3,
    alpha = 0.9
  ) +
  scale_fill_gradient(
    name = "Number of studies",
    low = "#B8E186",
    high = "#762A83",
    breaks = c(1, 5, 10, 20, 30, 37),
    guide = guide_colorbar(
      barwidth = grid::unit(0.4, "cm"),
      barheight = grid::unit(5, "cm"),
      title.position = "top"
    )
  ) +
  scale_x_continuous(
    breaks = seq(-100, 100, by = 100),
    labels = function(x) {
      paste0(
        abs(x), "°",
        ifelse(x < 0, "W", ifelse(x > 0, "E", ""))
      )
    }
  ) +
  scale_y_continuous(
    labels = function(y) {
      paste0(
        abs(y), "°",
        ifelse(y < 0, "S", ifelse(y > 0, "N", ""))
      )
    }
  ) +
  coord_fixed(1.3) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

fig2

ggsave(
  "Figure_2.tiff",
  fig2,
  width = 14,
  height = 8,
  dpi = 300
)

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
    
    NA_real_
    
  })
}

islands$wiki_views <- map_dbl(
  islands$island_wiki,
  get_views
)

islands_wiki_na <- islands %>%
  filter(is.na(wiki_views)) %>%
  mutate(
    island_wiki_no_island = gsub(" ", "_", island)
  )

islands_wiki_na$wiki_views_no_island <- map_dbl(
  islands_wiki_na$island_wiki_no_island,
  get_views
)


islands_wiki_na %>%
  summarise(
    n_total_NA_original = n(),
    n_found_without_Island = sum(!is.na(wiki_views_no_island)),
    n_still_NA = sum(is.na(wiki_views_no_island))
  )

islands <- islands %>%
  left_join(
    islands_wiki_na %>%
      dplyr::select(island, wiki_views_no_island),
    by = "island"
  ) %>%
  dplyr::mutate(
    wiki_views = ifelse(
      is.na(wiki_views) & !is.na(wiki_views_no_island),
      wiki_views_no_island,
      wiki_views
    )
  ) %>%
  dplyr::select(-wiki_views_no_island)

wiki_manual <- tibble(
  island = c(
    "Beer (Bjørnøya)",
    "Comores (Grande Comore)",
    "Euboea/Evia",
    "Marshall (Majuro)",
    "Maurício (Mauritius)",
    "Reunião",
    "Saint-Pierre e Miquelon (Arquipélago)",
    "Santo Antão",
    "São Miguel (Azores)"
  ),
  island_wiki_manual = c(
    "Beer_Island",
    "Grande_Comore",
    "Euboea",
    "Marshall_Islands",
    "Mauritius",
    "Réunion",
    "Saint_Pierre_and_Miquelon",
    "Santo_Antão,_Cape_Verde",
    "São_Miguel_Island"
  )
)

wiki_manual$wiki_views_manual <- map_dbl(
  wiki_manual$island_wiki_manual,
  get_views
)

wiki_manual

wiki_manual <- wiki_manual %>%
  mutate(
    island = ifelse(
      island == "Santo Antão",
      "Santa Antão",
      island
    )
  )

islands <- islands %>%
  left_join(
    wiki_manual %>%
      dplyr::select(island, island_wiki_manual, wiki_views_manual),
    by = "island"
  ) %>%
  mutate(
    island_wiki = ifelse(
      is.na(wiki_views) & !is.na(wiki_views_manual),
      island_wiki_manual,
      island_wiki
    ),
    wiki_views = ifelse(
      is.na(wiki_views) & !is.na(wiki_views_manual),
      wiki_views_manual,
      wiki_views
    )
  ) %>%
  dplyr::select(-island_wiki_manual, -wiki_views_manual)

sum(is.na(islands$wiki_views))

# Measuring Human footprint ---------------------------
hii = footprint(year=2009, path = "/Users/joaobiosmac/Desktop") 
class(hii)

coords <- islands %>%
 dplyr::select(lon, lat)

hii_values <- terra::extract(
  hii,
  coords
)

islands$HII <- hii_values[,2]

head(islands$HII)

# Biodiversity institutons distance ---------------------------
data("institutions")
institutions
institutions$type = as.factor(institutions$type)

inst_sel <- institutions |>
  as.data.frame() |>
  filter(type %in% c("Research_centre", "University", "Museum")) |>
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  )

islands_clean <- islands |>
  filter(!is.na(lon), !is.na(lat))

dist_mat <- geosphere::distm(
  x = islands_clean[, c("lon", "lat")],
  y = inst_sel[, c("decimalLongitude", "decimalLatitude")],
  fun = geosphere::distHaversine
)

nearest_id <- max.col(-dist_mat)

islands_clean$dist_nearest_inst_m  <- dist_mat[cbind(seq_len(nrow(dist_mat)), nearest_id)]
islands_clean$dist_nearest_inst_km <- islands_clean$dist_nearest_inst_m / 1000
islands_clean$nearest_inst_type    <- inst_sel$type[nearest_id]

if ("name" %in% names(inst_sel)) {
  islands_clean$nearest_inst_name <- inst_sel$name[nearest_id]
}

islands$dist_nearest_inst_km <- NA_real_
islands$nearest_inst_type <- NA_character_

idx <- which(!is.na(islands$lon) & !is.na(islands$lat))

islands$dist_nearest_inst_km[idx] <- islands_clean$dist_nearest_inst_km
islands$nearest_inst_type[idx] <- islands_clean$nearest_inst_type

# Country-level income ---------------------------------------
islands <- islands %>%
  dplyr::select(-any_of(c(
    "country_sovereign", "iso2c", "income_iso2c",
    "income_year", "income_per_capita", "income_status",
    "income_assignment", "income_source", "income_indicator"
  ))) %>%
  mutate(
    country_sovereign = dplyr::recode(
      str_squish(as.character(country)),
      "Autrália" = "Austrália",
      "New Zeland" = "Nova Zelândia",
      "Trinindad e Tabogo" = "Trinidad e Tobago",
      "Escócia" = "Reino Unido",
      "Polinésia Francesa" = "França",
      "Wallis e Futuna" = "França"
    )
  )


country_codes <- tibble::tribble(
  ~country_sovereign,   ~iso2c,
  "Austrália",          "AU",
  "Bahamas",            "BS",
  "Brasil",             "BR",
  "Cabo Verde",         "CV",
  "Canadá",             "CA",
  "Chile",              "CL",
  "China",              "CN",
  "Chipre",             "CY",
  "Colômbia",           "CO",
  "Comores",            "KM",
  "Coreia do Sul",      "KR",
  "Costa Rica",         "CR",
  "Croácia",            "HR",
  "Cuba",               "CU",
  "Dinamarca",          "DK",
  "Dominica",           "DM",
  "Equador",            "EC",
  "Estônia",            "EE",
  "EUA",                "US",
  "Fiji",               "FJ",
  "Filipinas",          "PH",
  "França",             "FR",
  "Grécia",             "GR",
  "Guiné Equatorial",   "GQ",
  "Honduras",           "HN",
  "Ilhas Cook",         "CK",
  "Ilhas Marshall",     "MH",
  "Ilhas Maurício",     "MU",
  "Ilhas Salomão",      "SB",
  "Índia",              "IN",
  "Indonésia",          "ID",
  "Jamaica",            "JM",
  "Japão",              "JP",
  "Madagascar",         "MG",
  "Malásia",            "MY",
  "México",             "MX",
  "Micronésia",         "FM",
  "Noruega",            "NO",
  "Nova Zelândia",      "NZ",
  "Países Baixos",      "NL",
  "Panamá",             "PA",
  "Papua-Nova Guiné",   "PG",
  "Península Antártica", NA_character_,
  "Portugal",           "PT",
  "Reino Unido",        "GB",
  "Rússia",             "RU",
  "Samoa",              "WS",
  "Seychelles",         "SC",
  "Tanzânia",           "TZ",
  "Tonga",              "TO",
  "Trinidad e Tobago",  "TT",
  "Turquia",            "TR",
  "Vanuatu",            "VU",
  "Vietnã",             "VN"
)

unmapped <- islands %>%
  distinct(country_sovereign) %>%
  anti_join(country_codes, by = "country_sovereign")

if (nrow(unmapped) > 0L) {
  print(unmapped)
  stop("Corrigir países sem correspondência antes de continuar.")
}

islands <- islands %>%
  left_join(country_codes, by = "country_sovereign") %>%
  mutate(
    income_iso2c = if_else(iso2c == "CK", "NZ", iso2c)
  )

income_indicator <- "NY.GNP.PCAP.PP.KD"

income_history <- WDI::WDI(
  country = unique(na.omit(islands$income_iso2c)),
  indicator = c(income_per_capita = income_indicator),
  start = 2020,
  end = 2024,
  extra = FALSE
)


income_recent <- income_history %>%
  filter(
    is.finite(income_per_capita),
    income_per_capita > 0
  ) %>%
  group_by(iso2c) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    income_iso2c = iso2c,
    income_year = year,
    income_per_capita
  )

islands <- islands %>%
  left_join(income_recent, by = "income_iso2c") %>%
  mutate(
    income_per_capita = if_else(
      country_sovereign == "Península Antártica",
      0,
      income_per_capita
    ),
    income_assignment = case_when(
      country_sovereign == "Península Antártica" ~
        "Zero atribuído: renda nacional não aplicável",
      is.na(income_per_capita) ~
        "Ausente: requer fonte compatível",
      country_sovereign == "Ilhas Cook" ~
        "Proxy: Nova Zelândia",
      TRUE ~ "Dado do próprio país"
    ),
    income_source = if_else(
      country_sovereign == "Península Antártica",
      "Decisão dos autores",
      "World Bank WDI"
    ),
    income_indicator = if_else(
      country_sovereign == "Península Antártica",
      NA_character_,
      .env$income_indicator
    )
  )


income_check <- islands %>%
  count(
    country_sovereign, iso2c, income_iso2c,
    income_year, income_per_capita,
    income_assignment, income_source, income_indicator,
    name = "n_islands"
  ) %>%
  arrange(country_sovereign)

print(income_check)
print(filter(income_check, is.na(income_per_capita)))

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
    population_density = if_else(
      !is.na(area_km2) & area_km2 > 0,
      population / area_km2,
      NA_real_
    ),
    population_density = log10(population_density + 1),
    income_per_capita = log10(income_per_capita + 1),
    area_km2 = log10(area_km2 + 1),
    dist_nearest_inst_km = log10(dist_nearest_inst_km + 1),
    wiki_views = log10(wiki_views + 1),
    distance_mainland_km = log10(distance_mainland_km + 1),
    HII = log10(HII + 1)
  )


continuous_predictors <- c(
  "population_density",
  "income_per_capita",
  "area_km2",
  "dist_nearest_inst_km",
  "wiki_views",
  "distance_mainland_km",
  "HII",
  "lat"
)

islands_model <- islands_model %>%
  mutate(
    valid_predictors =
      if_all(all_of(continuous_predictors), ~ is.finite(.)) &
      !is.na(island_type),
    included_studies =
      valid_predictors & is.finite(n_studies),
    included_species =
      valid_predictors & is.finite(number_of_studies_species)
  )


excluded_islands <- islands_model %>%
  filter(!included_studies | !included_species) %>%
  dplyr::select(
    any_of(c("island", "country", "country_sovereign")),
    included_studies,
    included_species,
    all_of(continuous_predictors),
    island_type,
    n_studies,
    number_of_studies_species
  )

excluded_islands

scaling_reference <- islands_model %>%
  filter(included_studies | included_species)

if (nrow(scaling_reference) < 2L) {
  stop("Número insuficiente de ilhas utilizáveis.")
}

scaling_parameters <- tibble::tibble(
  variable = continuous_predictors,
  center = vapply(
    scaling_reference[continuous_predictors],
    mean,
    numeric(1)
  ),
  scale = vapply(
    scaling_reference[continuous_predictors],
    sd,
    numeric(1)
  )
)

if (any(
  !is.finite(scaling_parameters$scale) |
  scaling_parameters$scale <= 0
)) {
  stop("Há preditor sem variação ou com escala inválida.")
}

islands_model <- islands_model %>%
  mutate(
    across(
      all_of(continuous_predictors),
      ~ {
        j <- match(cur_column(), scaling_parameters$variable)
        (. - scaling_parameters$center[j]) /
          scaling_parameters$scale[j]
      }
    )
  )


islands_model_studies <- islands_model %>%
  filter(included_studies)

islands_model_species <- islands_model %>%
  filter(included_species)

model_sample_sizes <- tibble::tibble(
  model = c("Number of studies", "Number of studied species"),
  n_total = nrow(islands),
  n_modelled = c(
    nrow(islands_model_studies),
    nrow(islands_model_species)
  )
) %>%
  mutate(n_excluded = n_total - n_modelled)

model_sample_sizes

mod_bayes <- brms::brm(
  n_studies ~
    population_density +
    income_per_capita +
    area_km2 +
    island_type +
    dist_nearest_inst_km +
    wiki_views +
    distance_mainland_km +
    HII +
    lat,
  
  data = islands_model_studies,
  family = brms::negbinomial(link = "log"),
  chains = 4,
  cores = 4,
  iter = 4000,
  seed = 2026
)

summary(mod_bayes)

nobs(mod_bayes)

posterior <- tidybayes::spread_draws(
  mod_bayes,
  b_population_density,
  b_income_per_capita,
  b_area_km2,
  b_dist_nearest_inst_km,
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
      "b_population_density" = "Population density",
      "b_income_per_capita" = "Country income per capita",
      "b_area_km2" = "Island area",
      "b_island_typeoceanic" = "Oceanic island",
      "b_dist_nearest_inst_km" = "Distance to biodiv. inst.",
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
  ) %>%
  mutate(term = forcats::fct_reorder(term, mean))

posterior_long <- posterior_long %>%
  mutate(
    term = factor(
      term,
      levels = levels(posterior_summary$term)
    )
  )

relevant_effects <- posterior_summary %>%
  filter(lower > 0 | upper < 0) %>%
  arrange(desc(abs(mean)))

relevant_effects

relevant_effects_text <- relevant_effects %>%
  mutate(across(c(mean, lower, upper), ~ round(.x, 2)))

relevant_effects_text

## VIF: Number of studies -----------------------------------

mod_vif <- lm(
  n_studies ~
    population_density +
    income_per_capita +
    area_km2 +
    island_type +
    dist_nearest_inst_km +
    wiki_views +
    distance_mainland_km +
    HII +
    lat,
  data = islands_model_studies
)

vif_values <- car::vif(mod_vif)
vif_values

vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
) %>%
  mutate(
    Variable = dplyr::recode(
      Variable,
      "population_density" = "Population density",
      "income_per_capita" = "Country income per capita",
      "area_km2" = "Island area",
      "island_type" = "Oceanic island",
      "dist_nearest_inst_km" = "Distance to biodiv. inst.",
      "wiki_views" = "Societal interest",
      "distance_mainland_km" = "Distance to mainland",
      "HII" = "Human footprint",
      "lat" = "Latitude"
    )
  ) %>%
  arrange(desc(VIF))

vif_df$Variable <- factor(
  vif_df$Variable,
  levels = rev(vif_df$Variable)
)

vif_df

## Model evaluation ------------------------------------

rhat_vals <- brms::rhat(mod_bayes)
summary(rhat_vals)
max(rhat_vals, na.rm = TRUE)
brms::bayes_R2(mod_bayes)

## Figure 3A --------------------------------

fig3 <- ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "gray40"
  ) +
  stat_halfeye(
    data = posterior_long,
    aes(x = estimate, y = term),
    fill = "cyan4",
    alpha = 0.2,
    color = "cyan4",
    slab_color = NA,
    point_interval = "mean_qi",
    .width = 0.95
  ) +
  geom_segment(
    data = posterior_summary,
    aes(x = lower, xend = upper, y = term, yend = term),
    linewidth = 1.2,
    color = "black"
  ) +
  geom_point(
    data = posterior_summary,
    aes(x = mean, y = term),
    shape = 21,
    size = 4,
    fill = "cyan4",
    color = "black"
  ) +
  labs(
    title = "Number of studies",
    x = "Posterior estimate",
    y = NULL
  )+
  theme_bw(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

fig3

ggsave(
  "Figure_3_A.tiff",
  fig3,
  width = 7,
  height = 6,
  dpi = 300
)

# Bayesian model: number of studied species ---------------------

mod_bayes_species <- brms::brm(
  number_of_studies_species ~
    population_density +
    income_per_capita +
    area_km2 +
    island_type +
    dist_nearest_inst_km +
    wiki_views +
    distance_mainland_km +
    HII +
    lat,
  
  data = islands_model_species,
  family = brms::negbinomial(link = "log"),
  chains = 4,
  cores = 4,
  iter = 4000,
  seed = 2026
)

summary(mod_bayes_species)
nobs(mod_bayes_species)

model_sample_sizes$n_modelled <- c(
  nobs(mod_bayes),
  nobs(mod_bayes_species)
)

model_sample_sizes <- model_sample_sizes %>%
  mutate(n_excluded = n_total - n_modelled)

model_sample_sizes

posterior_species <- tidybayes::spread_draws(
  mod_bayes_species,
  b_population_density,
  b_income_per_capita,
  b_area_km2,
  b_dist_nearest_inst_km,
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
      "b_population_density" = "Population density",
      "b_income_per_capita" = "Country income per capita",
      "b_area_km2" = "Island area",
      "b_island_typeoceanic" = "Oceanic island",
      "b_dist_nearest_inst_km" = "Distance to biodiv. inst.",
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
  "Latitude",
  "Population density",
  "Country income per capita",
  "Human footprint",
  "Distance to biodiv. inst.",
  "Oceanic island"
))

posterior_species_summary <- posterior_species_long %>%
  group_by(term) %>%
  summarise(
    mean = mean(estimate),
    lower = quantile(estimate, 0.025),
    upper = quantile(estimate, 0.975),
    .groups = "drop"
  ) %>%
  mutate(term = factor(term, levels = term_order))

posterior_species_long <- posterior_species_long %>%
  mutate(term = factor(term, levels = term_order))

relevant_effects_species <- posterior_species_summary %>%
  filter(lower > 0 | upper < 0) %>%
  arrange(desc(abs(mean)))

posterior_species_summary
relevant_effects_species

relevant_effects_text_sp <- posterior_species_summary %>%
  mutate(across(c(mean, lower, upper), ~ round(.x, 2)))

relevant_effects_text_sp

## VIF: Number of studied species ---------------------------

mod_vif_species <- lm(
  number_of_studies_species ~
    population_density +
    income_per_capita +
    area_km2 +
    island_type +
    dist_nearest_inst_km +
    wiki_views +
    distance_mainland_km +
    HII +
    lat,
  data = islands_model_species
)

car::vif(mod_vif_species)

### Model evaluation ------------------------------------

rhat_vals_s <- brms::rhat(mod_bayes_species)
summary(rhat_vals_s)
max(rhat_vals_s, na.rm = TRUE)
brms::bayes_R2(mod_bayes_species)

## Figure 3B --------------------------------------------

term_order <- levels(posterior_summary$term)

posterior_species_summary <- posterior_species_summary %>%
  mutate(term = factor(term, levels = term_order))

posterior_species_long <- posterior_species_long %>%
  mutate(term = factor(term, levels = term_order))

fig3_B <- ggplot() +
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
    title = "Number of species studied",
    x = "Posterior estimate",
    y = NULL
  )+
  theme_bw(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

fig3_B

ggsave(
  "Figure_3_B.tiff",
  fig3_B,
  width = 7,
  height = 6,
  dpi = 300
)

# Figure 3 Complete -------------------------------------

fig3_complete <- fig3 + fig3_B

ggsave(
  "Figure_3.tiff",
  fig3_complete,
  dpi = 300,
  width = 12,
  height = 6
)

# Supplementary -----------------------------------------
## Figure S1_A ------------------------------------------

fig_S1_A <- brms::pp_check(
  mod_bayes,
  type = "dens_overlay",
  ndraws = 100
) +
  theme_classic(base_size = 15) +
  labs(x = "Number of studies", y = "Density") +
  theme(
    legend.position = c(0.88, 0.88),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.8),
      colour = NA
    ),
    axis.text = element_text(color = "black")
  )

fig_S1_A

## Figure S1_B ------------------------------------------

fig_S1_B <- brms::pp_check(
  mod_bayes_species,
  type = "dens_overlay",
  ndraws = 100
) +
  theme_classic(base_size = 15) +
  labs(x = "Number of species studied", y = "Density") +
  theme(
    legend.position = c(0.88, 0.88),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.8),
      colour = NA
    ),
    axis.text = element_text(color = "black")
  )

fig_S1_B

## Figure S1 Complete -----------------------------------

Figure_S1 <- fig_S1_A +
  fig_S1_B +
  patchwork::plot_annotation(tag_levels = "A")

Figure_S1

ggsave(
  "Figure_S1.tiff",
  Figure_S1,
  width = 12,
  height = 5,
  dpi = 300
)
