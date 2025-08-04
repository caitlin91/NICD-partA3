library(brms)
library(tidybayes)
library(extraDistr)
library(tidyverse)
library(janitor)
library(MuMIn)
library(broom.mixed)
library(xtable)
library(scales)
library(rcartocolor)


# Seed and data ####
my_seed <- 7863
set.seed(my_seed)
num_cores <- parallel::detectCores()-2
df_read <- read_csv("house_prices_ne/house_prices_ne.csv") |>
  clean_names()
df <- df_read|>
  drop_na(price)

# na free data ####
df_clean <- na.omit(df)

# data without outliers
df_alt <- df |> filter(price <1500000)

# Priors ####
get_prior(formula = price ~ location +
            bedrooms +
            bathrooms +
            size_sqft +
            ownership +
            property_type +
            garden,
          family = lognormal,
          data = )

# note log normal family therefore log the priors

my_priors <- c(
  prior(normal(12.1,12.42), class = "Intercept"),
  prior(normal(0,10.8), class = "sigma"),
  prior(normal(6.21,5.52), class = "b", coef = size_sqft)
  # prior(normal(180000,250000), class = "Intercept"),
  # prior(normal(0,50000), class = "sigma"),
  # prior(normal(500,250), class = "b", coef = size_sqft)
  )


# Model specification ####

model_global <- lm(price ~ location +
                bedrooms +
                bathrooms +
                size_sqft +
                ownership +
                property_type +
                garden,
                data = _clean,
                na.action = "na.fail"
                )

formulas <- dredge(model_global, evaluate = FALSE)
model_formulas <- lapply(formulas, function(call_obj) {
  call_obj$formula
})



# Model fitting ####
model_list <- lapply(seq_along(model_formulas), function(i) {
  message("Fitting model ", i, " of ", length(model_formulas))

  brm(
    formula = model_formulas[[i]],
    data = _clean,
    seed = my_seed,
    family = lognormal,
    prior = my_priors,
    cores = num_cores,
    refresh = 0,
    silent = TRUE
  )
})

# Compile Results ####
valid_models <- Filter(function(m) {
  inherits(m, "brmsfit")
}, model_list)
r2_values <- sapply(valid_models, function(m) {
  bayes_R2(m)[1, "Estimate"]
})
loo_list <- lapply(valid_models, function(m) {
  tryCatch(
    brms::loo(m),
    error = function(e) {
      message("LOO failed for one model: ", e$message)
      return(NULL)
    }
  )
})


model_comparison <- data.frame(
  model_index = seq_along(model_list),
  bayes_R2 = round(r2_values, 3),
  elpd_diff = -loo_compare(loo_list)[, "elpd_diff"]
  )

write_csv(model_comparison, "modelcomparisontable.csv")

# Final model ####
model_final <- brm(price ~ location +
                     bedrooms +
                     bathrooms +
                     size_sqft +
                     ownership +
                     property_type +
                     garden,
                   data = df,
                   seed = my_seed,
                   family = lognormal,
                   prior = my_priors,
                   cores = num_cores,
                   refresh = 0
)

model_final
model_final_tidy <- tidy(model_final) |>
  mutate(intercept = estimate[term == "(Intercept)"]) |>
  mutate(
    conv_estimate  = if_else(term == "(Intercept)",
                             exp(estimate),
                             exp(intercept + estimate)),
    conv_conf.low  = if_else(term == "(Intercept)",
                             exp(conf.low),
                             exp(intercept + conf.low)),
    conv_conf.high = if_else(term == "(Intercept)",
                             exp(conf.high),
                             exp(intercept + conf.high))
  ) |>
  select(-intercept)

writeLines(print(
  xtable(model_final_tidy,
         caption = "Regression Coefficients of the final model, including conversion from log values",
         label = "tab:regcoef"),
  type = "latex",
  include.rownames = FALSE,
  caption.placement = "top",
  print.results = FALSE),
  "tex/regcoeff.tex"
  )

# Predictions ####
locations <- unique(df$location)
ownerships <- unique(df$ownership)
property_types <- unique(df$property_type)
gardens <- unique(df$garden)
bedroom_range <- range(df_alt$bedrooms, na.rm = TRUE)
bathroom_range <- range(df_alt$bathrooms, na.rm = TRUE)
size_range <- range(df_alt$size_sqft, na.rm = TRUE)

random_grid <- tibble(
  location = sample(locations, 10, replace = TRUE),
  bedrooms = sample(seq(bedroom_range[1], bedroom_range[2]), 10, replace = TRUE),
  bathrooms = sample(seq(bathroom_range[1], bathroom_range[2]), 10, replace = TRUE),
  size_sqft = sample(seq(size_range[1], size_range[2], by = 10), 10, replace = TRUE),
  ownership = sample(ownerships, 10, replace = TRUE),
  property_type = sample(property_types, 10, replace = TRUE),
  garden = sample(gardens, 10, replace = TRUE)
)


predictions <- predict(model_final,
                       newdata = random_grid,
                       probs = c(0.165, 0.835)) |>
  as_tibble() |>
  rename(
    prediction = Estimate,
  )
predictions_final <- bind_cols(random_grid, predictions)

writeLines(print(
  xtable(predictions_final,
         caption = "Example predictions",
         label = "tab:pred"),
  type = "latex",
  include.rownames = FALSE,
  caption.placement = "top",
  print.results = FALSE),
  "tex/pred.tex"
)

# Plotting ####

estimates.plot <- model_final %>%
  as_draws_df() %>%
  select(starts_with("b_")) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = fct_relevel(name, "b_Intercept", after = Inf)) %>%
  mutate(group = case_when(
    str_detect(name, "location") ~ "location",
    str_detect(name, "property_type") ~ "property_type",
    str_detect(name, "garden") ~ "garden",
    str_detect(name, "ownership") ~ "ownership",
    str_detect(name, "bedrooms") ~ "bedrooms",
    str_detect(name, "bathrooms") ~ "bathrooms",
    str_detect(name, "size_sqft") ~ "size_sqft",
    TRUE ~ "Other"
  )) %>%
  ggplot(aes(x = value, y = name, colour = group)) +
  stat_halfeye() +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Estimate", y = "Coefficient") +
  scale_colour_manual(values = carto_pal(8, "Safe")) +
  theme(legend.position = "none")+
  NULL

ggsave("figures/estimates.png", estimates.plot, width = 6, height = 4, units = "in")


model_estimates <- ggplot(model_final_tidy, aes(x = estimate, y = name)) +
  geom_halfeyeh() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Estimate", y = "Coefficient") +
  theme_minimal()

mean_plot <- as_draws_df(model_final) |>
  ggplot(aes(x = exp(b_Intercept))) +
  geom_density(fill = "#214d65", alpha = 0.8) +
  xlab("Mean price") +
  NULL
mean_plot

ggsave("figures/meanplot.png", mean_plot, width = 6, height = 4, units = "in")

ce_location <- conditional_effects(model_final,
                                   effects = "location",
                                   plot = FALSE)
df_location <- ce_location[[1]]

ce_location.plot <- ggplot(df_location, aes(x = location, y = estimate__)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2) +
  scale_y_continuous(limits = c(0,500000), labels = label_currency(prefix = "£"))+
  theme_bw()+
  NULL
ggsave("figures/celocation.png", ce_location.plot, , width = 6, height = 4, units = "in")

ce_garden <- conditional_effects(model_final,
                                   effects = "garden",
                                   plot = FALSE)
df_garden <- ce_garden[[1]]

ce_garden.plot <- ggplot(df_garden, aes(x = garden, y = estimate__)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2) +
  scale_y_continuous(limits = c(0,500000), labels = label_currency(prefix = "£"))+
  theme_bw()+
  NULL
ggsave("figures/cegarden.png", ce_garden.plot, , width = 6, height = 4, units = "in")
