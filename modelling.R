library(brms)
library(extraDistr)
library(tidyverse)
library(janitor)
library(MuMIn)
library(broom.mixed)
library(knitr)


# Seed and data ####
my_seed <- 7863
num_cores <- parallel::detectCores()-2

data <- read_csv("house_prices_ne/house_prices_ne.csv") |>
  clean_names() |>
  drop_na(price)

# Priors ####
get_prior(formula = price ~ location +
            bedrooms +
            bathrooms +
            size_sqft +
            ownership +
            property_type +
            garden,
          data = data)

my_priors <- c(
  prior(normal(12.2,13.8), class = "Intercept"),
  prior(normal(0,10.8), class = "sigma"),
  prior(normal(6.9,8.5), class = "b", coef = size_sqft)
  # prior(normal(200000,1000000), class = "Intercept"),
  # prior(normal(0,50000), class = "sigma"),
  # prior(normal(1000,5000), class = "b", coef = size_sqft)
  )

# na free data ####
data_clean <- na.omit(data)
# Model specification ####

model_global <- lm(price ~ location +
                bedrooms +
                bathrooms +
                size_sqft +
                ownership +
                property_type +
                garden,
                data = data_clean,
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
    data = data_clean,
    seed = my_seed,
    family = gaussian(),
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

model_final <- brm(price ~ location +
                     bedrooms +
                     bathrooms +
                     size_sqft +
                     ownership +
                     property_type +
                     garden,
                   data = data,
                   seed = my_seed,
                   family = lognormal,
                   prior = my_priors,
                   cores = num_cores,
                   refresh = 0
)

model_final
tidy(model_final) |>
  mutate(intercept = estimate[term == "(Intercept)"]) %>%
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

write
