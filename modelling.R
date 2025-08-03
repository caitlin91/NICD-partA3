library(brms)
library(extraDistr)
library(tidyverse)
library(janitor)
library(MuMIn)

# Seed and data ####
my_seed <- 7863
num_cores <- parallel::detectCores()-2

data <- read_csv("house_prices_ne/house_prices_ne.csv") |>
  clean_names()

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
  prior(normal(200000,1000000), class = "Intercept"),
  prior(normal(0,50000), class = "sigma")
  # set_prior(normal(0, 1), class = "b")
  )  # <-- Customize as needed

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
model_list <- lapply(seq_along(valid_formulas), function(i) {
  message("Fitting model ", i, " of ", length(model_formulas))

  brm(
    formula = valid_formulas[[i]],
    data = data_clean,
    family = gaussian(),
    cores = num_cores,
    refresh = 0,
    silent = TRUE
  )
})

# seed = my_seed,
# family = gaussian,
# prior = FSF1_priors,
# cores = 4,
# file = "models/FSF1-bmod"

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
      message("⚠️ LOO failed for one model: ", e$message)
      return(NULL)
    }
  )
})


model_comparison <- data.frame(
  model_index = seq_along(model_list),
  bayes_R2 = round(r2_values, 3),
  elpd_diff = -loo_compare(loo_list)[, "elpd_diff"]
  )


View(model_comparison)

