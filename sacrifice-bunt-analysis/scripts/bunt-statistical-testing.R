library(tidyverse)
library(broom)

# Statistical Testing #########################################################

# Logistic regression ---------------------------------------------------------

bunt_model <- glm(
  successful_bunt ~ outs_bucket + base_state,
  
  data = sac_bunts_2023_2026,
  
  family = "binomial"
)

summary(bunt_model)

# Odds ratios -----------------------------------------------------------------

exp(coef(bunt_model))

# Confidence intervals --------------------------------------------------------

exp(confint(bunt_model))

# Predicted probabilities -----------------------------------------------------

predict(
  bunt_model,
  
  type = "response"
)

# Clean model results ---------------------------------------------------------

model_results <- tidy(
  bunt_model,
  
  exponentiate = TRUE,
  
  conf.int = TRUE
) |>
  
  arrange(p.value)

model_results
