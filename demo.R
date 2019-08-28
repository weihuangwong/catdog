# Cats and dogs

# stats
library(survey)

# data
library(purrr)
library(dplyr)
library(broom)

# io
library(openxlsx)

# viz
library(ggplot2)

# =============================================================================
# Prepare data
# =============================================================================

d <- readRDS("catdog.rds")

# =============================================================================
# Model, tidy, and output to Excel
# =============================================================================

# Account for survey design
des <- svydesign(id = ~1, weights = ~wtssall, data = d)

fit1 <- svyglm(dog ~ sex, design = des)

# =============================================================================
# Prediction
# =============================================================================

groups <- distinct(d, educ1, sex) %>%
  arrange(sex, educ1) 

predictions <- groups %>%
  bind_cols(as.data.frame(predict(fit4, newdata = groups))) %>%
  mutate(
    lwr = link - 1.96 * SE, 
    upr = link + 1.96 * SE,
    label = paste(educ1, sex)
  )

predictions %>%
  ggplot(aes(x = label, y = link, ymin = lwr, ymax = upr, label = round(link, 2))) +
  geom_pointrange()

newdata_age_gender <- expand.grid(sex = c("male", "female"), age = seq(min(d$age), max(d$age)))

predictions_age <- newdata_age_gender %>%
  bind_cols(as.data.frame(predict(fit5, newdata = newdata_age_gender))) %>%
  mutate(
    lwr = link - 1.96 * SE, 
    upr = link + 1.96 * SE
  )

predictions_age %>%
  ggplot(aes(x = age, y = link, ymin = lwr, ymax = upr, color = sex, fill = sex)) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linetype = 0)
