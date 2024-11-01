# This code uses a large ACS sample to generate what we will consider to be true population parameters. All illustrations will work with simulated smaller samples using these true parameters.

library(tidyverse)
library(haven)

# Load an inflation adjustment factor to adjust all money to 2019 dollars

inflation_raw <- read_csv("/Users/ilundberg/Library/CloudStorage/Box-Box/description_project/data/inflation.csv")
inflation <- inflation_raw |>
  mutate(inflation_factor = inflation_factor / 
           inflation_raw |> filter(year == 2019) |> pull(inflation_factor))

# Load real data

d <- read_dta("/Users/ilundberg/Library/CloudStorage/Box-Box/description_project/data/usa_00042.dta") |>
  # Restrict to full-year full-time workers
  filter(wkswork2 == 6 & uhrswork >= 35) |>
  # Inflation adjustment
  left_join(inflation, by = join_by(year)) |>
  mutate(incwage = incwage * inflation_factor) |>
  select(-inflation_factor) |>
  # Bottom-code income at 5th percentile
  mutate(
    incwage = case_when(
      incwage < quantile(incwage, .05) ~ quantile(incwage, .05),
      T ~ incwage
    ),
    sex = as_factor(sex),
    year = as.numeric(year),
    age = as.numeric(age)
  )

true_parameters <- d |>
  group_by(year, age, sex) |>
  summarize(
    meanlog = weighted.mean(log(incwage), w = perwt),
    sdlog = sqrt(weighted.mean((log(incwage) - meanlog) ^ 2, w = perwt)),
    weight = sum(perwt),
    .groups = "drop"
  ) |>
  mutate(weight = weight / sum(weight)) |>
  ungroup()

write_csv(true_parameters, file = "assets/truth.csv")

