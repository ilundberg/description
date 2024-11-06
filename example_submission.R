
library(tidyverse)

simulate <- function(n = 100) {
  read_csv("https://ilundberg.github.io/description/assets/truth.csv") |>
    slice_sample(n = n, weight_by = weight, replace = T) |>
    mutate(income = exp(rnorm(n(), meanlog, sdlog))) |>
    select(year, age, sex, income)
}

simulated <- simulate(n = 100)
