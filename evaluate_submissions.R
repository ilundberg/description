
# Evaluate challenge exercise

predictions <- read_csv(
  "/users/ilundberg/downloads/Forecasting challenge_ Sex gap in pay (Responses) - Form Responses 1 (2).csv",
  col_names = c("time","name","sample_size","male","female"),
  skip = 1
)
                        

truth <- read_csv("assets/challenge_truth.csv")

results <- predictions |>
  pivot_longer(cols = c("male","female"), names_to = "sex", values_to = "yhat") |>
  left_join(truth, by = "sex") |>
  mutate(sq_error_log_scale = (log(yhat) - meanlog) ^ 2) |>
  select(name, sample_size, sex, prediction = yhat, truth = income, sq_error_log_scale) |>
  arrange(sq_error_log_scale)

results |>
  filter(sex == "male")

results |>
  filter(sex == "female")
