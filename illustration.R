
library(tidyverse)

set.seed(90095)

sim <- tibble(x = runif(20)) |>
  mutate(y = rnorm(n(), sqrt(x), .1))

fit <- lm(y ~ x, data = sim)
prediction <- predict(fit, newdata = tibble(x = .4))

plot <- sim |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  xlab("Predictor X") +
  ylab("Outcome Y") +
  theme_classic()

ggsave("slides/illustration_1.pdf", plot = plot, height = 4, width = 4)

plot <- plot +
  geom_smooth(method = "lm", se = F) +
  annotate(geom = "text", color = "blue",
           x = .6, y = .55,
           label = paste0("Beta = ",round(coef(fit)[2],2)))

ggsave("slides/illustration_2.pdf", plot = plot, height = 4, width = 4)

plot <- plot +
  geom_vline(xintercept = 0.4, size = 4, alpha = .2, color = "seagreen") +
  annotate(geom = "point", 
           x = 0.4, 
           y = prediction,
           color = "seagreen", size = 3) +
  annotate(geom = "text", x = .35, y = .85,
           hjust = 1, 
           label = paste0("At X = 0.40,\nYhat = ",round(prediction,2)),
           color = "seagreen")

ggsave("slides/illustration_3.pdf", plot = plot, height = 4, width = 4)

  
# rpart as plug-in

fit <- rpart::rpart(y ~ x, data = sim)
prediction <- predict(fit, newdata = tibble(x = .4))
predicted_steps <- tibble(x = seq(0,1,.001))
predicted_steps <- predicted_steps |>
  mutate(yhat = predict(fit, newdata = predicted_steps))

sim |> 
  ggplot(aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(data = predicted_steps, aes(x = x, y = yhat), color = "blue") +
  xlab("Predictor X") +
  ylab("Outcome Y") +
  theme_classic() +
  geom_vline(xintercept = 0.4, size = 4, alpha = .2, color = "seagreen") +
  annotate(geom = "point", 
           x = 0.4, 
           y = prediction,
           color = "seagreen", size = 3) +
  annotate(geom = "text", x = .35, y = .85,
           hjust = 1, 
           label = paste0("At X = 0.40,\nYhat = ",round(prediction,2)),
           color = "seagreen")

ggsave("slides/illustration_rpart.pdf", height = 4, width = 4)

# loess as plug-in

fit <- loess(y ~ x, data = sim)
prediction <- predict(fit, newdata = tibble(x = .4))
predicted_curve <- tibble(x = seq(0,1,.001))
predicted_curve <- predicted_curve |>
  mutate(yhat = predict(fit, newdata = predicted_curve))

sim |> 
  ggplot(aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(data = predicted_curve, aes(x = x, y = yhat), color = "blue") +
  xlab("Predictor X") +
  ylab("Outcome Y") +
  theme_classic() +
  geom_vline(xintercept = 0.4, size = 4, alpha = .2, color = "seagreen") +
  annotate(geom = "point", 
           x = 0.4, 
           y = prediction,
           color = "seagreen", size = 3) +
  annotate(geom = "text", x = .35, y = .85,
           hjust = 1, 
           label = paste0("At X = 0.40,\nYhat = ",round(prediction,2)),
           color = "seagreen")

ggsave("slides/illustration_loess.pdf", height = 4, width = 4)

