library(nnet)
library(dplyr)
library(tidyr)
library(ggplot2)

newdat <- expand.grid(
  adi_rank = seq(min(phx_bg$adi_rank, na.rm = TRUE),
                 max(phx_bg$adi_rank, na.rm = TRUE),
                 length.out = 100),
  pct_hispanic = c(25, 50, 75),
  pct_black = c(5, 20, 40)
)

probs <- predict(multinom2, newdata = newdat, type = "probs")
probs <- as.data.frame(probs)

plotdat <- bind_cols(newdat, probs) |>
  pivot_longer(cols = c(HH, HL, LH, LL),
               names_to = "category",
               values_to = "prob")

ggplot(plotdat, aes(x = adi_rank, y = prob, color = category)) +
  geom_line(linewidth = 1) +
  facet_grid(pct_hispanic ~ pct_black) +
  labs(x = "ADI rank", y = "Predicted probability")