r <- seq(0.1, 0.99, 0.01)
data_r_d <- tibble(
  r=r,
  t_d4=sapply(r, function(x) projektion_dauer(100, 50, x, 4)),
  t_d10=sapply(r, function(x) projektion_dauer(100, 50, x, 10))
) %>%
  pivot_longer(t_d4:t_d10)

ggplot(data_r_d,
       aes(x=r, y=value, col=name)) +
  geom_line() +
  scale_y_log10()
