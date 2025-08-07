library(ggplot2)
library(nloptr)


# Test mit solver2.R --> nloptr



V <- 100000
r <- 0.05
u <- 0.007
alpha <- 0.35
min_eigenmittel <- 0.2
amortisation_jahre <- 15
amortisation_grenze <- 2 / 3
z0 <- 0.5


solve_max_problem(100000, 35000, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

get_optimal_values(100000, 35000, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)





minE <- ceiling(0.02 * V * 0.001) * 1000

E <- seq(minE, V, by = 1000)

df1 <- purrr::map_df(E, ~ {
  solve_max_problem(V, .x, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)
})

df1 <- cbind(E, df1)


x <- which(df1$constraint1_amor <= 0 & df1$constraint2_amor > 0)[1]
x

x_range <- c(x-1, x, x+1)
df1[x_range, ]



options(scipen=999)

# this loop tests at what share of income compared to wealth the amortisation boundary (= 0.66) is reached
df_graph <- purrr::map_df( 1:100 / 100, ~{

  alpha <- .x

  V <- 100000
  r <- 0.05
  u <- 0.007

  min_eigenmittel <- 0.2
  amortisation_jahre <- 15
  amortisation_grenze <- 2 / 3
  z0 <- 0.5

  minE <- ceiling(0.02 * V * 0.001) * 1000

  E <- seq(minE, V, by = 100)

  df1 <- purrr::map_df(E, ~ {
    solve_max_problem(V, .x, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)
  })

  df1 <- cbind(E, alpha, df1)

  # find the entry that
  x <- min(which(df1$y_amor < df1$y_no_amor))
  x

  df1[x,]

})

df_graph <- df_graph |>
  dplyr::mutate(income_share_at_amo_boundary = E / 100000)


ggplot() +
  geom_line(data = df_graph, aes(x = alpha, y = income_share_at_amo_boundary)) +
  geom_vline(aes(xintercept = 0.35), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = 0.35), color = "red", linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = 1), color = "blue", linetype = "dotted")







