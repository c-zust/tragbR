testthat::test_that(paste0("As soon as the (V*z*m / Y) - x > 0 ",
                 "and when the amortisation constraint of the first max-problem,",
                 " is satisfied), the optimal solution is the y_amor "), {

  V <- 100000
  r <- 0.05
  u <- 0.007
  alpha <- 0.35
  min_eigenmittel <- 0.2
  amortisation_jahre <- 15
  amortisation_grenze <- 2 / 3
  z0 <- 0.5

  minE <- ceiling(0.02 * V * 0.001) * 1000

  E <- seq(minE, V, by = 1000)

  df1 <- purrr::map_df(E, ~ {
    solve_max_problem(V, .x, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)
  })

  amortisation_boundary <- which(df1$constraint1_amor <= 0 & df1$constraint2_amor > 0)[1]

  boundary <- df1[amortisation_boundary,]

  optimal_values <- get_optimal_values(V, E = E[amortisation_boundary], r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

  optimal_values


  testthat::expect_equal(boundary$y_amor, optimal_values[["y_optimal"]])

})





testthat::test_that("(E == 0 | is.na(E)) & (V < 0 | is.na(V))", {

  V <- -1
  E <- NA
  r <- 0.05
  u <- 0.007
  alpha <- 0.35
  min_eigenmittel <- 0.2
  amortisation_jahre <- 15
  amortisation_grenze <- 2 / 3
  z0 <- 0.5

  optimal_values <- get_optimal_values(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

  testthat::expect_equal(optimal_values, c(z_optimal = NA, y_optimal = NA))


})

testthat::test_that("(0.02 * V >= E | (E == 0 & V >= 0) | V == 0)", {

  V1 <- 100000
  V3 <- 0
  E1 <- 1000
  E2 <- 0
  r <- 0.05
  u <- 0.007
  alpha <- 0.35
  min_eigenmittel <- 0.2
  amortisation_jahre <- 15
  amortisation_grenze <- 2 / 3
  z0 <- 0.5

  optimal_values1 <- get_optimal_values(V1, E1, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)
  optimal_values2 <- get_optimal_values(V1, E2, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)
  optimal_values3 <- get_optimal_values(V3, E1, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

  # should either be 0 or 100'000 (= V1)
  x <- unique(c(optimal_values1, optimal_values2, optimal_values3))


  testthat::expect_equal(x, c(0, V1))

})


testthat::test_that("(V <= 0)", {


  V <- -1
  E <- 100000
  r <- 0.05
  u <- 0.007
  alpha <- 0.35
  min_eigenmittel <- 0.2
  amortisation_jahre <- 15
  amortisation_grenze <- 2 / 3
  z0 <- 0.5

  optimal_values <- get_optimal_values(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

  testthat::expect_equal(optimal_values, c(z_optimal = 0, y_optimal = 0))


})


testthat::test_that("(E >= V)", {

  V <- 100000
  E <- 100001
  r <- 0.05
  u <- 0.007
  alpha <- 0.35
  min_eigenmittel <- 0.2
  amortisation_jahre <- 15
  amortisation_grenze <- 2 / 3
  z0 <- 0.5

  optimal_values <- get_optimal_values(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

  testthat::expect_equal(optimal_values, c(z_optimal = 1, y_optimal = V * (1/min_eigenmittel)))


})
