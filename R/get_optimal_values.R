#' Hauspreisrechner
#'
#' Basierend auf dem Vermögen (V) und dem Einkommen (E) wird mit dieser Funktion
#' der maximal leistbare Hauspreis bestimmt. In einer Vielzahl von Fällen lässt
#' sich der maximale Hauspreis mit einfach Heuristiken herleiten. Wo dies nicht
#' der Fall ist, wird das optimale y und z mit Hilfer der Funktion
#' `solve_max_problem` berechnet.
#'
#'
#'
#' @inheritParams solve_max_problem
#'
#' @return Eine Liste mit dem optimalen y und z pro Maximierungsproblem. Dazu
#' kommen für beide Maximierungsprobleme die Werte, wenn die y und z in die
#' jeweiligen Nebenbedingungen eingesetzt werden.
#' @export
get_optimal_values <- function(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0) {
  if ((E == 0 | is.na(E)) & (V < 0 | is.na(V))) {
    x <- c(
      z_optimal = NA,
      y_optimal = NA
    )
  } else {
    # Maximierungsproblem konvergiert nicht, wenn dies der Fall ist
    if (0.02 * V >= E | (E == 0 & V >= 0) | V == 0) {
      x <- c(
        z_optimal = 0,
        y_optimal = V
      )
    } else {
      if (V <= 0) {
        x <- c(
          z_optimal = 0,
          y_optimal = 0
        )
      } else {
        # Maximierungsproblem muss nicht gelöst werden, wenn E > V, dann automatisch y = V* 1/min_eigenmittel
        if (E >= V) {
          x <- c(
            z_optimal = 1,
            y_optimal = V * 1/min_eigenmittel
          )
        } else {
          x <- solve_max_problem(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

          x <- c(z_optimal = ifelse(x[["constraint1_amor"]] <= 0 & x[["constraint2_amor"]] > 0,
                                    x[["z_amor"]],
                                    x[["z_no_amor"]]),
                 y_optimal  = ifelse(x[["constraint1_amor"]] <= 0 & x[["constraint2_amor"]] > 0,
                                     x[["y_amor"]],
                                     x[["y_no_amor"]])
          )
        }
      }
    }
  }

  return(x)
}


#' Get optimal z
#'
#' @inheritParams solve_max_problem
#'
#' @returns Der Anteil des Vermögens, welcher den Kaufpreis eines Hauses maximiert
#' @export
get_optimal_z <- function(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0) {

    x <- get_optimal_values(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

  return(x["z_optimal"])
}



#' Get optimal y
#'
#' @inheritParams solve_max_problem

#'
#' @returns Der maximale Kaufpreis eines Hauses
#' @export
get_optimal_y <- function(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0) {

  x <- get_optimal_values(V, E, r, u, alpha, min_eigenmittel, amortisation_jahre, amortisation_grenze, z0)

  return(x["y_optimal"])
}




