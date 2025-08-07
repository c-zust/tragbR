#' maximaler Hauspreis solver 2
#'
#' Ziel: Finde ein optimales z, welches den Kaufpreis y maximiert. Dabei stellt
#' z der Anteil des eigenen Vermögens dar, welcher von der Bank vervielfacht
#' wird (--> 0 <= z <= 1).
#'
#' @param V Vermögen
#' @param E Einkommen
#' @param r Zinssatz der Hypothek
#' @param u Prozentsatz Unterhaltskosten
#' @param alpha maximaler Einkommensanteil zur Bedienung der Hypothek
#' @param min_eigenmittel Minimale Eigenmittel in Prozent
#' @param amortisation_jahre Anzal Jahre bis zur minimalen Amortisation
#' @param amortisation_grenze maximale Belehnung nach X Jahren
#' @param z0 Startwert für Vermögensanteil z, über welchen iterriert wird
#'
#' @return Eine Liste mit dem optimalen y und z pro Maximierungsproblem. Dazu
#' kommen für beide Maximierungsprobleme die Werte, wenn die y und z in die
#' jeweiligen Nebenbedingungen eingesetzt werden.
#' @export
solve_max_problem <- function(V, E,
                              r = 0.05, u = 0.007,
                              alpha = 0.35, min_eigenmittel = 0.2,
                              amortisation_jahre = 15, amortisation_grenze = 2 / 3,
                              z0 = 0.5) {
  # berechne Multiplikator --> Faktor, mit dem das Vermögen multipliziert wird
  multiplikator <- (1 - min_eigenmittel) / min_eigenmittel



  # Objective function (Zielfunktion)
  objective <- function(z, V, E, r, u, alpha, amortisation_jahre, amortisation_grenze, multiplikator) {
    obj <- V + V * z * multiplikator

    # nloptr löst immer nur Minimierungsprobleme, deshabl müssen wir alle Vor-
    # zeichen der objective function umkehren, damit man ein Maximiuerungsproblem hat
    return(-obj)
  }


  # Constraints (Nebenbedingungen)
  eval_g_ineq_max1 <- function(z, V, E, r, u, alpha, amortisation_jahre, amortisation_grenze, multiplikator) {
    hypothek <- V * z * multiplikator
    hauspreis <- V + hypothek

    hypo_zins <- hypothek * r
    unterhalt <- hauspreis * u

    amortisation <- (hypothek / hauspreis - amortisation_grenze) *
      hauspreis / amortisation_jahre

    E_Anteil <- alpha * E

    # Formulierung der Constraints
    return(rbind(
      # Kosten des Hauses dürfen nicht grösser sein als Einkommenanteil
      c(hypo_zins + unterhalt + amortisation - E_Anteil),
      # Der Amortisationsterm muss immer >= 0 sein. --> Wenn Amortisation = 0,
      # dann fällt dieser Term automatisch aus dem obigem Constraint weg
      # Wichtig: Es braucht das "-" vor der Vektorfunktion, damit der Ausdruck
      # als >= 0 evaluiert wird.
      -c(hypothek / hauspreis - amortisation_grenze - 0.000001)
    ))
  }


  eval_g_ineq_max2 <- function(z, V, E, r, u, alpha, amortisation_jahre, amortisation_grenze, multiplikator) {
    hypothek <- V * z * multiplikator
    hauspreis <- V + hypothek

    hypo_zins <- hypothek * r
    unterhalt <- hauspreis * u

    E_Anteil <- alpha * E

    # Formulierung des Constraints
    return(
      # Kosten des Hauses dürfen nicht grösser sein als Einkommenanteil
      c(hypo_zins + unterhalt - E_Anteil)
    )
  }


  # Define bounds for z
  lb <- 0 # Lower bound
  ub <- 1 # Upper bound



  # Solve using nloptr

  # 2 constraints --> mit Amortisation
  result_max1 <- nloptr::nloptr(
    x0 = z0, # Startwert für z
    eval_f = objective,
    eval_g_ineq = eval_g_ineq_max1,
    lb = lb,
    ub = ub,
    opts = list(
      # Dieser Algorhythmus benötigt keinen Gradienten
      algorithm = "NLOPT_LN_COBYLA",
      xtol_rel = 1e-6, # Konvergenztoleranz
      xtol_abs = 1e-8,
      maxeval = 4000 # Maximale Anzahl Iterationen
    ),
    V = V,
    E = E,
    r = r,
    u = u,
    alpha = alpha,
    amortisation_jahre = amortisation_jahre,
    amortisation_grenze = amortisation_grenze,
    multiplikator = multiplikator
  )


  # nur 1 constraint --> keine Amortisation
  result_max2 <- nloptr::nloptr(
    x0 = z0, # Startwert für z
    eval_f = objective,
    eval_g_ineq = eval_g_ineq_max2,
    lb = lb,
    ub = ub,
    opts = list(
      # Dieser Algorhythmus benötigt keinen Gradienten
      algorithm = "NLOPT_LN_COBYLA",
      xtol_rel = 1e-6, # Konvergenztoleranz
      xtol_abs = 1e-8,
      maxeval = 4000 # Maximale Anzahl Iterationen
    ),
    V = V,
    E = E,
    r = r,
    u = u,
    alpha = alpha,
    amortisation_jahre = amortisation_jahre,
    amortisation_grenze = amortisation_grenze,
    multiplikator = multiplikator
  )

  constraints_max1 <- eval_g_ineq_max1(result_max1$solution, V, E, r, u, alpha, amortisation_jahre, amortisation_grenze, multiplikator)
  constraints_max2 <- eval_g_ineq_max2(result_max2$solution, V, E, r, u, alpha, amortisation_jahre, amortisation_grenze, multiplikator)


  x <- c(
    z_amor = round(result_max1$solution, 3),
    y_amor = -round(result_max1$objective, 1), # Vorzeichen umkehren, da Max-Problem
    iterations_amor = result_max1$iterations,
    constraint1_amor = round(constraints_max1[1, 1], 5),
    constraint2_amor = -round(constraints_max1[2, 1], 5), # Vorzeichen umkehren, da der Constraint > 0 sein muss
    z_no_amor = round(result_max2$solution, 3),
    y_no_amor = -round(result_max2$objective, 1), # Vorzeichen umkehren, da Max-Problem
    iterations_no_amor = result_max2$iterations,
    constraint1_no_amor = round(constraints_max2, 5)
  )



  return(x)
}
