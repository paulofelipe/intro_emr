library(emr)

# Inicializando os componentes --------------------------
params <- list()
variables <- list()
equations <- list()


# Oferta ------------------------------------------------------------------

params[["k_s"]] <- create_param(
  value = 1,
  indexes = list(k_s = "k_s"),
  desc = "shift da curva de oferta"
)

params[["epsilon"]] <- create_param(
  value = 1,
  indexes = list(epsilon = "epsilon"),
  desc = "elasticidade-preço da oferta"
)

variables[["Q_s"]] <- create_variable(
  value = 1,
  indexes = list(Q_s = "Q_s"),
  type = "defined",
  desc = "quantidade ofertada"
)

variables[["P"]] <- create_variable(
  value = 1,
  indexes = list(P = "P"),
  type = "undefined",
  desc = "preço"
)

equations[["E_Q_s"]] <- create_equation(
  'Q_s = k_s * P^epsilon',
  type = "defining",
  desc = "quantidade ofertada"
)


# Demanda -----------------------------------------------------------------

params[["k_d"]] <- create_param(
  value = 1,
  indexes = list(k_d = "k_d"),
  desc = "shift da curva de demanda"
)

params[["eta"]] <- create_param(
  value = -1,
  indexes = list(eta = "eta"),
  desc = "elasticidade-preço da demanda"
)

variables[["Q_d"]] <- create_variable(
  value = 1,
  indexes = list(Q_d = "Q_d"),
  type = "defined",
  desc = "quantidade demandada"
)

equations[["E_Q_d"]] <- create_equation(
  'Q_d = k_d * P^eta',
  type = "defining",
  desc = "quantidade demandada"
)


# Equilíbrio --------------------------------------------------------------

equations[["E_P"]] <- create_equation(
  'Q_s - Q_d',
  type = "mcc",
  desc = "equilíbrio de mercado"
)


# Definindo o modelo ------------------------------------------------------

modelo <- list(
  params = params,
  variables = variables,
  equations = equations
)

sol <- solve_emr(modelo)
sol$sol$message

# Elementos da solução
names(sol)

# Experimento: Choque positivo na oferta -------------------------------------

modelo$params$k_s$value <- 1.1

sol2 <- solve_emr(modelo)
sol2$sol$message

sol2$variables

