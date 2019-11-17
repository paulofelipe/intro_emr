library(emr)
library(tidyverse)

# Inicializando os componentes --------------------------
params <- list()
variables <- list()
equations <- list()
sets <- list()
update_equations <- list()

# Dados Cenário Base --------------------------------------

reg <- c("bra", "usa", "row") # Regiões
v0 <- c(bra = 60, usa = 30, row = 10) # Valores de comércio a preços internos
t0 <- c(bra = 0, usa = 0, row = 0) # tarifas iniciais
eta <- -1 # Elasticidade-preço da demanda
epsilon <- c(bra = 1,usa = 10,row = 10) # Elasticidades-preço das ofertas
sigma <- 4 # Elasticidade de substituição


# Sets - Conjunto de Índices ----------------------------------------------

sets[["REG"]] <- reg
sets



# Dispêndio ---------------------------------------------------------------

params[["k_d"]] <- create_param(
  value = 1,
  indexes = list(k_d = "k_d"),
  desc = "variação no shift da demanda total"
)

params[["eta"]] <- create_param(
  value = eta,
  indexes = list(eta = "eta"),
  desc = "elasticidade-preço da demanda"
)

variables[["E"]] <- create_variable(
  value = 1,
  indexes = list(E = "E"),
  type = "defined",
  desc = "variação no dispêndio total"
)

variables[["P"]] <- create_variable(
  value = 1,
  indexes = list(P = "P"),
  type = "defined",
  desc = "variação no índice de preço do bem composto"
)

equations[["E_E"]] <- create_equation(
  "E = k_d * P^(eta+1)",
  type = "defining",
  desc = "variação no dispêndio total"
)


# Índice de preço do bem composto -----------------------------------------

params[["alpha"]] <- create_param(
  value = 1,
  indexes = sets["REG"],
  desc = "variação no parâmetro de preferência"
)

params[["pi"]] <- create_param(
  value = v0/sum(v0),
  indexes = sets["REG"],
  desc = "participacao no dispêndio total"
)

params[["sigma"]] <- create_param(
  value = sigma,
  indexes = list(sigma = "sigma"),
  desc = "elasticidade de substituição" 
)

variables[["p"]] <- create_variable(
  value = 1,
  indexes = sets['REG'],
  type = 'undefined',
  desc = 'variação nos preços internos'
)

equations[["E_P"]] <- create_equation(
  'P = sum(pi * (alpha * p)^(1-sigma))^(1/(1-sigma))',
  type = "defining",
  desc = "variação no índice de preço do bem composto"
)


# Demanda por variedade ---------------------------------------------------

variables[["q"]] <- create_variable(
  value = 1,
  indexes = sets['REG'],
  type = "defined",
  desc = "variação na demanda por variedade"
)

equations[["E_q"]] <- create_equation(
  'q[i] = alpha[i]^(1-sigma)*(p[i]/P)^(-sigma)*E/P',
  indexes = "i in REG",
  type = "defining",
  desc = "variação na quantidade demandada"
)


# Oferta por varieade -----------------------------------------------------

params[["k_s"]] <- create_param(
  value = 1,
  indexes = sets['REG'],
  desc = "variação no shift da curva de oferta"
)

params[["epsilon"]] <- create_param(
  value = epsilon,
  indexes = sets['REG'],
  desc = 'elasticidade-preço da oferta'
)

params[["tau"]] <- create_param(
  value = 1,
  indexes = sets['REG'],
  desc = "variação no poder da tarifa"
)


variables[["x"]] <- create_variable(
  value = 1,
  indexes = sets['REG'],
  type = "defined",
  desc = "variação na oferta da variedade i"
)

equations[["e_x"]] <- create_equation(
  "x[i] = k_s[i] * (p[i]/(tau[i]))^epsilon[i]",
  indexes = "i in REG",
  type = "defining",
  desc = "variação na oferta da variedade i"
)


# Condições de equilíbrio -------------------------------------------------

equations[["E_p"]] <- create_equation(
  "x[i] - q[i]",
  indexes = "i in REG",
  type = "mcc",
  desc = "condição de equilíbrio pro preço"
)


# Equações de atualizações ------------------------------------------------

params[["v0"]] <- create_param(
  value = v0,
  indexes = sets["REG"],
  desc = "valor do dispêndio inicial"
)


update_equations[["v0"]] <- create_equation(
  'v0[i] = v0[i] * p[i] * q[i]',
  indexes = "i in REG",
  desc = "valor do dispêndio por origem"
)


# Definindo o modelo ------------------------------------------------------

armington <- list(
  sets = sets,
  params = params,
  variables = variables,
  equations = equations,
  update_equations = update_equations
)

# Checando
sol <- solve_emr(armington)
sol$sol$message


# Simulação ---------------------------------------------------------------
# Incremento em 10% na tarifa cobrada sobre produtos dos usa

armington$params$tau$value[["usa"]] <- 1.1

# Resolvendo o modelo -----------------------------------------------------

sol_cfl <- solve_emr_block(armington, trace = TRUE)
#sol_cfl$sol$message

# Resultados - variáveis --------------------------------------------------

sol_cfl$variables


# Análise de Sensibilidade ------------------------------------------------

# carregar fonts windows
#extrafont::loadfonts(device = "win", quiet = TRUE)

map_df(2:8, ~{
  armington$params$sigma$value <- .x
  sol_cfl <- solve_emr(armington)
  Q <- sol_cfl$variables$E/sol_cfl$variables$P - 1
  
  data.frame(
    Q = Q,
    sigma = .x
  )
}) %>% 
  ggplot(aes(x = sigma, y = Q)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Variação na Quantidade Consumida do Bem Composto"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  hrbrthemes::theme_ipsum()

#extrafont::loadfonts(device = "win", quiet = TRUE)
map_df(2:8, ~{
  armington$params$sigma$value <- .x
  sol_cfl <- solve_emr(armington)
  
  data.frame(
    p_bra = sol_cfl$variables$p["bra"] - 1,
    sigma = .x
  )
}) %>% 
  ggplot(aes(x = sigma, y = p_bra)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Variação no preço do produto de origem bra",
    y = "Variação no preço - bra"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  hrbrthemes::theme_ipsum()
