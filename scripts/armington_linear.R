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
  value = 0,
  indexes = list(k_d = "k_d"),
  desc = "variação no shift da demanda total"
)

params[["eta"]] <- create_param(
  value = eta,
  indexes = list(eta = "eta"),
  desc = "elasticidade-preço da demanda"
)

variables[["E"]] <- create_variable(
  value = 0,
  indexes = list(E = "E"),
  type = "defined",
  desc = "variação no dispêndio total"
)

variables[["P"]] <- create_variable(
  value = 0,
  indexes = list(P = "P"),
  type = "defined",
  desc = "variação no índice de preço do bem composto"
)

equations[["E_E"]] <- create_equation(
  "E = k_d + (eta+1) * P",
  type = "defining",
  desc = "variação no dispêndio total"
)


# Índice de preço do bem composto -----------------------------------------

params[["alpha"]] <- create_param(
  value = 0,
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
  value = 0,
  indexes = sets['REG'],
  type = 'undefined',
  desc = 'variação nos preços internos'
)

equations[["E_P"]] <- create_equation(
  'P = sum(pi * p)',
  type = "defining",
  desc = "variação no índice de preço do bem composto"
)


# Demanda por variedade ---------------------------------------------------

variables[["q"]] <- create_variable(
  value = 0,
  indexes = sets['REG'],
  type = "defined",
  desc = "variação na demanda por variedade"
)

equations[["E_q"]] <- create_equation(
  'q[i] = (1-sigma) * alpha[i] -sigma * (p[i] - P) + E - P',
  indexes = "i in REG",
  type = "defining",
  desc = "variação na quantidade demandada"
)


# Oferta por varieade -----------------------------------------------------

params[["k_s"]] <- create_param(
  value = 0,
  indexes = sets['REG'],
  desc = "variação no shift da curva de oferta"
)

params[["epsilon"]] <- create_param(
  value = epsilon,
  indexes = sets['REG'],
  desc = 'elasticidade-preço da oferta'
)

params[["tau"]] <- create_param(
  value = 0,
  indexes = sets['REG'],
  desc = "variação no poder da tarifa"
)


variables[["x"]] <- create_variable(
  value = 0,
  indexes = sets['REG'],
  type = "defined",
  desc = "variação na oferta da variedade i"
)

equations[["e_x"]] <- create_equation(
  "x[i] = k_s[i] + epsilon[i] * (p[i] - tau[i])",
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
  'v0[i] = v0[i] * (1 + p[i] + q[i])',
  indexes = "i in REG",
  desc = "valor do dispêndio por origem"
)

update_equations[["pi"]] <- create_equation(
  'pi[i] = v0[i]/sum(v0)',
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

solve_steps <- function(model, steps){
  
  N <- steps
  
  sol_cfl <- solve_emr(model, tol = 1e-10)
  results <- sol_cfl$variables
  
  for(n in 1:N){
    # Simulação ---------------------------------------------------------------
    # Incremento em 10% na tarifa cobrada sobre produtos dos usa
    
    model$params$tau$value[["usa"]] <- (1.1)^(1/N) - 1
    
    # Atualização
    for(upd in names(sol_cfl$updated_data)){
      model$params[[upd]]$value <- sol_cfl$updated_data[[upd]]
    }
    
    # Resolvendo o modelo -----------------------------------------------------
    
    sol_cfl <- solve_emr(model, trace = FALSE, tol = 1e-10)
    #sol_cfl$sol$message
    
    # Resultados - variáveis --------------------------------------------------
    
    for(name in names(results)){
      results[[name]] <- (1 + results[[name]]) * (1 + sol_cfl$variables[[name]]) - 1
    }
    
  }
  
  results
}

solve_euler <- function(model, steps = 3){
  
  results1 <- solve_steps(model, steps)
  results2 <- solve_steps(model, steps * 2)
  results3 <- solve_steps(model, steps * 4)
  
  results <- results1
  
  for(name in names(results1)){
    results[[name]] <- (8 * results3[[name]] - 6 * results2[[name]] + results1[[name]])/3
  }
  
  return(results)
  
}

system.time(results <- solve_euler(armington, steps = 1))

results
  