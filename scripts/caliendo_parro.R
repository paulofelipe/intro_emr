library(tidyverse)
library(emr)


# Dados -------------------------------------------------------------------

load('dados/caliendo_parro.RData')

# Inicializando os componentes --------------------------
params <- list()
variables <- list()
equations <- list()
update_equations <- list()

# Conjuntos Sets --------------------------------------

country <- as.character(unique(alphas_df$country))
sector <- as.character(unique(alphas_df$sector))

sets <- list(
  country = country,
  exporter = country,
  importer = country,
  sector = sector,
  input = sector
)


# Custo da cesta de insumos -----------------------------------------------

params[["gamma_va"]] <- create_param(
  value = gammas_va_df,
  indexes = sets[c("country", "sector")],
  desc = "participação do valor adicionado na produção"
)

params[["gammas_ii"]] <- create_param(
  value = gammas_ii_df,
  indexes = sets[c("country", "input", "sector")],
  desc = "participação do bem intermediário k na produção de j"
)


variables[["c"]] <- create_variable(
  value = 1,
  indexes = sets[c("country", "sector")],
  type = "undefined",
  desc = "variação no custo da cesta de insumos"
)

equations[["E_c"]] <- create_equation(
  "c[n,j] - w[n]^gamma_va[n,j] * prod(P[n,]^gammas_ii[n,,j])",
  indexes = c("n in country", "j in sector"),
  type = "mcc",
  desc = "variação no custoda cesta de insumos"
)


# Índice de preço setorial ------------------------------------------------

params[["pi"]] <- create_param(
  value = pi_df,
  indexes = sets[c('importer', "exporter", 'sector')],
  desc = "participações iniciais no dispêndio"
)

params[["k"]] <- create_param(
  value = 1,
  indexes = sets[c('importer', "exporter", 'sector')],
  desc = "variação nos custos de comércio (tarifa + iceberg)"
)

params[["theta"]] <- create_param(
  value = theta_df,
  indexes = sets['sector'],
  desc = "elasticidades de comércio"
)

variables[["P"]] <- create_variable(
  value = 1,
  indexes = sets[c("country", "sector")],
  type = "defined",
  desc = "Variação no índice de preço setorial"
)

equations[["E_P"]] <- create_equation(
  "P[n,j] = sum(pi[n,,j]*(k[n,,j] * c[,j])^(-theta[j]))^(-1/theta[j])",
  indexes = c("n in country", "j in sector"),
  type = "defining",
  desc = "variação no índice de preço setorial"
)

# Variação nas participações no comércio bilateral ------------------------

variables[["pi_hat"]] <- create_variable(
  value = 1,
  indexes = sets[c('importer', "exporter", 'sector')],
  type = "defined",
  desc = "variação na participação do comércio bilateral"
)

equations[["E_pi_hat"]] <- create_equation(
  "pi_hat[n,i,j] = (c[i,j]*k[n,i,j]/P[n,j])^(-theta[j])",
  indexes = c("n in importer", "i in exporter", "j in sector"),
  type = "defining",
  desc = "variação na participação bilateral"
)


# Novas participações -----------------------------------------------------

variables[["pi_new"]] <- create_variable(
  value = 1,
  indexes = sets[c('importer', "exporter", 'sector')],
  type = "defined",
  desc = "novas participações bilaterais"
)

equations[["E_pi_new"]] <- create_equation(
  "pi_new[n,i,j] = pi_hat[n,i,j]*pi[n,i,j]",
  indexes = c("n in importer", "i in exporter", "j in sector"),
  type = "defining",
  desc = "novas participações bilaterais"
)


# Valor da Produção -------------------------------------------------------

Y_df <- x_df %>% 
  left_join(tau_ij_93_df %>% 
              rename(tau = value)) %>% 
  group_by(exporter, sector) %>% 
  summarise(Y = sum(value/tau)/1e10)

X_df <- x_df %>% 
  group_by(importer, sector) %>% 
  summarise(value = sum(value)/1e10) %>% 
  ungroup() %>% 
  mutate_if(is.factor, as.character)

variables[["Y"]] <- create_variable(
  value = Y_df,
  indexes = sets[c("exporter", "sector")],
  type = "defined",
  desc = "valor da produção por setor"
)

equations[["E_Y"]] <- create_equation(
  "Y[n,j] = sum(pi_new[,n,j]/(1 + tau[,n,j]) * X[,j])",
  indexes = c("n in exporter", "j in sector"),
  type = "defining",
  desc = "novo valor da produção"
)


# Receita Tarifária -------------------------------------------------------

R_df <- x_df %>% 
  left_join(tau_ij_93_df %>% 
              rename(tau = value)) %>% 
  group_by(importer, sector) %>% 
  summarise(R = sum((tau - 1) * value/tau)/1e10)

variables[["R"]] <- create_variable(
  value = R_df,
  indexes = sets[c("importer", "sector")],
  type = "defined",
  desc = "Receita tarifária"
)

equations[["E_R"]] <- create_equation(
  "R[n,j] = sum(tau[n,,j] * pi_new[n,,j]/(1 + tau[n,,j]) * X[n,j])",
  indexes = c("n in importer", "j in sector"),
  type = "defining",
  desc = "Receita tarifária"
)


# Novo Dispêndio ----------------------------------------------------------

params[["D"]] <- create_param(
  value = D_df %>% 
    mutate(value = value/1e10),
  indexes = sets[c("country")],
  desc = "déficits/superávits por país"
)

params[["L"]] <- create_param(
  value = L_df %>% 
    mutate(value = value/1e10),
  indexes = sets[c("country")],
  desc = "estoque de trabalho (valor adicionado)"
)

tau_df <- tau_ij_93_df %>% 
  mutate(tau = value -1) %>% 
  select(-value)

params[["tau"]] <- create_param(
  value = tau_df,
  indexes = sets[c("importer", "exporter", "sector")],
  desc = "tarifa por importador, exportador, setor"
)


variables[["X"]] <- create_variable(
  value = X_df,
  indexes = sets[c("importer", "sector")],
  type = "undefined",
  desc = "valor do Dispêndio por setor"
)

params[["alpha"]] <- create_param(
  value = alphas_df,
  indexes = sets[c("country", "sector")],
  desc = "participação do setor j no consumo final do país n"
)

# variables[["X_hat"]] <- create_variable(
#   value = 1,
#   indexes = sets[c("importer", "sector")],
#   type = "undefined",
#   desc = "variação no dispêndio por país e setor"
# )

equations[["E_X"]] <- create_equation(
  "X[n,j] - (sum(gammas_ii[n,j,] * Y[n,]) + alpha[n,j] * (w[n] * L[n] + sum(R[n,]) + D[n]))",
  indexes = c("n in country", "j in sector"),
  type = 'mcc',
  desc = "dispêndio por país e setor"
)


# Equilíbrio Mercado de Trabalho ------------------------------------------

variables[["w"]] <- create_variable(
  value = 1,
  indexes = sets["country"],
  type = "undefined",
  desc = "variação nos salários"
)

equations[["E_w"]] <- create_equation(
  "(w[n]*L[n])/sum(gamma_va[n,] * Y[n,]) - 1",
  indexes = "n in country",
  type = "mcc",
  desc = "equilíbrio no mercado de trabalho (valor adicionado)"
)


cp_model <- list(
  sets = sets,
  params = params,
  variables = variables,
  equations = equations
)

system.time(sol <- solve_emr(cp_model, trace = TRUE, M = 300))

            