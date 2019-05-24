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


# Parâmetros --------------------------------------------------------------

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

params[["D"]] <- create_param(
  value = 0, #D_df %>% mutate(value = value/1e6),
  indexes = sets[c("country")],
  desc = "déficits/superávits por país"
)

params[["L"]] <- create_param(
  value = L_df %>% 
    mutate(value = value/1e6),
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

params[["alpha"]] <- create_param(
  value = alphas_df,
  indexes = sets[c("country", "sector")],
  desc = "participação do setor j no consumo final do país n"
)

params[["M0"]] <- create_param(
  value = NA,
  indexes = sets[c("importer", "exporter", "sector")],
  desc = "valor de importação inicial"
)

params[["E0"]] <- create_param(
  value = NA,
  indexes = sets[c("importer", "exporter", "sector")],
  desc = "valor de exportação inicial"
)

params[["I0"]] <- create_param(
  value = NA,
  indexes = sets[c("country")],
  desc = "absorção final inicial"
)


# Índice de preço setorial ------------------------------------------------

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
  "pi_hat[n,,j] = (c[,j]*k[n,,j]/P[n,j])^(-theta[j])",
  indexes = c("n in importer", "j in sector"),
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
  "pi_new[n,,j] = pi_hat[n,,j]*pi[n,,j]",
  indexes = c("n in importer", "j in sector"),
  type = "defining",
  desc = "novas participações bilaterais"
)


# Valor da Produção -------------------------------------------------------

Y_df <- x_df %>% 
  left_join(tau_ij_93_df %>% 
              rename(tau = value)) %>% 
  group_by(exporter, sector) %>% 
  summarise(Y = sum(value/tau)/1e6)

variables[["Y"]] <- create_variable(
  value = 1,
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


# Importação a preços mundiais --------------------------------------------

variables[["M"]] <- create_variable(
  value = 1,
  indexes = sets[c("importer", "sector")],
  type = "defined",
  desc = "valor da importação por setor"
)

equations[["E_M"]] <- create_equation(
  "M[n,j] = sum(pi_new[n,,j]/(1 + tau[n,,j]) * X[n,j])",
  indexes = c("n in importer", "j in sector"),
  type = "defining",
  desc = "novo valor da produção"
)

# Receita Tarifária -------------------------------------------------------

R_df <- x_df %>% 
  left_join(tau_ij_93_df %>% 
              rename(tau = value)) %>% 
  group_by(importer, sector) %>% 
  summarise(R = sum((tau - 1) * value/tau)/1e6)

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


# Absorção Final ----------------------------------------------------------

variables[["I"]] <- create_variable(
  value = 1,
  indexes = sets[c("country")],
  type = "defined",
  desc = "Absorção Final"
)

equations[["E_I"]] <- create_equation(
  "I[n] = w[n] * L[n] + sum(R[n,]) + D[n]",
  indexes = "n in country",
  type = "defining",
  desc = "Absorção final"
)



# Custo da cesta de insumos -----------------------------------------------

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


# Novo Dispêndio ----------------------------------------------------------

X_df <- x_df %>% 
  group_by(importer, sector) %>% 
  summarise(value = sum(value)/1e6) %>% 
  ungroup() %>% 
  mutate_if(is.factor, as.character)

variables[["X"]] <- create_variable(
  value = X_df,
  indexes = sets[c("importer", "sector")],
  type = "undefined",
  desc = "valor do Dispêndio por setor"
)


equations[["E_X"]] <- create_equation(
  "X[n,j] - (sum(gammas_ii[n,j,] * Y[n,]) + alpha[n,j] * I[n])",
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
  "w[n] - sum(gamma_va[n,] * Y[n,])/L[n]",
  indexes = "n in country",
  type = "mcc",
  desc = "equilíbrio no mercado de trabalho (valor adicionado)"
)


# Equações de atualização -----------------------------

variables[["P_c"]] <- create_variable(
  value = 1,
  indexes = sets["country"],
  type = "defined",
  desc = "variação no índice de preços ao consumidor"
)

update_equations[["P_c"]] <- create_equation(
  "P_c[n] = prod(P[n,]^alpha[n,])",
  indexes = "n in country",
  desc = "Variação no índice de preços ao consumidor"
)

variables[["wp"]] <- create_variable(
  value = 1,
  indexes = sets["country"],
  type = "defined",
  desc = "salário real"
)

update_equations[["wp"]] <- create_equation(
  "wp[n] = w[n]/P_c[n]",
  indexes = "n in country",
  desc = "Variação no salário real"
)

variables[["W"]] <- create_variable(
  value = 1,
  indexes = sets["country"],
  type = "defined",
  desc = "Bem-estar"
)

update_equations[["W"]] <- create_equation(
  "W[n] = (I[n]/I0[n])/P_c[n]",
  indexes = "n in country",
  desc = "Variação no salário real"
)

# modelo ------------------------------------------------------------------

cp_model <- list(
  sets = sets,
  params = params,
  variables = variables,
  equations = equations,
  update_equations = update_equations
)

system.time(sol <- solve_emr_block(cp_model, scale_alpha = rep(0.6, 3),
                                   trace = TRUE, triter = 100, tol = 1e-7))

# Contrafactual -----------------------------------------------------------

cp_model2 <- list(
  sets = sets,
  params = params,
  variables = variables,
  equations = equations,
  update_equations = update_equations
)

cp_model2$params$L$value[] <- params$L$value * sol$variables$w
cp_model2$params$pi$value[] <- sol$variables$pi_new
cp_model2$params$I0$value[] <- sol$variables$I
cp_model2$variables$X$value[] <- sol$variables$X

nafta <- c("USA", "Mexico", "Canada")

k_cfl <- left_join(tau_ij_05_df, tau_ij_93_df,
                   by = c("importer", "exporter", "sector"),
                   suffix = c("_05", "_93")) %>% 
  mutate(
    k = case_when(
      importer %in% nafta & exporter %in% nafta ~ value_05/value_93,
      TRUE ~ 1
    )
  ) %>% 
  select(importer, exporter, sector, k)

tau_cfl <- left_join(tau_ij_05_df, tau_ij_93_df,
                     by = c("importer", "exporter", "sector"),
                     suffix = c("_05", "_93")) %>% 
  mutate(
    tau = case_when(
      importer %in% nafta & exporter %in% nafta ~ value_05 - 1,
      TRUE ~ value_93 - 1
    )
  ) %>% 
  select(importer, exporter, sector, tau)

cp_model2$params[["k"]] <- create_param(
  value = k_cfl,
  indexes = sets[c('importer', "exporter", 'sector')],
  desc = "variação nos custos de comércio (tarifa + iceberg)"
)

cp_model2$params[["tau"]] <- create_param(
  value = tau_cfl,
  indexes = sets[c("importer", "exporter", "sector")],
  desc = "tarifa por importador, exportador, setor"
)

system.time(sol2 <- solve_emr_block(cp_model2, scale_alpha = rep(0.6, 3), trace = TRUE,
                                    triter = 100, tol = 1e-7))

round((sol2$variables$w/sol2$updated_data$P_c - 1) * 100, 2)
