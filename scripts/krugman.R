library(emr)
library(tidyverse)



# Sets --------------------------------------------------------------------

regions <- c("R1", "R2", "R3")
goods <- "G1"

sets <- list(
  regions = regions,
  goods = goods,
  exporter = regions,
  importer = regions
)


# Dados Base --------------------------------------------------------------

krugman_df <- expand.grid(
  goods = goods,
  exporter = regions,
  importer = regions,
  vx0 = 1
) %>% 
  mutate(
    vx0 = ifelse(exporter == importer, 3, 1),
    sig = 3,
    eta = 1.5,
    mu = 0.5,
    N0 = 10,
    P0 = 1,
    c0 = 1
  ) %>% 
  group_by(goods, exporter) %>% 
  mutate(Y0 = sum(vx0)/c0,
         fc = Y0/(sig * N0 * c0),
         Q0 = Y0/P0) %>% 
  group_by(goods, importer) %>% 
  mutate(Q0_s = Y0/P0) %>% 
  ungroup() %>% 
  mutate(pf0 = (vx0/(N0 * Q0_s))^(1/(1-sig)),
         qf0 = Q0_s * pf0^(-sig),
         tau = (1 - 1/sig) * pf0/c0)


# Parâmetros --------------------------------------------------------------

params <- list()

params[["sig"]] <- create_param(
  value = 3,
  indexes = list(sig = "sig"),
  desc = "elasticity of substitution"
)

params[["eta"]] <- create_param(
  value = 1.5,
  indexes = list(eta = "eta"),
  desc = "demand elasticity"
)

params[["mu"]] <- create_param(
  value = 0.5,
  indexes = list(mu = "mu"),
  desc = "supply elasticity"
)


params[["c0"]] <- create_param(
  value = 1,
  indexes = sets[c('goods', 'regions')],
  desc = "benchmark input cost"
)


params[["vx0"]] <- create_param(
  value = krugman_df %>% 
    select(goods, exporter, importer, vx0),
  indexes = sets[c('goods', 'exporter', 'importer')],
  desc = "arbitray bechmark export values"
)

params[["Y0"]] <- create_param(
  value = krugman_df %>% 
    select(goods, regions = exporter, Y0) %>% 
    distinct(),
  indexes = sets[c('goods', 'regions')],
  desc = "bechmark input supply"
)

params[["N0"]] <- create_param(
  value = krugman_df %>% 
    select(goods, regions = exporter, N0) %>% 
    distinct(),
  indexes = sets[c('goods', 'regions')],
  desc = "bechmark number of firms"
)

params[["fc"]] <- create_param(
  value = krugman_df %>% 
    select(goods, regions = exporter, fc) %>% 
    distinct(),
  indexes = sets[c('goods', 'regions')],
  desc = 'fixed costs'
)

params[["P0"]] <- create_param(
  value = krugman_df %>% 
    select(goods, regions = exporter, P0) %>% 
    distinct(),
  indexes = sets[c('goods', 'regions')],
  desc = "benchmark price index"
)

params[["Q0"]] <- create_param(
  value = krugman_df %>% 
    select(goods, regions = exporter, Q0) %>% 
    distinct(),
  indexes = sets[c('goods', 'regions')],
  desc = "benchmark aggregate quantity"
)

params[["pf0"]] <- create_param(
  value =  krugman_df %>% 
    select(goods, exporter, importer, pf0),
  indexes = sets[c('goods', 'exporter', 'importer')],
  desc = "benchmark firm-level pricing (gross of tau)"
)

params[["qf0"]] <- create_param(
  value =  krugman_df %>% 
    select(goods, exporter, importer, qf0),
  indexes = sets[c('goods', 'exporter', 'importer')],
  desc = "benchmark firm-level quantity"
)

params[["tau"]] <- create_param(
  value =  krugman_df %>% 
    select(goods, exporter, importer, tau),
  indexes = sets[c('goods', 'exporter', 'importer')],
  desc = "iceberg transport cost factor"
)


# Variáveis e equações ----------------------------------------------------
variables <- list()
equations <- list()

variables[["Q"]] <- create_variable(
  value = params$Q0$value,
  indexes = sets[c('goods', 'regions')],
  type = "defined",
  desc = "composite quantity"
)

equations[["Q"]] <- create_equation(
  "Q[k,r] = Q0[k,r] * (P0[k,r]/P[k,r])^eta",
  indexes = c('k in goods', 'r in regions'),
  type = "defining",
  desc = "composite quantity"
)

variables[["P"]] <- create_variable(
  value = params$P0$value,
  indexes = sets[c('goods', 'regions')],
  type = "defined",
  desc = "composite price index"
)

equations[["P"]] <- create_equation(
  'P[k,s] = sum(N[k,]*PF[k,,s]^(1-sig))^(1/(1-sig))',
  indexes = c('k in goods', 's in regions'),
  type = "defining",
  desc = "composite price index"
)

variables[["c"]] <- create_variable(
  value = 1,
  indexes = sets[c('goods', 'regions')],
  type = "defined",
  desc = "composite input price (marginal cost)"
)

equations[["c"]] <- create_equation(
  'c[k,r] = sum(PF[k,r,] * QF[k,r,]/sig)/fc[k,r]',
  indexes = c('k in goods', 'r in regions'),
  type = "defining",
  desc = "composite input price (marginal cost)"
)

variables[['QF']] <- create_variable(
  value = params$qf0$value,
  indexes = sets[c('goods', 'exporter', 'importer')],
  type = "defined",
  desc = "firm-level output in s market"
)

equations[["QF"]] <- create_equation(
  'QF[k,r,s] = Q[k,s] * (P[k,s]/PF[k,r,s])^sig',
  indexes = c('k in goods', 'r in exporter', 's in importer'),
  type = "defining",
  desc = "firm-level output in s market"
)

variables[["Y"]] <- create_variable(
  value = params$Y0$value,
  indexes = sets[c('goods', 'regions')],
  type = "defined",
  desc = "composite input supply (output)"
)

equations[["Y"]] <- create_equation(
  'Y[k,r] = Y0[k,r] * (c[k,r]/c0[k,r])^mu',
  indexes = c('k in goods', 'r in regions'),
  type = "defining",
  desc = "composite input supply (output)"
)

variables[['PF']] <- create_variable(
  value = params$pf0$value,
  indexes = sets[c('goods', 'exporter', 'importer')],
  type = "undefined",
  desc = "firm-level price in s market"
)

equations[["PF"]] <- create_equation(
  'PF[k,r,s] - (sig/(sig - 1)) * tau[k,r,s] * c[k,r]',
  indexes = c('k in goods', 'r in exporter', 's in importer'),
  type = "mcc",
  desc = "firm-level price in s market"
)

variables[["N"]] <- create_variable(
  value = params$N0$value,
  indexes = sets[c('goods', 'regions')],
  type = "undefined",
  desc = "number of firms (varieties)"
)

equations[["N"]] <- create_equation(
  'N[k,r] - Y[k,r]/(fc[k,r] + sum(tau[k,r,] * QF[k,r,]))',
  indexes = c('k in goods', 'r in regions'),
  type = "mcc",
  desc = "number of firms (varieties)"
)


# Modelo ------------------------------------------------------------------

krugman <- list(
  sets = sets,
  params = params,
  variables = variables,
  equations = equations
)

krugman$params$tau$value['G1', 'R1', 'R2'] <- 3

system.time(
  sol <- solve_emr_block(krugman, trace = TRUE,
                         scale_alpha = c(0.13, 0.4), tol = 1e-10)
)

memisc::to.data.frame(krugman$variables$PF$value, as.vars = 0, name = "value0") %>% 
  left_join(memisc::to.data.frame(sol$variables$PF, as.vars = 0, name = "value")) %>% 
  mutate(change = value/value0)
