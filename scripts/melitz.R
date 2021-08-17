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
sig <- 3
eta <- 1.5
mu <- 0.5
a <- 4.6
b <- 0.5

melitz_df_hrs <- expand.grid(
  goods = goods,
  exporter = regions,
  importer = regions
) %>% 
  mutate(
    vx0 = ifelse(exporter == importer, 3, 1),
    vx0_r = 3,
    c0 = 1,
    N0 = 9,
    N0 = (vx0/vx0_r)^2 * N0
  )

Y0_df <- melitz_df_hrs %>% 
  group_by(goods, exporter) %>% 
  summarise(Y0 = sum(vx0))

Q0_df <- melitz_df_hrs %>% 
  group_by(goods, importer) %>% 
  summarise(Q0 = sum(vx0))

melitz_df_hr <- Y0_df %>%
  mutate(
    P0 = 1,
    M0 = 10,
    delt_fs = Y0/M0 * (sig - 1)/(a * sig)
  )

melitz_df_hrs <- melitz_df_hrs %>% 
  left_join(melitz_df_hr, by = c("goods", "exporter")) %>% 
  left_join(Q0_df, by = c("goods", "importer")) %>% 
  mutate(
    fc = vx0/(N0 * c0) * (a + 1 - sig)/(a * sig),
    pf0 = (vx0/(N0 * Q0))^(1/(1 - sig)),
    qf0 = Q0 * pf0^(-sig),
    phi0 = b * (a/(a + 1 - sig))^(1/(sig - 1)) * (N0/M0)^(-1/a),
    tau = (1 - 1/sig) * pf0 * phi0/c0
  )
  

# Parâmetros --------------------------------------------------------------

params <- list()

params[["sig"]] <- create_param(
  value = sig,
  indexes = "sig",
  desc = "Elasticity of substitution"
)

params[["eta"]] <- create_param(
  value = eta,
  indexes = "eta",
  desc = "Demand elasticity"
)

params[["mu"]] <- create_param(
  value = mu,
  indexes = "mu",
  desc = "Supply elasticity"
)

params[["a"]] <- create_param(
  value = a,
  indexes = "a",
  desc = "Pareto shape parameter"
)

params[["b"]] <- create_param(
  value = b,
  indexes = "b",
  desc = "Pareto lower support"
)

params[["Q0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, regions = importer, Q0) %>% 
    distinct(),
  indexes = sets[c("goods", "regions")],
  desc = "Benchmark aggregate quantity"
)

params[["P0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, regions = exporter, P0) %>% 
    distinct(),
  indexes = sets[c("goods", "regions")],
  desc = "Benchmark price index"
)

params[["M0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, regions = exporter, M0) %>% 
    distinct(),
  indexes = sets[c("goods", "regions")],
  desc = "Benchmark number of entered firms"
)

params[["N0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, exporter, importer, N0) %>% 
    distinct(),
  indexes = sets[c("goods", "exporter", "importer")],
  desc = "Benchmark number of operating firms"
)

params[["qf0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, exporter, importer, qf0) %>% 
    distinct(),
  indexes = sets[c("goods", "exporter", "importer")],
  desc = "Benchmark avg firm level quantity"
)

params[["pf0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, exporter, importer, pf0) %>% 
    distinct(),
  indexes = sets[c("goods", "exporter", "importer")],
  desc = "Benchmark avg firm level price (gross)"
)

params[["phi0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, exporter, importer, phi0) %>% 
    distinct(),
  indexes = sets[c("goods", "exporter", "importer")],
  desc = "Benchmark avg productivity"
)

params[["c0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, regions = exporter, c0) %>% 
    distinct(),
  indexes = sets[c("goods", "regions")],
  desc = "Benchmark input cost"
)

params[["Y0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, regions = exporter, Y0) %>% 
    distinct(),
  indexes = sets[c("goods", "regions")],
  desc = "Benchmark input supply"
)

params[["fc"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, exporter, importer, fc) %>% 
    distinct(),
  indexes = sets[c("goods", "exporter", "importer")],
  desc = "Bilateral fixed costs"
)

params[["delt_fs"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, regions = exporter, delt_fs) %>% 
    distinct(),
  indexes = sets[c("goods", "regions")],
  desc = "Annualized sunk cost"
)

params[["tau"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, exporter, importer, tau) %>% 
    distinct(),
  indexes = sets[c("goods", "exporter", "importer")],
  desc = "Iceberg transport cost factor"
)

params[["vx0"]] <- create_param(
  value = melitz_df_hrs %>% 
    select(goods, exporter, importer, vx0) %>% 
    distinct(),
  indexes = sets[c("goods", "exporter", "importer")],
  desc = "Bechmark export values"
)


# Variáveis e Equações ------------------------------------------------------

variables <- list()
equations <- list()

variables[["Q"]] <- create_variable(
  value = params$Q0$value,
  indexes = sets[c("goods", "regions")],
  type = "defined",
  desc = "Composite quantity"
)

equations[["E_q"]] <- create_equation(
  "Q[g,r] = Q0[g,r] * (P0[g,r]/P[g,r])^eta",
  indexes = c("g in goods", "r in regions"),
  type = "defining",
  desc = "Aggregate demand"
)

variables[["P"]] <- create_variable(
  value = params$P0$value,
  indexes = sets[c("goods", "regions")],
  type = "defined",
  desc = "Composite price index"
)

equations[["E_P"]] <- create_equation(
  "P[g,s] = sum(N[g,,s] * PF[g,,s]^(1-sig))^(1/(1-sig))",
  indexes = c("g in goods", "s in regions"),
  type = "defining",
  desc = "Dixit-stiglitz price index"
)

variables[["M"]] <- create_variable(
  value = params$M0$value,
  indexes = sets[c("goods", "regions")],
  type = "undefined",
  desc = "Number of entered firms"
)

equations[["E_M"]] <- create_equation(
  "c[g,r] - 
  sum((N[g,r,]/M[g,r]) * PF[g,r,] * QF[g,r,]*(sig - 1)/(a*sig)) * 1/delt_fs[g,r]",
  indexes = c("g in goods", "r in regions"),
  type = "mcc",
  desc = "Free entry condition"
)

variables[["N"]] <- create_variable(
  value = params$N0$value,
  indexes = sets[c("goods", "exporter", "importer")],
  type = "undefined",
  desc = "Number of operating firms"
)

equations[["E_N"]] <- create_equation(
  "c[g,r] - (PF[g,r,s]*QF[g,r,s]*(a + 1 - sig))/(a * sig) * 1/fc[g,r,s]",
  indexes = c('g in goods', 'r in exporter', 's in importer'),
  type = "mcc",
  desc = "Zero cutoff profits"
)

variables[["QF"]] <- create_variable(
  value = params$qf0$value,
  indexes = sets[c("goods", "exporter", "importer")],
  type = "defined",
  desc = "Avg firm output in s-market"
)

equations[["E_QF"]] <- create_equation(
  "QF[g,r,s] = Q[g,s] * (P[g,s]/PF[g,r,s])^sig",
  indexes = c('g in goods', 'r in exporter', 's in importer'),
  type = "defining",
  desc = "Firm demand"
)

variables[["PF"]] <- create_variable(
  value = params$pf0$value,
  indexes = sets[c("goods", "exporter", "importer")],
  type = "undefined",
  desc = "Avg firm (gross) pricing in s-market"
)

equations[["E_PF"]] <- create_equation(
  "PF[g,r,s] - tau[g,r,s] * c[g,r]/PHI[g,r,s] * 1/((1 - 1/sig))",
  indexes = c('g in goods', 'r in exporter', 's in importer'),
  type = "mcc",
  desc = "Optimal firm pricing"
)

variables[["PHI"]] <- create_variable(
  value = params$phi0$value,
  indexes = sets[c("goods", "exporter", "importer")],
  type = "defined",
  desc = "Avg firm productivity"
)

equations[["E_PHI"]] <- create_equation(
  "PHI[g,r,] = b * (a/(a+1-sig))^(1/(sig-1)) * (N[g,r,]/M[g,r])^(-1/a)",
  indexes = c('g in goods', 'r in exporter'),
  type = "defining",
  desc = "Pareto productivity"
)

variables[["c"]] <- create_variable(
  value = params$c0$value,
  indexes = sets[c("goods", "regions")],
  type = "undefined",
  desc = "Composite input price (marginal cost)"
)

equations[["E_c"]] <- create_equation(
  "Y[g,r] - (delt_fs[g,r] * M[g,r] + 
              sum(N[g,r,]*(fc[g,r,] + tau[g,r,]*QF[g,r,]/PHI[g,r,])))",
  indexes = c("g in goods", "r in regions"),
  type = "mcc",
  desc = "Input market clearance"
)

variables[["Y"]] <- create_variable(
  value = params$Y0$value,
  indexes = sets[c("goods", "regions")],
  type = "defined",
  desc = "Composite input supply (output)"
)

equations[["E_Y"]] <- create_equation(
  "Y[g,r] = Y0[g,r] * (c[g,r]/c0[g,r])^mu",
  indexes = c("g in goods", "r in regions"),
  type = "defining",
  desc = "Input supply (output)"
)


# Modelo ------------------------------------------------------------------

melitz <- list(
  sets = sets,
  params = params,
  variables = variables,
  equations = equations
)

#debugonce(solve_emr_block)
melitz$params$tau$value["G1", "R1", "R2"] <- melitz$params$tau$value["G1", "R1", "R2"] * 0.95
system.time(solve_emr_block(melitz, scale_alpha = c(0.9, 0.3, 0.1, 0.4)))

round((solve_emr_block(melitz, scale_alpha = c(0.9, 0.3, 0.1, 0.4))$variables$c - 1) * 100, 4)

sol <- solve_emr_block(melitz, scale_alpha = c(0.9, 0.3, 0.1, 0.4))

(sol$variables$PHI[1,,] / melitz$params$phi0$value[1,,] - 1) * 100

(sol$variables$QF[1,,] / melitz$params$qf0$value[1,,] - 1) * 100