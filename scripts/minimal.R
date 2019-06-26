library(emr)
library(tidyverse)

params <- list()
variables <- list()
equations <- list()

# Sets --------------------------------------------------------------------

IND <- c("AgricMining", "Manufacture", "Utilities", "Construction", 
         "TradeTranspt", "FinanProprty", "Services")

COM <- c("AgricMining", "Manufacture", "Utilities", "Construction", 
         "TradeTranspt", "FinanProprty", "Services")

SRC <- c("dom", "imp")

USER <- c("AgricMining", "Manufacture", "Utilities", "Construction", 
          "TradeTranspt", "FinanProprty", "Services", "Investment", "Households", 
          "Government", "Exports")

IMPUSER <- c("AgricMining", "Manufacture", "Utilities", "Construction", 
             "TradeTranspt", "FinanProprty", "Services", "Investment", "Households", 
             "Government")

FINALUSER <- setdiff(USER, IND)

FAC <- c("Labour", "Capital")

sets <- list(
  IND = IND,
  COM = COM,
  SRC = SRC,
  USER = USER,
  IMPUSER = IMPUSER,
  FINALUSER = FINALUSER,
  FAC = FAC
)

# Prepare basedata --------------------------------------------------------

minimal <- read_lines('dados/minimal.csv')

headers <- which(str_detect(minimal, "Header"))

# Use data
use <- read_csv('dados/minimal.csv',
                skip = 1, n_max = 154)

use <- use %>% 
  mutate(
    COM = factor(COM, levels = sets$COM),
    SRC = factor(SRC, levels = sets$SRC),
    USER = factor(USER, levels = sets$USER)
  )

# Imp/Dom shares
srcshr <- use %>% 
  group_by(COM, USER) %>% 
  mutate(Value = Value/sum(Value)) %>% 
  group_by(COM, USER) %>% 
  mutate(Value = ifelse(is.nan(Value), 1/n(), Value))

# Primary Factor 
fac <- read_csv('dados/minimal.csv',
                skip = 157, n_max = 14)

fac <- fac %>% 
  mutate(
    FAC = factor(FAC, levels = sets$FAC),
    IND = factor(IND, levels = sets$IND)
  )

# Import tax
mtax <- read_csv('dados/minimal.csv',
                 skip = 173, n_max = 7)

mtax <- mtax %>% 
  mutate(
    COM = factor(COM, levels = sets$COM)
  )

# Output tax
otax <- read_csv('dados/minimal.csv',
                 skip = 182, n_max = 7)

otax <- otax %>% 
  mutate(
    IND = factor(IND, levels = sets$IND)
  )

# Use data by commodity and user
use_s <- use %>% 
  group_by(COM, USER) %>% 
  summarise(Value = sum(Value)) %>% 
  arrange(USER)

# Use data by user
use_cs <- use %>% 
  group_by(USER) %>% 
  summarise(Value = sum(Value))

# Sales
sales <- use %>% 
  group_by(COM, SRC) %>% 
  summarise(Value = sum(Value)) %>% 
  arrange(SRC)

# Wages plus profit
v1prim <- fac %>% 
  group_by(IND) %>% 
  summarise(Value = sum(Value))

# Intermediate costs
ic <- use_s %>% 
  filter(USER %in% IND) %>% 
  group_by(IND = USER) %>% 
  summarise(ic = sum(Value)) %>% 
  mutate(IND = factor(IND, levels = sets$IND))

# Industry costs
v1tot <- v1prim %>% 
  rename(va = Value) %>% 
  left_join(ic) %>% 
  mutate(Value = va + ic) %>% 
  select(IND, Value)

# Imports CIF
v0cif <- left_join(sales %>% filter(SRC == 'imp'),
                   mtax, by = c("COM"),
                   suffix = c("_imp", "_tax")) %>% 
  mutate(Value = Value_imp - Value_tax) %>% 
  select(COM, Value)


# Parameters --------------------------------------------------------------

params[["USE"]] <- create_param(
  value = use,
  indexes = sets[c("COM", "SRC", "USER")],
  desc = "USE by commodity, source and user"
)

params[["FACTOR"]] <- create_param(
  value = fac,
  indexes = sets[c("FAC","IND")],
  desc = "Factor use by industry"
)

params[["USE_S"]] <- create_param(
  value = use_s,
  indexes = sets[c("COM","USER")],
  desc = "Total use by commodity and user"
)

params[["USE_CS"]] <- create_param(
  value = use_cs,
  indexes = sets[c("USER")],
  desc = "Total use by user"
)

params[["SALES"]] <- create_param(
  value = sales,
  indexes = sets[c("COM","SRC")],
  desc = "Total sales by commodity and source"
)

params[["V1PRIM"]] <- create_param(
  value = v1prim,
  indexes = sets[c("IND")],
  desc = "Primary factor use by industry"
)

params[["V1TOT"]] <- create_param(
  value = v1tot,
  indexes = sets[c("IND")],
  desc = "Industry costs"
)

params[["V0CIF"]] <- create_param(
  value = v0cif,
  indexes = sets[c("COM")],
  desc = "Aggregate imports at border price"
)

params[["phi"]] <- create_param(
  value = 1,
  indexes = list(phi = "phi"),
  desc = "Exchange rate"
)

params[["realwage"]] <- create_param(
  value = 1,
  indexes = list(realwage = "realwage"),
  desc = "Real wage"
)

params[["x1cap"]] <- create_param(
  value = 1,
  indexes = sets[c("IND")],
  desc = "Current capital stock by industry"
)

params[["x3tot"]] <- create_param(
  value = 1,
  indexes = list(x3tot = 'x3tot'),
  desc = 'Real expenditure'
)

params[["x_s_inv"]] <- create_param(
  value = 1,
  indexes = sets[c("IND")],
  desc = "Investment demand"
)

params[["x_s_gov"]] <- create_param(
  value = 1,
  indexes = sets[c("COM")],
  desc = "Government demand"
)

params[["a1prim"]] <- create_param(
  value = 1,
  indexes = sets[c("IND")],
  desc = "All primary-factor augmenting technical change"
)

params[["pworld"]] <- create_param(
  value = 1,
  indexes = sets[c("IND")],
  desc = "World prices measured in foreign currency"
)

params[["f4q"]] <- create_param(
  value = 1,
  indexes = sets[c("COM")],
  desc = "Quantity shift in foreign demand"
)

params[["ptx"]] <- create_param(
  value = 1,
  indexes = sets[c("COM")],
  desc = "Production tax rate"
)

params[["mtx"]] <- create_param(
  value = 1,
  indexes = sets[c("COM")],
  desc = "Import tax rate"
)

params[["SRCSHR"]] <- create_param(
  value = srcshr,
  indexes = sets[c("COM", "SRC", "USER")],
  desc = "Dom/imp shares"
)

params[["SIGMA"]] <- create_param(
  value = 2,
  indexes = sets[c("COM")],
  desc = "Armington elasticity"
)

params[["EXP_ELAST"]] <- create_param(
  value = 5,
  indexes = sets[c("COM")],
  desc = "Export-price elasticity"
)

params[["SIGMA1PRIM"]] <- create_param(
  value = 0.5,
  indexes = sets[c("IND")],
  desc = "Elasticity of substitution between primary factors"
)


# Variables and equations -------------------------------------------------

#################### MCC ##################################################

variables[["p1tot"]] <- create_variable(
  value = 1,
  indexes = sets[c("IND")],
  type = "undefined",
  desc = "Price of commodity domestically produced"
)

equations[["E_p1tot"]] <- create_equation(
  'p1tot[i] -
  (sum(USE[,,i]*p) +
  FACTOR["Labour",i]*p1lab +
  FACTOR["Capital",i]*p1cap[i])/V1TOT[i]',
  indexes = "i in IND",
  type = "mcc",
  desc = "Zero profit conditions"
)

variables[["p1cap"]] <- create_variable(
  value = 1,
  indexes = sets[c("IND")],
  type = "undefined",
  desc = "Price of primary factor capital (return of capital)"
)

equations[["E_p1cap"]] <- create_equation(
  'x1cap[i] - x1prim[i]*(p1cap[i]/p1prim[i])^(-SIGMA1PRIM[i])',
  indexes = "i in IND",
  type = "mcc",
  desc = "Market clearing codition for capital in industry i"
)

variables[["x1tot"]] <- create_variable(
  value = 1,
  indexes = sets[c("IND")],
  type = "undefined",
  desc = "Change in production of domestically produced commodity"
)

equations[["E_x1tot"]] <- create_equation(
  'x1tot[c] - x0[c,"dom"]',
  indexes = 'c in COM',
  type = "mcc",
  desc = "Market clearing codition for domestically produced commodity"
)

variables[["employ"]] <- create_variable(
  value = 1,
  indexes = list(employ = "employ"),
  type = "undefined",
  desc = "Aggregate employment"
)

equations[["E_employ"]] <- create_equation(
  'employ - sum(FACTOR["Labour",] * x1lab)/sum(FACTOR["Labour", ])',
  type = "mcc",
  desc = "Employment equilibrium"
)

#################### DEFINING ##############################################

variables[["p"]] <- create_variable(
  value = 1,
  indexes = sets[c("COM","SRC")],
  type = "defined",
  desc = "User price by commodity and source"
)

equations[["E_p_imp"]] <- create_equation(
  'p[c,"imp"] = pworld[c] * phi * mtx[c]',
  indexes = 'c in COM',
  type = "defining",
  desc = "Price for imported commodities"
)

equations[["E_p_dom"]] <- create_equation(
  'p[c,"dom"] = p1tot[c] * ptx[c]',
  indexes = 'c in COM',
  type = "defining",
  desc = "Price for domestic commodity"
)

variables[["p_s"]] <- create_variable(
  value = 1,
  indexes = sets[c("COM","IMPUSER")],
  type = "defined",
  desc = "User price of composite good c"
)

equations[["E_p_s"]] <- create_equation(
  'p_s[c,u] = sum(SRCSHR[c,,u] * p[c,])',
  indexes = c('c in COM', 'u in IMPUSER'),
  type = "defining",
  desc = "User price of composite good c"
)

variables[["p3tot"]] <- create_variable(
  value = 1,
  indexes = list(p3tot = "p3tot"),
  type = "defined",
  desc = "Consumer price index"
)

equations[["E_p3tot"]] <- create_equation(
  'p3tot = sum(USE_S[,"Households"] * p_s[,"Households"])/USE_CS["Households"]',
  type = "defining",
  desc = "Consumer price index"
)

variables[["p1lab"]] <- create_variable(
  value = 1,
  indexes = list(p1lab = "p1lab"),
  type = "defined",
  desc = "Price of primary factor labor (wage rate)"
)

equations[["E_p1lab"]] <- create_equation(
  'p1lab = realwage * p3tot',
  type = "defining",
  desc = "Price of primary factor labor (wage rate)"
)

variables[["p1prim"]] <- create_variable(
  value = 1,
  indexes = sets[c("IND")],
  type = "defined",
  desc = "Price of primary factor composite"
)

equations[["E_p1prim"]] <- create_equation(
  'p1prim[i] = (FACTOR["Labour",i]*p1lab + FACTOR["Capital",i]*p1cap[i])/V1PRIM[i]',
  indexes = c('i in IND'),
  type = "defining",
  desc = "Price of primary factor composite"
)

variables[["x_s"]] <- create_variable(
  value = 1,
  indexes = sets[c("COM", "IMPUSER")],
  type = "defined",
  desc = "Use of composite c by impuser"
)

equations[["E_x_s_ind"]] <- create_equation(
  'x_s[c,i] = x1tot[i]',
  indexes = c('c in COM', 'i in IND'),
  type = "defining",
  desc = "Use of composity c by industry"
)

equations[["E_x_s_inv"]] <- create_equation(
  'x_s[c, "Investment"] = x_s_inv[c]',
  indexes = 'c in COM',
  type = "defining",
  desc = "Use of composite c for investment"
)

equations[["E_x_s_gov"]] <- create_equation(
  'x_s[c, "Government"] = x_s_gov[c]',
  indexes = 'c in COM',
  type = "defining",
  desc = "Use of composite c by the government"
)

variables[["w3tot"]] <- create_variable(
  value = 1,
  indexes = list(w3tot = "w3tot"),
  type = "defined",
  desc = "Total nominal households consumption"
)

equations[["E_w3tot"]] <- create_equation(
  'w3tot = x3tot * p3tot',
  type = "defining",
  desc = "Total nominal households consumption"
)

equations[["E_x_s_hh"]] <- create_equation(
  'x_s[c, "Households"] = w3tot/p_s[c, "Households"]',
  indexes = c('c in COM'),
  type = "defining",
  desc = "Use of composite c by the households"
)

variables[["x"]] <- create_variable(
  value = 1,
  indexes = sets[c("COM","SRC","USER")],
  type = "defined",
  desc = "Demand by commodity, source and user"
)

equations[["E_x_impuser"]] <- create_equation(
  'x[c,s,u] = x_s[c,u]*(p[c,s]/p_s[c,u])^(-SIGMA[c])',
  indexes = c('c in COM', 's in SRC', 'u in IMPUSER'),
  type = "defining",
  desc = "Demand by commodity, source and impuser"
)

equations[["E_x_expuser"]] <- create_equation(
  'x[c,"dom","Exports"] = f4q[c]*(p[c,"dom"]/(phi*pworld[c]))^(-EXP_ELAST[c])',
  indexes = 'c in COM',
  type = "defining",
  desc = "Exports of domestic commodity c"
)

variables[["x0"]] <- create_variable(
  value = 1,
  indexes = sets[c('COM', 'SRC')],
  type = "defined",
  desc = "Total demand for good c from source s"
)

equations[["E_x0"]] <- create_equation(
  'x0[c,s] = sum(USE[c,s,]*x[c,s,])/SALES[c,s]',
  indexes = c('c in COM', 's in SRC'),
  type = "defining",
  desc = "Total demand for good c from source s"
)

variables[["x1prim"]] <- create_variable(
  value = 1,
  indexes = sets[c('IND')],
  type = "defined",
  desc = "Industry demand for primary-factor composite"
)

equations[["E_x1prim"]] <- create_equation(
  'x1prim[i] = a1prim[i] * x1tot[i]',
  indexes = 'i in IND',
  type = "defining",
  desc = "Industry demand for primary-factor composite"
)

variables[["x1lab"]] <- create_variable(
  value = 1,
  indexes = sets[c('IND')],
  type = "defined",
  desc = "Employment by industry"
)

equations[["E_x1lab"]] <- create_equation(
  'x1lab[i] = x1prim[i]*(p1lab/p1prim[i])^(-SIGMA1PRIM[i])',
  indexes = c('i in IND'),
  type = "defining",
  desc = "Employment by industry"
)

minimal <- list(
  sets = sets,
  params = params,
  variables = variables,
  equations = equations
)

sol <- solve_emr(minimal)

sol$sol$message

minimal$params$x3tot$value[] <- 1.10

system.time(sol_cfl <- solve_emr_block(minimal, trace = TRUE, triter = 500,
                                       tol = 1e-7))

sol_cfl$variables$employ

