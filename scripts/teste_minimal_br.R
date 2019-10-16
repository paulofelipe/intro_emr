library(emr)
library(tidyverse)
library(readxl)


# Modelos -----------------------------------------------------------------

source('scripts/minimal_sr.R', encoding = "UTF-8")
source('scripts/minimal_lr.R', encoding = "UTF-8")


# Dados -------------------------------------------------------------------
dados_path <- 'dados/minimal_br_2015.xlsx'

use_df <- read_excel(dados_path, sheet = 'use') %>% 
  gather(key = USER, value = Value, -(COM:SRC))

fac_df <- read_excel(dados_path, sheet = 'fac') %>% 
  gather(key = IND, value = Value, -(FAC))

tar_df <- read_excel(dados_path, sheet = 'tar')

ptx_df <- read_excel(dados_path, sheet = 'ptx')

arm_df <- read_excel(dados_path, sheet = 'arm')

sigma1prim_df <- read_excel(dados_path, sheet = 'sigma1prim')

expelast_df <- read_excel(dados_path, sheet = 'expelast')

# Construindo os objetos com o modelo ------------------------------------

# fechamento de curto prazo
minimal_br_sr <- minimal_sr(
  use_df = use_df,
  fac_df = fac_df,
  tar_df = tar_df,
  ptx_df = ptx_df,
  arm_df = arm_df,
  sigma1prim_df = sigma1prim_df,
  expelast_df = expelast_df
)

# fechamento de longo prazo

minimal_br_lr <- minimal_lr(
  use_df = use_df,
  fac_df = fac_df,
  tar_df = tar_df,
  ptx_df = ptx_df,
  arm_df = arm_df,
  sigma1prim_df = sigma1prim_df,
  expelast_df = expelast_df
)


sol <- solve_emr(minimal_br_lr)
sol$sol

minimal_br_lr$params$SIGMA$value[] <- 4
minimal_br_lr$params$SIGMA1PRIM$value[] <- 0.5
minimal_br_lr$params$MTX$value[] <- (1 + 0 *  minimal_br_lr$params$MTX0$value[]) /
   (1 + minimal_br_lr$params$MTX0$value[])
# minimal_br_lr$params$PTX$value[] <- (1 + 0 *  minimal_br_lr$params$PTX0$value[]) /
#   (1 + minimal_br_lr$params$PTX0$value[])

#minimal_br_lr$params$L$value[] <- 1.01

system.time(sol1 <- solve_emr(minimal_br_lr, method = "dfsane", trace = TRUE, triter = 100,
                  noimp = 10000, M = 0, NM = FALSE))
sol1$sol$message

map_df(c("wgdpexp", "wgdpinc", "pgdpexp", "xgdpexp", "x3tot", "p3tot",
         "w3tot", "rw", "p1lab", "p2tot", "x4tot", "p4tot", "x0cif"), ~{
           tibble(
             variavel = sol1$variables_descriptions %>% 
               filter(variable == .x) %>% 
               pull(description) %>% 
               paste0(" (", .x,")"), 
             value = round((sol1$variables[[.x]] - 1) * 100, 2)
           )
         }) 

sol1$variables$x1tot %>% 
  enframe() %>% 
  mutate(value = (value - 1) * 100)
