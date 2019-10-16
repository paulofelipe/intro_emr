minimal_sr <- function(use_df, fac_df, tar_df, ptx_df,
                       arm_df, sigma1prim_df, expelast_df){
  
  names(use_df) <- c("COM", "SRC", "USER", "Value")
  names(arm_df) <- c("COM", "Value")
  names(expelast_df) <- c("COM", "Value")
  names(fac_df) <- c("FAC", "IND", "Value")
  
  params <- list()
  variables <- list()
  equations <- list()
  
  IND <- unique(fac_df[["IND"]])
  COM <- unique(use_df[["COM"]])
  SRC <- unique(use_df[["SRC"]])
  USER <- unique(use_df[["USER"]])
  IMPUSER <- USER[!grepl("expor", tolower(USER))]
  FINALUSER <- setdiff(USER, IND)
  FAC <- unique(fac_df[["FAC"]])
  
  sets <- list(
    IND = IND,
    COM = COM,
    SRC = SRC,
    USER = USER,
    IMPUSER = IMPUSER,
    FINALUSER = FINALUSER,
    FAC = FAC
  )
  
  params[["PTX"]] <- create_param(
    value = 1,
    indexes = sets['IND'],
    desc = "Variação no poder do imposto sobre a produção"
  )
  
  params[["PWORLD"]] <- create_param(
    value = 1,
    indexes = sets['COM'],
    desc = "Variação no preço internacional do produto c"
  )
  
  params[["PHI"]] <- create_param(
    value = 1,
    indexes = "PHI",
    desc = "Variação na taxa de câmbio"
  )
  
  params[["MTX"]] <- create_param(
    value = 1,
    indexes = sets['COM'],
    desc = "Variação no poder da tarifa de importação do produto c"
  )
  
  SRCSHARE <- use_df %>% 
    filter(USER %in% IMPUSER) %>% 
    group_by(COM, USER) %>% 
    mutate(SRCSHR = Value/sum(Value),
           SRCSHR = ifelse(is.nan(SRCSHR), 0.5, SRCSHR)) %>% 
    select(COM, SRC, IMPUSER = USER, SRCSHR)
  
  params[["SRCSHARE"]] <- create_param(
    value = SRCSHARE,
    indexes = sets[c("COM", "SRC", "IMPUSER")],
    desc = "Participação da origem s no consumo do produto c pelo usurário u"
  )
  
  variables[["p"]] <- create_variable(
    value = 1,
    indexes = sets[c("COM","SRC")],
    type = "defined",
    desc = "Variação no preço do produto c de origem s"
  )
  
  equations[["E_p"]] <- create_equation(
    'if(s == "dom"){
        p[c,s] = p1tot[c] * PTX[c]
      } else {
        p[c,s] = PWORLD[c] * PHI * MTX[c]
      }',
    indexes = c("c in COM", "s in SRC"),
    type = "defining",
    desc = "Variação no preço do produto c de origem s"
  )
  
  variables[["p_s"]] <- create_variable(
    value = 1,
    indexes = sets[c("COM", "IMPUSER")],
    type = "defined",
    desc = "Variação no índice de preço do bem composto c para o usuário u"
  )
  
  equations[["E_p_s"]] <- create_equation(
    'p_s[c,u] = sum(SRCSHARE[c,,u] * p[c,]^(1-SIGMA[c]))^(1/(1-SIGMA[c]))',
    indexes = c('c in COM', 'u in IMPUSER'),
    type = "defining",
    desc = "Variação no índice de preço do bem composto c para o usuário u"
  )
  
  params[["A"]] <- create_param(
    value = 1,
    indexes = sets[c('COM', 'IND')],
    desc = "Variação do coeficiente técnico para o produto c usado pela indústria i"
  )
  
  params[["A1PRIM"]] <- create_param(
    value = 1,
    indexes = sets['IND'],
    desc = "Variação do coeficiente técnico para o fator primário usado pela indústria i"
  )
  
  variables[["x_s"]] <- create_variable(
    value = 1,
    indexes = sets[c("COM", "IMPUSER")],
    type = "defined",
    desc = "Variação no uso do composto c por impuser"
  )
  
  equations[["E_x_s_ind"]] <- create_equation(
    'x_s[c,i] = A[c,i] * x1tot[i]',
    indexes = c('c in COM', 'i in IND'),
    type = "defining",
    desc = "Variação do uso do composto c por indústria"
  )
  
  variables[["x1prim"]] <- create_variable(
    value = 1,
    indexes = sets[c('IND')],
    type = "defined",
    desc = "Uso do fator primário composto por indústria"
  )
  
  equations[["E_x1prim"]] <- create_equation(
    'x1prim[i] = A1PRIM[i] * x1tot[i]',
    indexes = 'i in IND',
    type = "defining",
    desc = "Uso do fator primário composto por indústria"
  )
  
  SHARE_HH <- use_df %>% 
    filter(USER == "Households") %>% 
    group_by(COM) %>% 
    summarise(Value = sum(Value)) %>% 
    mutate(SHARE = Value/sum(Value)) %>% 
    select(COM, SHARE)
  
  params[["SHARE_HH"]] <- create_param(
    value = SHARE_HH,
    indexes = sets['COM'],
    desc = "Participação do bem c no dispêndio das famílias"
  )
  
  params[["X3TOT"]] <- create_param(
    value = 1,
    indexes = "X3TOT",
    desc = "Variação no dispêndio real das famílias"
  )
  
  variables[["p3tot"]] <- create_variable(
    value = 1,
    indexes = "p3tot",
    type = "defined",
    desc = "Variação no índice de preços das famílias"
  )
  
  equations[["E_p3tot"]] <- create_equation(
    'p3tot = prod(p_s[,"Households"]^SHARE_HH[])',
    type = "defining",
    desc = "Variação do índice de preços das famílias"
  )
  
  variables[["w3tot"]] <- create_variable(
    value = 1,
    indexes = "w3tot",
    type = "defined",
    desc = "Variação da renda nominal das famílias"
  )
  
  equations[["E_w3tot"]] <- create_equation(
    'w3tot = X3TOT * p3tot',
    type = "defining",
    desc = "Variação na renda (dispêndio) nominal das família"
  )
  
  equations[["E_x_s_hh"]] <- create_equation(
    'x_s[c, "Households"] = w3tot/p_s[c, "Households"]',
    indexes = c('c in COM'),
    type = "defining",
    desc = "Variação do uso do composto c pelas famílias"
  )
  
  params[["X_S_INV"]] <- create_param(
    value = 1,
    indexes = sets["COM"],
    desc = "Variação na demanda de investimento por produto c"
  )
  
  params[["X_S_GOV"]] <- create_param(
    value = 1,
    indexes = sets["COM"],
    desc = "Variação na demanda do governo por produto c"
  )
  
  equations[["E_x_s_inv"]] <- create_equation(
    'x_s[c, "Investment"] = X_S_INV[c]',
    indexes = 'c in COM',
    type = "defining",
    desc = "Variação no uso do composto c para investimento"
  )
  
  equations[["E_x_s_gov"]] <- create_equation(
    'x_s[c, "Government"] = X_S_GOV[c]',
    indexes = 'c in COM',
    type = "defining",
    desc = "Variação no uso do composto c pelo governo"
  )
  
  params[["SIGMA"]] <- create_param(
    value = arm_df,
    indexes = sets[c("COM")],
    desc = "Elasticidade de Armington"
  )
  
  variables[["x"]] <- create_variable(
    value = 1,
    indexes = sets[c("COM","SRC","USER")],
    type = "defined",
    desc = "Variação na demanda por produto, fonte e usuário"
  )
  
  equations[["E_x_impuser"]] <- create_equation(
    'x[c,s,u] = x_s[c,u]*(p[c,s]/p_s[c,u])^(-SIGMA[c])',
    indexes = c('c in COM', 's in SRC', 'u in IMPUSER'),
    type = "defining",
    desc = "Variação na demanda por produto, fonte e usuário"
  )
  
  params[["EXP_ELAST"]] <- create_param(
    value = expelast_df,
    indexes = sets["COM"],
    desc = "Elasticidade da demanda por exportações"
  )
  
  params[["F4Q"]] <- create_param(
    value = 1,
    indexes = sets[c("COM")],
    desc = "Shift na demanda externa para o produto c"
  )
  
  equations[["E_x_exp"]] <- create_equation(
    'x[c,"dom","Exports"] = F4Q[c]*(p[c,"dom"]/(PHI*PWORLD[c]))^(-EXP_ELAST[c])',
    indexes = 'c in COM',
    type = "defining",
    desc = "Variação das exportações do produto c"
  )
  
  params[["SIGMA1PRIM"]] <- create_param(
    value = sigma1prim_df,
    indexes = sets["IND"],
    desc = "Elasticidade de subsituição entre os fatores de produção"
  )
  
  SHAREPRIM <- fac_df %>% 
    group_by(IND) %>% 
    mutate(SHAREPRIM = Value/sum(Value)) %>% 
    select(FAC, IND, SHAREPRIM)
  
  params[["SHAREPRIM"]] <- create_param(
    value = SHAREPRIM,
    indexes = sets[c("FAC", "IND")],
    desc = "Part. de cada fator no uso do fator primário por indústria"
  )
  
  params[["RW"]] <- create_param(
    value = 1,
    indexes = "rw",
    desc = 'Variação no salário real'
  )
  
  variables[["x1lab"]] <- create_variable(
    value = 1,
    indexes = sets[c('IND')],
    type = "defined",
    desc = "Variação no emprego por indústria"
  )
  
  equations[["E_x1lab"]] <- create_equation(
    'x1lab[i] = x1prim[i]*(p1lab/p1prim[i])^(-SIGMA1PRIM[i])',
    indexes = c('i in IND'),
    type = "defining",
    desc = "Variação no emprego por indústria"
  )
  
  variables[["x1cap"]] <- create_variable(
    value = 1,
    indexes = sets[c('IND')],
    type = "defined",
    desc = "Variação no uso de capital por indústria"
  )
  
  equations[["E_x1cap"]] <- create_equation(
    'x1cap[i] = (p1cap[i]/p1prim[i])^(-SIGMA1PRIM[i]) * x1prim[i]',
    indexes = "i in IND",
    type = "defining",
    desc = "Variação na demanda por capital na indústria i"
  )
  
  variables[["p1lab"]] <- create_variable(
    value = 1,
    indexes = "p1lab",
    type = "defined",
    desc = "Variação no salário nominal"
  )
  
  equations[["E_p1lab"]] <- create_equation(
    'p1lab = RW * p3tot',
    type = "defining",
    desc = "Variação no salário nominal"
  )
  
  variables[["p1prim"]] <- create_variable(
    value = 1,
    indexes = sets['IND'],
    type = "defined",
    desc = "Variação no índice de preço do fator primário composto por indústria i"
  )
  
  equations[["E_p1prim"]] <- create_equation(
    'p1prim[i] = (SHAREPRIM["Labour", i] * p1lab^(1 - SIGMA1PRIM[i]) +
    SHAREPRIM["Capital", i] * p1cap[i]^(1 - SIGMA1PRIM[i]))^(1/(1-SIGMA1PRIM[i]))',
    indexes = c('i in IND'),
    type = "defining",
    desc = "Variação no índice de preço do fator primário para indústria i"
  )
  
  SHRSALES <- use_df %>% 
    group_by(COM, SRC) %>% 
    mutate(SALES = sum(Value),
           SHRSALES = Value/SALES) %>% 
    select(COM, SRC, USER, SHRSALES)
  
  # Salários + Remuneração do Capital
  V1PRIM <- fac_df %>% 
    group_by(IND) %>% 
    summarise(Value = sum(Value))
  
  # Custos Intermediários
  IC <- use_df %>% 
    filter(USER %in% IND) %>% 
    group_by(IND = USER) %>% 
    summarise(ic = sum(Value)) 
  
  # Custos totais
  V1TOT <- V1PRIM %>% 
    rename(va = Value) %>% 
    left_join(IC, by = "IND") %>% 
    mutate(Value = va + ic) %>% 
    select(IND, Value)
  
  
  params[["USE"]] <- create_param(
    value = use_df,
    indexes = sets[c("COM", "SRC", "USER")],
    desc = "Uso do produto c da fonte s pelo usuário u"
  )
  
  params[["USE_S"]] <- create_param(
    value = use_df %>% 
      group_by(COM, USER) %>% 
      summarise(Value = sum(Value)),
    indexes = sets[c("COM", "USER")],
    desc = "Uso do produto c da fonte s pelo usuário u"
  )
  
  params[["FAC0"]] <- create_param(
    value = fac_df,
    indexes = sets[c("FAC", "IND")],
    desc = "Uso inicial dos fatores primários pela indústria i"
  )
  
  params[["V1TOT"]] <- create_param(
    value = V1TOT,
    indexes = sets[c("IND")],
    desc = "Custo total da indústria i"
  )
  
  params[["SHRSALES"]] <- create_param(
    value = SHRSALES,
    indexes = sets[c("COM", "SRC", "USER")],
    desc = "Participação do usuário u nas vendas de c de origem s"
  )
  
  variables[["x0"]] <- create_variable(
    value = 1,
    indexes = sets[c("COM", "SRC")],
    type = "defined",
    desc = "Variação na demanda total de c de origem s"
  )
  
  equations[["E_x0"]] <- create_equation(
    "x0[c,s] = sum(SHRSALES[c,s,] * x[c,s,])",
    indexes = c("c in COM", "s in SRC"),
    type = "defining",
    desc = "Variação na demanda total de c de origem s"
  )
  
  # variables[["cost"]] <- create_variable(
  #   value = 1,
  #   indexes = sets[c("IND")],
  #   type = "undefined",
  #   desc = "Variação no custo marginal de produção "
  # )
  # 
  # equations[["E_cost"]] <- create_equation(
  #   "-cost[i] + (sum(USE[,,i]*p*x[,,i]) +
  # FAC0['Labour',i]*p1lab*x1lab[i] +
  # FAC0['Capital',i]*p1cap[i]*x1cap[i])/(V1TOT[i]*x1tot[i])",
  #   indexes = c("i in IND"),
  #   type = "mcc",
  #   desc = "Variação no custo marginal de produção da indústria i"
  # )
  
  variables[["p1tot"]] <- create_variable(
    value = 1,
    indexes = sets[c("IND")],
    type = "undefined",
    desc = "Variação no preço da indústria i"
  )
  
  equations[["E_p1tot"]] <- create_equation(
    "p1tot[i] - (sum(USE_S[,i] * p_s[,i]) +
                  sum(FAC0[,i]) *p1prim[i])/(V1TOT[i])",
    indexes = c("i in IND"),
    type = "mcc",
    desc = "Condição de lucro zero"
  )
  
  variables[["x1tot"]] <- create_variable(
    value = 1,
    indexes = sets[c("IND")],
    type = "undefined",
    desc = "Variação na produção da indústria i"
  )
  
  equations[["E_x1tot"]] <- create_equation(
    "-x1tot[i] + x0[i,'dom']",
    indexes = c("i in IND"),
    type = "mcc",
    desc = "Equilíbrio no mercado i"
  )
  
  SHARELAB <- fac_df %>%
    filter(FAC == "Labour") %>% 
    mutate(SHARELAB = Value/sum(Value)) %>% 
    select(IND, SHARELAB)
  
  params[["SHARELAB"]] <- create_param(
    value = SHARELAB,
    indexes = sets["IND"],
    desc = "Part. de cada indústria no uso do fator trabalho"
  )
  
  variables[["l"]] <- create_variable(
    value = 1,
    indexes = "emprego",
    type = "undefined",
    desc = "Variação no emprego total"
  )
  
  equations[["E_l"]] <- create_equation(
    'l - sum(SHARELAB * x1lab)',
    type = "mcc",
    desc = "Variação no emprego total"
  )
  
  params[["X1CAP_EXO"]] <- create_param(
    value = 1,
    indexes = sets["IND"],
    desc = "Variação exógena no uso de capital por indústria"
  )
  
  variables[["p1cap"]] <- create_variable(
    value = 1,
    indexes = sets['IND'],
    type = "undefined",
    desc = "Variação na remuneração do capital por indústria i"
  )
  
  equations[["E_p1cap"]] <- create_equation(
    'X1CAP_EXO[i] - x1cap[i]',
    indexes = 'i in IND',
    type = "mcc",
    desc = "equilíbrio no mercado de capital para indústria i"
  )
  
  v0gdpinc <- sum(fac_df$Value) +
    sum(tar_df$Value) +
    sum(ptx_df$Value)
  
  v0cif_tar <- use_df %>% 
    filter(SRC == "imp") %>% 
    group_by(COM) %>% 
    summarise(sales = sum(Value)) %>% 
    left_join(tar_df) %>% 
    mutate(v0cif = sales - Value,
           tar = Value/v0cif)
  
  v0cif <- v0cif_tar %>% 
    select(COM, v0cif)
  
  tarifas <- v0cif_tar %>% 
    select(COM, tar)
  
  ptx0 <- left_join(V1TOT, ptx_df, by = "IND",
                    suffix = c("_V1TOT", "_PTX")) %>% 
    mutate(ptx0 = Value_PTX/Value_V1TOT) %>% 
    select(IND, ptx0)
  
  params[["V0GDPINC"]] <- create_param(
    value = v0gdpinc,
    indexes = "v0gdpinc",
    desc = "Valor inicial do PIB"
  )
  
  params[["V0CIF"]] <- create_param(
    value = v0cif,
    indexes = sets["COM"],
    desc = "Valor CIF inicial das importações"
  )
  
  params[["MTX0"]] <- create_param(
    value = tarifas,
    indexes = sets["COM"],
    desc = "Tarifas Iniciais"
  )
  
  params[["PTX0"]] <- create_param(
    value = ptx0,
    indexes = sets["IND"],
    desc = "Taxa inicial de imposto sobre a produçao"
  )
  
  variables[["wgdpinc"]] <- create_variable(
    value = 1,
    indexes = "wgdpinc",
    type = "defined",
    desc = "Variação no PIB nominal"
  )
  
  equations[["E_wgdpinc"]] <- create_equation(
    "wgdpinc = (sum(FAC0['Labour',] * p1lab  * x1lab) +
  sum(FAC0['Capital',] * p1cap  * x1cap) +
  sum(V0CIF * x0[,'imp'] * PWORLD * PHI * ((1 + MTX0) * MTX - 1)) +
  sum(V1TOT * p1tot * x1tot * ((1 + PTX0) * PTX - 1)))/V0GDPINC",
    type = "defining",
    desc = "Variação no PIB nominal"
  )
  
  total_dispendio <- use_df %>% 
    filter(USER %in% FINALUSER) %>% 
    pull(Value) %>% 
    sum()
  
  total_importacoes <- sum(v0cif$v0cif)
  
  v0gdpexp <- total_dispendio - total_importacoes
  
  params[["V0GDPEXP"]] <- create_param(
    value = v0gdpexp,
    indexes = "v0gdpexp",
    desc = "Valor inicial do PIB pela ótica do dispêndio"
  )
  
  variables[["wgdpexp"]] <- create_variable(
    value = 1,
    indexes = "wgdpexp",
    type = "defined",
    desc = "Variação no PIB nominal pela ótica do dispêndio"
  )
  
  equations[["E_wgdpexp"]] <- create_equation(
    "wgdpexp = (@sum_emr(sum_emr(sum_emr('USE[c,s,u] * p[c,s] * x[c,s,u]',
  'u', 'FINALUSER'), 's', 'SRC'), 'c', 'COM')@
  - sum(V0CIF * x0[,'imp'] * PWORLD * PHI))/V0GDPEXP",
    type = "defining",
    desc = "Variação no PIB nominal pela ótica no dispêndio"
  )
  
  variables[["pgdpexp"]] <- create_variable(
    value = 1,
    indexes = "pgdpexp",
    type = "defined",
    desc = "Variação no deflator do PIB"
  )
  
  equations[["E_pgdpexp"]] <- create_equation(
    "pgdpexp = sqrt(
    ((@sum_emr(sum_emr(sum_emr('USE[c,s,u] * p[c,s]', 'u', 'FINALUSER'), 's', 'SRC'), 'c', 'COM')@ -
    sum(V0CIF * PWORLD * PHI))/(sum(USE[,,FINALUSER]) - sum(V0CIF))) *
    ((@sum_emr(sum_emr(sum_emr('USE[c,s,u] * p[c,s] * x[c,s,u]','u', 'FINALUSER'), 's', 'SRC'), 'c', 'COM')@ - 
    sum(V0CIF * PWORLD * PHI * x0[,'imp']))/(sum(USE[,,FINALUSER] * x[,,FINALUSER]) - sum(V0CIF * x0[,'imp'])))
    )",
    type = "defining",
    desc = "Variação no deflator do PIB"
  )
  
  variables[["xgdpexp"]] <- create_variable(
    value = 1,
    indexes = "xgdpexp",
    type = "defined",
    desc = "Variação no PIB real"
  )
  
  equations[["E_xgdpexp"]] <- create_equation(
    "xgdpexp = wgdpexp/pgdpexp",
    type = "defining",
    desc = "Variação no PIB real"
  )
  
  variables[["p2tot"]] <- create_variable(
    value = 1,
    indexes = "p2tot",
    type = "defined",
    desc = "Variação no índice de preço do investimento"
  )
  
  equations[["E_p2tot"]] <- create_equation(
    "p2tot = @sum_emr(sum_emr(\"USE[c,s,'Investment']/sum(USE[,,'Investment']) *
  p[c,s] * x[c,s, 'Investment'] \", 's', 'SRC'), 'c', 'COM')@",
    type = "defining",
    desc = "Variação no índice de preço do investimento"
  )
  
  variables[["p4tot"]] <- create_variable(
    value = 1,
    indexes = "p4tot",
    type = "defined",
    desc = "Variação no índice de preços da exportação"
  )
  
  equations[["E_p4tot"]] <- create_equation(
    "p4tot = sqrt((sum(USE[,'dom','Exports'] * p[,'dom'])/(sum(USE[,'dom','Exports']))) *
    (sum(USE[,'dom','Exports'] * p[,'dom'] * x[,'dom','Exports'])/(sum(USE[,'dom','Exports'] * x[,'dom','Exports']))))",
    type = "defining",
    desc = "Variação no índice de preços da exportação"
  )
  
  variables[["x4tot"]] <- create_variable(
    value = 1,
    indexes = "x4tot",
    type = "defined",
    desc = "Variação no índice de volume da exportação"
  )
  
  equations[["E_x4tot"]] <- create_equation(
    "x4tot = sum(USE[,'dom','Exports']/sum(USE[,'dom','Exports']) * 
    p[,'dom'] * x[,'dom','Exports'])/p4tot",
    type = "defining",
    desc = "Variação no índice de volume de exportação"
  )
  
  variables[["p0cif"]] <- create_variable(
    value = 1,
    indexes = "p0cif",
    type = "defined",
    desc = "Variação no índice de preços da importação"
  )
  
  equations[["E_p0cif"]] <- create_equation(
    "p0cif = (sum(V0CIF * PWORLD * PHI)/sum(V0CIF)) *
    (sum(V0CIF * PWORLD * PHI * x0[,'imp'])/sum(V0CIF * x0[,'imp']))",
    type = "defining",
    desc = "Variação no índice de preços da exportação"
  )
  
  variables[["x0cif"]] <- create_variable(
    value = 1,
    indexes = "x0cif",
    type = "defined",
    desc = "Variação no índice de volume da importação"
  )
  
  equations[["E_x0cif"]] <- create_equation(
    "x0cif = sum(V0CIF[] * PWORLD * PHI * x0[,'imp'])/(sum(V0CIF) * p0cif)",
    type = "defining",
    desc = "Variação no índice de preços da importação"
  )
  
  variables[["delB"]] <- create_variable(
    value = 1,
    indexes = "delB",
    type = "defined",
    desc = "Balança comercial como razão do PIB"
  )
  
  equations[["E_delB"]] <- create_equation(
    "delB = sum(USE[,'dom','Exports'] * p[,'dom'] * x[,'dom','Exports'] - 
  V0CIF[] * PWORLD[] * PHI * x0[,'imp'])/(V0GDPEXP * wgdpexp) - 
  sum(USE[,'dom','Exports'] - V0CIF[])/(V0GDPEXP)",
    type = "defining",
    desc = "Variação no índice de preços da importação"
  )
  
  update_equations <- list()
  
  update_equations[["USE"]] <- create_equation(
    "USE[c,s,u] = USE[c,s,u] * p[c,s] * x[c,s,u]",
    indexes = c("c in COM", "s in SRC", "u in USER"),
    desc = "Atualização dos dados de uso dos produto"
  )
  
  update_equations[["V0CIF"]] <- create_equation(
    "V0CIF = V0CIF * x0[,'imp'] * PWORLD * PHI",
    desc = "Atualização dos dados de importações"
  )
  
  update_equations[["V0TAR"]] <- create_equation(
    "V0TAR = V0CIF * ((1 + MTX0) * MTX - 1)",
    desc = "Atualização dos dados de importações"
  )
  
  update_equations[["V1TOT"]] <- create_equation(
    "V1TOT = V1TOT * x1tot * p1tot",
    desc = "Atualização dos dados de produção"
  )
  
  update_equations[["V1PTX"]] <- create_equation(
    "V1PTX = V1TOT * ((1 + PTX0) * PTX - 1)",
    desc = "Atualização dos dados de importações"
  )
  
  update_equations[["FAC0"]] <- create_equation(
    "if(f == 'Labour'){
    FAC0[f,i] = FAC0[f,i] * p1lab * x1lab[i]
  } else{
    FAC0[f,i] = FAC0[f,i] * p1cap[i] * x1cap[i]
  }",
    indexes = c("f in FAC", "i in IND"),
    desc = "Atualização dos dados de uso de fatores primários"
  )
  
  list(
    sets = sets,
    params = params,
    variables = variables,
    equations = equations,
    update_equations = update_equations
  )
}