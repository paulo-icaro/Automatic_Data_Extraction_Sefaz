# ==================== #
# === SIOF DATASET === #
# ==================== #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
source('https://raw.githubusercontent.com/paulo-icaro/Variables_Frequency_Transforming/refs/heads/main/variables_frequency_transforming.R')
source('https://raw.githubusercontent.com/paulo-icaro/Update_Variables_Actual_Value/refs/heads/main/update_actual_value.R')
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(lubridate)

# --- Path Auxiliar --- #
path = 'Databases/Inputs/'

# --- Bases Iniciais --- #
database_invest_programa_regiao = read_excel(path = paste0(path, 'investimentos_siof_ceara_programa_regiao.xlsx'))
database_invest_funcao = read_excel(path = paste0(path, 'investimentos_siof_ceara_funcao.xlsx'))


# ========================= #
# === Funções Auxliares === #
# ========================= #

# ----------------------------------- #
# --- Transformação de Frequência --- #
# ----------------------------------- #
bim_transform = function(df, vars_group){
  cumulative_transform(
    transform_type = 'diff_acumulado',
    frequency = 'bimestral',
    dataset = df,
    groupby_variables = vars_group)
}

# ----------------------------------------------- #
# --- Processamento de Dados - Investimentos ---- #
# ----------------------------------------------- #
process_invest = function(df, group_vars, min_year = 2015){
  df |> 
    filter(categoria == 'pago_acumulado', ano >= min_year) |> 
    group_by(across(all_of(group_vars))) |> 
    summarize(valor = sum(valor), .groups = 'drop') |> 
    mutate(data = as.character.Date(paste0(ano, '-', mes, '-01')))
}

# ------------------------------ #
# --- Matricizacao dos Dados --- #
# ------------------------------ #
pivotting = function(df, name_var = 'tipo', value_valor = 'valor'){
  pivot_wider(df, names_from = name_var, values_from = value_valor)
}



# ======================================================================== #
# === Processamento de Dados - Investimentos Publicos por Tipo - Macro === #
# ======================================================================== #


# ---------------------------------------------------- #
# --- Processamentos e Transformacao de Frequencia --- #
# ---------------------------------------------------- #
invest_programa_tipo_bim                 = bim_transform(process_invest(df = database_invest_programa_regiao, group_vars = c('ano', 'mes', 'tipo'), min_year =  2016)[,3:5], c('tipo'))
invest_programa_regiao_bim               = bim_transform(process_invest(df = database_invest_programa_regiao, group_vars = c('ano', 'mes', 'regiao'), min_year = 2016)[,3:5], c('regiao'))
invest_programa_tipo_regiao_bim          = bim_transform(process_invest(df = database_invest_programa_regiao, group_vars = c('ano', 'mes', 'tipo', 'regiao'), min_year = 2016)[,3:6], c('tipo', 'regiao'))
invest_programa_tipo_regiao_programa_bim = bim_transform(process_invest(df = database_invest_programa_regiao, group_vars = c('ano', 'mes', 'tipo', 'regiao', 'programa'), min_year = 2016)[,3:7], c('tipo', 'regiao', 'programa'))
invest_funcao_tipo_bim                   = bim_transform(process_invest(df = database_invest_funcao, group_vars = c('ano', 'mes', 'tipo'), min_year =  2016)[,3:5], c('tipo'))
invest_funcao_funcao_bim                 = bim_transform(process_invest(df = database_invest_funcao, group_vars = c('ano', 'mes', 'funcao'), min_year =  2016)[,3:5], c('funcao'))
invest_funcao_tipo_funcao_bim            = bim_transform(process_invest(df = database_invest_funcao, group_vars = c('ano', 'mes', 'tipo', 'funcao'), min_year =  2016)[,3:6], c('tipo','funcao'))

# ----------------------------- #
# --- Matricizando os Dados --- #
# ----------------------------- #
invest_programa_tipo_curr_bim_m                  = pivotting(invest_programa_tipo_bim)
invest_programa_regiao_curr_bim_m                = pivotting(invest_programa_regiao_bim, name_var = c('regiao'))
invest_programa_tipo_regiao_curr_bim_m           = pivotting(invest_programa_tipo_regiao_bim, name_var = c('tipo', 'regiao'))
invest_programa_tipo_regiao_programa_curr_bim_m  = pivotting(invest_programa_tipo_regiao_programa_bim, name_var = c('tipo', 'regiao', 'programa'))
invest_funcao_tipo_curr_bim_m                    = pivotting(invest_funcao_tipo_bim)
invest_funcao_funcao_curr_bim_m                  = pivotting(invest_funcao_funcao_bim, name_var = 'funcao')
invest_funcao_tipo_funcao_curr_bim_m             = pivotting(invest_funcao_tipo_funcao_bim, name_var = c('tipo', 'funcao'))


# ----------------------------------------- #
# --- Atualizacao de Valores Monetarios --- #
# ----------------------------------------- #
invest_programa_tipo_actual_bim_m                 = value_updating(series = invest_programa_tipo_curr_bim_m, variables = colnames(select(invest_programa_tipo_curr_bim_m, -data)), base_period = '2025-12', start = '2015', show_price_index = FALSE)
invest_programa_regiao_actual_bim_m               = value_updating(series = invest_programa_regiao_curr_bim_m, variables = colnames(select(invest_programa_regiao_curr_bim_m, -data)), base_period = '2025-12', start = '2015', show_price_index = FALSE)
invest_programa_tipo_regiao_actual_bim_m          = value_updating(series = invest_programa_tipo_regiao_curr_bim_m, variables = colnames(select(invest_programa_tipo_regiao_curr_bim_m, -data)), base_period = '2025-12', start = '2015', show_price_index = FALSE)
invest_programa_tipo_regiao_programa_actual_bim_m = value_updating(series = invest_programa_tipo_regiao_programa_curr_bim_m, variables = colnames(select(invest_programa_tipo_regiao_programa_curr_bim_m, -data)), base_period = '2025-12', start = '2015', show_price_index = FALSE)
invest_funcao_tipo_actual_bim_m                   = value_updating(series = invest_funcao_tipo_curr_bim_m, variables = colnames(select(invest_funcao_tipo_curr_bim_m, -data)), base_period = '2025-12', start = '2015', show_price_index = FALSE)
invest_funcao_funcao_actual_bim_m                 = value_updating(series = invest_funcao_funcao_curr_bim_m, variables = colnames(select(invest_funcao_funcao_curr_bim_m, -data)), base_period = '2025-12', start = '2015', show_price_index = FALSE)
invest_funcao_tipo_funcao_actual_bim_m            = value_updating(series = invest_funcao_tipo_funcao_curr_bim_m, variables = colnames(select(invest_funcao_tipo_funcao_curr_bim_m, -data)), base_period = '2025-12', start = '2015', show_price_index = FALSE)


# --------------------------------------------- #
# --- Base Final - Valores Nominais e Reais --- #
# --------------------------------------------- #
invest_programa_tipo_bim_m                 = rbind(invest_programa_tipo_curr_bim_m |> mutate(tipo_preco = 'nominal'), invest_programa_tipo_actual_bim_m |> mutate(tipo_preco = 'real'))
invest_programa_regiao_bim_m               = rbind(invest_programa_regiao_curr_bim_m |> mutate(tipo_preco = 'nominal'), invest_programa_regiao_actual_bim_m |> mutate(tipo_preco = 'real'))
invest_programa_tipo_regiao_bim_m          = rbind(invest_programa_tipo_regiao_curr_bim_m |> mutate(tipo_preco = 'nominal'), invest_programa_tipo_regiao_actual_bim_m |> mutate(tipo_preco = 'real'))
invest_programa_tipo_regiao_programa_bim_m = rbind(invest_programa_tipo_regiao_programa_curr_bim_m |> mutate(tipo_preco = 'nominal'), invest_programa_tipo_regiao_programa_actual_bim_m |> mutate(tipo_preco = 'real'))
invest_funcao_tipo_bim_m                   = rbind(invest_funcao_tipo_curr_bim_m |> mutate(tipo_preco = 'nominal'), invest_funcao_tipo_actual_bim_m |> mutate(tipo_preco = 'real'))
invest_funcao_funcao_bim_m                 = rbind(invest_funcao_funcao_curr_bim_m |> mutate(tipo_preco = 'nominal'), invest_funcao_funcao_actual_bim_m |> mutate(tipo_preco = 'real'))
invest_funcao_tipo_funcao_bim_m            = rbind(invest_funcao_tipo_funcao_curr_bim_m |> mutate(tipo_preco = 'nominal'), invest_funcao_tipo_funcao_actual_bim_m |> mutate(tipo_preco = 'real'))


# ------------------------------- #
# --- Verticalizando os Dados --- #
# ------------------------------- #
invest_programa_tipo_bim_t                 = pivot_longer(data = invest_programa_tipo_bim_m, cols = !starts_with(c('data', 'tipo_preco')), names_to = 'variavel', values_to = 'valor')
invest_programa_regiao_bim_t               = pivot_longer(data = invest_programa_regiao_bim_m, cols = !starts_with(c('data', 'tipo_preco', 'regiao')), names_to = 'variavel', values_to = 'valor')
invest_programa_tipo_regiao_bim_t          = pivot_longer(data = invest_programa_tipo_regiao_bim_m, cols = !starts_with(c('data', 'tipo_preco', 'regiao')), names_to = 'variavel', values_to = 'valor') |> separate(col = 'variavel', into = c('tipo', 'regiao'), sep = '_')
invest_programa_tipo_regiao_programa_bim_t = pivot_longer(data = invest_programa_tipo_regiao_programa_bim_m, !starts_with(c('data', 'tipo_preco', 'regiao', 'programa')), names_to = 'variavel', values_to = 'valor') |> separate(col = 'variavel', into = c('tipo', 'regiao', 'programa'), sep = '_')
invest_funcao_tipo_bim_t                   = pivot_longer(data = invest_funcao_tipo_bim_m, cols = !starts_with(c('data', 'tipo_preco')), names_to = 'variavel', values_to = 'valor')
invest_funcao_funcao_bim_t                 = pivot_longer(data = invest_funcao_funcao_bim_m, cols = !starts_with(c('data', 'tipo_preco')), names_to = 'variavel', values_to = 'valor')
invest_funcao_tipo_funcao_bim_t            = pivot_longer(data = invest_funcao_tipo_funcao_bim_m, cols = !starts_with(c('data', 'tipo_preco', 'funcao')), names_to = 'variavel', values_to = 'valor') |> separate(col = 'variavel', into = c('tipo', 'funcao'), sep = '_')
                                             

# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_siof_tableau', 'Databases/Outputs/Matlab/db_siof_matlab')
formato = c('tableau', 'matlab')
aba = list(
  tableau = c('progr_tipo', 'progr_regiao', 'progr_tipo_regiao', 'progr_tipo_regiao_programa', 'func_tipo', 'func_funcao', 'func_tipo_funcao'),
  matlab = c('progr_tipo_real', 'progr_regiao_real', 'progr_tipo_regiao_real', 'progr_tipo_regiao_programa_real', 'func_tipo_real', 'func_funcao_real', 'func_tipo_funcao_real')
  )
dataframe = list(
  tableau = list(invest_programa_tipo_bim_t, invest_programa_regiao_bim_t,
                 invest_programa_tipo_regiao_bim_t, invest_programa_tipo_regiao_programa_bim_t,
                 invest_funcao_tipo_bim_t, invest_funcao_funcao_bim_t, invest_funcao_tipo_funcao_bim_t),
  matlab = list(invest_programa_tipo_actual_bim_m, invest_programa_regiao_actual_bim_m,
                invest_programa_tipo_regiao_actual_bim_m, invest_programa_tipo_regiao_programa_actual_bim_m,
                invest_funcao_tipo_actual_bim_m, invest_funcao_funcao_actual_bim_m,invest_funcao_tipo_funcao_actual_bim_m)
  )

# --- Armazenamento --- #
for(f in seq_along(formato)){
  wb = createWorkbook(creator = 'Sefaz-CE')
  for(s in seq_along(aba[[f]])){
    addWorksheet(wb = wb, sheetName = aba[[f]][s])
    writeData(wb = wb, sheet = aba[[f]][s], x = as.data.frame(dataframe[[f]][s]), rowNames = FALSE, colNames = TRUE)
  }
  if(formato[f] == 'matlab'){
    addWorksheet(wb = wb, sheetName = 'tempo')
    writeData(wb = wb, sheet = 'tempo', as.numeric(invest_programa_tipo_bim_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}



# =============== #
# === Limpeza === #
# =============== #
rm(list = ls(pattern = '^invest|^database'))
rm(path, wb, f, s, formato, aba, save_path, dataframe, pivotting, process_invest, bim_transform)