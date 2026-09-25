# ==================== #
# === SIOF DATASET === #
# ==================== #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
source('https://raw.githubusercontent.com/paulo-icaro/Variables_Frequency_Transforming/refs/heads/main/variables_frequency_transforming.R')    # Package already loaded
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(lubridate)

# --- Path Auxiliar --- #
path = 'Databases/Inputs/'

# --- Bases Iniciais --- #
database_invest_program_regiao = read_excel(path = paste0(path, 'investimentos_siof_ceara_programa_regiao.xlsx'))
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

# ---------------------- #
# --- Processamentos --- #
# ---------------------- #
invest_program_tipo_macro_t          = process_invest(df = database_invest_program_regiao, group_vars = c('ano', 'mes', 'tipo'), min_year =  2016)
invest_program_tipo_program_t        = process_invest(df = database_invest_program_regiao, group_vars = c('ano', 'mes', 'tipo', 'programa'), min_year = 2016)
invest_program_tipo_regiao_t         = process_invest(df = database_invest_program_regiao, group_vars = c('ano', 'mes', 'tipo', 'regiao'), min_year = 2016)
invest_program_tipo_regiao_program_t = process_invest(df = database_invest_program_regiao, group_vars = c('ano', 'mes', 'tipo', 'regiao', 'programa'), min_year = 2016)
invest_funcao_tipo_macro_t           = process_invest(df = database_invest_funcao, group_vars = c('ano', 'mes', 'tipo'), min_year =  2016)
invest_funcao_tipo_funcao_t          = process_invest(df = database_invest_funcao, group_vars = c('ano', 'mes', 'tipo', 'funcao'), min_year =  2016)

# ----------------------------------- #
# --- Transformacao de Frequencia --- #
# ----------------------------------- #
invest_program_tipo_macro_bim_t          = bim_transform(invest_program_tipo_macro_t[,3:5], c('tipo'))
invest_program_tipo_program_bim_t        = bim_transform(invest_program_tipo_program_t[,3:6], c('tipo', 'programa'))
invest_program_tipo_regiao_bim_t         = bim_transform(invest_program_tipo_regiao_t[,3:6], c('regiao', 'tipo'))
invest_program_tipo_regiao_program_bim_t = bim_transform(invest_program_tipo_regiao_program_t[,3:7], c('regiao','programa','tipo'))
invest_funcao_tipo_macro_bim_t           = bim_transform(invest_funcao_tipo_macro_t[,3:5], c('tipo'))
invest_funcao_tipo_funcao_bim_t          = bim_transform(invest_funcao_tipo_funcao_t[,3:6], c('tipo','funcao'))

# ----------------------------- #
# --- Matricizando os Dados --- #
# ----------------------------- #
invest_program_tipo_macro_bim_m          = pivotting(invest_program_tipo_macro_bim_t)
invest_program_tipo_program_bim_m        = pivotting(invest_program_tipo_program_bim_t)
invest_program_tipo_regiao_bim_m         = pivotting(invest_program_tipo_regiao_bim_t)
invest_program_tipo_regiao_program_bim_m = pivotting(invest_program_tipo_regiao_program_bim_t)
invest_funcao_tipo_macro_bim_m           = pivotting(invest_funcao_tipo_macro_bim_t)
invest_funcao_tipo_funcao_bim_m          = pivotting(invest_funcao_tipo_funcao_bim_t)

                                             

# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_siof_tableau', 'Databases/Outputs/Matlab/db_siof_matlab')
formato = c('tableau', 'matlab')
aba = c('progr_tipo_macro', 'progr_tipo_programa', 'progr_tipo_regiao', 'progr_tipo_regiao_programa', 'func_tipo_macro', 'func_tipo_funcao')
dataframe = list(
  tableau = list(invest_program_tipo_macro_bim_t, invest_program_tipo_program_bim_t, invest_program_tipo_regiao_bim_t,
                   invest_program_tipo_regiao_program_bim_t, invest_funcao_tipo_macro_bim_t, invest_funcao_tipo_funcao_bim_t),
  matlab = list(invest_program_tipo_macro_bim_m, invest_program_tipo_program_bim_m, invest_program_tipo_regiao_bim_m,
                  invest_program_tipo_regiao_program_bim_m, invest_funcao_tipo_macro_bim_m, invest_funcao_tipo_funcao_bim_m))

# --- Armazenamento --- #
for(f in seq_along(formato)){
  wb = createWorkbook(creator = 'Sefaz-CE')
  for(s in seq_along(aba)){
    addWorksheet(wb = wb, sheetName = aba[s])
    writeData(wb = wb, sheet = aba[s], x = as.data.frame(dataframe[[f]][s]), rowNames = FALSE, colNames = TRUE)
  }
  if(formato[f] == 'matlab'){
    addWorksheet(wb = wb, sheetName = 'tempo')
    writeData(wb = wb, sheet = 'tempo', as.numeric(invest_program_tipo_macro_bim_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}



# =============== #
# === Limpeza === #
# =============== #
rm(list = ls(pattern = '^invest|^database'))
rm(path, wb, f, s, formato, aba, save_path, dataframe, pivotting, process_invest, bim_transform)