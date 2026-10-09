# ==================== #
# === ICMS DATASET === #
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


# --- Path Auxiliar --- #
path = 'Databases/Inputs/'

# --- Bases Iniciais --- #
database_icms = read_excel(path = paste0(path, 'icms_ceara_regiao_cnae.xlsx'))



# ========================= #
# === Funções Auxliares === #
# ========================= #

# -------------------------------------- #
# --- Processamento de Dados - ICMS ---- #
# -------------------------------------- #
process_icms = function(df, group_var, min_period = NULL){
  df |> 
    filter(periodo > min_period) %>%
    group_by(across(all_of(group_var))) %>%
    summarise(icms = sum(valor)) %>%
    mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
    ungroup()
}


# ----------------------------------- #
# --- Transformação de Frequência --- #
# ----------------------------------- #
bim_transform = function(df, transf_type, group_var = NULL){
  cumulative_transform(
    transform_type = transf_type,
    frequency = 'bimestral',
    dataset = df,
    groupby_variables = group_var)
}


# ------------------------------ #
# --- Matricizacao dos Dados --- #
# ------------------------------ #
pivotting = function(df, var_names, values, preffix = ''){
  pivot_wider(df, names_from = var_names, values_from = values, names_prefix = preffix)
}



# ============================== #
# === Processamento de Dados === #
# ============================== #


# ----------------------------------- #
# --- Transformação de Frequência --- #
# ----------------------------------- #
icms_macro_bim = bim_transform(df = process_icms(df = database_icms, group_var = 'periodo', min_period = '2014/12')[c(2,3)], transf_type = 'soma')
icms_region_bim = bim_transform(df = process_icms(df = database_icms, group_var = c('periodo', 'regiao'), min_period = '2014/12')[c(2:4)], transf_type = 'soma', group_var = 'regiao')

# ----------------------------- #
# --- Matricizando os Dados --- #
# ----------------------------- #
icms_macro_curr_bim_m = icms_macro_bim
icms_region_curr_bim_m = pivotting(df = icms_region_bim, var_names = 'regiao', values = 'icms')

# ----------------------------------------- #
# --- Atualizacao de Valores Monetarios --- #
# ----------------------------------------- #
icms_macro_actual_bim_m = value_updating(series = icms_macro_curr_bim_m, variables = colnames(select(icms_macro_curr_bim_m, -data)), base_period = '2025-12', start = '2015', , show_price_index = FALSE)
icms_region_actual_bim_m = value_updating(series = icms_region_curr_bim_m, variables = colnames(select(icms_region_curr_bim_m, -data)), base_period = '2025-12', start = '2015', , show_price_index = FALSE)

# --------------------------------------------- #
# --- Base Final - Valores Nominais e Reais --- #
# --------------------------------------------- #
icms_macro_bim_m = rbind(icms_macro_curr_bim_m |> mutate(tipo_preco = 'nominal'), icms_macro_actual_bim_m |> mutate(tipo_preco = 'real'))
icms_region_bim_m = rbind(icms_region_curr_bim_m |> mutate(tipo_preco = 'nominal'), icms_region_actual_bim_m |> mutate(tipo_preco = 'real'))

# ------------------------------- #
# --- Verticalizando os Dados --- #
# ------------------------------- #
icms_macro_bim_t = pivot_longer(data = icms_macro_bim_m, cols = !starts_with(c('data', 'tipo_preco')), names_to = 'variavel', values_to = 'icms')
icms_region_bim_t = pivot_longer(data = icms_region_bim_m, cols = !starts_with(c('data', 'tipo_preco', 'regiao')), names_to = 'variavel', values_to = 'icms')



# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_icms_tableau', 'Databases/Outputs/Matlab/db_icms_matlab')
formato = c('tableau', 'matlab')
aba = list(
  tableau = c('icms_macro_bimestre', 'icms_regiao_bimestre') ,
  matlab = c('icms_macro_bimestre_real', 'icms_regiao_bimestre_real')
)
dataframe = list(
  tableau = list(icms_macro_bim_t, icms_region_bim_t),
  matlab = list(icms_macro_actual_bim_m, icms_region_actual_bim_m)
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
    writeData(wb = wb, sheet = 'tempo', as.numeric(icms_macro_bim_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}



# ================ #
# === Cleasing === #
# ================ #
rm(list = ls(pattern = '^icms'))
rm(wb, database_icms, path, f, s, formato, aba, save_path, dataframe)