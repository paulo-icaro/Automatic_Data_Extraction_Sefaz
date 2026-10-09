# ========================== #
# === EMPLOYMENT DATASET === #
# ========================== #

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
employments_macro = read_excel(path = paste0(path, 'empregos_ceara_macro.xlsx'))
employments_region = read_excel(path = paste0(path, 'empregos_ceara_regiao.xlsx'))



# ========================= #
# === Funções Auxliares === #
# ========================= #

# ----------------------------------------------- #
# --- Processamento de Dados - Investimentos ---- #
# ----------------------------------------------- #
process_employments = function(df, group_var){
  df |> 
    mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
    select(all_of(group_var))
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
pivotting = function(df, var_names, values, preffix){
  pivot_wider(df, names_from = var_names, values_from = values, names_prefix = preffix)
}



# ============================== #
# === Processamento de Dados === #
# ============================== #

# ----------------------------------- #
# --- Transformação de Frequência --- #
# ----------------------------------- #
employments_amount_macro_bim = bim_transform(df = process_employments(df = employments_macro, group_var = c('data', 'estoque_empregos')), transf_type = 'periodo_final')
employments_amount_region_bim = bim_transform(df = process_employments(df = employments_region, group_var = c('data', 'regiao', 'estoque_empregos'))[c(1,2,3)], transf_type = 'periodo_final', group_var = 'regiao') 
avg_wage_macro_bim = bim_transform(df = process_employments(df = employments_macro, group_var = c('data', 'salario_medio')), transf_type = 'media')
avg_wage_region_bim = bim_transform(df = process_employments(df = employments_region, group_var = c('data', 'regiao', 'salario_medio')), transf_type = 'media', group_var = 'regiao')


# ----------------------------- #
# --- Matricizando os Dados --- #
# ----------------------------- #
employments_amount_macro_bim_m = employments_amount_macro_bim
employments_amount_region_bim_m = pivotting(employments_amount_region_bim, 'regiao', 'estoque_empregos', 'empregos_')
avg_wage_macro_bim_m = avg_wage_macro_bim
avg_wage_region_bim_m = pivotting(avg_wage_region_bim, 'regiao', 'salario_medio', 's.medio_')


# ------------------------- #
# --- Reunido Variaveis --- #
# ------------------------- #
employments_macro_curr_bim_m = left_join(x = employments_amount_macro_bim_m, y = avg_wage_macro_bim_m, by = 'data')
employments_region_curr_bim_m = left_join(x = employments_amount_region_bim_m, y = avg_wage_region_bim_m, by = 'data')


# ----------------------------------------- #
# --- Atualização de Valores Monetarios --- #
# ----------------------------------------- #
employments_macro_actual_bim_m = value_updating(employments_macro_curr_bim_m, colnames(select(employments_macro_curr_bim_m, !starts_with(c('data', 'estoque_empregos')))), base_period = '2025-12', start = '2015', show_price_index = FALSE)
employments_region_actual_bim_m = value_updating(employments_region_curr_bim_m, colnames(select(employments_region_curr_bim_m, !starts_with(c('data', 'empregos')))), base_period = '2025-12', start = '2015', show_price_index = FALSE)


# --------------------------------------------- #
# --- Base Final - Valores Nominais e Reais --- #
# --------------------------------------------- #
employments_macro_bim_m = rbind(employments_macro_curr_bim_m |> mutate(tipo_preco = 'nominal'), employments_macro_actual_bim_m |> mutate(tipo_preco = 'real'))
employments_region_bim_m = rbind(employments_region_curr_bim_m |> mutate(tipo_preco = 'nominal'), employments_region_actual_bim_m |> mutate(tipo_preco = 'real'))


# ------------------------------- #
# --- Verticalizando os Dados --- #
# ------------------------------- #
employments_macro_bim_t = pivot_longer(data = employments_macro_bim_m, cols = !starts_with((c('data', 'tipo_preco'))), names_to = 'variavel', values_to = 'valor')
employments_region_bim_t = pivot_longer(data = employments_region_bim_m, cols = !starts_with(c('data', 'tipo_preco', 'regiao')), names_to = 'variavel', values_to = 'valor') |> separate(col = 'variavel', into = c('variavel', 'regiao'), sep = '_')



# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_empregos_tableau', 'Databases/Outputs/Matlab/db_empregos_matlab')
formato = c('tableau', 'matlab')
aba = list(
  tableau = c('empregos_macro_bimestre', 'empregos_regiao_bimestre'),
  matlab = c('empregos_macro_bimestre_real', 'empregos_regiao_bimestre_real')
  )
dataframe = list(
  tableau = list(employments_macro_bim_t, employments_region_bim_t),
  matlab = list(employments_macro_actual_bim_m, employments_region_actual_bim_m)
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
    writeData(wb = wb, sheet = 'tempo', as.numeric(employments_region_actual_bim_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}



# =============== #
# === Limpeza === #
# =============== #

rm(list = ls(pattern = '^employment|^avg_wage'))
rm(wb, path, f, s, formato, aba, save_path, dataframe)