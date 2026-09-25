# ========================== #
# === EMPLOYMENT DATASET === #
# ========================== #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
source('https://raw.githubusercontent.com/paulo-icaro/Variables_Frequency_Transforming/refs/heads/main/variables_frequency_transforming.R')
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)



# ============================== #
# === Processamento de Dados === #
# ============================== #

# --- Path Auxiliar --- #
path = 'Databases/Inputs/'


# ------------------------ #
# --- Empregos - Macro --- #
# ------------------------ #

# --- Processamento --- #
employments_macro_m =
  read_excel(path = paste0(path, 'empregos_ceara_macro.xlsx')) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  select(data, estoque_empregos, salario_medio)

# --- Transformando Frequência dos Dados --- #
employments_amount_macro_bimonthly = cumulative_transform('periodo_final', 'bimestral', employments_macro_m[c(1,2)])
avg_wage_macro_bimonthly = cumulative_transform('media', 'bimestral', employments_macro_m[c(1,3)])

# --- Series Bimestrais --- #
employments_macro_bimonthly_m = left_join(x = employments_amount_macro_bimonthly, avg_wage_macro_bimonthly, by = 'data')


# ------------------------- #
# --- Empregos - Regiao --- #
# ------------------------- #

# --- Processamento --- #
employments_region = 
  read_excel(path = paste0(path, 'empregos_ceara_regiao.xlsx')) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  group_by(data, regiao) %>%
  select(data, regiao, estoque_empregos, salario_medio)
  
  
# --- Transformando Frequência dos Dados --- #
employments_amount_region_bimonthly = cumulative_transform('periodo_final', 'bimestral', employments_region[c(1,2,3)])
avg_wage_region_bimonthly = cumulative_transform('media', 'bimestral', employments_region[c(1,2,4)], 'regiao')

# --- Series Bimestrais --- #
employments_region_bimonthly = left_join(x = employments_amount_region_bimonthly, avg_wage_region_bimonthly, by = c('data', 'regiao'))



# ================================ #
# === Verticalizacao dos Dados === #
# ================================ #
employments_macro_t = pivot_longer(data = employments_macro_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')
employments_macro_bimonthly_t = pivot_longer(data = employments_macro_bimonthly_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')
employments_region_t = pivot_longer(data = employments_region, cols = !starts_with(c('data', 'regiao')), names_to = 'variavel', values_to = 'valor')
employments_region_bimonthly_t = pivot_longer(data = employments_region_bimonthly, cols = !starts_with(c('data', 'regiao')), names_to = 'variavel', values_to = 'valor')

# ============================== #
# === Matricizacao dos Dados === #
# ============================== #
employments_region_m = pivot_wider(data = employments_region_t, names_from = c('regiao', 'variavel'), values_from = 'valor')
employments_region_bimonthly_m = pivot_wider(data = employments_region_bimonthly_t, names_from = c('regiao', 'variavel'), values_from = 'valor')



# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_empregos_tableau', 'Databases/Outputs/Matlab/db_empregos_matlab')
formato = c('tableau', 'matlab')
aba = c('empregos_macro_bimestre', 'empregos_regiao_bimestre', 'empregos_macro', 'empregos_regiao')
dataframe = list(
  tableau = list(employments_macro_bimonthly_t, employments_region_bimonthly_t, employments_macro_t, employments_region_t),
  matlab = list(employments_macro_bimonthly_m, employments_region_bimonthly_m, employments_macro_m, employments_region_m)
)

# --- Armazenamento --- #
for(f in seq_along(formato)){
  wb = createWorkbook(creator = 'Sefaz-CE')
  for(s in seq_along(aba)){
    addWorksheet(wb = wb, sheetName = aba[s])
    writeData(wb = wb, sheet = aba[s], x = as.data.frame(dataframe[[f]][s]), rowNames = FALSE, colNames = TRUE)
  }
  if(formato[f] == 'matlab'){
    addWorksheet(wb = wb, sheetName = 'tempo')
    writeData(wb = wb, sheet = 'tempo', as.numeric(employments_region_bimonthly_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}


# wb = createWorkbook(creator = 'Sefaz-CE')
# addWorksheet(wb = wb, sheetName = 'tempo')
# addWorksheet(wb = wb, sheetName = 'empregos_macro')
# addWorksheet(wb = wb, sheetName = 'empregos_regiao')
# #writeData(wb = wb, sheet = 'tempo', x =  employments_macro_bimonthly %>% select(data), rowNames = FALSE)
# writeData(wb = wb, sheet = 'tempo', x =  as.numeric(employments_macro_bimonthly$data) + 25569, rowNames = FALSE)
# writeData(wb = wb, sheet = 'empregos_macro', x = employments_macro_bimonthly, rowNames = FALSE)
# writeData(wb = wb, sheet = 'empregos_regiao', x = employments_region_bimonthly, rowNames = FALSE)
# saveWorkbook(wb = wb, file = 'Databases/Outputs/db_empregos.xlsx', overwrite = TRUE)



# =============== #
# === Limpeza === #
# =============== #

rm(list = ls(pattern = '^employment|^avg_wage'))
rm(wb, path, f, s, formato, aba, save_path, dataframe)