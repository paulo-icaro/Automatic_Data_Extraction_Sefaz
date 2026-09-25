# ==================== #
# === ICMS DATASET === #
# ==================== #

# --- Script by Paulo Icaro --- #



# ================= #
# === Libraries === #
# ================= #
source('https://raw.githubusercontent.com/paulo-icaro/Variables_Frequency_Transforming/refs/heads/main/variables_frequency_transforming.R')
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)



# ======================= #
# === Data Processing === #
# ======================= #

# --- Previous Info --- #
path = 'Databases/Inputs/'

# --- Main Databases --- #
database_icms = read_excel(path = paste0(path, 'icms_ceara_regiao_cnae.xlsx'))



# ------------------ #
# --- ICMS Macro --- #
# ------------------ #

# --- Grouping Results --- #
icms_macro_m = 
  database_icms %>%
  filter(periodo > '2014/12') %>%
  group_by(periodo) %>%
  summarise(icms = sum(valor)) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  select(data, icms)

# --- Bimonthly Series --- #
icms_macro_bimonthly_m = cumulative_transform('soma', 'bimestral', icms_macro_m)



# ------------------- #
# --- ICMS Region --- #
# ------------------- #

# --- Grouping Results --- #
icms_region_m = 
  database_icms %>%
  filter(periodo > '2014/12') %>%
  group_by(periodo, regiao) %>%
  summarise(icms = sum(valor)) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  ungroup() %>%
  select(data, regiao, icms) %>%
  pivot_wider(names_from = regiao, values_from = icms)

# --- Bimonthly Series --- #
icms_region_bimonthly_m = cumulative_transform('soma', 'bimestral', icms_region_m)



# ================================ #
# === Verticalizacao dos Dados === #
# ================================ #
icms_macro_t = pivot_longer(data = icms_macro_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')
icms_macro_bimonthly_t = pivot_longer(data = icms_macro_bimonthly_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')
icms_region_t = pivot_longer(data = icms_region_m, cols = !starts_with(c('data', 'region')), names_to = 'variavel', values_to = 'valor')                                      
icms_region_bimonthly_t = pivot_longer(data = icms_region_bimonthly_m, cols = !starts_with(c('data', 'region')), names_to = 'variavel', values_to = 'valor')



# ======================= #
# === Storing Results === #
# ======================= #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_icms_tableau', 'Databases/Outputs/Matlab/db_icms_matlab')
formato = c('tableau', 'matlab')
aba = c('icms_macro_bimestre', 'icms_regiao_bimestre', 'icms_macro', 'icms_regiao')
dataframe = list(
  tableau = list(icms_macro_bimonthly_t, icms_region_bimonthly_t, icms_macro_t, icms_region_t),
  matlab = list(icms_macro_bimonthly_m, icms_region_bimonthly_m, icms_macro_m, icms_region_m)
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
    writeData(wb = wb, sheet = 'tempo', as.numeric(icms_macro_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}



# ================ #
# === Cleasing === #
# ================ #
rm(list = ls(pattern = '^icms'))
rm(wb, database_icms, path, f, s, formato, aba, save_path, dataframe)