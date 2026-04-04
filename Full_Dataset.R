# ==================== #
# === FULL DATASET === #
# ==================== #

# --- Script by Paulo Icaro --- #


# ====================================== #
# === Source Data Processing Scripts === #
# ====================================== #
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Bacen_Dataset.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Ipeadata_Dataset.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Siof_Dataset.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Employment_Dataset.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/ICMS_Dataset.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Siconfi_Dataset.R')
library(dplyr)



# ======================== #
# === Joining Datasets === #
# ======================== #

# ---------------------- #
# --- Macro Database --- #
# ---------------------- #
macro_dataset_nominal = 
  bacen_dataset_bimonthly %>%
  left_join(y = ipeadata_dataset_bimonthly, by = 'data') %>%
  left_join(y = invest_macro_bimonthly, by = 'data') %>%
  left_join(y = invest_custeio_bimonthly, by = 'data') %>%
  left_join(y = employments_macro_bimonthly, by = 'data') %>%
  left_join(y = icms_macro_bimonthly, by = 'data') %>%
  left_join(y = siconfi_macro_bimonthly, by = 'data')
macro_dataset_nominal = rename_with(macro_dataset_nominal, tolower)   # Renaming columns
macro_dataset_real = macro_dataset_nominal


# ------------------------- #
# --- Regional Database --- #
# ------------------------- #
regional_dataset_nominal =
  invest_region_type_bimonthly %>%
  left_join(y = ipeadata_dataset_bimonthly %>% select(data, ipca), by = 'data') %>%
  left_join(y = employments_region_bimonthly, by = 'data') %>%
  left_join(y = icms_region_bimonthly, by = 'data')

# ------------------------- #
# --- Function Database --- #
# ------------------------- #
funcao_dataset_nominal = 
  left_join(x = invest_funcao_type_bimonthly, y = ipeadata_dataset_bimonthly %>% select(data, ipca), by = 'data')



# ================================ #
# === Nominal to Actual Values === #
# ================================ #

# ---------------------- #
# --- Macro Database --- #
# ---------------------- #
macro_dataset_real =
  macro_dataset_real %>%
  mutate(across(c(importacao, exportacao, credito_pf, credito_pj, saldo_oper_cred, equip, obras, total, custeio, icms, div_cons, res_prev, res_prim, res_prim_prev, inv_mun), ~ .x/ipca * 100))


# ------------------------- #
# --- Regional Database --- #
# ------------------------- #
regional_dataset_real =
  regional_dataset_nominal %>%
  mutate(across(-c(ipca, data, ends_with('_empregos')), ~ .x/ipca*100))
  

# ------------------------- #
# --- Function Database --- #
# ------------------------- #
funcao_dataset_real =
  funcao_dataset_nominal %>%
  mutate(across(-c(ipca, data), ~ .x/ipca*100))



# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb , sheetName = 'macro_dataset_real')
addWorksheet(wb = wb , sheetName = 'regional_dataset_real')
addWorksheet(wb = wb , sheetName = 'funcao_dataset_real')
addWorksheet(wb = wb , sheetName = 'macro_dataset_nominal')
addWorksheet(wb = wb , sheetName = 'regional_dataset_nominal')
addWorksheet(wb = wb , sheetName = 'funcao_dataset_nominal')

writeData(wb = wb, sheet = 'tempo', x = macro_dataset_nominal[c(1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'macro_dataset_real', x = macro_dataset_real %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'macro_dataset_nominal', x = macro_dataset_nominal %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'regional_dataset_real', x = regional_dataset_real %>% select(-data, - ipca), rowNames = FALSE)
writeData(wb = wb, sheet = 'regional_dataset_nominal', x = regional_dataset_nominal %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'funcao_dataset_real', x = funcao_dataset_real %>% select(-data, - ipca), rowNames = FALSE)
writeData(wb = wb, sheet = 'funcao_dataset_nominal', x = funcao_dataset_nominal %>% select(-data), rowNames = FALSE)

saveWorkbook(wb = wb, file = 'Databases/Outputs/db_full_dataset.xlsx', overwrite = TRUE)



# ================ #
# === Cleasing === #
# ================ #
patterns = c('^invest', '^bacen', '^ipeadata', 'employments', 'icms')
for(i in seq_along(patterns)){
  rm(list = ls(pattern = patterns[i]))
}
rm(patterns, i)