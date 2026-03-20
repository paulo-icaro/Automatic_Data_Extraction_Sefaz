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
library(dplyr)



# ======================== #
# === Joining Datasets === #
# ======================== #

# ---------------------- #
# --- Macro Database --- #
# ---------------------- #
macro_dataset_nominal = left_join(x = bacen_dataset_bimonthly, y = ipeadata_dataset_bimonthly, by = 'data')
macro_dataset_nominal = left_join(x = macro_dataset_nominal, y = invest_macro_bimonthly, by = 'data')
macro_dataset_current = macro_dataset_nominal

# ------------------------- #
# --- Regional Database --- #
# ------------------------- #
regional_dataset_nominal = left_join(x = invest_region_type_bimonthly, y = ipeadata_dataset_bimonthly %>% select(data, ipca), by = 'data')

# ------------------------- #
# --- Function Database --- #
# ------------------------- #
funcao_dataset_nominal = left_join(x = invest_funcao_type_bimonthly, y = ipeadata_dataset_bimonthly %>% select(data, ipca), by = 'data')



# ================================ #
# === Nominal do Current Value === #
# ================================ #

# ---------------------- #
# --- Macro Database --- #
# ---------------------- #
macro_dataset_current =
  macro_dataset_current %>%
  mutate(across(c(importacao, exportacao, credito_pf, credito_pj, saldo_oper_cred, EQUIP, OBRAS, TOTAL), ~ .x/ipca * 100))
  

macro_dataset_current$importacao = macro_dataset_nominal$importacao/macro_dataset_nominal$ipca * 100
macro_dataset_current$exportacao = macro_dataset_nominal$exportacao/macro_dataset_nominal$ipca * 100
macro_dataset_current$credito_pf = macro_dataset_nominal$credito_pf/macro_dataset_nominal$ipca * 100
macro_dataset_current$credito_pj = macro_dataset_nominal$credito_pj/macro_dataset_nominal$ipca * 100
macro_dataset_current$saldo_oper_cred = macro_dataset_nominal$saldo_oper_cred/macro_dataset_nominal$ipca * 100
macro_dataset_current$EQUIP = macro_dataset_nominal$EQUIP/macro_dataset_nominal$ipca*100
macro_dataset_current$OBRAS = macro_dataset_nominal$OBRAS/macro_dataset_nominal$ipca*100
macro_dataset_current$TOTAL = macro_dataset_nominal$TOTAL/macro_dataset_nominal$ipca*100

# ------------------------- #
# --- Regional Database --- #
# ------------------------- #
regional_dataset_nominal =
  regional_dataset_nominal %>%
  mutate(across(-c(ipca, data), ~ .x/ipca*100))
  

# ------------------------- #
# --- Function Database --- #
# ------------------------- #
funcao_dataset_nominal =
  funcao_dataset_nominal %>%
  mutate(across(-c(ipca, data), ~ .x/ipca*100))



# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb , sheetName = 'macro_current')
addWorksheet(wb = wb , sheetName = 'macro_nominal')
addWorksheet(wb = wb , sheetName = 'funcao')
addWorksheet(wb = wb , sheetName = 'regional')
writeData(wb = wb, sheet = 'tempo', x = macro_dataset_nominal[c(1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'macro_current', x = macro_dataset_current[c(-1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'macro_nominal', x = macro_dataset_nominal[c(-1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'funcao', x = invest_funcao_bimonthly[c(-1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'regional', x = invest_region_bimonthly[c(-1)], rowNames = FALSE)
saveWorkbook(wb = wb, file = 'Databases/Outputs/db_full_dataset.xlsx', overwrite = TRUE)
