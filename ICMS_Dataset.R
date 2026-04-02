# ==================== #
# === ICMS DATASET === #
# ==================== #

# --- Script by Paulo Icaro --- #



# ================= #
# === Libraries === #
# ================= #
#source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Frequency_Transforming.R')   # Package already loaded
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
icms_macro = 
  database_icms %>%
  filter(periodo > '2014/12') %>%
  group_by(periodo) %>%
  summarise(icms = sum(valor)) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  select(data, icms)

# --- Bimonthly Series --- #
icms_macro_bimonthly = cumulative_transform('soma', 'bimestral', icms_macro)



# ------------------- #
# --- ICMS Region --- #
# ------------------- #

# --- Grouping Results --- #
icms_region = 
  database_icms %>%
  filter(periodo > '2014/12') %>%
  group_by(periodo, regiao) %>%
  summarise(icms = sum(valor)) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  ungroup() %>%
  select(data, regiao, icms) %>%
  pivot_wider(names_from = regiao, values_from = icms)

# --- Bimonthly Series --- #
icms_region_bimonthly = cumulative_transform('soma', 'bimestral', icms_region)



# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb, sheetName = 'icms_macro')
addWorksheet(wb = wb, sheetName = 'icms_regiao')
writeData(wb = wb, sheet = 'tempo', x = icms_macro_bimonthly %>% select(data), rowNames = FALSE)
writeData(wb = wb, sheet = 'icms_macro', x = icms_macro_bimonthly %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'icms_regiao', x = icms_region_bimonthly %>% select(-data), rowNames = FALSE)
saveWorkbook(wb = wb, file = 'Databases/Outputs/db_icms.xlsx', overwrite = TRUE)