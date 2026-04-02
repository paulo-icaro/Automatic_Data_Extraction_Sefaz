# ========================== #
# === EMPLOYMENT DATASET === #
# ========================== #

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
database_employment = read_excel(path = paste0(path, 'empregos_ceara_regiao.xlsx'))


# ------------------------- #
# --- Employments Macro --- #
# ------------------------- #

# --- Data Processing --- #
employments_macro =
  database_employment %>%
  group_by(periodo) %>%
  summarize(empregos = sum(estoque_empregos)) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  select(data, empregos)

# --- Bimonthly Series --- #
employments_macro_bimonthly = cumulative_transform('periodo_final', 'bimestral', employments_macro)


# -------------------------- #
# --- Employments Region --- #
# -------------------------- #
employments_region = 
  database_employment %>% 
  group_by(periodo, regiao) %>%
  summarize(empregos = sum(estoque_empregos)) %>%
  mutate(data = as.Date(paste0(gsub('/', '-', periodo), '-01'))) %>%
  ungroup() %>%
  select(data, regiao, empregos) %>%
  pivot_wider(names_from = regiao, values_from = empregos)
  
  
# --- Bimonthly Series --- #
employments_region_bimonthly = cumulative_transform('periodo_final', 'bimestral', employments_region)
colnames(employments_region_bimonthly)[-1] = paste0(colnames(employments_region_bimonthly)[-1], '_empregos')


# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb, sheetName = 'empregos_macro')
addWorksheet(wb = wb, sheetName = 'empregos_regiao')
writeData(wb = wb, sheet = 'tempo', x = employments_macro_bimonthly %>% select(data), rowNames = FALSE)
writeData(wb = wb, sheet = 'empregos_macro', x = employments_macro_bimonthly %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'empregos_regiao', x = employments_region_bimonthly %>% select(-data), rowNames = FALSE)
saveWorkbook(wb = wb, file = 'Databases/Outputs/db_empregos.xlsx', overwrite = TRUE)



# ================= #
# === Cleansing === #
# ================= #
rm(database_employment, employments_macro, employments_region)