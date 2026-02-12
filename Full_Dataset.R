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
macro_dataset = left_join(x = bacen_dataset_bimonthly, y = ipeadata_dataset_bimonthly, by = 'data')
macro_dataset = left_join(x = macro_dataset, y = invest_macro_bimonthly, by = 'data')


# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb , sheetName = 'macro')
addWorksheet(wb = wb , sheetName = 'funcao')
addWorksheet(wb = wb , sheetName = 'regional')
writeData(wb = wb, sheet = 'tempo', x = macro_dataset[c(1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'macro', x = macro_dataset[c(-1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'funcao', x = invest_funcao_bimonthly[c(-1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'regional', x = invest_region_bimonthly[c(-1)], rowNames = FALSE)
saveWorkbook(wb = wb, file = 'Databases/Outputs/db_full_dataset.xlsx', overwrite = TRUE)