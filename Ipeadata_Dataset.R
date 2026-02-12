# ======================== #
# === IPEADATA DATASET === #
# ======================== #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
source('https://raw.githubusercontent.com/paulo-icaro/Ipeadata_API/refs/heads/main/Ipeadata_Query.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Frequency_Transforming.R')
library(openxlsx)       # Armazenar arquivos em formato excel


# Obs: Importação manual, caso o link direto não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/Ipeadata_API.R')
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/Ipeadata_URL.R')
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/Ipeadata_Query.R')


# ======================= #
# === Data Extraction === #
# ======================= #

# --- Previous Info --- #
cod_ipeadata_series = c('PRECOS12_IPCA12', 'DIMAC_CF_INVBR_TOT12')
name_ipeadata_series = c('ipca', 'inv_bruto_total')
periodo = as.character(2014:2025)

# --- Extraction --- #
ipeadata_dataset = ipeadata_query(cod_ipeadata_series, name_ipeadata_series, periodo)
ipeadata_dataset = ipeadata_dataset %>% mutate(data = as.Date(data))
ipeadata_dataset$ipca = (ipeadata_dataset$ipca/last(ipeadata_dataset$ipca))*100
ipeadata_dataset = ipeadata_dataset %>% mutate('ipca_%' = ipca/lag(ipca) - 1) 
ipeadata_dataset = ipeadata_dataset %>% filter(substr(data, 1, 4) != '2014')



# ================================== #
# == Transforming Data Frequency === #
# ================================== #
ipeadata_dataset_bimonthly_sum = cumulative_transform('soma', 'bimestral', ipeadata_dataset[c(1,3)])
ipeadata_dataset_bimonthly_cum = cumulative_transform('acumulado', 'bimestral', ipeadata_dataset[c(1,4)])
ipeadata_dataset_bimonthly_end = cumulative_transform('periodo_final', 'bimestral', ipeadata_dataset[c(1,2)])
ipeadata_dataset_bimonthly = left_join(x = ipeadata_dataset_bimonthly_sum, y = ipeadata_dataset_bimonthly_cum, by = 'data')
ipeadata_dataset_bimonthly = left_join(x = ipeadata_dataset_bimonthly, y = ipeadata_dataset_bimonthly_end, by = 'data')



# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb, sheetName = 'db_ipeadata_bimonthly')
addWorksheet(wb = wb, sheetName = 'db_ipeadata_original')
writeData(wb = wb, sheet = 'tempo', x = ipeadata_dataset_bimonthly[c(1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'db_ipeadata_bimonthly', x = ipeadata_dataset_bimonthly[c(-1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'db_ipeadata_original', x = ipeadata_dataset, rowNames = FALSE)
saveWorkbook(wb = wb, file = 'Databases/Outputs/db_ipeadata.xlsx', overwrite = TRUE)


# =============== #
# === Limpeza === #
# =============== #
rm(cod_ipeadata_series, name_ipeadata_series, periodo, ipeadata_dataset_bimonthly_cum, ipeadata_dataset_bimonthly_sum,
   ipeadata_dataset, cod_ipeadata_series, name_ipeadata_series, path, periodo, wb)