# ===================== #
# === BACEN DATASET === #
# ===================== #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
source('https://raw.githubusercontent.com/paulo-icaro/Bacen_API/main/Bacen_Query.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Frequency_Transforming.R')
library(openxlsx)       # Armazenar arquivos em formato excel

# Obs: Importação manual, caso o link direto não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_API.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_URL.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_Query.R')




# ======================= #
# === Data Extraction === #
# ======================= #

# --- Previous Info --- #
cod_bacen_series = c('13010', '13093', '13094', '14007', '14034', '25390', '4189', '3697', '433', '14061')
name_bacen_series = c('variacao_emprego', 'exportacao', 'importacao', 'credito_pf', 'credito_pj', 'ibcrce', 'selic_acum_anual', 'tx_cambio_mp', 'inflação_ipca', 'saldo_oper_cred')
start_date = '01/01/2015'
end_date = '31/12/2025'
#end_date = format(Sys.Date(), '%d/%m/%Y')

# --- Extraction --- #
bacen_dataset = bacen_query(cod_bacen_series, name_bacen_series, start_date, end_date)

# --- Date Adjustment --- #
bacen_dataset = bacen_dataset %>% mutate(data = as.Date(data, tryFormats = c('%d/%m/%Y')))
bacen_dataset[c(-1)] = lapply(X = bacen_dataset[c(-1)], FUN = as.numeric)



# =================================== #
# === Transforming Data Frequency === #
# =================================== #
bacen_dataset_bimonthly_sum = cumulative_transform('soma', 'bimestral', bacen_dataset[c(1:4)], TRUE)
bacen_dataset_bimonthly_end = cumulative_transform('periodo_final', 'bimestral', bacen_dataset[c(1, 5:8, 11)], TRUE)
bacen_dataset_bimonthly_cum = cumulative_transform('tx_acumulada', 'bimestral', bacen_dataset[c(1, 10)], TRUE)
bacen_dataset_bimonthly_med = cumulative_transform('media', 'bimestral', bacen_dataset[c(1, 9)], TRUE)
bacen_dataset_bimonthly = left_join(x = bacen_dataset_bimonthly_sum, y = bacen_dataset_bimonthly_end, by = 'data')
bacen_dataset_bimonthly = left_join(x = bacen_dataset_bimonthly, y = bacen_dataset_bimonthly_cum, by = 'data')
bacen_dataset_bimonthly = left_join(x = bacen_dataset_bimonthly, y = bacen_dataset_bimonthly_med, by = 'data')



# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb, sheetName = 'db_banco_central_bimonthly')
addWorksheet(wb = wb, sheetName = 'db_banco_central_original')
writeData(wb = wb, sheet = 'tempo', x = bacen_dataset_bimonthly[c(1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'db_banco_central_bimonthly', x = bacen_dataset_bimonthly[c(-1)], rowNames = FALSE)
writeData(wb = wb, sheet = 'db_banco_central_original', x = bacen_dataset, rowNames = FALSE)
saveWorkbook(wb = wb, file = 'db_banco_central.xlsx', overwrite = TRUE)



# ================ #  
# === Cleasing === #
# ================ #
rm(cod_bacen_series, name_bacen_series, start_date, end_date, bacen_dataset_bimonthly_sum,
   bacen_dataset_bimonthly_end, bacen_dataset_bimonthly_cum, bacen_dataset_bimonthly_med, bacen_dataset, wb)
