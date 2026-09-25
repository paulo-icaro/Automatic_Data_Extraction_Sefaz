# ===================== #
# === BACEN DATASET === #
# ===================== #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #

source('https://raw.githubusercontent.com/paulo-icaro/Variables_Frequency_Transforming/refs/heads/main/variables_frequency_transforming.R')
source('https://raw.githubusercontent.com/paulo-icaro/Bacen_API/main/bacen_query.R')
library(openxlsx)       # Armazenar arquivos em formato excel

# Obs: Importação manual, caso o link direto não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_API.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_URL.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_Query.R')




# ========================= #
# === Extração de Dados === #
# ========================= #

# --- Informações Prévias --- #
cod_bacen_series = c('13010', '13093', '13094', '14007', '14034', '25390', '4189', '3697', '433', '14061')
name_bacen_series = c('variacao_emprego', 'exportacao', 'importacao', 'credito_pf', 'credito_pj', 'ibcrce', 'selic_acum_anual', 'tx_cambio_mp', 'inflação_ipca', 'saldo_oper_cred')
start_date = '01/01/2015'
end_date = '31/12/2025'
#end_date = format(Sys.Date(), '%d/%m/%Y')

# --- Extracao --- #
bacen_dataset_m = bacen_query(cod_bacen_series, name_bacen_series, start_date, end_date)

# --- Ajuste na Data --- #
bacen_dataset_m = bacen_dataset_m %>% mutate(data = as.Date(data, tryFormats = c('%d/%m/%Y')))

# --- Ajuste para tipo numérico
bacen_dataset_m[c(-1)] = lapply(X = bacen_dataset_m[c(-1)], FUN = as.numeric)



# =========================================== #
# == Tranformação da Frequência dos Dados === #
# =========================================== #
bacen_dataset_bimonthly_sum_m = cumulative_transform('soma', 'bimestral', bacen_dataset_m[c(1:4)])
bacen_dataset_bimonthly_end_m = cumulative_transform('periodo_final', 'bimestral', bacen_dataset_m[c(1, 5:8, 11)])
bacen_dataset_bimonthly_cum_m = cumulative_transform('tx_acumulada', 'bimestral', bacen_dataset_m[c(1, 10)])
bacen_dataset_bimonthly_med_m = cumulative_transform('media', 'bimestral', bacen_dataset_m[c(1, 9)])
bacen_dataset_bimonthly_m = left_join(x = bacen_dataset_bimonthly_sum_m, y = bacen_dataset_bimonthly_end_m, by = 'data')
bacen_dataset_bimonthly_m = left_join(x = bacen_dataset_bimonthly_m, y = bacen_dataset_bimonthly_cum_m, by = 'data')
bacen_dataset_bimonthly_m = left_join(x = bacen_dataset_bimonthly_m, y = bacen_dataset_bimonthly_med_m, by = 'data')



# =============================== #
# === Verticalizando os Dados === #
# =============================== #
bacen_dataset_t = pivot_longer(data = bacen_dataset_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')
bacen_dataset_bimonthly_t = pivot_longer(data = bacen_dataset_bimonthly_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')


# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_bacen_tableau', 'Databases/Outputs/Matlab/db_bacen_matlab')
formato = c('tableau', 'matlab')
aba = c('bacen_bimestre', 'bacen_original')
dataframe = list(
  tableau = list(bacen_dataset_bimonthly_t, bacen_dataset_t),
  matlab = list(bacen_dataset_bimonthly_m, bacen_dataset_m)
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
    writeData(wb = wb, sheet = 'tempo', as.numeric(bacen_dataset_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}




# =============== #
# === Limpeza === #
# =============== #
rm(list = ls(pattern = '^bacen'))
rm(cod_bacen_series, name_bacen_series, start_date, end_date, wb, f, s, formato, aba, save_path, dataframe)