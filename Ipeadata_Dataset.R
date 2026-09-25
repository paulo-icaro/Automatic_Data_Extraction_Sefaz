# ======================== #
# === IPEADATA DATASET === #
# ======================== #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
source('https://raw.githubusercontent.com/paulo-icaro/Variables_Frequency_Transforming/refs/heads/main/variables_frequency_transforming.R')    # Pacote já carregado
source('https://raw.githubusercontent.com/paulo-icaro/Ipeadata_API/refs/heads/main/ipeadata_query.R')
library(openxlsx)       # Armazenar arquivos em formato excel


# Obs: Importação manual, caso o link direto não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/ipeadata_api.R')
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/ipeadata_url.R')
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/ipeadata_query.R')


# ========================= #
# === Extracao de Dados === #
# ========================= #

# --- Informacoes Previas --- #
cod_ipeadata_series = c('PRECOS12_IPCA12')#, 'DIMAC_CF_INVBR_TOT')
name_ipeadata_series = c('ipca')#, 'inv_bruto_total')
periodo = as.character(2014:2025)
#periodo = as.character(2014:year(Sys.Date()))


# --- Extração --- #
ipeadata_dataset_m = ipeadata_query(cod_ipeadata_series, name_ipeadata_series, periodo)

# --- Ajuste na Data --- #
ipeadata_dataset_m = ipeadata_dataset_m %>% mutate(data = as.Date(data))

# --- Ajuste na Série de Preços --- #
ipeadata_dataset_m$ipca = (ipeadata_dataset_m$ipca/last(ipeadata_dataset_m$ipca))*100
ipeadata_dataset_m = ipeadata_dataset_m %>% mutate('ipca_%' = (ipca/lag(ipca) - 1)*100)
ipeadata_dataset_m = ipeadata_dataset_m %>% filter(substr(data, 1, 4) != '2014')



# =========================================== #
# == Tranformacao da Frequencia dos Dados === #
# =========================================== #
#ipeadata_dataset_bimonthly_sum = cumulative_transform('soma', 'bimestral', ipeadata_dataset[c(1,3)])
ipeadata_dataset_bimonthly_end_m = cumulative_transform('periodo_final', 'bimestral', ipeadata_dataset_m[c(1,2)])
ipeadata_dataset_bimonthly_cum_m = cumulative_transform('tx_acumulada', 'bimestral', ipeadata_dataset_m[c(1,3)])
ipeadata_dataset_bimonthly_m = left_join(x = ipeadata_dataset_bimonthly_end_m, y = ipeadata_dataset_bimonthly_cum_m, by = 'data')



# =============================== #
# === Verticalizando os Dados === #
# =============================== #
ipeadata_dataset_t = pivot_longer(data = ipeadata_dataset_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')
ipeadata_dataset_bimonthly_t = pivot_longer(data = ipeadata_dataset_bimonthly_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')



# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_ipeadata_tableau', 'Databases/Outputs/Matlab/db_ipeadata_matlab')
formato = c('tableau', 'matlab')
aba = c('ipeadata_bimestre', 'ipeadata_original')
dataframe = list(
  tableau = list(ipeadata_dataset_bimonthly_t, ipeadata_dataset_t),
  matlab = list( ipeadata_dataset_bimonthly_m, ipeadata_dataset_m)
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
    writeData(wb = wb, sheet = 'tempo', as.numeric(ipeadata_dataset_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}



# =============== #
# === Limpeza === #
# =============== #
rm(list = ls(pattern = '^ipeadata'))
rm(wb, name_ipeadata_series, periodo, cod_ipeadata_series, f, s, formato, aba, save_path, dataframe)