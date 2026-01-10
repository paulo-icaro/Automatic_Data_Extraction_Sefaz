# ===================== #
# === BACEN DATASET === #
# ===================== #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
source('https://raw.githubusercontent.com/paulo-icaro/Bacen_API/main/Bacen_Query.R')

# Obs: Importação manual, caso o link direto não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_API.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_URL.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_Query.R')



# ======================= #
# === Data Extraction === #
# ======================= #

# --- Previous Info --- #
cod_bacen_series = c('13010', '13093', '13094', '14007', '14034', '25390', '433', '4390', '3696', '3698')
name_bacen_series = c('variacao_emprego', 'exportacao', 'importacao', 'credito_pf', 'credito_pj', 'ibcrce', 'inflação_ipca', 'selic', 'tx_cambio_fp', 'tx_cambio_mp')
start_date = '01/01/2015'
end_date = '30/11/2025'

# --- Extraction --- #
bacen_dataset = bacen_query(cod_bacen_series, name_bacen_series, start_date, end_date)
bacen_dataset = bacen_dataset %>% mutate(data = as.Date(data, tryFormats = c('%d/%m/%Y')))
bacen_dataset[c(-1)] = lapply(X = bacen_dataset[c(-1)], FUN = as.numeric)


# ================ #
# === Cleasing === #
# ================ #
rm(cod_bacen_series, name_bacen_series, start_date, end_date)