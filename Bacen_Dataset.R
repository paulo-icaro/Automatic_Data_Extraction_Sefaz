# ===================== #
# === BACEN DATASET === #
# ===================== #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
source('https://raw.githubusercontent.com/paulo-icaro/Bacen_API/main/Bacen_Query.R')

# Obs: Importação manual, caso o link direto não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_API.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_URL.R')
# source('C://Users/Paulo/Documents/Repositorios/Bacen_API/Bacen_Query.R')



# ===================================== #
# === Extração do Conjunto de Dados === #
# ===================================== #

# --- Informações Prévias --- #
cod_bacen_series = c('13010', '13093', '13094', '14007', '14034', '25390', '433', '4390', '3696', '3698')
name_bacen_series = c('variacao_emprego', 'exportacao', 'importacao', 'credito_pf', 'credito_pj', 'ibcrce', 'inflação_ipca', 'selic', 'tx_cambio_fp', 'tx_cambio_mp')
start_date = '01/01/2015'
end_date = '30/11/2025'

# --- Extração --- #
bacen_dataset = bacen_query(cod_bacen_series, name_bacen_series, start_date, end_date)



# =============== #
# === Limpeza === #
# =============== #
rm(cod_bacen_series, name_bacen_series, start_date, end_date)