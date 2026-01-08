# ======================== #
# === IPEADATA DATASET === #
# ======================== #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
source('https://raw.githubusercontent.com/paulo-icaro/Ipeadata_API/refs/heads/main/Ipeadata_Query.R')

# Obs: Importação manual, caso o link direto não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/Ipeadata_API.R')
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/Ipeadata_URL.R')
# source('C://Users/Paulo/Documents/Repositorios/Ipeadata_API/Ipeadata_Query.R')


# ===================================== #
# === Extração do Conjunto de Dados === #
# ===================================== #

# --- Informações Prévias --- #
cod_ipeadata_series = c('PRECOS12_IPCA12', 'DIMAC_CF_INVBR_TOT12')
name_ipeadata_series = c('ipca', 'inv_bruto_total')
periodo = as.character(2015:2025)

# --- Extração --- #
ipeadata_dataset = ipeadata_query(cod_ipeadata_series, name_ipeadata_series, periodo)



# =============== #
# === Limpeza === #
# =============== #
rm(cod_ipeadata_series, name_ipeadata_series, periodo)