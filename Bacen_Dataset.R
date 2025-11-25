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
cod_bacen_series = c('13010', '13093', '13094', '14007', '14034', '25390', '433', '4390')
name_bacen_series = c('varicao_emprego', 'exportacao', 'importacao', 'credito_pf', 'credito_pj', 'ibcrce', 'ipca', 'selic')

# --- Extração --- #
bacen_dataset = bacen_query(cod_bacen_series, name_bacen_series, '01/01/2015', '31/08/2025', TRUE)



# =============== #
# === Limpeza === #
# =============== #
rm(cod_bacen_series, name_bacen_series)