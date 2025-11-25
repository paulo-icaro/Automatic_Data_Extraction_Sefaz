# ======================= #
# === SICONFI DATASET === #
# ======================= #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
library(dplyr)
library(stringr)
source('https://raw.githubusercontent.com/paulo-icaro/Siconfi_API/refs/heads/main/Query_RREO.R')

# Obs: Importação manual, caso a linha acima não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/Query_RREO.R')
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/API_Siconfi.R')
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/FG_URL_RREO.R')



# ===================================== #
# === Extração do Conjunto de Dados === #
# ===================================== #

# --- Informações Prévias --- #
ano = 2018:2025
bimestre = 1:6
ente = 23
relatorio = c('01', '04', '06')
esfera = 'M'
tipo_demonstrativo = 'RREO'

# --- Extração --- #
siconfi_dataset = query_rreo(ano, bimestre, tipo_demonstrativo, '06', 'E', ente, TRUE)



# ======================== #
# === Seleção de Dados === #
# ======================== #

# Resultado Previdenciário Pago (Acumulado do Bimestre)
resultado_previdenciario = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta == 'RESULTADO PREVIDENCIÁRIO - FUNDO EM REPARTIÇÃO (XI) = (IX ¿ X)', coluna == 'DESPESAS PAGAS ATÉ O BIMESTRE (f)')
  
# Dívida Consolidada Líquida (Até o Bimestre)
divida_consolidada_liquida = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta == 'DÍVIDA CONSOLIDADA LÍQUIDA (XXXI) = (XXVIII - XXIX)' & str_detect(coluna, 'Até o Bimestre'))

# Despesa Corrente Paga (Acumulado do Bimestre)
depesa_corrente = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta == 'DESPESAS CORRENTES', coluna == 'DESPESAS PAGAS ATÉ O BIMESTRE (j)', cod_conta == 'DespesasCorrentes')

# Resultado Primário sem RPPS (Acumulado do Bimestre)
resultado_primario_s_rpps = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta == 'RESULTADO PRIMÁRIO (SEM RPPS) - Acima da Linha (XXXV) = (XVIIa - (XXXIIIa +XXXIIIb + XXXIIIc))',
         coluna == 'VALOR')



# =============== #
# === Limpeza === #
# =============== #
rm(ano, bimestre, ente, esfera, relatorio, tipo_demonstrativo)