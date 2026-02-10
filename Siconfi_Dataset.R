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
ano = 2015:2025
bimestre = 1:6
ente = 23
relatorio = c('01', '04', '06')
esfera = 'M'
tipo_demonstrativo = 'RREO'

# --- Extração --- #
siconfi_dataset = query_rreo(ano, bimestre, tipo_demonstrativo, relatorio, 'E', ente, TRUE)



# ======================== #
# === Seleção de Dados === #
# ======================== #

# --------------------------- #
# --- Filtragens Iniciais --- #
# --------------------------- #

# Resultado Previdenciário #
resultado_previdenciario_bruto = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'RESULTADO PREVIDENCIÁRIO'), str_detect(anexo, '04'))

# Dívida Consolidada Líquida #
divida_consolidada_liquida_bruto = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'DÍVIDA CONSOLIDADA LÍQUIDA'), str_detect(anexo, '06'))

# Despesa Corrente #
despesa_corrente_bruto = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'DESPESAS CORRENTES'), str_detect(anexo, '01'))

# Resultado Primário #
resultado_primario_bruto = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'RESULTADO PRIMÁRIO'), str_detect(anexo, '06'))


# ---------------------------- #
# --- Filtragens Avançadas --- #
# ---------------------------- #

# Resultado Previdenciário Pago (Acumulado do Bimestre)
resultado_previdenciario = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta %in% c('RESULTADO PREVIDENCIÁRIO (XIV) = (X - XIII)',
                      'RESULTADO PREVIDENCIÁRIO (XV) = (XI - XIV)',
                      'RESULTADO PREVIDENCIÁRIO (XI) = (IX - X)',
                      'RESULTADO PREVIDENCIÁRIO - FUNDO EM REPARTIÇÃO (XI) = (IX ¿ X)'),
         str_detect(coluna, 'DESPESAS LIQUIDADAS ATÉ O BIMESTRE|DESPESAS PAGAS ATÉ O BIMESTRE (f)'))
  
# Dívida Consolidada Líquida (Até o Bimestre)
divida_consolidada_liquida = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta %in% c('DÍVIDA CONSOLIDADA LÍQUIDA (XXXI) = (XXVIII - XXIX)',
                      'DÍVIDA CONSOLIDADA LÍQUIDA (XLII) = (XXXIX - XL)') & str_detect(coluna, 'Até o Bimestre'))

# Despesa Corrente Paga (Acumulado do Bimestre)
despesa_corrente = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta == 'DESPESAS CORRENTES',
         coluna == 'DESPESAS PAGAS ATÉ O BIMESTRE (j)',
         cod_conta == 'DespesasCorrentes')

# Resultado Primário (Acumulado no Ano) Obs: sem RPPS a partir de 2023
resultado_primario = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(conta %in% c('RESULTADO PRIMÁRIO (XIX) = (VII - XVIII)',
                       'RESULTADO PRIMÁRIO (SEM RPPS) - Abaixo da Linha (LI) = (L) - (XXXVI - XXXVII)',
                       'RESULTADO PRIMÁRIO (SEM RPPS) - Acima da Linha (XXXV) = (XVIIa - (XXXIIIa +XXXIIIb + XXXIIIc))',
                       'RESULTADO PRIMÁRIO - Abaixo da Linha (XXXVII) =  XXXVI - (XXV - XXVI)',
                       'RESULTADO PRIMÁRIO - Abaixo da Linha (XXXIX) =  XXXVIII - (XXV - XXVI)',
                       'RESULTADO PRIMÁRIO - Acima da Linha (XXIV) = (XIIa - (XXIIIa +XXIIIb + XXIIIc))'),
         str_detect(coluna, regex('VALOR|VALOR INCORRIDO|Até o Bimestre|DESPESAS LIQUIDADAS ATÉ O BIMESTRE|Despesas Liquidadas até o Bimestre')))
  # mutate('valor_bimestre' = valor - lag(valor))


for(i in seq_along(resultado_primario_s_rpps)){
  if(resultado_primario_s_rpps$periodo[i] == 1){resultado_primario_s_rpps$valor_bimestre[i] = resultado_primario_s_rpps$valor[i]}
}



# =============== #
# === Limpeza === #
# =============== #
rm(ano, bimestre, ente, esfera, relatorio, tipo_demonstrativo)