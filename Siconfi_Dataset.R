# ======================= #
# === SICONFI DATASET === #
# ======================= #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
source('https://raw.githubusercontent.com/paulo-icaro/Siconfi_API/refs/heads/main/Query_RREO.R')

# Obs: Importação manual, caso a linha acima não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/Query_RREO.R')
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/API_Siconfi.R')
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/FG_URL_RREO.R')



# ======================= #
# === Data Extraction === #
# ======================= #

# --- Previous Info --- #
ano = 2015:2025
bimestre = 1:6
tipo_demonstrativo = 'RREO'
state_municipalities = read_excel(path = 'Databases/Inputs/Lista_Municipios_Brasil.xlsx', sheet = 'Ceara')

# --- Extraction --- #
siconfi_dataset_est = query_rreo(ano, bimestre, tipo_demonstrativo, c('01', '04', '06'), 'E', 23, TRUE)
siconfi_dataset_mun = query_rreo(2025, bimestre, tipo_demonstrativo, '01', 'M', state_municipalities$`Cod. 7 Digitos`, TRUE)


# ======================== #
# === Data Selection === #
# ======================== #

# ----------------------- #
# --- Major Filtering --- #
# ----------------------- #

# --- Resultado Previdenciário --- #
resultado_previdenciario_bruto = siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'RESULTADO PREVIDENCIÁRIO'), str_detect(anexo, '04'))

# --- Dívida Consolidada Líquida --- #
divida_consolidada_liquida_bruto = siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'DÍVIDA CONSOLIDADA LÍQUIDA'), str_detect(anexo, '06'))

# --- Resultado Primário --- #
resultado_primario_bruto = siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'RESULTADO PRIMÁRIO'), str_detect(anexo, '06'))

# --- Investimento Municipal --- #
investimento_municipal_bruto = siconfi_dataset_mun[c(1, 3, 5, 9, 12, 13, 14, 15)] %>%
  filter(str_detect(conta, 'INVESTIMENTOS'), str_detect(anexo, '01'))

# Despesa Corrente #
# despesa_corrente_bruto = siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)] %>%
#   filter(str_detect(conta, 'DESPESAS CORRENTES'), str_detect(anexo, '01'))



# ----------------------- #
# --- Minor Filtering --- #
# ----------------------- #

# --- Resultado Previdenciário Pago (Acumulado do Bimestre) --- #
resultado_previdenciario =
  resultado_previdenciario_bruto %>%
  filter(conta %in% c('RESULTADO PREVIDENCIÁRIO - FUNDO EM REPARTIÇÃO (XI) = (IX ¿ X)'),
         str_detect(coluna, 'DESPESAS PAGAS ATÉ O BIMESTRE')) %>%
  mutate(data = paste0(exercicio, 'B', periodo)) %>%
  mutate('res_prev' = ifelse(periodo == 1, valor, valor - lag(valor)))

  
# --- Dívida Consolidada Líquida (Até o Bimestre) --- #
divida_consolidada_liquida =
  divida_consolidada_liquida_bruto %>%
  filter(conta %in% c('DÍVIDA CONSOLIDADA LÍQUIDA (XXXI) = (XXVIII - XXIX)',
                      'DÍVIDA CONSOLIDADA LÍQUIDA (XLII) = (XXXIX - XL)') & str_detect(coluna, 'Até o Bimestre')) %>%
  mutate(data = paste0(exercicio, 'B', periodo))

colnames(divida_consolidada_liquida)[7] = 'div_cons'



# --- Resultado Primário (Acumulado no Ano) --- #
resultado_primario =
  resultado_primario_bruto %>%
  filter(conta %in% c('RESULTADO PRIMÁRIO (SEM RPPS) - Acima da Linha (XXXV) = (XVIIa - (XXXIIIa +XXXIIIb + XXXIIIc))'),
         str_detect(coluna, regex('VALOR'))) %>%
  mutate(data = paste0(exercicio, 'B', periodo)) %>%
  mutate('res_prim' = ifelse(periodo == 1, valor, valor - lag(valor)))

# --- Investimento Municipal --- # 
investimento_municipal =
  investimento_municipal_bruto %>%
  filter(conta %in% c('INVESTIMENTOS'),
         str_detect(coluna, 'DESPESAS PAGAS ATÉ O BIMESTRE')) %>%
  mutate(data = paste0(exercicio, 'B', periodo)) %>%
  group_by(data) %>%
  select(data, exercicio, periodo, valor) %>%
  summarize('inv_mun_acum' = sum(valor)) %>%
  mutate('inv_mun' = ifelse(substr(data, 6, 6) == 1, inv_mun_acum, inv_mun_acum - lag(inv_mun_acum)))
  

# Despesa Corrente Paga (Acumulado do Bimestre)
# despesa_corrente = siconfi_dataset[c(1, 3, 9, 12, 13, 14, 15)] %>%
#   filter(conta == 'DESPESAS CORRENTES',
#          coluna == 'DESPESAS PAGAS ATÉ O BIMESTRE (j)',
#          cod_conta == 'DespesasCorrentes')




# ========================= #
# === Joining Variables === #
# ========================= #

# Obs: Adjust according to the largest dataframe
siconfi_macro_bimonthly = 
  divida_consolidada_liquida[c('data', 'div_cons')] %>%
  left_join(y = resultado_previdenciario[c('data', 'res_prev')], by = 'data') %>%
  left_join(y = resultado_primario[c('data', 'res_prim')], by = 'data') %>%
  left_join(y = investimento_municipal[c('data', 'inv_mun')], by = 'data') %>%
  mutate(res_prim_prev = res_prev + res_prim)



# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb, sheetName = 'macro')
writeData(wb = wb, sheet = 'tempo', x = siconfi_macro_bimonthly %>% select(data), rowNames = FALSE)
writeData(wb = wb, sheet = 'macro', x = siconfi_macro_bimonthly %>% select(-data), rowNames = FALSE)
saveWorkbook(wb = wb, file = 'Databases/Outputs/db_siconfi.xlsx', overwrite = TRUE)



# ================= #
# === Cleansing === #
# ================= #
patterns = c('_bruto', '_dataset')
for(i in seq_along(patterns)){
  rm(list = ls(pattern = patterns[i]))
}
rm(patterns, i, ano, bimestre, tipo_demonstrativo)