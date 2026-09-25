# ======================= #
# === SICONFI DATASET === #
# ======================= #

# --- Script by Paulo Icaro --- #


# =================== #
# === Bibliotecas === #
# =================== #
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
source('https://raw.githubusercontent.com/paulo-icaro/Siconfi_API/refs/heads/main/siconfi_rreo_query.R')

# Obs: Importação manual, caso a linha acima não funcione. Lembre-se de trocar para o seu diretório.
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/Query_RREO.R')
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/API_Siconfi.R')
# source('C://Users/Paulo/Documents/Repositorios/Siconfi_API/FG_URL_RREO.R')



# ========================= #
# === Extração de Dados === #
# ========================= #

# --- Informações Prévias --- #
ano = 2015:2025
bimestre = 1:6
tipo_demonstrativo = 'RREO'
state_municipalities = read_excel(path = 'Databases/Inputs/Lista_Municipios_Brasil.xlsx', sheet = 'Ceara')

# --- Extracao --- #
siconfi_dataset_est = siconfi_rreo_query(ano, bimestre, tipo_demonstrativo, c('01', '04', '06'), 'E', 23, TRUE)
siconfi_dataset_mun = siconfi_rreo_query(2025, bimestre, tipo_demonstrativo, c('01'), 'M', state_municipalities$`Cod. 7 Digitos`, TRUE)


# ======================== #
# === Selecao de Dados === #
# ======================== #

# -------------------------------- #
# --- Função de Ajuste de Data --- #
# -------------------------------- #

# Obs: ajuste feito apenas para dados bimestrais
ajuste_data_siconfi = function(base){
  
  base = base %>%
    mutate(data = 
             as.Date(paste0(exercicio,
                    case_when(periodo == 1 ~ '-02-01', 
                              periodo == 2 ~ '-04-01',
                              periodo == 3 ~ '-06-01',
                              periodo == 4 ~ '-08-01',
                              periodo == 5 ~ '-10-01',
                              .default = '-12-01')), format = '%Y-%m-%d'))
  return(base)
}


# ---------------------- #
# --- Filtros Gerais --- #
# ---------------------- #

# --- Resultado Previdenciário --- #
resultado_previdenciario_bruto = ajuste_data_siconfi(siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)]) %>%
  filter(str_detect(conta, 'RESULTADO PREVIDENCIÁRIO'), str_detect(anexo, '04'))

# --- Dívida Consolidada Líquida --- #
divida_consolidada_liquida_bruto = ajuste_data_siconfi(siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)]) %>%
  filter(str_detect(conta, 'DÍVIDA CONSOLIDADA LÍQUIDA'), str_detect(anexo, '06'))

# --- Resultado Primário --- #
resultado_primario_bruto = ajuste_data_siconfi(siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)]) %>%
  filter(str_detect(conta, 'RESULTADO PRIMÁRIO'), str_detect(anexo, '06'))

# --- Investimento Municipal --- #
investimento_municipal_bruto = ajuste_data_siconfi(siconfi_dataset_mun[c(1, 3, 5, 9, 12, 13, 14, 15)]) %>%
  filter(str_detect(conta, 'INVESTIMENTOS'), str_detect(anexo, '01'))

# Despesa Corrente #
# despesa_corrente_bruto = siconfi_dataset_est[c(1, 3, 9, 12, 13, 14, 15)] %>%
#   filter(str_detect(conta, 'DESPESAS CORRENTES'), str_detect(anexo, '01'))



# --------------------------- #
# --- Filtros Especificos --- #
# --------------------------- #

# --- Resultado Previdenciário Pago (Acumulado do Bimestre) --- #
resultado_previdenciario =
  resultado_previdenciario_bruto %>%
  filter(conta %in% c('RESULTADO PREVIDENCIÁRIO - FUNDO EM REPARTIÇÃO (XI) = (IX ¿ X)'),
         str_detect(coluna, 'DESPESAS PAGAS ATÉ O BIMESTRE')) %>%
  mutate('res_prev' = ifelse(periodo == 1, valor, valor - lag(valor)))

  
# --- Dívida Consolidada Líquida (Até o Bimestre) --- #
divida_consolidada_liquida =
  divida_consolidada_liquida_bruto %>%
  filter(conta %in% c('DÍVIDA CONSOLIDADA LÍQUIDA (XXXI) = (XXVIII - XXIX)',
                      'DÍVIDA CONSOLIDADA LÍQUIDA (XLII) = (XXXIX - XL)') & str_detect(coluna, 'Até o Bimestre'))
colnames(divida_consolidada_liquida)[7] = 'div_cons'



# --- Resultado Primário (Acumulado no Ano) --- #
resultado_primario =
  resultado_primario_bruto %>%
  filter(conta %in% c('RESULTADO PRIMÁRIO (SEM RPPS) - Acima da Linha (XXXV) = (XVIIa - (XXXIIIa +XXXIIIb + XXXIIIc))'),
         str_detect(coluna, regex('VALOR'))) %>%
  mutate('res_prim' = ifelse(periodo == 1, valor, valor - lag(valor)))

# --- Investimento Municipal --- # 
investimento_municipal =
  investimento_municipal_bruto %>%
  filter(conta %in% c('INVESTIMENTOS'),
         str_detect(coluna, 'DESPESAS PAGAS ATÉ O BIMESTRE')) %>%
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
siconfi_macro_bimonthly_m = 
  divida_consolidada_liquida[c('data', 'div_cons')] %>%
  left_join(y = resultado_previdenciario[c('data', 'res_prev')], by = 'data') %>%
  left_join(y = resultado_primario[c('data', 'res_prim')], by = 'data') %>%
  left_join(y = investimento_municipal[c('data', 'inv_mun')], by = 'data') %>%
  mutate(res_prim_prev = res_prev + res_prim)



# =============================== #
# === Verticalizando os Dados === #
# =============================== #
siconfi_macro_bimonthly_t = pivot_longer(data = siconfi_macro_bimonthly_m, cols = !starts_with('data'), names_to = 'variavel', values_to = 'valor')


# ==================================== #
# === Armazenamento dos Resultados === #
# ==================================== #

# --- Pre Definicoes --- #
save_path = c('Databases/Outputs/Tableau/db_siconfi_tableau', 'Databases/Outputs/Matlab/db_siconfi_matlab')
formato = c('tableau', 'matlab')
aba = c('siconfi_macro')
dataframe = list(
  tableau = list(siconfi_macro_bimonthly_t),
  matlab = list(siconfi_macro_bimonthly_m)
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
    writeData(wb = wb, sheet = 'tempo', as.numeric(siconfi_macro_bimonthly_m$data) + 25569, rowNames = FALSE)
  }
  saveWorkbook(wb = wb, file = paste0(save_path[f], '.xlsx'), overwrite = TRUE)
}

# wb = createWorkbook(creator = 'Sefaz-CE')
# addWorksheet(wb = wb, sheetName = 'tempo')
# addWorksheet(wb = wb, sheetName = 'macro')
# writeData(wb = wb, sheet = 'tempo', x = as.numeric(siconfi_macro_bimonthly$data) + 25569, rowNames = FALSE)
# writeData(wb = wb, sheet = 'macro', x = siconfi_macro_bimonthly, rowNames = FALSE)
# saveWorkbook(wb = wb, file = 'Databases/Outputs/db_siconfi_teste.xlsx', overwrite = TRUE)


# =============== #
# === Limpeza === #
# =============== #
rm(list = ls(pattern = '^divida|^investimento|^resultado'))#|^siconfi'))
rm(wb, ano, bimestre, tipo_demonstrativo, state_municipalities, f, s, formato, aba, save_path, dataframe)