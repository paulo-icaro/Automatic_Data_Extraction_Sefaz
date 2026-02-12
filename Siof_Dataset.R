# ==================== #
# === SIOF DATASET === #
# ==================== #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
source('https://raw.githubusercontent.com/paulo-icaro/Ipeadata_API/refs/heads/main/Ipeadata_Query.R')
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Frequency_Transforming.R')
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)


# ======================= #
# === Data Processing === #
# ======================= #

# --- Previous Info --- #
path = 'Databases/Inputs/'

# --- Extracting Prince Index --- #
ipca_series = ipeadata_query('PRECOS12_IPCA12', 'ipca', as.character(2015:2025))
ipca_series = ipca_series %>% mutate(data = as.Date(data))
ipca_series$ipca = (ipca_series$ipca/last(ipca_series$ipca))*100

# --- Main Databases --- #
invest_programa_regiao = read_excel(path = paste0(path, 'investimentos_siof_ceara_programa_regiao_versao_atualizada.xlsx'))
invest_funcao = read_excel(path = paste0(path, 'investimentos_siof_ceara_funcao_versao_atualizada.xlsx'))


# ------------------------- #
# --- Investments Macro --- #
# ------------------------- #

# --- Data Processing --- #
invest_macro = 
  invest_programa_regiao %>% 
  filter(categoria == 'pago_mensal', ano != '2012') %>% #, mes %in% c('02', '04', '06', '08', '10', '12')) %>%
  group_by(ano, mes, tipo) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'tipo', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = tipo, values_from = valor)

invest_macro = cbind(invest_macro, ipca_series[-1:-12,] %>% filter())


# --- Investments to Present Value --- #
invest_macro = 
  invest_macro %>%
  mutate(EQUIP = (EQUIP/ipca)*100) %>% 
  mutate(OBRAS = (OBRAS/ipca)*100) %>%
  mutate(TOTAL = (TOTAL/ipca)*100)


# --- Bimonthly Series --- #
invest_macro_bimonthly = cumulative_transform('soma', 'bimestral', invest_macro[c(3:6)])



# ------------------------------ #
# --- Investments per Region --- #
# ------------------------------ #

# --- Data Processing --- #
invest_region_equip = 
  invest_programa_regiao %>% 
  filter(categoria == 'pago_mensal', ano != '2012', tipo == 'EQUIP') %>%
  group_by(ano, mes, regiao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'regiao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = regiao, values_from = valor)

invest_region_obras = 
  invest_programa_regiao %>% 
  filter(categoria == 'pago_mensal', ano != '2012', tipo == 'OBRAS') %>%
  group_by(ano, mes, regiao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'regiao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = regiao, values_from = valor)

invest_region_total = 
  invest_programa_regiao %>% 
  filter(categoria == 'pago_mensal', ano != '2012', tipo == 'TOTAL') %>%
  group_by(ano, mes, regiao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'regiao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = regiao, values_from = valor)


invest_region_equip = cbind(invest_region_equip, ipca_series[-1:-12,])
invest_region_obras = cbind(invest_region_obras, ipca_series[-1:-12,])
invest_region_total = cbind(invest_region_total, ipca_series[-1:-12,])


# --- Investments to Present Value --- #
invest_region_equip = 
  invest_region_equip %>%
  mutate(across(!c('ano', 'mes', 'data', 'ipca'), ~ .x/ipca*100))

invest_region_obras = 
  invest_region_obras %>%
  mutate(across(!c('ano', 'mes', 'data', 'ipca'), ~ .x/ipca*100))

invest_region_total = 
  invest_region_total %>%
  mutate(across(!c('ano', 'mes', 'data', 'ipca'), ~ .x/ipca*100))


# --- Bimonthly Series --- #
invest_region_equip_bimonthly = cumulative_transform('soma', 'bimestral', invest_region_equip[c(3:18)])
invest_region_obras_bimonthly = cumulative_transform('soma', 'bimestral', invest_region_obras[c(3:18)])
invest_region_total_bimonthly = cumulative_transform('soma', 'bimestral', invest_region_total[c(3:18)])



# --------------------------------- #
# --- Investiments per Function --- #
# --------------------------------- #

# --- Data Processing --- #
invest_funcao_equip = 
  invest_funcao %>%
  filter(categoria == 'pago_mensal', tipo == 'EQUIP') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor)

invest_funcao_obras = 
  invest_funcao %>%
  filter(categoria == 'pago_mensal', tipo == 'OBRAS') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor)

invest_funcao_total = 
  invest_funcao %>%
  filter(categoria == 'pago_mensal', tipo == 'TOTAL') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor)

invest_funcao_corre = 
  invest_funcao %>%
  filter(categoria == 'pago_mensal', tipo == 'CORRE') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor)


invest_funcao_equip = cbind(invest_funcao_equip, ipca_series)
invest_funcao_obras = cbind(invest_funcao_obras, ipca_series)
invest_funcao_total = cbind(invest_funcao_total, ipca_series)
invest_funcao_corre = cbind(invest_funcao_corre, ipca_series)


# --- Investments to Present Value --- #
invest_funcao_equip = 
  invest_funcao_equip %>%
  mutate(across(!c('ano', 'mes', 'data', 'ipca'), ~ .x/ipca*100))

invest_funcao_obras = 
  invest_funcao_obras %>%
  mutate(across(!c('ano', 'mes', 'data', 'ipca'), ~ .x/ipca*100))

invest_funcao_total = 
  invest_funcao_total %>%
  mutate(across(!c('ano', 'mes', 'data', 'ipca'), ~ .x/ipca*100))

invest_funcao_corre = 
  invest_funcao_corre %>%
  mutate(across(!c('ano', 'mes', 'data', 'ipca'), ~ .x/ipca*100))


# --- Bimonthly Series --- #
invest_funcao_equip_bimonthly = cumulative_transform('soma', 'bimestral', invest_funcao_equip[c(3:35)])
invest_funcao_obras_bimonthly = cumulative_transform('soma', 'bimestral', invest_funcao_obras[c(3:34)])
invest_funcao_total_bimonthly = cumulative_transform('soma', 'bimestral', invest_funcao_total[c(3:36)])
invest_funcao_corre_bimonthly = cumulative_transform('soma', 'bimestral', invest_funcao_corre[c(3:40)])



# ================ #
# === Cleasing === #
# ================ #
#rm(invest_macro, invest_region_equip, invest_region_obras, invest_region_total, invest_funcao_equip, invest_funcao_obras,
#   invest_funcao_total, invest_funcao_corre, invest_programa_regiao, invest_funcao, ipca_series)
