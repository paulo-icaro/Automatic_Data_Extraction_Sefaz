# ==================== #
# === SIOF DATASET === #
# ==================== #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
#source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Frequency_Transforming.R')   # Package already loaded
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(lubridate)


# ======================= #
# === Data Processing === #
# ======================= #

# --- Previous Info --- #
path = 'Databases/Inputs/'


# --- Main Databases --- #
database_invest_programa_regiao = read_excel(path = paste0(path, 'investimentos_siof_ceara_programa_regiao.xlsx'))
database_invest_funcao = read_excel(path = paste0(path, 'investimentos_siof_ceara_funcao.xlsx'))


# ------------------------- #
# --- Investments Macro --- #
# ------------------------- #

# --- Data Processing --- #
invest_macro = 
  database_invest_programa_regiao %>% 
  filter(categoria == 'pago_acumulado', ano != '2012') %>%
  group_by(ano, mes, tipo) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'tipo', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = tipo, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))


# --- Bimonthly Series --- #
invest_macro_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_macro[c(3:6)])



# ------------------------------ #
# --- Investments per Region --- #
# ------------------------------ #

# --- Data Processing --- #
invest_region = 
  database_invest_programa_regiao %>% 
  filter(categoria == 'pago_acumulado', ano != '2012') %>%
  group_by(ano, mes, regiao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'regiao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = regiao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))


invest_region_equip = 
  database_invest_programa_regiao %>% 
  filter(categoria == 'pago_acumulado', ano != '2012', tipo == 'EQUIP') %>%
  group_by(ano, mes, regiao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'regiao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = regiao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))

invest_region_obras = 
  database_invest_programa_regiao %>% 
  filter(categoria == 'pago_acumulado', ano != '2012', tipo == 'OBRAS') %>%
  group_by(ano, mes, regiao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'regiao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = regiao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))

invest_region_total = 
  database_invest_programa_regiao %>% 
  filter(categoria == 'pago_acumulado', ano != '2012', tipo == 'TOTAL') %>%
  group_by(ano, mes, regiao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'regiao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = regiao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))


# --- Bimonthly Series --- #
invest_region_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_region[c(3:18)])
invest_region_equip_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_region_equip[c(3:18)])
invest_region_obras_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_region_obras[c(3:18)])
invest_region_total_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_region_total[c(3:18)])

# --- Renaming --- #
invest_region_equip_bimonthly = invest_region_equip_bimonthly %>% rename_with(~paste0(.x, '_equip'), -data)
invest_region_obras_bimonthly = invest_region_obras_bimonthly %>% rename_with(~paste0(.x, '_obras'), -data)
invest_region_total_bimonthly = invest_region_total_bimonthly %>% rename_with(~paste0(.x, '_total'), -data)

# --- Joining Tables --- #
invest_region_type_bimonthly =
  invest_region_equip_bimonthly %>%
  left_join(invest_region_obras_bimonthly, by = 'data') %>%
  left_join(invest_region_total_bimonthly, by = 'data')



# --------------------------------- #
# --- Investiments per Function --- #
# --------------------------------- #

# --- Data Processing --- #
invest_custeio = 
  database_invest_funcao %>%
  filter(categoria == 'pago_acumulado', tipo == 'CORRE') %>%
  group_by(ano, mes) %>%
  summarize(custeio = sum(valor)) %>%
  select('ano', 'mes', 'custeio') %>%
  ungroup() %>%
  #pivot_wider(names_from = custeio, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))

invest_funcao = 
  database_invest_funcao %>%
  filter(categoria == 'pago_acumulado') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))

invest_funcao_equip = 
  database_invest_funcao %>%
  filter(categoria == 'pago_acumulado', tipo == 'EQUIP') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))

invest_funcao_obras = 
  database_invest_funcao %>%
  filter(categoria == 'pago_acumulado', tipo == 'OBRAS') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))

invest_funcao_total = 
  database_invest_funcao %>%
  filter(categoria == 'pago_acumulado', tipo == 'TOTAL') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))

invest_funcao_corre = 
  database_invest_funcao %>%
  filter(categoria == 'pago_acumulado', tipo == 'CORRE') %>%
  group_by(ano, mes, funcao) %>%
  summarize(valor = sum(valor)) %>%
  select('ano', 'mes', 'funcao', 'valor') %>%
  ungroup() %>%
  pivot_wider(names_from = funcao, values_from = valor) %>%
  mutate(data = as.Date(paste0(ano, '-', mes, '-01')))


# --- Bimonthly Series --- #
invest_custeio_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_custeio[c(3:4)])
invest_funcao_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_funcao_equip[c(3:35)])
invest_funcao_equip_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_funcao_equip[c(3:35)])
invest_funcao_obras_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_funcao_obras[c(3:34)])
invest_funcao_total_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_funcao_total[c(3:36)])
invest_funcao_corre_bimonthly = cumulative_transform('diff_acumulado', 'bimestral', invest_funcao_corre[c(3:40)])

# --- Renaming --- #
invest_funcao_equip_bimonthly = invest_funcao_equip_bimonthly %>% rename_with(~paste0(.x, '_equip'), -data)
invest_funcao_obras_bimonthly = invest_funcao_obras_bimonthly %>% rename_with(~paste0(.x, '_obras'), -data)
invest_funcao_total_bimonthly = invest_funcao_total_bimonthly %>% rename_with(~paste0(.x, '_total'), -data)
invest_funcao_corre_bimonthly = invest_funcao_corre_bimonthly %>% rename_with(~paste0(.x, '_corre'), -data)

# --- Joining Tables --- #
invest_funcao_type_bimonthly =
  invest_funcao_equip_bimonthly %>%
  left_join(invest_funcao_obras_bimonthly, by = 'data') %>%
  left_join(invest_funcao_total_bimonthly, by = 'data') %>%
  left_join(invest_funcao_corre_bimonthly, by = 'data')




# ======================= #
# === Storing Results === #
# ======================= #
wb = createWorkbook(creator = 'Sefaz-CE')
addWorksheet(wb = wb, sheetName = 'tempo')
addWorksheet(wb = wb, sheetName = 'macro')
addWorksheet(wb = wb, sheetName = 'funcao')
addWorksheet(wb = wb, sheetName = 'funcao_type')
addWorksheet(wb = wb, sheetName = 'regional')
addWorksheet(wb = wb, sheetName = 'regional_type')
writeData(wb = wb, sheet = 'tempo', x = invest_funcao_bimonthly %>% select(data), rowNames = FALSE)
writeData(wb = wb, sheet = 'macro', x = invest_macro_bimonthly %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'funcao', x = invest_funcao_bimonthly %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'funcao_type', x = invest_funcao_type_bimonthly %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'regional', x = invest_region_bimonthly %>% select(-data), rowNames = FALSE)
writeData(wb = wb, sheet = 'regional_type', x = invest_region_type_bimonthly %>% select(data), rowNames = FALSE)
saveWorkbook(wb = wb, file = 'Databases/Outputs/db_investimentos.xlsx', overwrite = TRUE)



# ================ #
# === Cleasing === #
# ================ #
rm(invest_macro, invest_region, invest_funcao, invest_custeio, database_invest_programa_regiao, database_invest_funcao, wb,
   invest_region_equip, invest_region_obras, invest_region_total,
   invest_funcao_equip, invest_funcao_obras, invest_funcao_total, invest_funcao_corre,
   invest_region_equip_bimonthly, invest_region_obras_bimonthly, invest_region_total_bimonthly,
   invest_funcao_equip_bimonthly, invest_funcao_obras_bimonthly, invest_funcao_total_bimonthly, invest_funcao_corre_bimonthly)
