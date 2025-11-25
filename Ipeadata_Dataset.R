# ======================== #
# === IPEADATA DATASET === #
# ======================== #

# --- Script by Paulo Icaro --- #


# ===================================== #
# === Extração do Conjunto de Dados === #
# ===================================== #

# --- Informações Prévias --- #
base_url = c("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='")
cod_ipeadata_series = c('PRECOS12_IPCA12', 'DIMAC_CF_INVBR_TOT12')
name_ipeadata_series = c('IPCA', 'Inv_Bruto_Total')
periodo = as.character(2015:2025)


# -- Acessando a API --- #
for(i in 1:length(cod_ipeadata_series)){
  
  # --- Extração --- #
  url_ipeadata = paste0(base_url, cod_ipeadata_series[i], "')")   # Uso de aspas duplas para não confundir com as aspas simples da URL
  ipeadata_dataset_raw = request(url_ipeadata) %>% req_perform()
  ipeadata_dataset_raw = rawToChar(x = ipeadata_dataset_raw$body)
  ipeadata_dataset_raw = fromJSON(ipeadata_dataset_raw, flatten = TRUE)$value
  ipeadata_dataset_raw = ipeadata_dataset_raw[c(2,3)] %>% filter(substr(VALDATA, start = 1, stop = 4) %in% c(periodo))
  
  # --- Agrupango Colunas --- #
  if(i == 1){ipeadata_dataset = ipeadata_dataset_raw}
  else{ipeadata_dataset = left_join(x = ipeadata_dataset, y = ipeadata_dataset_raw, by = join_by('VALDATA' == 'VALDATA'))}
  
  # --- Nomeando Cabeçalhos --- #
  if(i == length(cod_ipeadata_series)){
    colnames(ipeadata_dataset) = c('data', name_ipeadata_series)
    rm(ipeadata_dataset_raw, base_url, cod_ipeadata_series, name_ipeadata_series, periodo, url_ipeadata, i)
  }
}



# =============== #
# === Limpeza === #
# =============== #
#rm(cod_ipeadata_series, name_ipeadata_series, periodo, base_url)