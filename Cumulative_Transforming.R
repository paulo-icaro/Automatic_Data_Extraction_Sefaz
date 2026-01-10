# ========================= #
# === BIMONTHLY DATASET === #
# ========================= #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Bacen_Dataset.R')
library(dplyr)
library(lubridate)


# ============================ #
# === Transforming Dataset === #
# ============================ #
cumulative_transform = function(transform_type, frequency, dataset){
  #stopifnot('Argumento "data" inválido !' = is.data.frame(dataset))
  #ncol = ncol(dataset)
  min_date = min(dataset[[1]])
  max_date = max(dataset[[1]])
  
  
  if(transform_type == 'soma' && frequency == 'bimestre'){
    dataset = 
      dataset %>% 
        mutate(data = floor_date(data, unit = 'bimonth')) %>%
        group_by(data) %>%
        summarise(across(colnames(dataset[c(-1)]), sum)) %>%
        mutate(data = 
                 paste0(
                   year(data), 
                   case_when(
                     month(data) %in% c(1,2) ~ 'B1', 
                     month(data) %in% c(3,4) ~ 'B2',
                     month(data) %in% c(5,6) ~ 'B3',
                     month(data) %in% c(7,8) ~ 'B4',
                     month(data) %in% c(9,10) ~ 'B5',
                     .default = 'B6')
                   )
               )
    
      
    
    
    
    
  }
  
  # if(transform_type == 'final_periodo' && frequency == 'bimestre'){
  #     dataset %>% 
  #       mutate(data = floor_date(data, unit = 'bimonth')) %>% 
  #       group_by(data) %>%
  #       summarise(data = max(data))
  # }
  

}

