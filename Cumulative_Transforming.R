# ========================= #
# === BIMONTHLY DATASET === #
# ========================= #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
#source('https://raw.githubusercontent.com/paulo-icaro/Automatic_Data_Extraction_Sefaz/refs/heads/main/Bacen_Dataset.R')
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
  
  
  if(transform_type == 'soma'){
    if(frequency == 'mensal'){
        dataset = 
          dataset %>% 
          mutate(data = floor_date(data, unit = 'month')) %>%
          group_by(data) %>%
          summarise(across(colnames(dataset[c(-1)]), sum)) %>%
          mutate(data = 
                   paste0(
                     year(data), 
                     case_when(
                       month(data) == 1 ~ 'M1',
                       month(data) == 2 ~ 'M2',
                       month(data) == 3 ~ 'M3',
                       month(data) == 4 ~ 'M4',
                       month(data) == 5 ~ 'M5',
                       month(data) == 6 ~ 'M6',
                       month(data) == 7 ~ 'M7',
                       month(data) == 8 ~ 'M8',
                       month(data) == 9 ~ 'M9',
                       month(data) == 10 ~ 'M10',
                       month(data) == 11 ~ 'M11',
                       .default = 'M12')))
    } else if(frequency == 'bimestre'){
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
                       .default = 'B6')))
    } else if(frequency == 'trimestre'){
        dataset = 
          dataset %>% 
          mutate(data = floor_date(data, unit = 'quarter')) %>%
          group_by(data) %>%
          summarise(across(colnames(dataset[c(-1)]), sum)) %>%
          mutate(data = 
                   paste0(
                     year(data), 
                     case_when(
                       month(data) %in% c(1,2,3) ~ 'T1', 
                       month(data) %in% c(4,5,6) ~ 'T2',
                       month(data) %in% c(7,8,9) ~ 'T3',
                       .default = 'T4')))
    } else if(frequency == 'semestre'){
        dataset = 
          dataset %>% 
          mutate(data = floor_date(data, unit = 'halfyear')) %>%
          group_by(data) %>%
          summarise(across(colnames(dataset[c(-1)]), sum)) %>%
          mutate(data = 
                   paste0(
                     year(data), 
                     case_when(
                       month(data) - 6 >= 1 ~ 'S2', 
                       .default = 'S1')))
    }
    
  } else if(transform_type == 'last_period'){
    if(frequency == 'bimestre'){
      dataset = 
        dataset %>%
        filter(month(data) %% 2 = 0) %>%
        mutate(data = 
                 paste0()
                 )
    }
  }
  
  # if(transform_type == 'final_periodo' && frequency == 'bimestre'){
  #     dataset %>% 
  #       mutate(data = floor_date(data, unit = 'bimonth')) %>% 
  #       group_by(data) %>%
  #       summarise(data = max(data))
  # }
  

}

