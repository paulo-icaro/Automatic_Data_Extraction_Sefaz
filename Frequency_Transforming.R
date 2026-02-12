# ========================= #
# === BIMONTHLY DATASET === #
# ========================= #

# --- Script by Paulo Icaro --- #


# ================= #
# === Libraries === #
# ================= #
tryCatch(expr = suppressWarnings(library(dplyr)),
         error = function(e){stop('Não é possível prosseguir. Instale os pacotes dplyr.')})
tryCatch(expr = suppressWarnings(library(lubridate)),
         error = function(e){stop('Não é possível prosseguir. Instale o pacotes lubridate.')})



# ============================ #
# === Transforming Dataset === #
# ============================ #
cumulative_transform = function(transform_type, frequency, dataset, change_date = TRUE){
  
  # ---------------------------- #
  # --- Transform Type - Sum --- #
  # ---------------------------- #
  if(transform_type == 'soma'){
    
    if(frequency == 'mensal'){
        dataset = 
          dataset
    }
    
    else if(frequency == 'bimestral'){
        dataset = 
          dataset %>% 
          mutate(data = floor_date(data, unit = 'bimonth')) %>%
          group_by(data) %>%
          summarise(across(colnames(dataset[names(dataset) != 'data']), sum))
    }
    
    else if(frequency == 'trimestral'){
        dataset = 
          dataset %>% 
          mutate(data = floor_date(data, unit = 'quarter')) %>%
          group_by(data) %>%
          summarise(across(colnames(dataset[names(dataset) != 'data']), sum))
    }
    
    else if(frequency == 'semestral'){
        dataset = 
          dataset %>% 
          mutate(data = floor_date(data, unit = 'halfyear')) %>%
          group_by(data) %>%
          summarise(across(colnames(dataset[names(dataset) != 'data']), sum))
    }
  }
  
  
  # ------------------------------------ #  
  # --- Transform Type - Last Period --- #
  # ------------------------------------ #
  else if(transform_type == 'periodo_final'){
    
    if(frequency == 'mensal'){
      dataset = 
        dataset
    }
    
    else if(frequency == 'bimestral'){
        dataset = 
          dataset %>%
          filter(month(data) %% 2 == 0)
    }
    
    else if(frequency == 'trimestral'){
        dataset = 
          dataset %>% 
          filter(month(data) %% 3 == 0)
    }
    
    else if(frequency == 'semestral'){
        dataset = 
          dataset %>% 
          filter(month(data) %% 6 == 0)
    }
  }
  
  
  # ----------------------------- #  
  # --- Transform Type - Mean --- #
  # ----------------------------- #
  else if(transform_type == 'media'){
    
    if(frequency == 'mensal'){
      dataset = unique(
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'month')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
    
    else if(frequency == 'bimestral'){
      dataset = unique(
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'bimonth')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
    
    else if(frequency == 'trimestral'){
      dataset = unique(
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'quarter')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
    
    else if(frequency == 'semestral'){
      dataset = unique(
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'halfyear')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), mean)))
    }
  }
  
  
  # ----------------------------------- #  
  # --- Transform Type - Cumulative --- #
  # ----------------------------------- #
  else if(transform_type == 'acumulado'){
    
    if(frequency == 'mensal'){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'month')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency == 'bimestral'){
      dataset = 
        dataset %>% 
        mutate(data = floor_date(x = data, unit = 'bimonth')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency == 'trimestral'){
      dataset = 
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'quarter')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
    
    else if(frequency == 'semestral'){
      dataset = 
        dataset %>%
        mutate(data = floor_date(x = data, unit = 'halfyear')) %>%
        group_by(data) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ cumprod(1 + .x/100) - 1)*100) %>%
        slice_tail(n = 1) %>%
        ungroup()
    }
  }
  
  
  # --------------------------------------------- #
  # --- Change Date Column to Match Frequency --- #
  # --------------------------------------------- #
  if(change_date == TRUE){
    if(frequency == 'mensal'){
      dataset = dataset %>% mutate(data = 
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
    }
    
    else if(frequency == 'bimestral'){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2) ~ 'B1', 
                                         month(data) %in% c(3,4) ~ 'B2',
                                         month(data) %in% c(5,6) ~ 'B3',
                                         month(data) %in% c(7,8) ~ 'B4',
                                         month(data) %in% c(9,10) ~ 'B5',
                                         .default = 'B6')))
    }
    
    else if(frequency == 'trimestral'){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2,3) ~ 'T1', 
                                         month(data) %in% c(4,5,6) ~ 'T2',
                                         month(data) %in% c(7,8,9) ~ 'T3',
                                         .default = 'T4')))
    }
    
    else if(frequency == 'semestral'){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) - 6 >= 1 ~ 'S2', 
                                         .default = 'S1')))
    }
  }
  
  
  # ------------------------ #
  # --- Returning Output --- #
  # ------------------------ #
  return(dataset)
}