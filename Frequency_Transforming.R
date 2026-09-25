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
  else if(transform_type == 'tx_acumulada'){
    
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
  
  
  # ----------------------------- #  
  # --- Transform Type - Diff  --- #
  # ----------------------------- #
  else if(transform_type == 'diff_acumulado'){
    
    if(frequency == 'mensal'){
      dataset = unique(
        dataset %>% 
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 1, .x, .x - lag(.x)))))
    }
    
    else if(frequency == 'bimestral'){
      dataset = unique(
        dataset %>% 
        filter(month(data) %% 2 == 0) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 2, .x, .x - lag(.x)))))
    }
    
    else if(frequency == 'trimestral'){
      dataset = unique(
        dataset %>% 
        filter(month(data) %% 3 == 0) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 3, .x, .x - lag(.x)))))
    }
    
    else if(frequency == 'semestral'){
      dataset = unique(
        dataset %>% 
        filter(month(data) %% 6 == 0) %>%
        mutate(across(colnames(dataset[names(dataset) != 'data']), ~ ifelse(month(data) == 6, .x, .x - lag(.x)))))
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
                                         month(data) == 1 ~ '_jan',
                                         month(data) == 2 ~ '_fev',
                                         month(data) == 3 ~ '_mar',
                                         month(data) == 4 ~ '_abr',
                                         month(data) == 5 ~ '_mai',
                                         month(data) == 6 ~ '_jun',
                                         month(data) == 7 ~ '_jul',
                                         month(data) == 8 ~ '_ago',
                                         month(data) == 9 ~ '_set',
                                         month(data) == 10 ~ '_out',
                                         month(data) == 11 ~ '_nov',
                                         .default = '_dez')))
    }
    
    else if(frequency == 'bimestral'){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2) ~ '_fev', 
                                         month(data) %in% c(3,4) ~ '_abr',
                                         month(data) %in% c(5,6) ~ '_jun',
                                         month(data) %in% c(7,8) ~ '_ago',
                                         month(data) %in% c(9,10) ~ '_out',
                                         .default = '_dez')))
    }
    
    else if(frequency == 'trimestral'){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) %in% c(1,2,3) ~ '_mar', 
                                         month(data) %in% c(4,5,6) ~ '_jun',
                                         month(data) %in% c(7,8,9) ~ '_set',
                                         .default = '_dez')))
    }
    
    else if(frequency == 'semestral'){
      dataset = dataset %>% mutate(data = 
                                     paste0(
                                       year(data), 
                                       case_when(
                                         month(data) - 6 >= 1 ~ '_dez', 
                                         .default = '_jun')))
    }
  }
  
  
  # ------------------------ #
  # --- Returning Output --- #
  # ------------------------ #
  return(dataset)
}