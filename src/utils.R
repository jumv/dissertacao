


create_percentis <- function(dados,percentTemp,percentPrecip){
  # Formando dummys
  dados %>% mutate(percent_temp = factor(ntile(temp, percentTemp)),
                            percent_precip = factor(ntile(precip,percentPrecip)))
}

creat_linear_tend_fact_muni <- function(dados){
  # criando variavel de tendencia linear
  df_trend <- tibble(
    data = dados$data %>% unique(),
    trend = 1:(dados$data %>% unique() %>% length())
  )
  dados <- dados %>% left_join(df_trend)
  # fazendo code do municipio virar factor
  dados$code_muni <- as.factor(dados$code_muni)
  dados
}

create_lead <- function(dados,n = 1){
  dados %>%
    group_by(ano,code_muni) %>%
    mutate(temp = dplyr::lead(temp,n),
           precip = dplyr::lead(precip,n)) %>%
    ungroup() %>%
    na.omit()
}

create_lag <- function(dados,n = 1){
  dados %>%
    group_by(ano,code_muni) %>%
    mutate(temp = dplyr::lag(temp,n),
           precip = dplyr::lag(precip,n)) %>%
    ungroup() %>%
    na.omit()
}


filter_population <- function(dados,n){
  dados %>% filter(populacao > n)
}

make_regressions <- function(dados){
  #### OLS
  model_ols_fixed <- feols(feminicidio  ~ percent_temp + percent_precip | data + code_muni, dados)
  #### Poisson
  model_poisson_fixed <- fepois(feminicidio  ~ percent_temp + percent_precip | data + code_muni, dados)
  #### Binomial Negativo
  model_bn_fixed <- fenegbin(feminicidio  ~ percent_temp + percent_precip | data + code_muni, dados)
  list(model_ols_fixed,model_poisson_fixed,model_bn_fixed)
}



# tabela de depara
create_name_variables <- function(df){
  depara_temp <- df %>% group_by(percent_temp) %>%
    summarise(max = max(temp),
              min = min(temp)) %>%
    mutate(interval = paste('Temperatura ',round(min,1),'° - ',round(max,1),'°',sep = ''),
           variavel = paste('percent_temp',percent_temp,sep =  '')) %>%
    dplyr::select(variavel,interval)
  
  depara_precip <- df %>% group_by(percent_precip) %>%
    summarise(max = max(precip),
              min = min(precip)) %>%
    mutate(interval = paste('Precipitação ',round(min,1),'mm - ',round(max,1),'mm',sep = ''),
           variavel = paste('percent_precip',percent_precip,sep =  '')) %>%
    dplyr::select(variavel,interval)
  
  df_depara <- rbind(depara_temp,depara_precip)
  nomes_variaveis <- df_depara$interval
  names(nomes_variaveis) <- df_depara$variavel
  nomes_variaveis
}


change_ref <- function(df,ref_temp){
  df$percent_temp <- relevel(df$percent_temp,ref = ref_temp)
  df
}

# preprocess










