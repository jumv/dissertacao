

library(tidyverse)
library(tictoc)
library(furrr)
library(plm)
library(MASS)
library(pscl)
library(vcd)
library(pglm)
library(stargazer)
library(fixest)

future::plan(multiprocess)


# lendo os dados
dados <- read_csv('data/models/df_join.csv')

# funcao para criar tabela de percentis de uma coluna
create_df_percentile <- function(intervalo,coluna,dados){
  # criando intervalos dos percentis
  q <- quantile(dados %>% pull({{coluna}}),seq(0,1,intervalo)) + rnorm(1,mean = 0.0000005,sd = 0.000001)
  # criando tabela
  tibble(
    values = q,
    next_value = dplyr::lead(q),
    interval = names(q),
    next_interval = dplyr::lead(names(q))
   ) %>%
    unite('intervalo',interval:next_interval,remove = F, na.rm = T) %>%
    dplyr::select(min = values,max = next_value,intervalo) %>%
    na.omit()
  
}

# funcao para verificar a qual percentil o valor esta
check_percentil <- function(value,df_percentile){
  df_percentile %>% mutate(
    check = ifelse(value > min & value < max,1,0)
  ) %>% 
  filter(check == 1) %>% 
  pull(intervalo)
}


fast_check_percentil <- function(value,max,min){
  values <- c(which(value > min),which(value < max))
  ind <- which(duplicated(values))
  paste(names(c(values[ind - 1],values[ind])),collapse = '_')
}




# calculando percentis da temperatura e chuva
df_percent_temp <- create_df_percentile(0.2,temp,dados)
df_percent_precip <- create_df_percentile(0.1,avg_precip,dados)

# criando as dummys na base
dados$dummy_temp <- dados$temp %>% future_map_chr(fast_check_percentil,df_percent_temp$max,df_percent_temp$min) %>% as.factor()
dados$dummy_avg_precip <- dados$avg_precip %>% future_map_chr(fast_check_percentil,
                                                              df_percent_precip$max,
                                                              df_percent_precip$min) %>% as.factor()
# # arredondando
df_percent_precip$min <- round(df_percent_precip$min,1)
df_percent_precip$max <- round(df_percent_precip$max,1)
df_percent_temp$min <- round(df_percent_temp$min,1)
df_percent_temp$max <- round(df_percent_temp$max,1)


# fazendo join com intervalos de graus e precipitacao
dados <- dados %>%
  left_join(df_percent_temp %>% unite('dummy_temp_celcius',min:max),by = c('dummy_temp' = 'intervalo'))
dados <- dados %>%
  left_join(df_percent_precip %>% unite('dummy_precip_milimetro',min:max),by = c('dummy_avg_precip' = 'intervalo'))

# transformando para factor
dados$dummy_temp_celcius <- as.factor(dados$dummy_temp_celcius)
dados$dummy_precip_milimetro <- as.factor(dados$dummy_precip_milimetro)


# criando variavel de tendencia linear
df_trend <- tibble(
  data = dados$data %>% unique(),
  trend = 1:(dados$data %>% unique() %>% length())
)
dados <- dados %>% left_join(df_trend)


# fazendo code do municipio virar factor
dados$code_muni <- as.factor(dados$code_muni)


## FAZENDO COM FIXEST

### sem efeito fixo 

#### OLS
model_ols <- feols(feminicidio  ~ dummy_temp_celcius + dummy_precip_milimetro,dados)

#### Poisson 
model_poisson <- fepois(feminicidio  ~ dummy_temp_celcius + dummy_precip_milimetro, dados)

#### Binomial Negativo
model_bn <- fenegbin(feminicidio  ~ dummy_temp_celcius + dummy_precip_milimetro, dados)

### Com efeitos fixos

#### OLS
model_ols_fixed <- feols(feminicidio  ~ dummy_temp_celcius + dummy_precip_milimetro | data + code_muni, dados)

#### Poisson 
model_poisson_fixed <- fepois(feminicidio  ~ dummy_temp_celcius + dummy_precip_milimetro | data + code_muni, dados)

#### Binomial Negativo
model_bn_fixed <- fenegbin(feminicidio  ~ dummy_temp_celcius + dummy_precip_milimetro | data + code_muni, dados)


# Tabela sem efeitos fixos
etable(model_ols,model_poisson,model_bn)

# Tabela com efeitos fixos
etable(model_ols_fixed,model_poisson_fixed,model_bn_fixed)


## Testando com lag

#### OLS
model_ols <- feols(feminicidio  ~ lag(dummy_temp_celcius) + lag(dummy_precip_milimetro),dados)

#### Poisson 
model_poisson <- fepois(feminicidio  ~ lag(dummy_temp_celcius) + lag(dummy_precip_milimetro), dados)

#### Binomial Negativo
model_bn <- fenegbin(feminicidio  ~ lag(dummy_temp_celcius) + lag(dummy_precip_milimetro), dados)

### Com efeitos fixos

#### OLS
model_ols_fixed <- feols(feminicidio  ~ lag(dummy_temp_celcius) + lag(dummy_precip_milimetro)| data + code_muni, dados)

#### Poisson 
model_poisson_fixed <- fepois(feminicidio  ~ lag(dummy_temp_celcius) + lag(dummy_precip_milimetro)| data + code_muni, dados)

#### Binomial Negativo
model_bn_fixed <- fenegbin(feminicidio  ~ lag(dummy_temp_celcius) + lag(dummy_precip_milimetro)| data + code_muni, dados)


# Tabela sem efeitos fixos
etable(model_ols,model_poisson,model_bn)

# Tabela com efeitos fixos
etable(model_ols_fixed,model_poisson_fixed,model_bn_fixed)
