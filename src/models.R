

library(tidyverse)
library(tictoc)
library(furrr)
library(plm)
library(MASS)
library(pscl)
library(vcd)
library(pglm)
library(stargazer)

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
df_percent_temp <- create_df_percentile(0.1,temp,dados)
df_percent_precip <- create_df_percentile(0.2,avg_precip,dados)

# criando as dummys na base
dados$dummy_temp <- dados$temp %>% future_map_chr(fast_check_percentil,df_percent_temp$max,df_percent_temp$min) %>% as.factor()
dados$dummy_avg_precip <- dados$avg_precip %>% future_map_chr(fast_check_percentil,
                                                              df_percent_precip$max,
                                                              df_percent_precip$min) %>% as.factor()
# # arredondando
# df_percent_precip$min <- round(df_percent_precip$min,1)
# df_percent_precip$max <- round(df_percent_precip$max,1)
# df_percent_temp$min <- round(df_percent_temp$min,1)
# df_percent_temp$max <- round(df_percent_temp$max,1)


# # fazendo join com intervalos de graus e precipitacao
# dados <- dados %>% 
#   left_join(df_percent_temp %>% unite('dummy_temp_celcius',min:max),by = c('dummy_temp' = 'intervalo'))
# dados <- dados %>% 
#   left_join(df_percent_precip %>% unite('dummy_precip_milimetro',min:max),by = c('dummy_avg_precip' = 'intervalo'))

# transformando para factor
# dados$dummy_temp_celcius <- as.factor(dados$dummy_temp_celcius)
# dados$dummy_precip_milimetro <- as.factor(dados$dummy_precip_milimetro)


# # criando variavel de tendencia linear
# df_trend <- tibble( 
#   data = dados$data %>% unique(),
#   trend = 1:(dados$data %>% unique() %>% length())
# ) 
# dados <- dados %>% left_join(df_trend)


# fazendo code do municipio virar factor
dados$code_muni <- as.factor(dados$code_muni)


# fazendo modelo de regressao multipla
model_simple <- lm(feminicidio ~ dummy_temp + dummy_avg_precip,
                   data = dados)

# fazendo modelo de regressao com efeitos fixos de tempo e municipio
model_fixed_effects <- plm(feminicidio ~ dummy_temp + dummy_avg_precip,
                          index = c("data", "code_muni"), 
                          effect = "twoways", model = "within",
                          data = dados)

# fazendo modelo de poisson
model_poisson <- glm(
  feminicidio ~ dummy_temp + dummy_avg_precip,
  family = 'poisson', data = dados
)

# fazendo modelo de efeitos fixos de poisson
model_fixed_effects_poisson <- pglm(
  feminicidio ~ dummy_temp_celcius + dummy_avg_precip,
  index = c("data", "code_muni"), 
  effect = "twoways", model = "within",
  family = poisson,
  data = dados
)

# fazendo modelo binomial negativo
model_bn <- glm.nb(feminicidio ~ dummy_temp + dummy_avg_precip,
                   data = dados)

# salvando os modelos
write_rds(model_simple,file = 'models/simple_regression.rds')
write_rds(model_fixed_effects,file = 'models/linear_fixed_effects.rds')
write_rds(model_poisson,file = 'models/simple_poisson.rds')
write_rds(model_bn,file = 'models/bn.rds')
write_rds(model_fixed_effects_poisson ,file = 'models/poisson_fixed_effects.rds')

# carregando
# model_simple <- read_rds(file = 'models/simple_regression.rds')
# model_fixed_effects <- read_rds(file = 'models/linear_fixed_effects.rds')
# model_poisson <- read_rds(file = 'models/simple_poisson.rds')
# model_bn <- read_rds(file = 'models/bn.rds')
# model_fixed_effects_poisson <- read_rds(file = 'models/poisson_fixed_effects.rds')


# renomeando covariaveis
precip <- df_percent_precip %>% mutate(min = round(min),max = round(max)) %>% unite('precip',min:max,sep = '-') %>% pull(precip) %>% paste('Milimetros')
temp <- df_percent_temp %>% mutate(min = round(min),max = round(max)) %>% unite('temp',min:max,sep = '-') %>% pull(temp) %>% paste('Celcius')


# estatisticas
stargazer(model_simple, model_fixed_effects,model_poisson,model_bn, title="Results", align=TRUE,type="text",
          covariate.labels = c(temp,precip),
          out = 'results/results.png')

