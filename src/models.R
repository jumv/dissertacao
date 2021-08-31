

library(tidyverse)
library(tictoc)
library(furrr)
library(plm)
library(MASS)
library(pscl)
library(vcd)
library(pglm)

future::plan(multiprocess)


# lendo os dados
dados <- read_csv('src/data/df_join.csv')

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
  paste(names(c(values[ind - 1],values[ind])),collapse = '-')
}




# calculando percentis da temperatura e chuva
df_percent_temp <- create_df_percentile(0.05,temp,dados)
df_percent_precip <- create_df_percentile(0.05,avg_precip,dados)

# criando as dummys na base
dados$dummy_temp <- dados$temp %>% future_map_chr(fast_check_percentil,df_percent_temp$max,df_percent_temp$min) %>% as.factor()
dados$dummy_avg_precip <- dados$avg_precip %>% future_map_chr(fast_check_percentil,
                                                              df_percent_precip$max,
                                                              df_percent_precip$min) %>% as.factor()

# criando variavel de tendencia linear
df_trend <- tibble( 
  data = dados$data %>% unique(),
  trend = 1:(dados$data %>% unique() %>% length())
) 
dados <- dados %>% left_join(df_trend)


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
model_poisson <- pglm(
  feminicidio ~ dummy_temp + dummy_avg_precip,
  family = 'poisson', data = dados
)

# fazendo modelo de efeitos fixos de poisson
model_fixed_effects_poisson <- pglm(
  feminicidio ~ dummy_temp + dummy_avg_precip,
  index = c("data", "code_muni"), 
  effect = "twoways", model = "within",
  family = poisson,
  data = dados
)

# fazendo modelo binomial negativo
model_bn <- glm.nb(feminicidio ~ dummy_temp + dummy_avg_precip,
                   data = dados)


# estatisticas
summary(model_simple)
summary(model_fixed_effects)
summary(model_poisson)
summary(model_fixed_effects_poisson)
summary(model_bn)

