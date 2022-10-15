
library(tidyverse)
library(lubridate)
library(sidrar)


# lendo os dados
air_temp <- read_csv2('data/weather_data/udel_monthly_avg_air_temp_br_mun_long_buffer.csv')
precip <- read_csv2('data/weather_data/udel_monthly_avg_precip_br_mun_long_buffer.csv')
feminicide <- read_csv('data/femicide/df_feminicidio_model.csv')
df_feminicidio <- read_csv('data/femicide/df_feminicidio.csv')
pop1 <- read_rds('data/pop/pop1.rds')
pop2 <- read_rds('data/pop/pop2.rds')


feminicide <- df_feminicidio %>% 
  filter(ESC %in% c(5)) %>%
  count(cod_mun,ano,mes,name = 'Total_homicidio') %>%
  arrange(desc(Total_homicidio))



# buscando dados no sidra
decada1 <- seq(2001,2010) %>% as.character()
decada2 <- seq(2011,2017) %>% as.character()
#pop1 <- sidrar::get_sidra(6579,period = decada1,geo = 'City') %>% 
#pop2 <- sidrar::get_sidra(6579,period = decada2,geo = 'City') %>% 

# juntando no pop
pop <- bind_rows(pop1,pop2) %>% dplyr::select(ano = Ano,code_muni = `Município (Código)`,populacao = Valor) %>%
  unite('unique_key',code_muni:ano,sep = '_',remove = F) %>% dplyr::select(unique_key,populacao)


# # criando colunas de ano e mes
# air_temp <- air_temp %>% 
#   mutate(
#     ano = year(ymd(data)),
#     mes = month(ymd(data))
# )
# 
# precip <- precip %>%
#   mutate(
#     ano = year(ymd(data)),
#     mes = month(ymd(data))
# )
#   
# 
# # filtrando dados anuais
# air_temp <- air_temp %>% 
#   filter(ano >= 2000)
# 
# precip <- precip %>% 
#   filter(ano >= 2000)


precip <- read_rds('data/weather_data/precip.rds')
air_temp <- read_rds('data/weather_data/air_temp.rds')



# criando chaves unicas para fazer o join
air_temp <- air_temp %>% 
  relocate(code_muni,.after = temp) %>%
  unite('unique_key',code_muni:mes,sep = '_',remove = F)

precip <- precip %>% 
  relocate(code_muni,.after = precip) %>%
  unite('unique_key',code_muni:mes,sep = '_',remove = F) %>%
  dplyr::select(unique_key,precip)

feminicide <- feminicide %>%
  unite('unique_key',cod_mun:mes,sep = '_',remove = F) %>%
  dplyr::select(unique_key,Total_homicidio)

# fazendo join
df_join <- list(air_temp,precip,feminicide) %>%
  reduce(left_join) %>%
  dplyr::select(code_muni,data,ano,mes,temp,precip,feminicidio = Total_homicidio)

# susbtituindo na por zero
df_join <- df_join %>% 
  replace_na(list(feminicidio = 0))

# criando unique key para fazer join com pop
df_join <- df_join %>% relocate(ano,.after = code_muni) %>%
  unite('unique_key',code_muni:ano,sep = '_',remove = F) %>%
  left_join(pop) 

# preenchendo nas com o ultimo ou proximo valor
df_join <-  df_join %>% 
  fill(populacao,.direction = 'downup')

# salvando no disco
df_join %>%
  write_csv('data/df_join_12anos+.csv')






dados %>%
  group_by(code_muni) %>%
  mutate(temp = dplyr::lag(temp,1),
         precip = dplyr::lag(precip,1)) %>%
  ungroup() %>%
  na.omit()

  


