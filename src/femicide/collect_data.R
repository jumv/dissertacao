
# pacotes
library(microdatasus)
library(dplyr)
library(lubridate)
library(stringdist)


# Obtendo os dados
df <- fetch_datasus(year_start = 2000, year_end = 2019,information_system = "SIM-DO",
                        vars = c('DTOBITO','SEXO','LOCOCOR','CODMUNOCOR','CIRCOBITO','IDADEanos',
                                 'RACACOR','ESTCIV','ESC'))

# lendo via disco casos ja tenhamos os dados
#df <- readr::read_csv('src/femicide/data/df_obitos.csv')
uniques_codes <- readr::read_rds('src/femicide/data/unique_codes_ibge_mun.rds')



# Filtrando 
df_feminicidio <- df %>%
  filter(SEXO == 2, # SEXO FEMININO 
         LOCOCOR == 3, # LOCALIDADE DA OCORRENCIA (domiciliar)
         CIRCOBITO == 3) # TIPO DE OBITO (homicidio)


# Pegando mes e ano
df_feminicidio <- df_feminicidio %>%
  mutate(ano = year(dmy(DTOBITO)),
         mes = month(dmy(DTOBITO)))

# tratando codigos de municipios com apenas 6 digitos
dist_words <- function(word,list_words){
  index <- stringsim(word,list_words,method = 'lv') %>% which.max()
  list_words[index]
}

df_feminicidio$cod_mun <- df_feminicidio$CODMUNOCOR %>% 
  as.character() %>%
  map_chr(dist_words,uniques_codes)


# Agrupando por municipio,ano,mes
df_feminicidio_agg <- df_feminicidio %>% 
  count(cod_mun,ano,mes,name = 'Total_homicidio') %>%
  arrange(desc(Total_homicidio))


# Salvando 
df %>% readr::write_csv('src/femicide/data/df_obitos.csv')
df_feminicidio_agg %>% readr::write_csv('src/femicide/data/df_feminicidio.csv')
