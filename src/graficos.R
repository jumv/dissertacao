
library(tidyverse)
library(geobr)
library(ggthemes)
library(lubridate)

# lendo os dados
df_feminicide <- read_csv('data/femicide/df_feminicidio.csv')
df_code_state <- read_csv('data/pop/code_states.csv',col_types = 'cc')
df_pop <- read_csv('data/pop/pop_mun.csv')[-1,]
state <- read_state(year=2017)
state$code_state <- as.character(state$code_state)

# Criando coluna de identificacao do estado na base de feminicidio
df_feminicide$code_state <- str_extract(df_feminicide$cod_mun, "^.{2}")
df_pop$code_state <- str_extract(df_pop$cod_mun, "^.{2}")

# pegando dados de pop apenas de 2001 e 2017
df_pop_state <- df_pop %>% select(`2017`,cod_mun,code_state) %>% 
  gather('ano','populacao',-c(cod_mun,code_state)) %>%
  group_by(code_state,ano) %>%
  summarise(pop = sum(as.numeric(populacao),na.rm = T))


# juntando as bases
df_feminicide_state <- df_feminicide %>%
  left_join(df_code_state,by = c('code_state' = 'code'))

df_feminicide_state %>% write_csv('df_feminicidio_estado.csv')

# Proporcao de homicidio por estado
d <- df_feminicide_state %>% 
  filter(ano == 2017) %>%
  count(code_state,name = 'Total')
  
d <- d %>%
left_join(df_pop_state) %>%
  left_join(state) %>%
  mutate(prop = (Total/pop)*100000)

ggplot() +
  geom_sf(data = d,aes(fill=prop,geometry = geom), color= NA, size=.15)  +
  scale_fill_distiller(palette = "Blues", limits = c(0,2),direction = 0) +
  theme_minimal() +
  labs(title="Taxa de feminicidio por 100 mil habitantes, 2017", size=8,
       caption = 'Fonte: Do Autor (SUS + IBGE)')

# Proporcao por cor
d <- df_feminicide_state %>%
  count(RACACOR) %>%
  replace_na(list(RACACOR = '6'))
d$cor <- c('branca','preta','amarela','parda','indigena','outras')

# Juntando preto e pardo
d <- d %>% mutate(cor = case_when(
  cor == 'preta' ~ 'pardo/preto',
  cor == 'parda' ~ 'pardo/preto',
  TRUE ~ cor
)) %>% group_by(cor) %>% summarise(total = sum(n)) %>%
  ungroup() %>% mutate(prop = (total/sum(total)*100))


d %>%
  ggplot(aes(x = reorder(cor,prop),y = prop,fill = cor)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_economist() +
  scale_fill_economist() +
  labs(title = 'Total de feminicidios por cor',
       y = '',x = '',
       caption = 'Notas: O gráfico ilustra a percentagem por raça de mulheres mortas em casa no ano de 2017
                 Fonte: SUS') +
  geom_text(aes(label=paste(round(prop,1),'%',sep = '')),hjust = -0.1,
            colour = "black", size = 5) +
  theme(panel.grid.major = element_blank(),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.09, size = 20),
        legend.position = "off")
  
# proporcao por escolaridade

d <- df_feminicide_state %>%
  count(ESC) %>%
  replace_na(list(ESC = 'Nao Informado')) %>%
  left_join(
    tibble(
      ESC = c('0','1','2','3','4','5','9','Nao Informado'),
      anos_escolaridade = c('Nao Informado','0 Anos','1-3 Anos','4-7 Anos','7-9 Anos','12 Anos ou mais','9-11 Anos','Nao Informado')
    )
  ) %>%
  group_by(anos_escolaridade) %>% summarise(total = sum(n)) %>%
  mutate(prop = (total/sum(total))*100)


d %>%
  ggplot(aes(x = reorder(anos_escolaridade,prop),y = prop,fill = anos_escolaridade)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_economist() +
  scale_fill_economist() +
  geom_text(aes(label=paste(round(prop,1),'%',sep = '')),hjust = -0.1,
            colour = "black", size = 5) +
  labs(title = 'Total de feminicidios por anos de escolaridade',
       y = '',x = '',
       caption = '
       O gráfico ilustra a porcentagem por anos de escolaridade das mulheres mortas em casa no ano de 2017
       Fonte: SUS') +
  theme(panel.grid.major = element_blank(),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.30, size = 20),
        legend.position = "off")

# proporcao por ano
df_feminicide_state %>%
  count(ano,name = 'Total_Feminicidio') %>%
  ggplot(aes(ano,n)) +
  geom_line(size = 1) +
  theme_economist() +
  labs(title = 'Total de feminicidios por anos',
       y = '',x = '',
       caption = 'Fonte: Do Autor (SUS)')

df_feminicide_state %>%
  count(ano,name = 'Total_Feminicidio') %>%
  formattable()
  

# 
df_feminicide %>% mutate(IDADE = as.numeric(IDADE) - 400) %>%
  filter(IDADE > 0,IDADE < 100) %>%
  ggplot(aes(x = IDADE)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme_economist() +
  scale_fill_economist() +
  labs(y = '',x = 'Idade',
       caption = 'Esse gráfico ilustra a distribuição por idade das mulheres mortas em casa no ano de 2017
       Fonte: SUS') +
  theme(panel.grid.major = element_blank(),
        axis.line=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.30, size = 20),
        legend.position = "off")


# Distribuicao dos hobitos por hora e dia
df_feminicide %>%
  mutate(day_week = weekdays(dmy(DTOBITO))) %>%
  count(day_week) %>% 
  mutate(prop = 100*n/sum(n)) %>%
  ggplot(aes(x = reorder(day_week,prop),y = prop,fill = 'darkblue')) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_economist() +
  scale_fill_economist() +
  geom_text(aes(label=paste(round(prop,1),'%',sep = '')),hjust = -0.1,
            colour = "black", size = 5) +
  labs(y = '',x = '',
       caption = 'Gráfico que ilustra a proporçãp por dia da semana das mortes de mulheres em casa
       Fonte: SUS') +
  theme(panel.grid.major = element_blank(),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.30, size = 20),
        legend.position = "off")





