
library(tidyverse)
library(geobr)
library(ggthemes)

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
       caption = 'Fonte: Do Autor (SUS)') +
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
  mutate(prop = (n/sum(n))*100)

d %>%
  ggplot(aes(x = reorder(ESC,prop),y = prop,fill = ESC)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_economist() +
  scale_fill_economist() +
  geom_text(aes(label=paste(round(prop,1),'%',sep = '')),hjust = -0.1,
            colour = "black", size = 5) +
  labs(title = 'Total de feminicidios por anos de escolaridade',
       y = '',x = '',
       caption = 'Fonte: Do Autor (SUS)') +
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
  labs(y = '',x = '',
       caption = 'Fonte: Do Autor (SUS)') +
  theme(panel.grid.major = element_blank(),
        axis.line=element_blank(),
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
       caption = 'Fonte: Do Autor (SUS)') +
  theme(panel.grid.major = element_blank(),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.30, size = 20),
        legend.position = "off")





