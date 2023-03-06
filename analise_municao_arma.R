library(dplyr)
library(tidyr)
library(geobr)
library(readr)
library(readxl)
library(ggplot2)


df = read_excel('ArmasApreendidasEvolucaoCisp.xlsx')

# Observando o total de apreenções por CISP

df_2022 = df %>% filter(ano == 2022)

cisp_2022 = df_2022 %>% group_by(cisp) %>% summarise(total_2022 = sum(total)) %>% 
  arrange(desc(total_2022)) %>% head(15)

por_ano_cisp = df %>% group_by(ano) %>% summarise(total = sum(total)) %>% 
  arrange(desc(total)) %>% filter(ano != 2023)

# Total de apreenção de armas de fogo por ano
ggplot(data = por_ano_cisp, aes(x=ano, y = total))+
  geom_line(col = 'black')+
  geom_point(size = 3, shape = 21, fill = 'white') +
  geom_text(aes(label = total), vjust = -0.5)+
  scale_y_continuous(limits = c(0, 12000))+
  scale_x_continuous(breaks = seq(2000, 2022, 2))+
  labs(title = 'Total de Armas de Fogo Apreendidas entre 2007 - 2022',
       x='Ano',
       y = 'Ocorrências',
       subtitle = 'Fonte - ISP RJ'
       )+
  theme_classic()


# Grafico de barra do ano de 2022 por cisp, 

cisp_2022$cisp = as.factor(cisp_2022$cisp)

ggplot(data = cisp_2022, aes(x=cisp, y = total_2022))+
  geom_col(fill = '#028f76', col = 'black')+
  geom_text(aes(label=total_2022), vjust = -0.5)+
  labs(title = 'Apreenção de Armas de Fogo no Rio de Janeiro - Top 15 CISP',
       subtitle = "Fonte - ISP-RJ",caption = 'Produzido por Christian Basilio')


# total de ocorrências em 2022
sum(df_2022$total)
# 6705 armas de fogo apreendidas

# Analisando dados sobre munições

municao = read_excel('ArmasApreendidasEvolucaoCisp.xlsx', sheet = 4)

municao_porano = municao %>% group_by(ano) %>% filter(ano != 2023) %>% summarise(total = sum(total)) 

# Grafico de muniçoes por ano


ggplot(data = municao_porano, aes(x = ano, y = total)) +
  geom_line() +
  geom_text(aes(label = total), vjust = -0.7, hjust = 1) +
  geom_point(shape = 21, size = 3, fill = 'red') +
  scale_y_continuous(limits = c(0, 230000),
                     labels = function(x) paste0(x/1000, "K")) +
  scale_x_continuous(breaks = seq(2014, 2022, 1)) +
  labs(title = 'Total de Munições Apreendidas entre 2014 e 2022',
       subtitle = 'Fonte: ISP-RJ',
       caption = 'Produzido por Christian Basilio',
       x = 'Ano',
       y = 'Apreensões (em milhares)')+
  theme_classic()

# Analisando a quantidade de apreenções no ano de 2022 por CISP

cisp_muni_2022 = municao %>% filter(ano == 2022) %>% group_by(cisp) %>% summarise(total = sum(total)) %>% 
  arrange(desc(total))

top_cisp_muni_2022 = cisp_muni_2022 %>% head(15)

# Transformando os dados de numerico para Factor
top_cisp_muni_2022$cisp = top_cisp_muni_2022$cisp %>% as.factor()


#Plot dos graficos - top 15 cisp apreenções
ggplot(data = top_cisp_muni_2022, aes(x = reorder(cisp, -total), y = total)) +
  geom_col(fill = '#028f76', col = 'black') +
  geom_text(aes(label = total), vjust = -0.5) +
  labs(title = "Total de Munições Apreendidas por CISPs em 2022",
       subtitle = "Fonte: ISP-RJ",
       x = "CISPs",
       y = "Apreensões") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_classic()
