source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #


## Teremos que trabalhar com CityID(Ambar Seco), Age:infos_clientes,StoreID:infos_lojas

## Primeira coisa, vamos organizar os nomes
rm(infos_lojas)
rm(infos_cidades)
infos_lojas<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_lojas")
infos_cidades<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_cidades")

infos_lojas<- infos_lojas %>%
  rename(StoreID=Stor3ID)
infos_cidades<-infos_cidades %>%
  rename(CityID=C1tyID)





## Criando uma nova tabela com infos_clientes,infos_lojas e infos_cidades
## 1- selecionamos a tabela info_cidades e infos_lojas e usamos a coluna CityID,que é a coluna em comum entre elas, para juntá-las. 
## 2- fazemos a mesma coisa com tabela_organizar e relatorio_vendas e tabela_organizar e infos_clientes
## 3- criamos uma nova tabela, filtrando a tabela_organizar apenas para ambar seco, a partir de filter.

new_relatorio_vendas<- relatorio_vendas

tabela_organizar<- inner_join(infos_cidades,infos_lojas,by="CityID")

tabela_organizar<-inner_join(tabela_organizar,new_relatorio_vendas,by="StoreID")
tabela_organizar<-inner_join(tabela_organizar,infos_clientes,by="ClientID")

tabela_organizar_filtrada <- tabela_organizar %>%
  filter(NameCity == "Âmbar Seco")





  


## O gráfico mais adequado será um boxplot bivariado, pois queremos saber como as idades dos clientes variam dependendo da loja.

install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

grafico_ambarseco<-ggplot(tabela_organizar_filtrada, aes(x =reorder(NameStore,Age,Fun=median) , y = Age)) +
  geom_boxplot(fill = "#A11D21", width = 0.7) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "grey"
  ) +
  labs(
    x = "Loja da Cidade Âmbar Seco",
    y = "Idade do Cliente",
    title = "Perfis das Idades dos Clientes por Loja — Âmbar Seco"
  ) +
  theme_estat()




  
