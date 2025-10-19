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

library(tidyverse)
library(readxl)

relatorio<-read_xlsx("relatorio_old_town_road.xlsx")
relatorio_vendas<- read_excel("relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
infos_produtos<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_produtos")
infos_vendas<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_vendas")
infos_funcionarios<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_funcionarios")
infos_cidades<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_cidades")
infos_clientes<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_clientes")
infos_funcionarios<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_funcionarios")
infos_lojas<- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_lojas")

infos_clientes <- infos_clientes %>%
  rename(ClientID=Cli3ntID) %>%
  rename(WeightID=Weight_lbs) %>%
  rename(HeightID=Height_dm)

library(dplyr)

## criar uma tabela de peso e altura para comparar as variáveis

peso_altura<- infos_clientes %>%
  mutate(HeightID=HeightID*10) %>%
  mutate(WeightID=WeightID*0.45)

peso_altura<- peso_altura %>%
  select(HeightID,WeightID)

grafico_peso_altura<-ggplot(peso_altura, aes(x = HeightID, y = WeightID)) +
  geom_jitter(colour = "#A11D21", size = 1.3) +
  labs(
    x = "Altura do cliente em centímetros",
    y = "Peso do cliente em quilogramas"
  ) +
  theme_estat()

