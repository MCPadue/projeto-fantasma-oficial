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

##Análise 1

##código pra limpar o banco

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

infos_vendas <- infos_vendas %>% 
  rename(SaleID = Sal3ID)
infos_vendas
infos_produtos <- infos_produtos %>%
  rename(ItemID = Ite3ID)
infos_produtos
infos_lojas <- infos_lojas %>%
  rename(StoreID = Stor3ID)
infos_lojas

relatorio_vendas<-relatorio_vendas|>
  mutate(Ano=year(Date))

library(dplyr)

# left_join() serve para juntar duas tabelas com coluna em comum: left_join(tabela1, tabela2, by = "nome_coluna_em_comum")

tabela_vendas_raw <- left_join(relatorio_vendas, infos_vendas, by = "SaleID")

tabela_vendas <- left_join(tabela_vendas_raw, infos_produtos, by = "ItemID")



 

# a função %>% vai receber a mudança que será feita, que no caso vai ser a adição de mais uma coluna na planilha, quem adiciona essa coluna é a função mutate

#criando uma nova tabela relatorio_vendas a partir da tabela_vendas com as novas colunas total_compra e media_por_ano
#a função group_by serviu para separar o total da compra sendo ano por ano, e assim fazer a média por ano
relatorio_vendas<-tabela_vendas %>%
  mutate(total_compra = Quantity * UnityPrice) %>%
  group_by(Ano,total_compra)%>%
  mutate(media_por_ano= mean(total_compra * 5.31)) 

nova_tabela<-relatorio_vendas %>%
  group_by(Ano)%>%
  summarise(soma_medias=sum(media_por_ano))

media_tabela<- nova_tabela %>%
  group_by(Ano) %>%
  summarise(media_geral = (soma_medias/18))




# Instalando cores estat
cores_estat <- c(
  "#A11D21", "#003366", "#663333", "#CC9900", "#006606",
  "#CC9966", "#999966", "#FF6600", "#008091", "#041835",
  "#666666")


# Instalando a função theme_estat
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat),
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',',
                                       #accuracy = 0.01,
                                       big.mark = "."))
    )
  )
}

# Criando o gráfico com a nova_tabela com as variaveis Ano e soma_medias
ggplot(media_tabela) +
  aes(x = Ano, y = media_geral, group = 1) +
  geom_line(size = 1, colour = "#A11D21") +
  geom_point(colour = "#A11D21", size = 2) +
  labs(x = "Ano", y = "Media") +
  theme_estat() +
  scale_x_continuous(breaks = seq(1880, 1889, by = 1))


