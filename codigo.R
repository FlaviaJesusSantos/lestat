rm(list=ls(all=TRUE))


###########################
#### Leitura dos Dados ####
###########################

library(readxl)
library(tidyverse)
library(viridis)
library(lubridate)

menor_1 <- read_excel("menor_1.xlsx", 
                      col_types = c("text", "numeric", "numeric", 
                                    "text"))

um_quatro <- read_excel("um_quatro.xlsx", 
                        col_types = c("text", "numeric", "numeric", 
                                      "text"))

cinco_nove <- read_excel("cinco_nove.xlsx", 
                         col_types = c("text", "numeric", "numeric", 
                                       "text"))

dez_quatorze <- read_excel("dez_quatorze.xlsx", 
                           col_types = c("text", "numeric", "numeric", 
                                         "text"))

quinze_dezenove <- read_excel("quinze_dezenove.xlsx", 
                              col_types = c("text", "numeric", "numeric", 
                                            "text"))


##############################################################################

###########################
#### Juntando os Dados ####
###########################

dados2<- rbind(menor_1, um_quatro, cinco_nove, dez_quatorze, quinze_dezenove)

dados2 <- na.exclude(dados2) #Exclui NA


###Separa por região

dados2 <- dados2 %>%
  separate(`Macrorregião de Saúde`, c("regiao", "macrorregiao")) %>%
  mutate(
    regiao = case_when(
      regiao = 1101 & regiao <= 1702 ~ "Norte",
      regiao = 2109 & regiao <= 2918 ~ "Nordeste",
      regiao = 3101 & regiao <= 3534 ~ "Sudeste",
      regiao = 4105 & regiao <= 4314 ~ "Sul",
      regiao = 5005 & regiao <= 5305 ~ "Centro-Oeste")) 


### Agrupa por ano
dados_agrupados <- dados2 %>%
  group_by(ano) %>%
  summarise(casos_soma = sum(casos))

ggplot(dados_agrupados, aes(ano, casos_soma))+
  geom_line(color = "purple", size = 0.9) +
  theme_minimal() +
  labs(x = "Ano",
       y = "Casos de óbito por leucemia") +
  scale_y_continuous(
    limits = c(0, 1100),
    breaks = seq(0, 1100, 100)
  )


#############

###Agrupa por faixa_etaria
dados_agrupados1 <- dados2 %>%
  group_by(faixa_etaria, ano) %>%
  summarise(casos_soma = sum(casos))

ggplot(dados_agrupados1, aes(ano, casos_soma, color = faixa_etaria))+
  geom_line(size = 0.9) +
  theme_minimal() +
  labs(x = "Ano",
       y = "Casos de óbito por leucemia",
       color = "Faixa etária") +
  scale_y_continuous(
    limits = c(0, 400),
    breaks = seq(0, 400, 50)
  ) +
  scale_color_discrete(breaks = c("menor que 1", "1-4 anos", "5-9 anos",
                                  "10-14 anos",
                                  "15-19 anos"),
                       labels = c("< 1", "1 a 4 anos",
                                  "5 a 9 anos", "10 a 14 anos", "15 a 19 anos"))

####################

###Agrupa por regiao
dados_agrupados2 <- dados2 %>%
  group_by(regiao, ano) %>%
  summarise(casos_soma = sum(casos))


ggplot(dados_agrupados2, aes(ano, casos_soma, color = regiao))+
  geom_line(size = 0.9) +
  theme_minimal() +
  labs(x = "Ano",
       y = "Casos de óbito por leucemia",
       color = "Região do Brasil") +
  scale_y_continuous(
    limits = c(0, 450),
    breaks = seq(0, 450, 50)
  )
  

#########################

#Mapa de casos regiao

library(sf)

dados_agrupados3 <- dados2 %>%
  group_by(regiao) %>%
  summarise(casos_soma = sum(casos))

shp1 <- st_read("regioes_2010.shp")

mapa <- merge(shp1, dados_agrupados3, by.x = "nome", by.y = "regiao", all = TRUE)


ggplot(mapa) + geom_sf(aes(fill = dados_agrupados3$casos_soma)) +
  scale_fill_viridis(alpha=0.80, direction = -1) +
  hrbrthemes::theme_ipsum() +labs(fill = "Casos de óbito por leucemia")


