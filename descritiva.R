library(readxl)
library(tidyverse)
flavia <- read_excel("flavia.xlsx", col_types = c("text",
                                                    "text", "numeric", "numeric", "numeric",
                                                    "numeric", "numeric"))

dados<- flavia



dados<- mutate(dados, soma = dados$`menor que 1`+dados$`1-4 anos`+ dados$`5-9 anos`+
                 dados$`10-14 anos`+dados$`15-19 anos`)


View(dados)

dados_anos <- dados %>%
  group_by(ano) %>%
  summarise(casos = sum(soma))

View(dados_anos)


dados_mapa<- dados %>%
  group_by(`Macrorregião de Saúde`) %>%
  summarise(casos_mapa = sum(soma))

View(dados_mapa)


anos_casos <- data.frame(Anos = c("1996-2000", "2001-2005", "2006-2010", "2011-2015", "2016-2020"),
                         Casos = c(4763,4901,4851,4627,4188),
                         stringsAsFactors = FALSE)




###########MAPA)
library(sf)
library(readr)
library(viridis)



#dados <- dados %>%
#separate("Macrorregião de Saúde", into = c("n", "regiao"))



shp1 <- st_read("regiao_saude.shp"); View(shp1)

shp2<- st_read("a__031_002_macrorregioesBrasil.shp");View(shp2)

shp3<- st_read("MACRO_PPI.shp");View(shp3)

mapa <- merge(shp1, dados_mapa, by.x = "co_colegia", by.y = "Macrorregião de Saúde", all = TRUE)


ggplot(mapa) + geom_sf(aes(fill = dados_mapa$casos_mapa)) +
  scale_fill_viridis(alpha=0.80, direction = -1) +
  hrbrthemes::theme_ipsum() +labs(fill = "Casos")





