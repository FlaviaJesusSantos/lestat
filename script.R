library(data.table)
library(tidyverse)
library(lubridate)

menor1 <- read.csv2("menor1.csv")
head(teste1)

menor1 <- rename(menor1, c("1996" = X1996, "1997" = X1997, "1998" = X1998, 
                 "1999" = X1999, "2000" = X2000, "2001" = X2001,
                 "2002" = X2002, "2003" = X2003, "2004" = X2004,
                 "2005" = X2005, "2006" = X2006, "2007" = X2007,
                 "2008" = X2008, "2009" = X2009, "2010" = X2010,
                 "2011" = X2011, "2012" = X2012, "2013" = X2013,
                 "2014" = X2014, "2015" = X2015, "2016" = X2016,
                 "2017" = X2017, "2018" = X2018, "2019" = X2019,
                 "2020" = X2020,
                 "Macrorregião de Saúde" = Macrorregião.de.Saúde))

menor1$"1996"[menor1$"1996" == "-"] <- 0
menor1$"1997"[menor1$"1997" == "-"] <- 0
menor1$"1998"[menor1$"1998" == "-"] <- 0
menor1$"1999"[menor1$"1999" == "-"] <- 0
menor1$"2000"[menor1$"2000" == "-"] <- 0
menor1$"2001"[menor1$"2001" == "-"] <- 0
menor1$"2002"[menor1$"2002" == "-"] <- 0
menor1$"2003"[menor1$"2003" == "-"] <- 0
menor1$"2004"[menor1$"2004" == "-"] <- 0
menor1$"2005"[menor1$"2005" == "-"] <- 0
menor1$"2006"[menor1$"2006" == "-"] <- 0
menor1$"2007"[menor1$"2007" == "-"] <- 0
menor1$"2008"[menor1$"2008" == "-"] <- 0
menor1$"2009"[menor1$"2009" == "-"] <- 0
menor1$"2010"[menor1$"2010" == "-"] <- 0
menor1$"2011"[menor1$"2011" == "-"] <- 0
menor1$"2012"[menor1$"2012" == "-"] <- 0
menor1$"2013"[menor1$"2013" == "-"] <- 0
menor1$"2014"[menor1$"2014" == "-"] <- 0
menor1$"2015"[menor1$"2015" == "-"] <- 0
menor1$"2016"[menor1$"2016" == "-"] <- 0
menor1$"2017"[menor1$"2017" == "-"] <- 0
menor1$"2018"[menor1$"2018" == "-"] <- 0
menor1$"2019"[menor1$"2019" == "-"] <- 0
menor1$"2020"[menor1$"2020" == "-"] <- 0


menor1 <- menor1 %>%
  gather("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
         "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
         "2015", "2016", "2017", "2018", "2019", "2020", key = "ano", value = "casos")

writexl::write_xlsx(menor1, "menor_1.xlsx")
menor1 <- readxl::read_xlsx("menor_1.xlsx")  
  
#-------------------------------------------------------------------------------  
um_quatro <- read.csv2("um_quatro.csv")
head(um_quatro)

um_quatro <- rename(um_quatro, c("1996" = X1996, "1997" = X1997, "1998" = X1998, 
                           "1999" = X1999, "2000" = X2000, "2001" = X2001,
                           "2002" = X2002, "2003" = X2003, "2004" = X2004,
                           "2005" = X2005, "2006" = X2006, "2007" = X2007,
                           "2008" = X2008, "2009" = X2009, "2010" = X2010,
                           "2011" = X2011, "2012" = X2012, "2013" = X2013,
                           "2014" = X2014, "2015" = X2015, "2016" = X2016,
                           "2017" = X2017, "2018" = X2018, "2019" = X2019,
                           "2020" = X2020,
                           "Macrorregião de Saúde" = Macrorregião.de.Saúde))

um_quatro$"1996"[um_quatro$"1996" == "-"] <- 0
um_quatro$"1997"[um_quatro$"1997" == "-"] <- 0
um_quatro$"1998"[um_quatro$"1998" == "-"] <- 0
um_quatro$"1999"[um_quatro$"1999" == "-"] <- 0
um_quatro$"2000"[um_quatro$"2000" == "-"] <- 0
um_quatro$"2001"[um_quatro$"2001" == "-"] <- 0
um_quatro$"2002"[um_quatro$"2002" == "-"] <- 0
um_quatro$"2003"[um_quatro$"2003" == "-"] <- 0
um_quatro$"2004"[um_quatro$"2004" == "-"] <- 0
um_quatro$"2005"[um_quatro$"2005" == "-"] <- 0
um_quatro$"2006"[um_quatro$"2006" == "-"] <- 0
um_quatro$"2007"[um_quatro$"2007" == "-"] <- 0
um_quatro$"2008"[um_quatro$"2008" == "-"] <- 0
um_quatro$"2009"[um_quatro$"2009" == "-"] <- 0
um_quatro$"2010"[um_quatro$"2010" == "-"] <- 0
um_quatro$"2011"[um_quatro$"2011" == "-"] <- 0
um_quatro$"2012"[um_quatro$"2012" == "-"] <- 0
um_quatro$"2013"[um_quatro$"2013" == "-"] <- 0
um_quatro$"2014"[um_quatro$"2014" == "-"] <- 0
um_quatro$"2015"[um_quatro$"2015" == "-"] <- 0
um_quatro$"2016"[um_quatro$"2016" == "-"] <- 0
um_quatro$"2017"[um_quatro$"2017" == "-"] <- 0
um_quatro$"2018"[um_quatro$"2018" == "-"] <- 0
um_quatro$"2019"[um_quatro$"2019" == "-"] <- 0
um_quatro$"2020"[um_quatro$"2020" == "-"] <- 0


um_quatro <- um_quatro %>%
  gather("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
         "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
         "2015", "2016", "2017", "2018", "2019", "2020", key = "ano", value = "casos")

writexl::write_xlsx(um_quatro, "um_quatro.xlsx")
um_quatro <- readxl::read_xlsx("um_quatro.xlsx")    
  
## MERGE -----------------------------------------------------------------------
cinco_nove <- read.csv2("cinco_nove.csv")
head(cinco_nove)

cinco_nove <- rename(cinco_nove, c("1996" = X1996, "1997" = X1997, "1998" = X1998, 
                                 "1999" = X1999, "2000" = X2000, "2001" = X2001,
                                 "2002" = X2002, "2003" = X2003, "2004" = X2004,
                                 "2005" = X2005, "2006" = X2006, "2007" = X2007,
                                 "2008" = X2008, "2009" = X2009, "2010" = X2010,
                                 "2011" = X2011, "2012" = X2012, "2013" = X2013,
                                 "2014" = X2014, "2015" = X2015, "2016" = X2016,
                                 "2017" = X2017, "2018" = X2018, "2019" = X2019,
                                 "2020" = X2020,
                                 "Macrorregião de Saúde" = Macrorregião.de.Saúde))

cinco_nove$"1996"[cinco_nove$"1996" == "-"] <- 0
cinco_nove$"1997"[cinco_nove$"1997" == "-"] <- 0
cinco_nove$"1998"[cinco_nove$"1998" == "-"] <- 0
cinco_nove$"1999"[cinco_nove$"1999" == "-"] <- 0
cinco_nove$"2000"[cinco_nove$"2000" == "-"] <- 0
cinco_nove$"2001"[cinco_nove$"2001" == "-"] <- 0
cinco_nove$"2002"[cinco_nove$"2002" == "-"] <- 0
cinco_nove$"2003"[cinco_nove$"2003" == "-"] <- 0
cinco_nove$"2004"[cinco_nove$"2004" == "-"] <- 0
cinco_nove$"2005"[cinco_nove$"2005" == "-"] <- 0
cinco_nove$"2006"[cinco_nove$"2006" == "-"] <- 0
cinco_nove$"2007"[cinco_nove$"2007" == "-"] <- 0
cinco_nove$"2008"[cinco_nove$"2008" == "-"] <- 0
cinco_nove$"2009"[cinco_nove$"2009" == "-"] <- 0
cinco_nove$"2010"[cinco_nove$"2010" == "-"] <- 0
cinco_nove$"2011"[cinco_nove$"2011" == "-"] <- 0
cinco_nove$"2012"[cinco_nove$"2012" == "-"] <- 0
cinco_nove$"2013"[cinco_nove$"2013" == "-"] <- 0
cinco_nove$"2014"[cinco_nove$"2014" == "-"] <- 0
cinco_nove$"2015"[cinco_nove$"2015" == "-"] <- 0
cinco_nove$"2016"[cinco_nove$"2016" == "-"] <- 0
cinco_nove$"2017"[cinco_nove$"2017" == "-"] <- 0
cinco_nove$"2018"[cinco_nove$"2018" == "-"] <- 0
cinco_nove$"2019"[cinco_nove$"2019" == "-"] <- 0
cinco_nove$"2020"[cinco_nove$"2020" == "-"] <- 0


cinco_nove <- cinco_nove %>%
  gather("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
         "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
         "2015", "2016", "2017", "2018", "2019", "2020", key = "ano", value = "casos")

writexl::write_xlsx(cinco_nove, "cinco_nove.xlsx")
cinco_nove <- readxl::read_xlsx("cinco_nove.xlsx")   
  
#-------------------------------------------------------------------------------  
dez_quatorze <- read.csv2("dez_quatorze.csv")
head(dez_quatorze)

dez_quatorze <- rename(dez_quatorze, c("1996" = X1996, "1997" = X1997, "1998" = X1998, 
                                   "1999" = X1999, "2000" = X2000, "2001" = X2001,
                                   "2002" = X2002, "2003" = X2003, "2004" = X2004,
                                   "2005" = X2005, "2006" = X2006, "2007" = X2007,
                                   "2008" = X2008, "2009" = X2009, "2010" = X2010,
                                   "2011" = X2011, "2012" = X2012, "2013" = X2013,
                                   "2014" = X2014, "2015" = X2015, "2016" = X2016,
                                   "2017" = X2017, "2018" = X2018, "2019" = X2019,
                                   "2020" = X2020,
                                   "Macrorregião de Saúde" = Macrorregião.de.Saúde))

dez_quatorze$"1996"[dez_quatorze$"1996" == "-"] <- 0
dez_quatorze$"1997"[dez_quatorze$"1997" == "-"] <- 0
dez_quatorze$"1998"[dez_quatorze$"1998" == "-"] <- 0
dez_quatorze$"1999"[dez_quatorze$"1999" == "-"] <- 0
dez_quatorze$"2000"[dez_quatorze$"2000" == "-"] <- 0
dez_quatorze$"2001"[dez_quatorze$"2001" == "-"] <- 0
dez_quatorze$"2002"[dez_quatorze$"2002" == "-"] <- 0
dez_quatorze$"2003"[dez_quatorze$"2003" == "-"] <- 0
dez_quatorze$"2004"[dez_quatorze$"2004" == "-"] <- 0
dez_quatorze$"2005"[dez_quatorze$"2005" == "-"] <- 0
dez_quatorze$"2006"[dez_quatorze$"2006" == "-"] <- 0
dez_quatorze$"2007"[dez_quatorze$"2007" == "-"] <- 0
dez_quatorze$"2008"[dez_quatorze$"2008" == "-"] <- 0
dez_quatorze$"2009"[dez_quatorze$"2009" == "-"] <- 0
dez_quatorze$"2010"[dez_quatorze$"2010" == "-"] <- 0
dez_quatorze$"2011"[dez_quatorze$"2011" == "-"] <- 0
dez_quatorze$"2012"[dez_quatorze$"2012" == "-"] <- 0
dez_quatorze$"2013"[dez_quatorze$"2013" == "-"] <- 0
dez_quatorze$"2014"[dez_quatorze$"2014" == "-"] <- 0
dez_quatorze$"2015"[dez_quatorze$"2015" == "-"] <- 0
dez_quatorze$"2016"[dez_quatorze$"2016" == "-"] <- 0
dez_quatorze$"2017"[dez_quatorze$"2017" == "-"] <- 0
dez_quatorze$"2018"[dez_quatorze$"2018" == "-"] <- 0
dez_quatorze$"2019"[dez_quatorze$"2019" == "-"] <- 0
dez_quatorze$"2020"[dez_quatorze$"2020" == "-"] <- 0


dez_quatorze <- dez_quatorze %>%
  gather("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
         "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
         "2015", "2016", "2017", "2018", "2019", "2020", key = "ano", value = "casos")

writexl::write_xlsx(dez_quatorze, "dez_quatorze.xlsx")
dez_quatorze <- readxl::read_xlsx("dez_quatorze.xlsx")

##------------------------------------------------------------------------------
quinze_dezenove <- read.csv2("quinze_dezenove.csv")
head(quinze_dezenove)

quinze_dezenove <- rename(quinze_dezenove, c("1996" = X1996, "1997" = X1997, "1998" = X1998, 
                                       "1999" = X1999, "2000" = X2000, "2001" = X2001,
                                       "2002" = X2002, "2003" = X2003, "2004" = X2004,
                                       "2005" = X2005, "2006" = X2006, "2007" = X2007,
                                       "2008" = X2008, "2009" = X2009, "2010" = X2010,
                                       "2011" = X2011, "2012" = X2012, "2013" = X2013,
                                       "2014" = X2014, "2015" = X2015, "2016" = X2016,
                                       "2017" = X2017, "2018" = X2018, "2019" = X2019,
                                       "2020" = X2020,
                                       "Macrorregião de Saúde" = Macrorregião.de.Saúde))

quinze_dezenove$"1996"[quinze_dezenove$"1996" == "-"] <- 0
quinze_dezenove$"1997"[quinze_dezenove$"1997" == "-"] <- 0
quinze_dezenove$"1998"[quinze_dezenove$"1998" == "-"] <- 0
quinze_dezenove$"1999"[quinze_dezenove$"1999" == "-"] <- 0
quinze_dezenove$"2000"[quinze_dezenove$"2000" == "-"] <- 0
quinze_dezenove$"2001"[quinze_dezenove$"2001" == "-"] <- 0
quinze_dezenove$"2002"[quinze_dezenove$"2002" == "-"] <- 0
quinze_dezenove$"2003"[quinze_dezenove$"2003" == "-"] <- 0
quinze_dezenove$"2004"[quinze_dezenove$"2004" == "-"] <- 0
quinze_dezenove$"2005"[quinze_dezenove$"2005" == "-"] <- 0
quinze_dezenove$"2006"[quinze_dezenove$"2006" == "-"] <- 0
quinze_dezenove$"2007"[quinze_dezenove$"2007" == "-"] <- 0
quinze_dezenove$"2008"[quinze_dezenove$"2008" == "-"] <- 0
quinze_dezenove$"2009"[quinze_dezenove$"2009" == "-"] <- 0
quinze_dezenove$"2010"[quinze_dezenove$"2010" == "-"] <- 0
quinze_dezenove$"2011"[quinze_dezenove$"2011" == "-"] <- 0
quinze_dezenove$"2012"[quinze_dezenove$"2012" == "-"] <- 0
quinze_dezenove$"2013"[quinze_dezenove$"2013" == "-"] <- 0
quinze_dezenove$"2014"[quinze_dezenove$"2014" == "-"] <- 0
quinze_dezenove$"2015"[quinze_dezenove$"2015" == "-"] <- 0
quinze_dezenove$"2016"[quinze_dezenove$"2016" == "-"] <- 0
quinze_dezenove$"2017"[quinze_dezenove$"2017" == "-"] <- 0
quinze_dezenove$"2018"[quinze_dezenove$"2018" == "-"] <- 0
quinze_dezenove$"2019"[quinze_dezenove$"2019" == "-"] <- 0
quinze_dezenove$"2020"[quinze_dezenove$"2020" == "-"] <- 0


quinze_dezenove <- quinze_dezenove %>%
  gather("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
         "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
         "2015", "2016", "2017", "2018", "2019", "2020", key = "ano", value = "casos")

writexl::write_xlsx(quinze_dezenove, "quinze_dezenove.xlsx")
quinze_dezenove <- readxl::read_xlsx("quinze_dezenove.xlsx")

##------------------------------------------------------------------------------
dados_teste <- readxl::read_xls("dados_teste.xls")

dados <- rename(dados, c("regiao" = "Macrorregião de Saúde"))

dados <- readxl::read_xlsx("flavia.xlsx")


c <- dados %>%
  group_by("menor que 1", "1-4 anos", "5-9 anos", "10-14 anos", "15-19 anos") %>%
  gather("menor que 1", "1-4 anos", "5-9 anos", "10-14 anos", "15-19 anos",
         key = "idade", value = "cases") 

d <- c %>%
  group_by(idade, cases) %>%
  summarise(
    n = n()
  )

ggplot(d, aes(x=idade)) +
  geom_bar()
  
dados <- dados %>%
  rename(c("menor1" = "menor que 1", "um_quatro" = "1-4 anos", "cinco_nove" = "5-9 anos",
           "dez_quatorze" = "10-14 anos", "quinze_dezenove" = "15-19 anos"))

t <- dados %>%
  separate(regiao, into = c("n", "regiao"))


dados$menor1 <- as.numeric(dados$menor1)
dados$um_quatro <- as.numeric(dados$um_quatro)
dados$cinco_nove <- as.numeric(dados$cinco_nove)
dados$dez_quatorze <- as.numeric(dados$dez_quatorze)
dados$quinze_dezenove <- as.numeric(dados$quinze_dezenove)

dados3 <- mutate(dados, soma = dados$`menor1`+dados$`1_4`+ dados$`5_9`+
                 dados$`10_14`+dados$`15_19`)

dados_anos$ano <- as.numeric(dados_anos$ano)

dados_anos <- dados3 %>%
  group_by(ano) %>%
  summarise(casos = sum(soma))


ggplot(anos_casos, aes(x=Anos, y=Casos)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  geom_text(aes(label = Casos, vjust = -0.5)) +
  scale_y_continuous(
    limits = c(0, 6000),
    breaks = seq(0, 6000, 500)
  ) +
  scale_fill_continuous()

anos_casos <- data.frame(Anos = c("1996-2000", "2001-2005", "2006-2010", "2011-2015", "2016-2020"),
                         Casos = c(4763,4901,4851,4627,4188),
                         stringsAsFactor=FALSE)

dados<- mutate(dados, soma = dados$menor1+dados$um_quatro+ dados$cinco_nove+
                 dados$dez_quatorze+dados$quinze_dezenove)









