c <- dados %>%
  pivot_longer(!(c(regiao, ano)), names_to = "income", values_to = "count")


d <- c %>%
  group_by(ano, regiao) %>%
  summarise(
    n = n()
  )
##Frequencia por região
ggplot(c, aes(x = regiao)) +
  geom_bar()

