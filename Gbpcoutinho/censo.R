install.packages("censobr")

library(censobr)
library(arrow)
library(tidyverse)





#PEGUEI DO HELP DA FUNÇÃO para codigo do projeto
?censobr::read_tracts



# Tabela basica do censobr apenas 36 vars
basico <- read_tracts(
  year = 2022,
  dataset = 'Basico',
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = TRUE #trocar pra true dps
) %>%
  filter(code_state == 33)


#talvez esse aq seja demais
pessoas <- read_tracts(
  year = 2022,
  dataset = "Pessoas",
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = TRUE #trocar pra true dps
)%>%
  filter(code_state == 33)



#Para Domicilio
domicilio <- read_tracts(
  year = 2022,
  dataset = "Domicilio",
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = TRUE
)%>%
  filter(code_state == 33)




#verificar o nome das variaveis disponiveis
basico %>%
  variable.names()






# codigo para acessar dicionario dos codigos
censobr::data_dictionary(year = 2022, dataset = "tracts")






# testar a variavel e agrupar por municipio os setores censitarios
apagar = df2 %>%
  filter(code_state == 33) %>%
  select(code_muni,name_muni,alfabetizacao_V00650) %>%
  group_by(name_muni) %>%
  summarise(n=n(),soma = sum(alfabetizacao_V00650, na.rm = T))

