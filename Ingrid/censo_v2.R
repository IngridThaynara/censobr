# Fazendo a instalação do pacote censobr
#install.packages("censobr")

# Fazendo a liberação dos pacotes necessários para a criação dos indicadores
library(censobr)
library(arrow)
library(tidyverse)

# HELP da função utilizada nesse script
?censobr::read_tracts

# Acesso ao dicionário dos codigos:
censobr::data_dictionary(year = 2022, dataset = "tracts")

# Tabela basica do censobr apenas 36 vars
basico <- read_tracts(
  year = 2022,
  dataset = 'Basico',
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = TRUE #trocar pra true após o primeiro download
) %>%
  filter(code_state == 33)

# No estado do Rio de Janeiro há 41700 malhas de setores censitários.

# Verificando os nomes das variaveis disponiveis
basico %>%
  variable.names()

# Verificando se a partir dos dados censitários, agrupando por município, retornará os 92 municípios existentes no Estado do Rio de Janeiro
verificação = basico %>%
  filter(code_state == 33) %>%
  select(code_muni,name_muni) %>%
  group_by(name_muni) %>%
  summarise(n=n())

# Dados do censo 2022 por setor censitário referente as pessoas
pessoas <- read_tracts(
  year = 2022,
  dataset = "Pessoas",
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = TRUE
)%>%
  filter(code_state == 33)

# Dados do censo 2022 por setor censitário referente ao domicilio
domicilio <- read_tracts(
  year = 2022,
  dataset = "Domicilio",
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = TRUE
)%>%
  filter(code_state == 33)

# Dados do censo 2022 por setor censitário referente ao domicilio
responsavel_renda <- read_tracts(
  year = 2022,
  dataset = "ResponsavelRenda",
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = TRUE
)%>%
  filter(code_state == 33)


