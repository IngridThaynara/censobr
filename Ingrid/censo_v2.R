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

# Criando um vetor com as 29 primeiras variáveis do basico
chaves <- names(basico)[1:29]

# Fazendo o join usando apenas essas variáveis como chave
base <- basico %>%
  left_join(pessoas, by = chaves) %>%
  left_join(domicilio, by = chaves) %>%
  left_join(responsavel_renda, by = chaves)

write.csv(base, "base_censo2022_rj.csv", row.names = FALSE)

var <- names(base)

library(dplyr)
library(tidyr)

# === 1. Indicadores DEMOGRÁFICOS ===============================

## 1) Densidade Demográfica
ind1 <- base %>%
  mutate(densidade = V0001 / area_km2) %>%
  group_by(code_muni, name_muni) %>%
  summarise(densidade = sum(densidade, na.rm = TRUE))


## 2) Índice de Urbanização
ind2 <- base %>%
  mutate(pop_urb = if_else(situacao == "Urbana", V0001, 0)) %>%
  group_by(code_muni, name_muni) %>%
  summarise(ind_urbanizacao = sum(pop_urb) / sum(V0001))


## 3) Razão de Dependência
base_dep <- base %>%
  mutate(
    pop_0_14 = rowSums(select(., demografia_V01031:demografia_V01033), na.rm = TRUE),
    pop_15_64 = rowSums(select(., demografia_V01034:demografia_V01038), na.rm = TRUE),
    pop_65mais = rowSums(select(., demografia_V01039:demografia_V01041), na.rm = TRUE)
  )

ind3 <- base_dep %>%
  group_by(code_muni, name_muni) %>%
  summarise(
    razao_dependencia = (sum(pop_0_14) + sum(pop_65mais)) / sum(pop_15_64)
  )


## 4) Proporção de Idosos 65+
ind4 <- base_dep %>%
  group_by(code_muni, name_muni) %>%
  summarise(prop_idosos = sum(pop_65mais) / sum(V0001))

## 5) Proporção de Domicílios Permanentes
ind5 <- base %>%
  group_by(code_muni, name_muni) %>%
  summarise(prop_dom_permanentes = sum(domicilio01_V00010, na.rm = TRUE) /
              sum(domicilio01_V00001, na.rm = TRUE))


# === 2. Indicadores SOCIAIS =====================================

## 6) Proporção de Chefes Mulheres
ind6 <- base %>%
  mutate(
    chefes_total = parentesco_V01062 + parentesco_V01063,
    prop_chefes_mulheres = parentesco_V01063 / chefes_total
  ) %>%
  group_by(code_muni, name_muni) %>%
  summarise(prop_chefes_mulheres = weighted.mean(prop_chefes_mulheres, V0001, na.rm = TRUE))


## 7) PFSP — Proporção de Filhos sem Presença Paterna
ind7 <- base %>%
  mutate(
    filhos_sem_pai = parentesco_V01045,
    filhos_total = rowSums(select(., parentesco_V01042:parentesco_V01050), na.rm = TRUE)
  ) %>%
  group_by(code_muni, name_muni) %>%
  summarise(pfsp = sum(filhos_sem_pai) / sum(filhos_total))


## 8) Tamanho Médio dos Domicílios
ind8 <- base %>%
  group_by(code_muni, name_muni) %>%
  summarise(tamanho_medio_dom = sum(V0001) / sum(domicilio01_V00001))


## 9) Índice de Domicílios Unipessoais (IDU)
ind9 <- base %>%
  group_by(code_muni, name_muni) %>%
  summarise(idu = sum(domicilio01_V00033) / sum(domicilio01_V00001))


# === 3. Indicadores de INFRAESTRUTURA ===========================

## 10) Coleta de Lixo Adequada
ind10 <- base %>%
  mutate(
    lixo_adequado = domicilio02_V00397 + domicilio02_V00398,
    lixo_total = rowSums(select(., domicilio02_V00397:domicilio02_V00403), na.rm = TRUE)
  ) %>%
  group_by(code_muni, name_muni) %>%
  summarise(prop_lixo = sum(lixo_adequado) / sum(lixo_total))


## 11) Proporção de Esgoto Adequado
ind11 <- base %>%
  mutate(
    esgoto_adequado = domicilio02_V00309,
    esgoto_total = rowSums(select(., domicilio02_V00309:domicilio02_V00316), na.rm = TRUE)
  ) %>%
  group_by(code_muni, name_muni) %>%
  summarise(prop_esgoto = sum(esgoto_adequado) / sum(esgoto_total))


## 12) Proporção de Domicílios com Internet
ind12 <- base %>%
  mutate(
    dom_internet = rowSums(select(., domicilio02_V00290:domicilio02_V00305), na.rm = TRUE)
  ) %>%
  group_by(code_muni, name_muni) %>%
  summarise(prop_internet = sum(dom_internet) / sum(domicilio01_V00001))

names(base)[grepl("^demografia_", names(base))]
names(base)[grepl("^parentesco_", names(base))]

# === 4. Juntando os 12 indicadores ================================

indicadores_municipios <- ind1 %>%
  left_join(ind2) %>%
  left_join(ind3) %>%
  left_join(ind4) %>%
  left_join(ind5) %>%
  left_join(ind6) %>%
  left_join(ind7) %>%
  left_join(ind8) %>%
  left_join(ind9) %>%
  left_join(ind10) %>%
  left_join(ind11) %>%
  left_join(ind12)

summary(indicadores_municipios)
names(base)[grepl("V0029", names(base))]
names(base)[grepl("V003", names(base))]
names(base)[grepl("^domicilio", names(base))]

names(base)[grepl("V0039", names(base))]

names(base)[grepl("V0030", names(base))]
names(base)[grepl("V0031", names(base))]
names(base)[grepl("V0032", names(base))]

intersect(names(basico), names(pessoas)) |> intersect(names(domicilio)) |> intersect(names(responsavel_renda))


names(read_tracts(2022, "Domicilio01", as_data_frame = TRUE))
names(read_tracts(2022, "Domicilio02", as_data_frame = TRUE))
names(read_tracts(2022, "Domicilio03", as_data_frame = TRUE))

dic <- censobr::data_dictionary(year = 2022, dataset = "tracts")
View(dic)
