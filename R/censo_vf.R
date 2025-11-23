# Introdução ####

# Esse script tem como objetivo criar uma base com  12 indicadores a nível municipal
# para os 92 municípios do Rio de Janeiro que foram construídos a partir da base
# do Censo 2022. Foi utilizado o pacote censobr para fazer a importação dos dados
# do Censo 2022.


# 1 - Importação dos pacotes ####

# Linha de código para apagar todos os objetos do environment para evitar sobreposições
rm(list=ls())

# O primeiro passo é a instalação dos pacotes necessários. Caso você nunca tenha
# instalado no R esses pacotes, descomentar a próxima linha para fazer a instalação
# antes de usar o library

# install.packages("arrow")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("naniar")
# install.packages("skimr")
# install.packages("gt")
# install.packages("readxl")

# Mesmo que você já tenha instalado os pacotes anterios, é necessário instalar o
# pacote do censobr. Caso você já tenha instalado, comentar a próxima linha.
install.packages("censobr")

# Liberando os pacotes necessários para a criação dos indicadores
library(censobr)
library(arrow)
library(tidyverse)
library(dplyr)
library(naniar)
library(skimr)
library(gt)
library(readxl)

# 2 - Importação das bases do Censo ####

# HELP da função utilizada nesse script
?censobr::read_tracts

# Acesso ao dicionário dos codigos
censobr::data_dictionary(year = 2022, dataset = "tracts")

# Tabela basica do censobr apenas 36 variáveis
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

## 2.1 - Manipulação das 4 bases do censo para juntar em apenas uma base ####

# Verficando quais são as variáveis presentes nas 4 bases que importamos no Script
#é essencial para fazermos o join corretamente.
intersect(names(basico), names(pessoas)) |> intersect(names(domicilio)) |> intersect(names(responsavel_renda))

# Criando um vetor com as 29 primeiras variáveis do basico
chaves <- names(basico)[1:29]

# Fazendo o join usando apenas essas variáveis como chave
base <- basico %>%
  left_join(pessoas, by = chaves) %>%
  left_join(domicilio, by = chaves) %>%
  left_join(responsavel_renda, by = chaves)

## 2.2 - Manipulação para fazer o agrupando da base por nível municipal ####

# Variáveis fixas que não podem ser somadas
fixas_proibidas <- c(
  "code_tract",
  "situacao", "code_situacao", "code_type",
  "code_district", "name_district",
  "code_subdistrict", "name_subdistrict",
  "code_neighborhood", "name_neighborhood",
  "code_nucleo_urbano", "name_nucleo_urbano",
  "code_favela", "name_favela",
  "code_aglomerado", "name_aglomerado",
  "code_muni", "name_muni",
  "area_km2"
)

# Fixas válidas = primeiras 29 menos fixas proibidas
fixas <- names(base)[1:29]
fixas_validas <- setdiff(fixas, fixas_proibidas)

# Variáveis que não podem ser somadas nunca
nao_somar <- c(fixas_proibidas, fixas_validas)

# Variáveis realmente somáveis (numéricas e não proibidas)
variaveis_somar <- base %>%
  select(-all_of(nao_somar)) %>%
  select(where(is.numeric)) %>%
  names()

# EVITAR QUE code_muni APAREÇA POR ACIDENTE:
variaveis_somar <- setdiff(variaveis_somar, c("code_muni"))

# Agregando a base a nível municipal:
base_mun <- base %>%
  group_by(code_muni, name_muni) %>%
  summarise(
    across(all_of(fixas_validas), first),
    area_km2 = sum(area_km2, na.rm = TRUE),
    across(all_of(variaveis_somar), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Tratamento da base municipal para retirada de variáveis que não serão usadas
base_mun <- base_mun |>
  select(-c(code_intermediate,name_intermediate,code_immediate,name_immediate,code_urban_concentration,name_urban_concentration))

# Verificando a quantidade de dados faltantes nas colunas
gg_miss_var(base_mun)
glimpse(base_mun)

#Transformando a variável code_muni em fator
base_mun$code_muni = as.factor(base_mun$code_muni)

## 2.3 - Exportando a base a nível municipal ####

base_mun |>
  write_csv2(file = "base_censo2022_rj.xlsx")

# 3 - Indicadores ####

## Indicadores Demográficos ####

### 3.1 - Indicador de densidade demográfica ####
ind1 <- base_mun %>%
  mutate(densidade_demografica = V0001 / area_km2) %>%
  select(code_muni, name_muni, densidade_demografica)

summary(ind1)
# Não há dados faltantes

### 3.2 - Indicador do Índice de Urbanização ####
ind2 <- base_mun %>%
  mutate(indice_de_urbanizacao = V0002 / V0001) %>%
  select(code_muni, name_muni, indice_de_urbanizacao)

summary(ind2)
# Não há dados faltantes

### 3.3 - Indicador da Razão de Dependência ####
ind3 <- base_mun %>%
  mutate(
    pop_0_14   = demografia_V01031 + demografia_V01032 + demografia_V01033,
    pop_15_64  = demografia_V01034 + demografia_V01035 + demografia_V01036 + demografia_V01037 + demografia_V01038,
    pop_65mais = demografia_V01039 + demografia_V01040 + demografia_V01041,
    razao_dependencia = (pop_0_14 + pop_65mais) / pop_15_64
  ) %>%
  select(code_muni, name_muni, razao_dependencia)

summary(ind3)
# Não há dados faltantes

### 3.4 - Indicador da Proporção de Idosos (65+) ####
ind4 <- base_mun %>%
  mutate(
    pop_65mais = demografia_V01039 + demografia_V01040 + demografia_V01041,
    proporcao_idosos = pop_65mais / V0001
  ) %>%
  select(code_muni, name_muni, proporcao_idosos)

summary(ind4)
# Não há dados faltantes

### 3.5 - Indicador da Taxa de Domicílios Permanentes ####
# ind5 <- base_mun %>%
#   mutate(prop_dom_permanentes = (domicilio01_V00010 / domicilio01_V00001)*10000) %>%
#   select(code_muni, name_muni, prop_dom_permanentes)
#
# summary(ind5)
# Não há dados faltantes

### 3.6 - Indicador da Proporção de Chefes Mulheres ####
ind6 <- base_mun %>%
  mutate(
    total_chefes = parentesco_V01062 + parentesco_V01063,
    prop_chefes_mulheres = parentesco_V01063 / total_chefes
  ) %>%
  select(code_muni, name_muni, prop_chefes_mulheres)

summary(ind6)
# Não há dados faltantes

### 3.7 - Indicador da Proporção de Filhos Sem Presença Paterna (PFSP) ####
ind7 <- base_mun %>%
  mutate(
    filhos_sem_pai = parentesco_V01045,
    filhos_total   = parentesco_V01042 + parentesco_V01043 + parentesco_V01044 +
      parentesco_V01045 + parentesco_V01046 + parentesco_V01047 +
      parentesco_V01048 + parentesco_V01049 + parentesco_V01050,
    pfsp = filhos_sem_pai / filhos_total
  ) %>%
  select(code_muni, name_muni, pfsp)

summary(ind7)
# Não há dados faltantes

### 3.8 - Indicador do Tamanho Médio do Domicílio ####
ind8 <- base_mun %>%
  mutate(tamanho_medio_dom = V0001 / domicilio01_V00001) %>%
  select(code_muni, name_muni, tamanho_medio_dom)

summary(ind8)
# Não há dados faltantes

### 3.9 - Indicador de Domicílios com Internet ####
ind9 <- base_mun %>%
  mutate(
    dom_internet = domicilio02_V00290 + domicilio02_V00291 + domicilio02_V00292 +
      domicilio02_V00293 + domicilio02_V00294 + domicilio02_V00295 +
      domicilio02_V00296 + domicilio02_V00297 + domicilio02_V00298 +
      domicilio02_V00299 + domicilio02_V00300 + domicilio02_V00301 +
      domicilio02_V00302 + domicilio02_V00303 + domicilio02_V00304 +
      domicilio02_V00305,
    prop_internet = dom_internet / domicilio01_V00001
  ) %>%
  select(code_muni, name_muni, prop_internet)

summary(ind9)
# Não há dados faltantes

## Indicadores de Saúde ####

### 3.10 - Indicador da Coleta de Lixo Adequada ####
ind10 <- base_mun %>%
  mutate(
    lixo_adequado = domicilio02_V00397 + domicilio02_V00398,
    lixo_total    = domicilio02_V00397 + domicilio02_V00398 +
      domicilio02_V00399 + domicilio02_V00400 +
      domicilio02_V00401 + domicilio02_V00402 +
      domicilio02_V00403,
    coleta_lixo = lixo_adequado / lixo_total
  ) %>%
  select(code_muni, name_muni, coleta_lixo)

summary(ind10)
# Não há dados faltantes

### 3.11 - Indicador do Esgoto Adequado ####
ind11 <- base_mun %>%
  mutate(
    esgoto_total    = domicilio02_V00309 + domicilio02_V00310 + domicilio02_V00311 +
      domicilio02_V00312 + domicilio02_V00313 + domicilio02_V00314 +
      domicilio02_V00315 + domicilio02_V00316,
    esgoto_adequado = domicilio02_V00309,
    prop_esgoto = esgoto_adequado / esgoto_total
  ) %>%
  select(code_muni, name_muni, prop_esgoto)

summary(ind11)
# Não há dados faltantes

## Indicadores Econômicos ####

### 3.12 - Indicador da Taxa de Renda Domiciliar Per Capita Municipal ####
ind12 <- base_mun %>%
  mutate(
    renda_baixa = (V06001 + V06002) /
      (V06001 + V06002 + V06003 + V06004 + V06005)*100000
  ) %>%
  select(code_muni, name_muni, renda_baixa)

summary(ind12)
# Não há dados faltantes

### 3.13 - Indicador do Índice de Segurança Econômica (ISE) ####
ind13 <- base_mun %>%
  mutate(
    baixa_renda = V06001 + V06002,
    alta_renda  = V06004 + V06005,
    ise = baixa_renda / alta_renda * 100000
  ) %>%
  select(code_muni, name_muni, ise)

summary(ind13)
# Não há dados faltantes

## Indicadores Educacional ####

### 3.14 - Indicador do Índice de Baixa Escolaridade Estimada ####
ind14 <- base_mun %>%
  mutate(
    ibe = (V06001 + V06002) /
      (V06001 + V06002 + V06003 + V06004 + V06005) *100000
  ) %>%
  select(code_muni, name_muni, ibe)

summary(ind14)
# Não há dados faltantes

### 3.15 - Indicador do Índice de Escolaridade Superior Estimada (IESE) ####
ind15 <- base_mun %>%
  mutate(
    iese = (V06004 + V06005) /
      (V06001 + V06002 + V06003 + V06004 + V06005)
  ) %>%
  select(code_muni, name_muni, iese)

summary(ind15)
# Não há dados faltantes

# 4 - Base dos indicadores criados acima ####
indicadores_df <- ind1 %>%
  left_join(ind2,  by = c("code_muni","name_muni")) %>%
  left_join(ind3,  by = c("code_muni","name_muni")) %>%
  left_join(ind4,  by = c("code_muni","name_muni")) %>%
  #left_join(ind5,  by = c("code_muni","name_muni")) %>%
  left_join(ind6,  by = c("code_muni","name_muni")) %>%
  left_join(ind7,  by = c("code_muni","name_muni")) %>%
  left_join(ind8,  by = c("code_muni","name_muni")) %>%
  left_join(ind9,  by = c("code_muni","name_muni")) %>%
  left_join(ind10, by = c("code_muni","name_muni")) %>%
  left_join(ind11, by = c("code_muni","name_muni")) %>%
  left_join(ind12, by = c("code_muni","name_muni")) %>%
  left_join(ind13, by = c("code_muni","name_muni")) %>%
  left_join(ind14, by = c("code_muni","name_muni")) %>%
  left_join(ind15, by = c("code_muni","name_muni"))

# Manipulação para que todos os indicadores tenha apenas 4 casas decimais
indicadores_df <- indicadores_df %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Visualizando a base de indicadores
View(indicadores_df)

## 4.1 - Exportando a base dos indicadores municipal ####

indicadores_df |>
  write_csv2(file = "base_indicadores_rj.xlsx")
