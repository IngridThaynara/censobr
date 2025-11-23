# Fazendo a instalação do pacote censobr
#install.packages("censobr")

rm(list=ls())
# Fazendo a liberação dos pacotes necessários para a criação dos indicadores
library(censobr)
library(arrow)
library(tidyverse)
library(dplyr)
library(naniar)
library(skimr)

# HELP da função utilizada nesse script
?censobr::read_tracts

# Acesso ao dicionário dos codigos:
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

# Exportando o banco de dados do censo 2022
write.csv(base, "base_censo2022_rj.csv", row.names = FALSE)

# Agrupando a base por setor censitário para nível municipal

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

# 2) Fixas válidas = primeiras 29 menos fixas proibidas
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

# Reorganizando a base municipal
base_mun <- base_mun |>
  select(-c(code_intermediate,name_intermediate,code_immediate,name_immediate,code_urban_concentration,name_urban_concentration))

# Verificando a quantidade de dados faltantes nas colunas

gg_miss_var(base_mun)

glimpse(base_mun)

base_mun$code_muni = as.factor(base_mun$code_muni)

# Indicadores criados a partir da base_mun

# Densidade Demográfica

ind1 <- base_mun %>%
  mutate(densidade_demografica = V0001 / area_km2) %>%
  select(code_muni, name_muni, densidade_demografica)

summary(ind1)
# Não há dados faltantes

# Índice de Urbanização
ind2 <- base_mun %>%
  mutate(indice_de_urbanizacao = V0002 / V0001) %>%
  select(code_muni, name_muni, indice_de_urbanizacao)

summary(ind2)
# Não há dados faltantes

# Razão de Dependência

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

# Proporção de Idosos (65+)

ind4 <- base_mun %>%
  mutate(
    pop_65mais = demografia_V01039 + demografia_V01040 + demografia_V01041,
    proporcao_idosos = pop_65mais / V0001
  ) %>%
  select(code_muni, name_muni, proporcao_idosos)

summary(ind4)
# Não há dados faltantes

# Taxa0 de Domicílios Permanentes
ind5 <- base_mun %>%
  mutate(prop_dom_permanentes = (domicilio01_V00010 / domicilio01_V00001)*10000) %>%
  select(code_muni, name_muni, prop_dom_permanentes)

summary(ind5)
# Não há dados faltantes

# Proporção de Chefes Mulheres
ind6 <- base_mun %>%
  mutate(
    total_chefes = parentesco_V01062 + parentesco_V01063,
    prop_chefes_mulheres = parentesco_V01063 / total_chefes
  ) %>%
  select(code_muni, name_muni, prop_chefes_mulheres)

summary(ind6)
# Não há dados faltantes

# Filhos Sem Presença Paterna (PFSP)
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

# Tamanho Médio do Domicílio
ind8 <- base_mun %>%
  mutate(tamanho_medio_dom = V0001 / domicilio01_V00001) %>%
  select(code_muni, name_muni, tamanho_medio_dom)

summary(ind8)
# Não há dados faltantes

# Coleta de Lixo Adequada
ind9 <- base_mun %>%
  mutate(
    lixo_adequado = domicilio02_V00397 + domicilio02_V00398,
    lixo_total    = domicilio02_V00397 + domicilio02_V00398 +
      domicilio02_V00399 + domicilio02_V00400 +
      domicilio02_V00401 + domicilio02_V00402 +
      domicilio02_V00403,
    coleta_lixo = lixo_adequado / lixo_total
  ) %>%
  select(code_muni, name_muni, coleta_lixo)

summary(ind9)
# Não há dados faltantes

# Esgoto Adequado
ind10 <- base_mun %>%
  mutate(
    esgoto_total    = domicilio02_V00309 + domicilio02_V00310 + domicilio02_V00311 +
      domicilio02_V00312 + domicilio02_V00313 + domicilio02_V00314 +
      domicilio02_V00315 + domicilio02_V00316,
    esgoto_adequado = domicilio02_V00309,
    prop_esgoto = esgoto_adequado / esgoto_total
  ) %>%
  select(code_muni, name_muni, prop_esgoto)

summary(ind10)
# Não há dados faltantes

# Domicílios com Internet
ind11 <- base_mun %>%
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

summary(ind11)
# Não há dados faltantes

# Taxa de Renda Domiciliar Per Capita Municipal
ind12 <- base_mun %>%
  mutate(
    renda_baixa = (V06001 + V06002) /
      (V06001 + V06002 + V06003 + V06004 + V06005)*100000
  ) %>%
  select(code_muni, name_muni, renda_baixa)

summary(ind12)
# Não há dados faltantes

# Índice de Segurança Econômica (ISE)
ind13 <- base_mun %>%
  mutate(
  baixa_renda = V06001 + V06002,
  alta_renda  = V06004 + V06005,
  ise = baixa_renda / alta_renda * 100000
) %>%
  select(code_muni, name_muni, ise)

summary(ind13)
# Não há dados faltantes

# Fazendo o join com todos indicadores criados individualmente
indicadores_df <- ind1 %>%
  left_join(ind2,  by = c("code_muni","name_muni")) %>%
  left_join(ind3,  by = c("code_muni","name_muni")) %>%
  left_join(ind4,  by = c("code_muni","name_muni")) %>%
  left_join(ind5,  by = c("code_muni","name_muni")) %>%
  left_join(ind6,  by = c("code_muni","name_muni")) %>%
  left_join(ind7,  by = c("code_muni","name_muni")) %>%
  left_join(ind8,  by = c("code_muni","name_muni")) %>%
  left_join(ind9,  by = c("code_muni","name_muni")) %>%
  left_join(ind10, by = c("code_muni","name_muni")) %>%
  left_join(ind11, by = c("code_muni","name_muni")) %>%
  left_join(ind12, by = c("code_muni","name_muni")) %>%
  left_join(ind13, by = c("code_muni","name_muni"))

