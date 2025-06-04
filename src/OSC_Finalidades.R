# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo "As
# Finalidades de Atuação das OSCs"

# Autor do Script: Murilo Junqueira
# (m.junqueira@yahoo.com.br; murilo.junqueira@ipea.gov.br)

# Data de Criação do Scrip: 2025-03-06

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Pacotes de programação e manipulação de dados
library(magrittr)
library(tidyverse)
library(assertthat)

# Gráficos
library(ggplot2)
library(ggpmisc) # informações estatísticas adicionais aos gráficos

# Mapas
library(geobr)

# Pacotes de leitura de dados externos
library(data.table)
library(readxl)

# Manipulação de datas
library(lubridate)

# Manipulação de Textos
library(stringr)
library(stringi)
library(glue)

# Criação de tabelas
library(gt)
library(gtsummary)

# Pacotes de Bancos de dados (ex: PostgreSQL)
library(DBI)
library(RODBC)
library(RPostgres)
library(dbplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leitura de Dados ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Informações sobre as OSC ativas:
tb_osc <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_osc.RDS")

# Localização das OSC:
tb_localicazao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_localizacao.RDS")

# Dados Gerais OSC:
tb_dados_gerais <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_dados_gerais.RDS")

# Áreas de Atuaçãpo
tb_area_atuacao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_area_atuacao.RDS")

dc_area_subarea_atuacao <- fread("data/dc_area_subarea_atuacao.csv",
                                 encoding = "Latin-1")

# Dados básicos das Unidades Federativas:
UFs <- fread("data/UFs.csv", encoding = "Latin-1") %>%
  mutate(UF_Id = as.character(UF_Id))

# Informações Básicas dos Municípios:
Municipios <- fread("data/Municipios.csv", encoding = "Latin-1")

# População Municipal (censo 2022):
Pop2022 <- fread("data/Pop2022.csv", encoding = "UTF-8") %>%
  magrittr::set_names(c("cd_municipio", "Munic_Nome", "Pop2022"))

# IDH Municipal
IDHMunic <- fread("data/IDHMunic2010.csv", encoding = "UTF-8") %>%
  magrittr::set_names(c("Sigla", "cd_municipio", "Municipio", "IDH2010",
                        "V5")) %>%
  select(cd_municipio, IDH2010)

# Vou juntar dados dos municípios em uma única tabela:
MunicSocialData <- Municipios %>%
  left_join(select(Pop2022, cd_municipio, Pop2022),
            by = c("Munic_Id" = "cd_municipio") ) %>%
  rename(cd_municipio = Munic_Id) %>%
  left_join(IDHMunic, by = "cd_municipio")

rm(Municipios, Pop2022, IDHMunic)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas de uso comum no capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OSCAtiva_AreaAtuacao <- tb_area_atuacao %>%
  # Adiciona a variável de OSC ativa
  left_join(
    select(tb_osc,
           id_osc, bo_osc_ativa),
    by = "id_osc"
  ) %>%
  # Filtra somente OSC ativas
  dplyr::filter(bo_osc_ativa == TRUE) %>%
  # Adiciona município das OSC
  left_join(
    select(tb_localicazao,
           id_osc, cd_municipio),
    by = "id_osc"
  ) %>%
  # Extrai a UF
  mutate(UF_Id = str_sub(cd_municipio, 1, 2)) %>%
  # Deixa apenas variáveis relevantes
  select(id_osc, cd_area_atuacao, cd_subarea_atuacao, UF_Id, cd_municipio)

# acho que posso já retirar esses bancos:
rm(tb_area_atuacao, tb_osc, tb_localicazao)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Número de OSCs, segundo a finalidade de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

distinct(dc_area_subarea_atuacao, cd_area_atuacao, tx_area_atuacao)



# names(dc_area_subarea_atuacao)

# names(OSCAtiva_AreaAtuacao)

# TABELA - Número de OSCs, segundo a finalidade de atuação: Brasil
OSC_AreaSubAreaAtuacao <- OSCAtiva_AreaAtuacao %>%
  group_by(cd_area_atuacao, cd_subarea_atuacao) %>%
  summarise(FreqSubArea = n()) %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao, cd_subarea_atuacao, tx_subarea_atuacao),
            by = "cd_subarea_atuacao") %>%

  left_join(distinct(dc_area_subarea_atuacao, cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  mutate(
    tx_area_atuacao = ifelse(is.na(tx_area_atuacao), "Área Não Identificada",
                             tx_area_atuacao),
    tx_subarea_atuacao = ifelse(is.na(tx_subarea_atuacao), "Subárea Não Identificada",
                                tx_subarea_atuacao),
  ) %>%
  select(tx_area_atuacao, tx_subarea_atuacao, FreqSubArea) %>%
  mutate(Per = FreqSubArea / sum(FreqSubArea, na.rm = TRUE),
         Per = round(Per * 100, 1) ) %>%
  group_by(tx_area_atuacao) %>%
  mutate(PerGrupo = FreqSubArea / sum(FreqSubArea, na.rm = TRUE),
         PerGrupo = round(PerGrupo * 100, 1) )

sum(OSC_AreaSubAreaAtuacao$FreqSubArea)


# Corrige um bug de encoding
# OSC_AreaSubAreaAtuacao$tx_area_atuacao <-
#   iconv(OSC_AreaSubAreaAtuacao$tx_area_atuacao, "latin1", "UTF-8")

# Checa dados
# View(OSC_AreaSubAreaAtuacao)

# Salva Tabela
saveRDS(OSC_AreaSubAreaAtuacao, "tables/OSC_AreaSubAreaAtuacao.RDS")


# Formata tabela:
OSC_AreaSubAreaAtuacao %>%
  gt(locale = "pt-BR",
     groupname_col = "tx_area_atuacao") %>%

  # Nomes amigáveis das Colunas
  cols_label(
    tx_subarea_atuacao = "Áreas de Atuacao",
    FreqSubArea = "Total de OCS",
    Per = "Em relacao ao total",
    PerGrupo = "Em relacao ao  grupo"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels()
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "(%)",
    columns = c(Per, PerGrupo)
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = FreqSubArea,
    sep_mark = ".",
    dec_mark = ",",
    decimals = 0
  ) %>%

  # Intentação nos dados
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body()
  ) %>%

  # Torna as áreas de atuação mais destacadas
  tab_style(
    style = cell_text(weight = "bold"), # Negrito
    locations = cells_row_groups()
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapa - Densidade de OSCs defesa de direitos e interesses ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FIGURA- Densidade de OSCs de desenvolvimento e defesa de direitos e interesses
# por mil habitantes, por município

municipios_geo <- geobr::read_municipality(year=2022)

# Defesa de Direitos:
mapa_dados <- municipios_geo %>%
  left_join(select(MunicSocialData, cd_municipio, Pop2022),
            by = c("code_muni" = "cd_municipio")) %>%
  dplyr::filter( !is.na(Pop2022) ) %>%
  left_join(
    summarise(
      dplyr::filter(OSCAtiva_AreaAtuacao, cd_area_atuacao == 9),
      N_defesa_direito = n(), .by = cd_municipio),
    by = c("code_muni" = "cd_municipio")) %>%

  mutate(

    # Defesa de Direitos por mil habitantes
    DefDireitospor1000 = (N_defesa_direito / Pop2022) * 1000,

    # Agrupa o IDH em grupos
    DensidadeDefDireitos = case_when(
      DefDireitospor1000 < 1.4 ~ "Muito Baixo",
      DefDireitospor1000 < 1.93 ~ "Baixo",
      DefDireitospor1000 < 2.61 ~ "Médio",
      DefDireitospor1000 < 3.54 ~ "Alto",
      DefDireitospor1000 < 100 ~ "Muito Alto",
      TRUE ~ "Erro" ),

    # Ordena os grupos
    DefDireitosOrdem = case_when(
      DensidadeDefDireitos == "Muito Baixo" ~ 1,
      DensidadeDefDireitos == "Baixo" ~ 2,
      DensidadeDefDireitos == "Médio" ~ 3,
      DensidadeDefDireitos == "Alto" ~ 4,
      DensidadeDefDireitos == "Muito Alto" ~ 5,
      DensidadeDefDireitos == "Erro" ~ 6 ),

    # Transforma IDHGrupo em fator ordenado
    DensidadeDefDireitos = fct_reorder(DensidadeDefDireitos, DefDireitosOrdem, .desc = TRUE)
  )

summary(mapa_dados$DefDireitospor1000)

# Distribuição de frequência dos grupos de IDH
table(mapa_dados$DensidadeDefDireitos)

mapa_DefDireitospor1000 <- mapa_dados %>%
  select(name_muni, DensidadeDefDireitos, geom) %>%
  dplyr::filter(DensidadeDefDireitos != "Erro" )


# Checa dados
# View(mapa_DefDireitospor1000)

# Salva Tabela
saveRDS(mapa_DefDireitospor1000, "tables/mapa_DefDireitospor1000.RDS")


HeatDefDireitospor1000 <- ggplot() +
  geom_sf(data = mapa_DefDireitospor1000,
          aes(fill = DensidadeDefDireitos), color=NA, size=.15) +

  scale_fill_manual(values = c(
    "blue",
    "lightgreen",
    "orange",
    "yellow",
    "pink"
    )) +

  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.text  = element_text(size = 6),
        legend.position = "bottom" )

HeatDefDireitospor1000

ggsave(plot = HeatDefDireitospor1000,
       filename = "plots/HeatDefDireitospor1000.png",
       width = 12,
       height = 12,
       units = "cm")

rm(mapa_dados)


# Religiosas:
mapa_dados <- municipios_geo %>%
  left_join(select(MunicSocialData, cd_municipio, Pop2022),
            by = c("code_muni" = "cd_municipio")) %>%
  dplyr::filter( !is.na(Pop2022) ) %>%
  left_join(
    summarise(
      dplyr::filter(OSCAtiva_AreaAtuacao, cd_area_atuacao == 6),
      N_religiosas = n(), .by = cd_municipio),
    by = c("code_muni" = "cd_municipio")) %>%

  mutate(

    # Defesa de Direitos por mil habitantes
    Religpor1000 = (N_religiosas / Pop2022) * 1000,

    # Agrupa o IDH em grupos
    DensidadeRelig = case_when(
      Religpor1000 < 0.383 ~ "Muito Baixo",
      Religpor1000 < 0.658 ~ "Baixo",
      Religpor1000 < 0.921 ~ "Médio",
      Religpor1000 < 1.27 ~ "Alto",
      Religpor1000 < 20 ~ "Muito Alto",
      TRUE ~ "Erro" ),

    # Ordena os grupos
    ReligOrdem = case_when(
      DensidadeRelig == "Muito Baixo" ~ 1,
      DensidadeRelig == "Baixo" ~ 2,
      DensidadeRelig == "Médio" ~ 3,
      DensidadeRelig == "Alto" ~ 4,
      DensidadeRelig == "Muito Alto" ~ 5,
      DensidadeRelig == "Erro" ~ 6 ),

    # Transforma IDHGrupo em fator ordenado
    DensidadeRelig = fct_reorder(DensidadeRelig, ReligOrdem, .desc = TRUE)
  )

summary(mapa_dados$Religpor1000)

# Distribuição de frequência
table(mapa_dados$DensidadeRelig)

mapa_Religpor1000 <- mapa_dados %>%
  select(name_muni, DensidadeRelig, geom) %>%
  dplyr::filter(DensidadeRelig != "Erro" )


# Checa dados
# View(mapa_Religpor1000)

# Salva Tabela
saveRDS(mapa_Religpor1000, "tables/mapa_Religpor1000.RDS")


Heatmap_Religpor1000 <- ggplot() +
  geom_sf(data = mapa_Religpor1000,
          aes(fill = DensidadeRelig), color=NA, size=.15) +

  scale_fill_manual(values = c(
    "blue",
    "lightgreen",
    "orange",
    "yellow",
    "pink"
  )) +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.text  = element_text(size = 6),
        legend.position = "bottom" )

HeatDefDireitospor1000

ggsave(plot = Heatmap_Religpor1000,
       filename = "plots/Heatmap_Religpor1000.png",
       width = 12,
       height = 12,
       units = "cm")

# Fim ####
