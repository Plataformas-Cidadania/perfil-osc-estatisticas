# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo:
# "Porte das OSCs"

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

# Dados básicos das Unidades Federativas:
UFs <- fread("data/UFs.csv", encoding = "Latin-1") %>%
  mutate(UF_Id = as.character(UF_Id))

# Informações Básicas dos Municípios:
Municipios <- fread("data/Municipios.csv", encoding = "Latin-1")

# Áreas de Atuaçãpo
tb_area_atuacao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_area_atuacao.RDS")

dc_area_subarea_atuacao <- fread("data/dc_area_subarea_atuacao.csv",
                                 encoding = "Latin-1") %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_",
                            str_pad(cd_subarea_atuacao, 2, pad = "0") ) )

dados_RAIS <- readRDS("temp/RAIS/RAIS_2022.RDS")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas de uso comum no capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Vou criar aqui alguns tratamentos que vão se repetir em muitas tabelas:

OrdemUF <- fread("data/OrdemUF.csv", encoding = "Latin-1")

area_atuacao_clean <- tb_area_atuacao %>%
  mutate(flag = case_when(
    str_detect(ft_area_atuacao, "AreaAtuacaoOSC") ~ TRUE,
    str_detect(ft_area_atuacao, "CNPJ") ~ TRUE,
    TRUE ~ FALSE),
    Ordem = case_when(
      str_detect(ft_area_atuacao, "AreaAtuacaoOSC.R_2025_01") ~ 1,
      str_detect(ft_area_atuacao, "AreaAtuacaoOSC.R_2023_01") ~ 2,
      str_detect(ft_area_atuacao, "CNPJ/RFB/MF 2020") ~ 3,
      str_detect(ft_area_atuacao, "CNPJ/SRF/MF 2018") ~ 4,
      str_detect(ft_area_atuacao, "CNPJ/SRF/MF 2016") ~ 5,
      TRUE ~ 10)
  ) %>%
  dplyr::filter(
    !is.na(cd_area_atuacao),
    flag
  ) %>%
  arrange(Ordem) %>%
  distinct(id_osc, .keep_all = TRUE) %>%
  select(id_osc, cd_area_atuacao, cd_subarea_atuacao)

rm(tb_area_atuacao)


# Agrega dados relevantes por OSC
ResumoRais <- dados_RAIS %>%
  # Seleciona apenas as OSC
  dplyr::filter(cd_identificador_osc %in% tb_osc$cd_identificador_osc) %>%

  mutate(
    # Gênero
    dummy_mulheres = ifelse(genero == 2L, 1, 0),

    # Raça
    dummy_branco = ifelse(raca_cor == 2L, 1, 0),
    dummy_indigena = ifelse(raca_cor == 1L, 1, 0),
    dummy_preto = ifelse(raca_cor == 4L, 1, 0),
    dummy_pardo = ifelse(raca_cor == 8L, 1, 0),
    dummy_amarelo = ifelse(raca_cor == 6L, 1, 0),
    dummy_defic = ifelse(is.na(ind_defic), 0, 1),

    # Escolaridade
    dummy_esc_analfabeto = ifelse(grau_instr == 1L, 1, 0),
    dummy_esc_leescreve = ifelse(grau_instr == 2L, 1, 0),
    dummy_esc_fund_0ate5ano = ifelse(grau_instr == 3L, 1, 0),
    dummy_esc_fund_6ate9ano = ifelse(grau_instr == 4L, 1, 0),
    dummy_esc_fund_completo = ifelse(grau_instr == 5L, 1, 0),
    dummy_esc_medio_incompleto = ifelse(grau_instr == 6L, 1, 0),
    dummy_esc_medio_completo = ifelse(grau_instr == 7L, 1, 0),
    dummy_esc_sup_incompleto = ifelse(grau_instr == 8L, 1, 0),
    dummy_esc_sup_completo = ifelse(grau_instr == 9L, 1, 0),
    dummy_esc_mestrado = ifelse(grau_instr == 10L, 1, 0),
    dummy_esc_doutorado = ifelse(grau_instr == 11L, 1, 0),

    # Idade
    Idade = 2025 - year(ymd(data_nasc)),
    FaixaEtaria = case_when(
      Idade < 10 ~ "0 a 9",
      Idade <= 14 ~ "10 a 14",
      Idade <= 19 ~ "15 a 19",
      Idade <= 24 ~ "20 a 24",
      Idade <= 29 ~ "25 a 29",
      Idade <= 34 ~ "30 a 34",
      Idade <= 39 ~ "35 a 39",
      Idade <= 44 ~ "40 a 44",
      Idade <= 49 ~ "45 a 49",
      Idade <= 54 ~ "50 a 54",
      Idade <= 59 ~ "55 a 59",
      Idade <= 64 ~ "60 a 64",
      Idade <= 69 ~ "65 a 69",
      Idade <= 74 ~ "70 a 74",
      Idade <= 79 ~ "75 a 79",
      Idade <= 108 ~ "80 ou mais",
      is.na(Idade) ~ NA,
      TRUE ~ "erro"
    ),

    # Indica o número de valores que são missing em 'rem_media_reais'
    dummy_RemuneracaoNA = ifelse(is.na(rem_media_reais), 1, 0)
  ) %>%

  group_by(cd_identificador_osc) %>%

  summarise(
    # Total de Vínculos
    n_vinculos = n(),

    # Gênero
    n_vinculos_mulheres = sum(dummy_mulheres, na.rm = TRUE),
    n_vinculos_homens = n_vinculos - n_vinculos_mulheres,

    # Trabalhadores com deficiência
    n_vinculos_defic = sum(dummy_defic, na.rm = TRUE),

    # Raça
    n_vinculos_branco = sum(dummy_branco, na.rm = TRUE),
    n_vinculos_indigena = sum(dummy_indigena, na.rm = TRUE),
    n_vinculos_preto = sum(dummy_preto, na.rm = TRUE),
    n_vinculos_pardo = sum(dummy_pardo, na.rm = TRUE),
    n_vinculos_amarelo = sum(dummy_amarelo, na.rm = TRUE),

    # Escolaridade
    n_vinculos_esc_analfabeto = sum(dummy_esc_analfabeto, na.rm = TRUE),
    n_vinculos_esc_leescreve = sum(dummy_esc_leescreve, na.rm = TRUE),
    n_vinculos_esc_fund_0ate5ano = sum(dummy_esc_fund_0ate5ano, na.rm = TRUE),
    n_vinculos_esc_fund_6ate9ano = sum(dummy_esc_fund_6ate9ano, na.rm = TRUE),
    n_vinculos_esc_fund_completo = sum(dummy_esc_fund_completo, na.rm = TRUE),
    n_vinculos_esc_medio_incompleto = sum(dummy_esc_medio_incompleto, na.rm = TRUE),
    n_vinculos_esc_medio_completo = sum(dummy_esc_medio_completo, na.rm = TRUE),
    n_vinculos_esc_sup_incompleto = sum(dummy_esc_sup_incompleto, na.rm = TRUE),
    n_vinculos_esc_sup_completo = sum(dummy_esc_sup_completo, na.rm = TRUE),
    n_vinculos_esc_mestrado = sum(dummy_esc_mestrado, na.rm = TRUE),
    n_vinculos_esc_doutorado = sum(dummy_esc_doutorado, na.rm = TRUE),

    # Idade
    n_vinculos_idade_0a9 = sum( ifelse(FaixaEtaria == "0 a 9", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_10a14 = sum( ifelse(FaixaEtaria == "10 a 14", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_15a19 = sum( ifelse(FaixaEtaria == "15 a 19", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_20a24 = sum( ifelse(FaixaEtaria == "20 a 24", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_25a29 = sum( ifelse(FaixaEtaria == "25 a 29", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_30a34 = sum( ifelse(FaixaEtaria == "30 a 34", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_35a39 = sum( ifelse(FaixaEtaria == "35 a 39", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_40a44 = sum( ifelse(FaixaEtaria == "40 a 44", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_45a49 = sum( ifelse(FaixaEtaria == "45 a 49", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_50a54 = sum( ifelse(FaixaEtaria == "50 a 54", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_55a59 = sum( ifelse(FaixaEtaria == "55 a 59", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_60a64 = sum( ifelse(FaixaEtaria == "60 a 64", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_65a69 = sum( ifelse(FaixaEtaria == "65 a 69", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_70a74 = sum( ifelse(FaixaEtaria == "70 a 74", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_75a79 = sum( ifelse(FaixaEtaria == "75 a 79", 1, 0), na.rm = TRUE ),
    n_vinculos_idade_80mais = sum( ifelse(FaixaEtaria == "80 ou mais", 1, 0), na.rm = TRUE ),

    # IdadeSexo
    n_vinculos_mulher_idade_0a9 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria == "0 a 9", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_10a14 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "10 a 14", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_15a19 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "15 a 19", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_20a24 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "20 a 24", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_25a29 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "25 a 29", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_30a34 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "30 a 34", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_35a39 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "35 a 39", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_40a44 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "40 a 44", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_45a49 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "45 a 49", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_50a54 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "50 a 54", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_55a59 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "55 a 59", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_60a64 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "60 a 64", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_65a69 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "65 a 69", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_70a74 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "70 a 74", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_75a79 = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "75 a 79", 1, 0), na.rm = TRUE ),
    n_vinculos_mulher_idade_80mais = sum( ifelse(dummy_mulheres == 1 & FaixaEtaria ==  "80 ou mais", 1, 0), na.rm = TRUE ),

    n_vinculos_homem_idade_0a9 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria == "0 a 9", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_10a14 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "10 a 14", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_15a19 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "15 a 19", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_20a24 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "20 a 24", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_25a29 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "25 a 29", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_30a34 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "30 a 34", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_35a39 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "35 a 39", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_40a44 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "40 a 44", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_45a49 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "45 a 49", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_50a54 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "50 a 54", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_55a59 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "55 a 59", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_60a64 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "60 a 64", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_65a69 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "65 a 69", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_70a74 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "70 a 74", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_75a79 = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "75 a 79", 1, 0), na.rm = TRUE ),
    n_vinculos_homem_idade_80mais = sum( ifelse(dummy_mulheres == 0 & FaixaEtaria ==  "80 ou mais", 1, 0), na.rm = TRUE ),

    # Remuneração média
    rem_media_reais = mean(rem_media_reais, na.rm = TRUE),
    n_vinculos_RemNA = sum(dummy_RemuneracaoNA, na.rm = TRUE)
    ) %>%
  mutate(
    rem_media_reais = ifelse(is.nan(rem_media_reais), NA, rem_media_reais)
    )

# # View(ResumoRais)
rm(dados_RAIS)


# Filtra as OSCs ativas em tb_dados_gerais
OscAtiva <- tb_dados_gerais %>%
  select(id_osc, dt_fundacao_osc, dt_fechamento_osc) %>%
  # Adiciona a variável de OSC ativa
  left_join(
    select(tb_osc,
           id_osc, cd_identificador_osc, bo_osc_ativa),
    by = "id_osc"
  ) %>%
  # Filtra somente OSC ativas
  dplyr::filter(bo_osc_ativa == TRUE) %>%
  # Remove a variável de OSC ativa.
  select(-bo_osc_ativa) %>%
  # Área de atuação
  left_join(area_atuacao_clean, by = "id_osc") %>%

  # Insere Dados de Vínculos de Trabalho
  mutate(cd_identificador_osc = str_pad(cd_identificador_osc,
                                        width = 14, side = "left", pad = 0)) %>%
  left_join(ResumoRais, by = "cd_identificador_osc") %>%

  # Insere dados de município e UF.
  left_join(
    select(tb_localicazao,
           id_osc, cd_municipio),
    by = "id_osc"
  ) %>%
  mutate(UF_Id = str_sub(cd_municipio, 1, 2)) %>%
  left_join(
    select(UFs,
           UF_Id, UF_Nome, UF_Sigla, UF_Regiao),
    by = "UF_Id"
  ) %>%
  select(everything())

# names(OscAtiva)

rm(tb_osc, tb_dados_gerais, tb_localicazao)
rm(Municipios, UFs, area_atuacao_clean, ResumoRais)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Vínculos por Raça e Região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e representatividade de pessoas ocupadas por raças
# desconsideradas das análises,  por Grandes Regiões

# names(OscAtiva)

# Dados por Região
NVincRacaReg_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao) %>%
  summarise(
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_indigena = sum(n_vinculos_indigena, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE),
    Soma_Vinculos_amarelo = sum(n_vinculos_amarelo, na.rm = TRUE),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Regiao",
         UF_Regiao = ifelse(UF_Regiao == "SE", "SD", UF_Regiao)) %>%
  ungroup() %>%
  left_join(OrdemUF, by = c("UF_Regiao" = "UF_Sigla")) %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, everything())


# Checa dados
# View(NVincRacaReg_OSC)


# Dados do Brasil como um todo
NVincRacaBR_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  summarise(
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_indigena = sum(n_vinculos_indigena, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE),
    Soma_Vinculos_amarelo = sum(n_vinculos_amarelo, na.rm = TRUE),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "BR",
         NomeRegUF = "Brasil",
         UF_Ordem = 0) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Soma_Vinculos, everything())


# Checa dados
# View(NVincRacaBR_OSC)

# Calcula totais
Total_Soma_Vinculos <- NVincRacaBR_OSC$Soma_Vinculos[[1]]


# Junta tudo
Join_NVincRaca_OSC <- bind_rows(NVincRacaReg_OSC, NVincRacaBR_OSC) %>%
  arrange(UF_Ordem) %>%
  mutate(
    Per_Vinculos_branco = (Soma_Vinculos_branco / Total_Soma_Vinculos) * 100,
    Per_Vinculos_indigena = (Soma_Vinculos_indigena / Total_Soma_Vinculos) * 100,
    Per_Vinculos_preto = (Soma_Vinculos_preto / Total_Soma_Vinculos) * 100,
    Per_Vinculos_pardo = (Soma_Vinculos_pardo / Total_Soma_Vinculos) * 100,
    Per_Vinculos_amarelo = (Soma_Vinculos_amarelo / Total_Soma_Vinculos) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, UF_Ordem, NomeRegUF,
         Soma_Vinculos_indigena, Per_Vinculos_indigena,
         Soma_Vinculos_amarelo, Per_Vinculos_amarelo,

         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_preto, Per_Vinculos_preto,
         Soma_Vinculos_pardo, Per_Vinculos_pardo,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincRaca_OSC)
# names(Join_NVincRaca_OSC)

# Salva Tabela
saveRDS(Join_NVincRaca_OSC, "tables/Join_NVincRaca_OSC.RDS")

# Tabela 1:
Join_NVincRaca_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Soma_Vinculos_branco, Per_Vinculos_branco,
              Soma_Vinculos_preto, Per_Vinculos_preto,
              Soma_Vinculos_pardo, Per_Vinculos_pardo,
              Agregacao, UF_Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "Região geográfica",

    Soma_Vinculos_indigena = "N",
    Per_Vinculos_indigena = "Representatividade (%)",
    Soma_Vinculos_amarelo = "N",
    Per_Vinculos_amarelo = "Representatividade (%)",

    Soma_Vinculos = "N",
    PerVinculos = "Representatividade (%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_indigena, Soma_Vinculos_amarelo, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_indigena, Per_Vinculos_amarelo, PerVinculos),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeRegUF )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeRegUF,
      rows = (Agregacao ==  "UF") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Amarelo",
    columns = c(Soma_Vinculos_amarelo, Per_Vinculos_amarelo)
  ) %>%

  tab_spanner(
    label = "Indígena",
    columns = c(Soma_Vinculos_indigena, Per_Vinculos_indigena)
  ) %>%


  tab_spanner(
    label = "Total",
    columns = c(Soma_Vinculos, PerVinculos)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_amarelo, Per_Vinculos_indigena, PerVinculos)
    )
  )


# Tabela 2:
Join_NVincRaca_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Soma_Vinculos_indigena, Per_Vinculos_indigena,
              Soma_Vinculos_amarelo, Per_Vinculos_amarelo,
              Agregacao, UF_Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "Região geográfica",

    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "Representatividade (%)",
    Soma_Vinculos_preto = "N",
    Per_Vinculos_preto = "Representatividade (%)",
    Soma_Vinculos_pardo = "N",
    Per_Vinculos_pardo = "Representatividade (%)",

    Soma_Vinculos = "N",
    PerVinculos = "Representatividade (%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_preto, Soma_Vinculos_pardo,
                Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_preto, Per_Vinculos_pardo,
                PerVinculos),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeRegUF )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeRegUF,
      rows = (Agregacao ==  "UF") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Branco",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Preto",
    columns = c(Soma_Vinculos_preto, Per_Vinculos_preto)
  ) %>%

  tab_spanner(
    label = "Pardo",
    columns = c(Soma_Vinculos_pardo, Per_Vinculos_pardo)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(Soma_Vinculos, PerVinculos)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_preto, Per_Vinculos_pardo)
    )
  )


rm(NVincRacaReg_OSC, NVincRacaBR_OSC)
rm(Total_Soma_Vinculos)
# ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Vínculos por Raça, UF e Região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais em Grandes
# Regiões e  Unidades da Federação, por raça

# names(OscAtiva)

# Dados por UF
NVincRacaUF_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Sigla) %>%
  summarise(
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "UF") %>%
  ungroup() %>%
  left_join(OrdemUF, by = "UF_Sigla") %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, everything())


# Checa dados
# View(NVincRacaUF_OSC)


# Dados por Região
NVincRacaReg_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao) %>%
  summarise(
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Regiao",
         UF_Regiao = ifelse(UF_Regiao == "SE", "SD", UF_Regiao)) %>%
  ungroup() %>%
  left_join(OrdemUF, by = c("UF_Regiao" = "UF_Sigla")) %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, everything())


# Checa dados
# View(NVincRacaReg_OSC)


# Dados do Brasil como um todo
NVincRacaBR_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  summarise(
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "BR",
         NomeRegUF = "Brasil",
         UF_Ordem = 0) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, everything())


# Checa dados
# View(NVincRacaBR_OSC)


# Junta tudo
Join_NVincUFRegRaca_OSC <- bind_rows(NVincRacaUF_OSC,
                                NVincRacaReg_OSC,
                                NVincRacaBR_OSC) %>%
  arrange(UF_Ordem) %>%
  mutate(
    Soma_Vinculos_negro = Soma_Vinculos_preto + Soma_Vinculos_pardo,
    Soma_PB = Soma_Vinculos_branco + Soma_Vinculos_negro,
    Per_PB = 100,

    Per_Vinculos_branco = (Soma_Vinculos_branco / Soma_PB) * 100,
    Per_Vinculos_negro = (Soma_Vinculos_negro / Soma_PB) * 100
  ) %>%
  select(Agregacao, UF_Ordem, NomeRegUF,
         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_negro, Per_Vinculos_negro,
         Soma_PB, Per_PB)


# Checa dados
# View(Join_NVincUFRegRaca_OSC)
# names(Join_NVincUFRegRaca_OSC)

# Salva Tabela
saveRDS(Join_NVincUFRegRaca_OSC, "tables/Join_NVincUFRegRaca_OSC.RDS")

# Tabela inteira:
Join_NVincUFRegRaca_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "Região geográfica",

    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "(%)",
    Soma_Vinculos_negro = "N",
    Per_Vinculos_negro = "(%)",
    Soma_PB = "N",
    Per_PB = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_negro, Soma_PB),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeRegUF )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeRegUF,
      rows = (Agregacao ==  "UF") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Brancos",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Negros",
    columns = c(Soma_Vinculos_negro, Per_Vinculos_negro)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB)
    )
  )

rm(NVincRacaUF_OSC, NVincRacaReg_OSC, NVincRacaBR_OSC, Join_NVincUFRegRaca_OSC)
# ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e raça  - Geral ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais, por
# finalidades de  atuação e raça

# names(OscAtiva)

# Dados por UF
NVincAreaRaca_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincAreaRaca_OSC)

# Dados por Região
NVincSubAreaRaca_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincSubAreaRaca_OSC)


# Junta tudo
Join_NVincAreaSubAreaRaca_OSC <- bind_rows(NVincAreaRaca_OSC,
                                           NVincSubAreaRaca_OSC) %>%
  arrange(OrdemArea) %>%
  mutate(
    Soma_Vinculos_negro = Soma_Vinculos_preto + Soma_Vinculos_pardo,
    Soma_PB = Soma_Vinculos_branco + Soma_Vinculos_negro,
    Per_PB = 100,

    Per_Vinculos_branco = (Soma_Vinculos_branco / Soma_PB) * 100,
    Per_Vinculos_negro = (Soma_Vinculos_negro / Soma_PB) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_negro, Per_Vinculos_negro,
         Soma_PB, Per_PB)


# Checa dados
# View(Join_NVincAreaSubAreaRaca_OSC)
# names(Join_NVincAreaSubAreaRaca_OSC)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaRaca_OSC, "tables/Join_NVincAreaSubAreaRaca_OSC.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaRaca_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "(%)",
    Soma_Vinculos_negro = "N",
    Per_Vinculos_negro = "(%)",
    Soma_PB = "N",
    Per_PB = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_negro, Soma_PB),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeAreaSubArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Brancos",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Negros",
    columns = c(Soma_Vinculos_negro, Per_Vinculos_negro)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB)
    )
  )


rm(NVincAreaRaca_OSC, NVincSubAreaRaca_OSC, Join_NVincAreaSubAreaRaca_OSC)
# ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Vinculos por raça e área de atuação - Geral ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Distribuição do pessoal ocupado em empregos formais, por
# finalidades de atuação  e raça

# names(Join_NVincAreaSubAreaRaca_OSC)

PlotData <- Join_NVincAreaSubAreaRaca_OSC %>%
  dplyr::filter(Agregacao == "Area") %>%
  select(NomeAreaSubArea, Soma_Vinculos_negro, Soma_Vinculos_branco) %>%
  gather("key", "value", Soma_Vinculos_negro:Soma_Vinculos_branco) %>%
  group_by(NomeAreaSubArea) %>%
  mutate(Per_Vinculo = value / sum(value)) %>%
  ungroup() %>%
  mutate(NomeAreaSubArea = fct_inorder(NomeAreaSubArea),
         key = ifelse(key == "Soma_Vinculos_negro", "Negros", "Brancos")
  )

PlotData %>%
  ggplot(aes(x = Per_Vinculo, y = NomeAreaSubArea, fill = key)) +

  geom_col() +
  scale_fill_manual( values = c("blue1","blue4")  ) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_discrete(labels = label_wrap(25)) +
  geom_text(
    aes(
      label = value
    ),
    color="white",
    hjust = ifelse(PlotData$key == "Negros", 1.5, 0)
  ) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

rm(Join_NVincAreaSubAreaRaca_OSC, PlotData)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e raça  - Norte ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais, por
# finalidades de  atuação e raça

# Dados por UF
NVincAreaRaca_OSC_N <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "N") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincAreaRaca_OSC_N)

# Dados por Região
NVincSubAreaRaca_OSC_N <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "N") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincSubAreaRaca_OSC_N)


# Junta tudo
Join_NVincAreaSubAreaRaca_OSC_N <- bind_rows(NVincAreaRaca_OSC_N,
                                           NVincSubAreaRaca_OSC_N) %>%
  arrange(OrdemArea) %>%
  mutate(
    Soma_Vinculos_negro = Soma_Vinculos_preto + Soma_Vinculos_pardo,
    Soma_PB = Soma_Vinculos_branco + Soma_Vinculos_negro,
    Per_PB = 100,

    Per_Vinculos_branco = (Soma_Vinculos_branco / Soma_PB) * 100,
    Per_Vinculos_negro = (Soma_Vinculos_negro / Soma_PB) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_negro, Per_Vinculos_negro,
         Soma_PB, Per_PB)


# Checa dados
# View(Join_NVincAreaSubAreaRaca_OSC_N)
# names(Join_NVincAreaSubAreaRaca_OSC_N)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaRaca_OSC_N, "tables/Join_NVincAreaSubAreaRaca_OSC_N.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaRaca_OSC_N %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "(%)",
    Soma_Vinculos_negro = "N",
    Per_Vinculos_negro = "(%)",
    Soma_PB = "N",
    Per_PB = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_negro, Soma_PB),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeAreaSubArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Brancos",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Negros",
    columns = c(Soma_Vinculos_negro, Per_Vinculos_negro)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB)
    )
  )


rm(NVincAreaRaca_OSC_N, NVincSubAreaRaca_OSC_N, Join_NVincAreaSubAreaRaca_OSC_N)
# ls()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e raça  - Nordeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais, por
# finalidades de  atuação e raça

# Dados por UF
NVincAreaRaca_OSC_NE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "NE") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincAreaRaca_OSC_NE)

# Dados por Região
NVincSubAreaRaca_OSC_NE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "NE") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincSubAreaRaca_OSC_NE)


# Junta tudo
Join_NVincAreaSubAreaRaca_OSC_NE <- bind_rows(NVincAreaRaca_OSC_NE,
                                             NVincSubAreaRaca_OSC_NE) %>%
  arrange(OrdemArea) %>%
  mutate(
    Soma_Vinculos_negro = Soma_Vinculos_preto + Soma_Vinculos_pardo,
    Soma_PB = Soma_Vinculos_branco + Soma_Vinculos_negro,
    Per_PB = 100,

    Per_Vinculos_branco = (Soma_Vinculos_branco / Soma_PB) * 100,
    Per_Vinculos_negro = (Soma_Vinculos_negro / Soma_PB) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_negro, Per_Vinculos_negro,
         Soma_PB, Per_PB)


# Checa dados
# View(Join_NVincAreaSubAreaRaca_OSC_NE)
# names(Join_NVincAreaSubAreaRaca_OSC_NE)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaRaca_OSC_NE, "tables/Join_NVincAreaSubAreaRaca_OSC_NE.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaRaca_OSC_NE %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "(%)",
    Soma_Vinculos_negro = "N",
    Per_Vinculos_negro = "(%)",
    Soma_PB = "N",
    Per_PB = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_negro, Soma_PB),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeAreaSubArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Brancos",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Negros",
    columns = c(Soma_Vinculos_negro, Per_Vinculos_negro)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB)
    )
  )


rm(NVincAreaRaca_OSC_NE, NVincSubAreaRaca_OSC_NE, Join_NVincAreaSubAreaRaca_OSC_NE)
# ls()





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e raça  - Centro-Oeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais, por
# finalidades de  atuação e raça

# Dados por UF
NVincAreaRaca_OSC_CO <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "CO") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincAreaRaca_OSC_CO)

# Dados por Região
NVincSubAreaRaca_OSC_CO <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "CO") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincSubAreaRaca_OSC_CO)


# Junta tudo
Join_NVincAreaSubAreaRaca_OSC_CO <- bind_rows(NVincAreaRaca_OSC_CO,
                                              NVincSubAreaRaca_OSC_CO) %>%
  arrange(OrdemArea) %>%
  mutate(
    Soma_Vinculos_negro = Soma_Vinculos_preto + Soma_Vinculos_pardo,
    Soma_PB = Soma_Vinculos_branco + Soma_Vinculos_negro,
    Per_PB = 100,

    Per_Vinculos_branco = (Soma_Vinculos_branco / Soma_PB) * 100,
    Per_Vinculos_negro = (Soma_Vinculos_negro / Soma_PB) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_negro, Per_Vinculos_negro,
         Soma_PB, Per_PB)


# Checa dados
# View(Join_NVincAreaSubAreaRaca_OSC_CO)
# names(Join_NVincAreaSubAreaRaca_OSC_CO)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaRaca_OSC_CO, "tables/Join_NVincAreaSubAreaRaca_OSC_CO.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaRaca_OSC_CO %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "(%)",
    Soma_Vinculos_negro = "N",
    Per_Vinculos_negro = "(%)",
    Soma_PB = "N",
    Per_PB = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_negro, Soma_PB),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeAreaSubArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Brancos",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Negros",
    columns = c(Soma_Vinculos_negro, Per_Vinculos_negro)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB)
    )
  )


rm(NVincAreaRaca_OSC_CO, NVincSubAreaRaca_OSC_CO, Join_NVincAreaSubAreaRaca_OSC_CO)
# ls()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e raça  - Sudeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais, por
# finalidades de  atuação e raça

# Dados por UF
NVincAreaRaca_OSC_SE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "SE") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincAreaRaca_OSC_SE)

# Dados por Região
NVincSubAreaRaca_OSC_SE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "SE") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincSubAreaRaca_OSC_SE)


# Junta tudo
Join_NVincAreaSubAreaRaca_OSC_SE <- bind_rows(NVincAreaRaca_OSC_SE,
                                              NVincSubAreaRaca_OSC_SE) %>%
  arrange(OrdemArea) %>%
  mutate(
    Soma_Vinculos_negro = Soma_Vinculos_preto + Soma_Vinculos_pardo,
    Soma_PB = Soma_Vinculos_branco + Soma_Vinculos_negro,
    Per_PB = 100,

    Per_Vinculos_branco = (Soma_Vinculos_branco / Soma_PB) * 100,
    Per_Vinculos_negro = (Soma_Vinculos_negro / Soma_PB) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_negro, Per_Vinculos_negro,
         Soma_PB, Per_PB)


# Checa dados
# View(Join_NVincAreaSubAreaRaca_OSC_SE)
# names(Join_NVincAreaSubAreaRaca_OSC_SE)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaRaca_OSC_SE, "tables/Join_NVincAreaSubAreaRaca_OSC_SE.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaRaca_OSC_SE %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "(%)",
    Soma_Vinculos_negro = "N",
    Per_Vinculos_negro = "(%)",
    Soma_PB = "N",
    Per_PB = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_negro, Soma_PB),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeAreaSubArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Brancos",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Negros",
    columns = c(Soma_Vinculos_negro, Per_Vinculos_negro)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB)
    )
  )


rm(NVincAreaRaca_OSC_SE, NVincSubAreaRaca_OSC_SE, Join_NVincAreaSubAreaRaca_OSC_SE)
# ls()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e raça  - Sul ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais, por
# finalidades de  atuação e raça

# Dados por UF
NVincAreaRaca_OSC_S <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "S") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincAreaRaca_OSC_S)

# Dados por Região
NVincSubAreaRaca_OSC_S <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "S") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_branco = sum(n_vinculos_branco, na.rm = TRUE),
    Soma_Vinculos_preto = sum(n_vinculos_preto, na.rm = TRUE),
    Soma_Vinculos_pardo = sum(n_vinculos_pardo, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincSubAreaRaca_OSC_S)


# Junta tudo
Join_NVincAreaSubAreaRaca_OSC_S <- bind_rows(NVincAreaRaca_OSC_S,
                                              NVincSubAreaRaca_OSC_S) %>%
  arrange(OrdemArea) %>%
  mutate(
    Soma_Vinculos_negro = Soma_Vinculos_preto + Soma_Vinculos_pardo,
    Soma_PB = Soma_Vinculos_branco + Soma_Vinculos_negro,
    Per_PB = 100,

    Per_Vinculos_branco = (Soma_Vinculos_branco / Soma_PB) * 100,
    Per_Vinculos_negro = (Soma_Vinculos_negro / Soma_PB) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_branco, Per_Vinculos_branco,
         Soma_Vinculos_negro, Per_Vinculos_negro,
         Soma_PB, Per_PB)


# Checa dados
# View(Join_NVincAreaSubAreaRaca_OSC_S)
# names(Join_NVincAreaSubAreaRaca_OSC_S)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaRaca_OSC_S, "tables/Join_NVincAreaSubAreaRaca_OSC_S.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaRaca_OSC_S %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_branco = "N",
    Per_Vinculos_branco = "(%)",
    Soma_Vinculos_negro = "N",
    Per_Vinculos_negro = "(%)",
    Soma_PB = "N",
    Per_PB = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_branco, Soma_Vinculos_negro, Soma_PB),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeAreaSubArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Brancos",
    columns = c(Soma_Vinculos_branco, Per_Vinculos_branco)
  ) %>%

  tab_spanner(
    label = "Negros",
    columns = c(Soma_Vinculos_negro, Per_Vinculos_negro)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_branco, Per_Vinculos_negro, Per_PB)
    )
  )


rm(NVincAreaRaca_OSC_S, NVincSubAreaRaca_OSC_S, Join_NVincAreaSubAreaRaca_OSC_S)
# ls()




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Vínculos de pessoas com deficiências por área e UF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoas com deficiência nas Grandes Regiões,
# por finalidade  de atuação

# names(OscAtiva)

# Dados por Área e UF
NVincAreaRegDef_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_area_atuacao, UF_Regiao) %>%
  summarise(
    Soma_Vinculos_defic = sum(n_vinculos_defic, na.rm = TRUE),
  ) %>%
  # mutate(cd_area_atuacao = paste("Area_", cd_area_atuacao)) %>%
  ungroup() %>%
  spread(UF_Regiao, Soma_Vinculos_defic) %>%
  mutate(Agregacao = "Area") %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincAreaRegDef_OSC)

# Dados por SubArea  e UF
NVincSubAreaDef_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_subarea_atuacao, UF_Regiao) %>%
  summarise(
    Soma_Vinculos_defic = sum(n_vinculos_defic, na.rm = TRUE),
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  spread(UF_Regiao, Soma_Vinculos_defic) %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(NVincSubAreaDef_OSC)

# names(Join_NVincAreaSubAreaDef_OSC)

Total_Soma_Vinculos <- sum(OscAtiva$n_vinculos, na.rm = TRUE)


# Junta tudo
Join_NVincAreaSubAreaDef_OSC <- bind_rows(NVincAreaRegDef_OSC,
                                           NVincSubAreaDef_OSC) %>%
  arrange(OrdemArea) %>%
  mutate(
    Total_Soma_Vinculos_Def = CO + N + NE + SE + S,

    Per_CO = (CO / Total_Soma_Vinculos) * 100,
    Per_N = (N / Total_Soma_Vinculos) * 100,
    Per_NE = (NE / Total_Soma_Vinculos) * 100,
    Per_SE = (SE / Total_Soma_Vinculos) * 100,
    Per_S =  (S / Total_Soma_Vinculos) * 100,

    Per_Total =  (Total_Soma_Vinculos_Def / Total_Soma_Vinculos) * 100

  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         CO, Per_CO,
         N, Per_N,
         NE, Per_NE,
         SE, Per_SE,
         S, Per_S,
         Total_Soma_Vinculos_Def, Per_Total
         )


# Checa dados
# View(Join_NVincAreaSubAreaDef_OSC)
# names(Join_NVincAreaSubAreaDef_OSC)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaDef_OSC, "tables/Join_NVincAreaSubAreaDef_OSC.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaDef_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",

    CO = "N",
    Per_CO = "(%)",
    N = "N",
    Per_N = "(%)",
    NE = "N",
    Per_NE = "(%)",
    SE = "N",
    Per_SE = "(%)",
    S = "N",
    Per_S = "(%)",
    Total_Soma_Vinculos_Def = "N",
    Per_Total = "(%)"
    ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(CO, N, NE, SE, S, Total_Soma_Vinculos_Def),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_CO, Per_N, Per_NE, Per_SE, Per_S, Per_Total),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeAreaSubArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Centro-Oeste",
    columns = c(CO, Per_CO)
  ) %>%

  tab_spanner(
    label = "Norte",
    columns = c(N, Per_N)
  ) %>%

  tab_spanner(
    label = "Nordeste",
    columns = c(NE, Per_NE)
  ) %>%

  tab_spanner(
    label = "Sudeste",
    columns = c(SE, Per_SE)
  ) %>%

  tab_spanner(
    label = "Sul",
    columns = c(S, Per_S)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(Total_Soma_Vinculos_Def, Per_Total)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_CO, Per_N, Per_NE, Per_SE, Per_S, Per_Total)
    )
  )


rm(NVincAreaRegDef_OSC, NVincSubAreaDef_OSC, Join_NVincAreaSubAreaDef_OSC,
   Total_Soma_Vinculos)
# ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Percentual de vínculos com deficiência por porte ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gráfico - Total e percentual de pessoal ocupado com deficiência, por
# faixas de vínculos


Total_Soma_Vinculos <- sum(OscAtiva$n_vinculos, na.rm = TRUE)

# names(OscAtiva)

# Dados por Região
NVincRegDef_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao),
                n_vinculos > 0) %>%
  mutate(
    n_vinculos = ifelse(is.na(n_vinculos), 0, n_vinculos),
    PorteOSC = case_when(
      n_vinculos == 0 ~ "Sem vínculos",
      n_vinculos <= 2 ~ "De 1 a 2",
      n_vinculos <= 4 ~ "De 3 a 4",
      n_vinculos <= 9 ~ "De 5 a 9",
      n_vinculos <= 49 ~ "De 10 a 49",
      n_vinculos <= 99 ~ "De 50 a 99",
      n_vinculos <= 499 ~ "De 100 a 499",
      n_vinculos > 499 ~ "500 ou mais"
    ),
    OrdemPorte = case_when(
      PorteOSC == "Sem vínculos" ~ 1,
      PorteOSC == "De 1 a 2" ~ 2,
      PorteOSC == "De 3 a 4" ~ 3,
      PorteOSC == "De 5 a 9" ~ 4,
      PorteOSC == "De 10 a 49" ~ 5,
      PorteOSC == "De 50 a 99" ~ 6,
      PorteOSC == "De 100 a 499" ~ 7,
      PorteOSC == "500 ou mais" ~ 8,
      TRUE ~ -1 ),
    PorteOSC = fct_reorder(PorteOSC, OrdemPorte)
  ) %>%
  group_by(PorteOSC) %>%
  summarise(
    Soma_Vinculos_defic = sum(n_vinculos_defic, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Per_Vinculos_defic = (Soma_Vinculos_defic / Total_Soma_Vinculos) * 100)

# Checa dados
# View(NVincRegDef_OSC)
# names(NVincRegDef_OSC)

# Salva Tabela
saveRDS(NVincRegDef_OSC, "tables/NVincRegDef_OSC.RDS")

# Gera Gráfico:
NVincRegDef_OSC %>%
  ggplot(aes(x = PorteOSC, y = Soma_Vinculos_defic)) +

  geom_bar(stat = "identity", fill = "blue3") +
  geom_text(
    aes(
      label = format(Soma_Vinculos_defic, big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = Soma_Vinculos_defic
    ),
    position = position_dodge(width=0.9),
    vjust=-0.25,
    hjust = 1.3) +

  geom_point(aes(y = Per_Vinculos_defic * 35000), color = "red", size = 3) +
  geom_label(
    aes(
      y = Per_Vinculos_defic * 35000,
      label = format(round(Per_Vinculos_defic, 1),
                     big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE)
    ),
    fill = "pink",
    label.size = 0,
    position = position_dodge(width=0.9),
    vjust = 0.25,
    hjust = -0.2) +

  theme_classic() +

  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE),
    # Features of the first axis
    name = "Pessoal Ocupado",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~./17000, name="%")
  ) +

  theme(
    axis.title.x = element_blank(),
  )

rm(NVincRegDef_OSC, Total_Soma_Vinculos)


# Fim ####
