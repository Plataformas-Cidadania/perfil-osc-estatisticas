# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo:
# "AS OSCs E A ESCOLARIDADE DO PESSOAL OCUPADO"

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
library(scales)

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
# Concecta aos bancos de dados do MOSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Chaves do banco de dados
assert_that(file.exists(keys))

# Concecta aos bancos de dados do MOSC:
source("src/generalFunctions/postCon.R") 
conexao_mosc <- postCon(keys, Con_options = "-c search_path=osc")

dbIsValid(conexao_mosc)

rm(postCon)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leitura de Dados ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Informações sobre as OSC ativas:
# tb_osc <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_osc.RDS")
tb_osc <- dbGetQuery(conexao_mosc, "SELECT * FROM tb_osc;")

# Localização das OSC:
# tb_localizacao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_localizacao.RDS")
tb_localizacao <- dbGetQuery(conexao_mosc, "SELECT * FROM tb_localizacao;")

# Dados Gerais OSC:
# tb_dados_gerais <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_dados_gerais.RDS")
tb_dados_gerais <- dbGetQuery(conexao_mosc, "SELECT * FROM tb_dados_gerais;")

# Áreas de Atuaçãpo
# tb_area_atuacao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_area_atuacao.RDS")
tb_area_atuacao <- dbGetQuery(conexao_mosc, "SELECT * FROM tb_area_atuacao;")

dados_RAIS <- readRDS("temp/RAIS/RAIS_2022.RDS")

# Dados básicos das Unidades Federativas:
UFs <- fread("data/UFs.csv", encoding = "Latin-1") %>%
  mutate(UF_Id = as.character(UF_Id))

# Informações Básicas dos Municípios:
Municipios <- fread("data/Municipios.csv", encoding = "Latin-1")

dc_area_subarea_atuacao <- fread("data/dc_area_subarea_atuacao.csv",
                                 encoding = "Latin-1") %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_",
                            str_pad(cd_subarea_atuacao, 2, pad = "0") ) )

OrdemUF <- fread("data/OrdemUF.csv", encoding = "Latin-1")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas de uso comum no capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Vou criar aqui alguns tratamentos que vão se repetir em muitas tabelas:

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

# View(ResumoRais)
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
    select(tb_localizacao,
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

rm(tb_osc, tb_dados_gerais, tb_localizacao)
rm(Municipios, UFs, area_atuacao_clean, ResumoRais)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Escolaridade dos vínculos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Escolaridade dos vínculos

# Dados por Região
NVincEsc_OSC <- OscAtiva %>%
  summarise(
    Soma_Vinculos_ateFund = sum(n_vinculos_esc_leescreve +
                                         n_vinculos_esc_fund_completo +
                                         n_vinculos_esc_fund_0ate5ano +
                                         n_vinculos_esc_fund_6ate9ano +
                                         n_vinculos_esc_fund_completo
                                       , na.rm = TRUE),

    Soma_Vinculos_ateEM = sum(n_vinculos_esc_medio_incompleto +
                                       n_vinculos_esc_medio_completo
                                     , na.rm = TRUE),

    Soma_Vinculos_ateSupIn = sum(n_vinculos_esc_sup_incompleto
                                        , na.rm = TRUE),

    Soma_Vinculos_ateSup = sum(n_vinculos_esc_sup_completo +
                                        n_vinculos_esc_mestrado +
                                        n_vinculos_esc_doutorado
                                      , na.rm = TRUE)
  ) %>%
  gather("Escolaridade", "N") %>%
  mutate(Per_Vinculos = N / sum(N, na.rm = TRUE) * 100,
         Escolaridade = case_when(
           Escolaridade == "Soma_Vinculos_ateFund"  ~ "Até fundamental completo",
           Escolaridade == "Soma_Vinculos_ateEM"  ~ "Até médio completo",
           Escolaridade == "Soma_Vinculos_ateSupIn" ~ "Superior incompleto",
           Escolaridade == "Soma_Vinculos_ateSup" ~ "Superior completo" )
         ) %>%

  bind_rows(.,
            summarise(.,
                      Escolaridade = "Total",
                      N = sum(N),
                      Per_Vinculos = sum(Per_Vinculos) )
            )

# Checa dados
NVincEsc_OSC
# names(NVincEsc_OSC)

# Salva Tabela
saveRDS(NVincEsc_OSC, "tables/NVincEsc_OSC.RDS")

# Tabela 1:
NVincEsc_OSC %>%
  gt(locale = "pt-BR") %>%
  # Nomes amigáveis das Colunas
  cols_label(
    Escolaridade = "Escolaridade",
    N = "N",
    Per_Vinculos = "(%)"
    ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(N),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !Escolaridade )
  )




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Escolaridade dos vínculos por UF e Região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Escolaridade dos vínculos, por Grande Região e UF

# names(OscAtiva)

TotalVinculos = sum(OscAtiva$n_vinculos, na.rm = TRUE)

# Dados por UF
NVincEscUF_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao, UF_Sigla) %>%
  summarise(
    Soma_Vinculos_SemSup = sum(n_vinculos_esc_analfabeto +
                               n_vinculos_esc_leescreve +
                               n_vinculos_esc_fund_0ate5ano +
                               n_vinculos_esc_fund_6ate9ano +
                               n_vinculos_esc_fund_completo +
                               n_vinculos_esc_medio_incompleto +
                               n_vinculos_esc_medio_completo +
                               n_vinculos_esc_sup_incompleto,
                               na.rm = TRUE),

    Soma_Vinculos_Sup = sum(n_vinculos_esc_sup_completo +
                            n_vinculos_esc_mestrado +
                            n_vinculos_esc_doutorado,
                            na.rm = TRUE),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "UF") %>%
  ungroup() %>%
  bind_rows(.,
            summarise(.,
                      Soma_Vinculos_SemSup = sum(Soma_Vinculos_SemSup, na.rm = TRUE),
                      Soma_Vinculos_Sup = sum(Soma_Vinculos_Sup, na.rm = TRUE),
                      Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
                      Agregacao = "Regiao",
                      .by = UF_Regiao)
            ) %>%

  bind_rows(.,
            summarise(dplyr::filter(., Agregacao == "UF"),
                      Soma_Vinculos_SemSup = sum(Soma_Vinculos_SemSup, na.rm = TRUE),
                      Soma_Vinculos_Sup = sum(Soma_Vinculos_Sup, na.rm = TRUE),
                      Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
                      Agregacao = "BR",
                      UF_Sigla = "BR")
            ) %>%

  mutate(
    UF_Regiao = ifelse(UF_Regiao == "SE", "SD", UF_Regiao),
    UF_Sigla = ifelse(is.na(UF_Sigla), UF_Regiao, UF_Sigla),
    Per_Vinculos_SemSup = (Soma_Vinculos_SemSup / TotalVinculos) * 100,
    Per_Vinculos_Sup = (Soma_Vinculos_Sup / TotalVinculos) * 100,
    Per_Vinculos = (Soma_Vinculos / TotalVinculos) * 100
    ) %>%
  left_join(OrdemUF, by = c("UF_Sigla")) %>%
  arrange(UF_Ordem) %>%
  ungroup() %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF,
         Soma_Vinculos_SemSup, Per_Vinculos_SemSup,
         Soma_Vinculos_Sup, Per_Vinculos_Sup,
         Soma_Vinculos, Per_Vinculos )


# Checa dados
# View(NVincEscUF_OSC)
# names(NVincEscUF_OSC)

# Salva Tabela
saveRDS(NVincEscUF_OSC, "tables/NVincEscUF_OSC.RDS")


# Tabela 1:
NVincEscUF_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "Região geográfica",

    Soma_Vinculos_SemSup = "N",
    Per_Vinculos_SemSup = "(%)",

    Soma_Vinculos_Sup = "N",
    Per_Vinculos_Sup = "(%)",

    Soma_Vinculos = "N",
    Per_Vinculos = "(%)",
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_SemSup, Soma_Vinculos_Sup, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_SemSup, Per_Vinculos_Sup, Per_Vinculos),
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

  # Torna as UFs mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeRegUF,
      rows = (Agregacao ==  "UF") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Sem nível superior",
    columns = c(Soma_Vinculos_SemSup, Per_Vinculos_SemSup)
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(Soma_Vinculos_Sup, Per_Vinculos_Sup)
  ) %>%


  tab_spanner(
    label = "Total",
    columns = c(Soma_Vinculos, Per_Vinculos)
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
      columns = c(Per_Vinculos_SemSup, Per_Vinculos_Sup, Per_Vinculos)
    )
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Escolaridade dos vínculos por Área de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Escolaridade dos vínculos, por finalidade e região

TotalVinculos = sum(OscAtiva$n_vinculos, na.rm = TRUE)

# Dados por Região
NVincAreaSubAreaEsc_OSC <- OscAtiva %>%
  # dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos_SemSup = sum(n_vinculos_esc_analfabeto +
                                 n_vinculos_esc_leescreve +
                                 n_vinculos_esc_fund_0ate5ano +
                                 n_vinculos_esc_fund_6ate9ano +
                                 n_vinculos_esc_fund_completo +
                                 n_vinculos_esc_medio_incompleto +
                                 n_vinculos_esc_medio_completo +
                                 n_vinculos_esc_sup_incompleto,
                               na.rm = TRUE),

    Soma_Vinculos_Sup = sum(n_vinculos_esc_sup_completo +
                              n_vinculos_esc_mestrado +
                              n_vinculos_esc_doutorado,
                            na.rm = TRUE),

    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, cd_area_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  bind_rows(.,
            summarise(.,
                      Soma_Vinculos_SemSup = sum(Soma_Vinculos_SemSup, na.rm = TRUE),
                      Soma_Vinculos_Sup = sum(Soma_Vinculos_Sup, na.rm = TRUE),
                      Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
                      Agregacao = "Area",
                      .by = cd_area_atuacao)
  ) %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  mutate(OrdemArea = ifelse(Agregacao == "Area",
                            paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_0"),
                            OrdemArea)) %>%
  # bind_rows(.,
  #           summarise(dplyr::filter(., Agregacao == "SubArea"),
  #                     Soma_Vinculos_SemSup = sum(Soma_Vinculos_SemSup, na.rm = TRUE),
  #                     Soma_Vinculos_Sup = sum(Soma_Vinculos_Sup, na.rm = TRUE),
  #                     Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
  #                     Agregacao = "Todas",
  #                     tx_area_atuacao = "Todas",
  #                     OrdemArea = "0")
  # ) %>%
  mutate(NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                                  tx_subarea_atuacao,
                                  tx_area_atuacao),
         Per_Vinculos_SemSup = (Soma_Vinculos_SemSup / TotalVinculos) * 100,
         Per_Vinculos_Sup = (Soma_Vinculos_Sup / TotalVinculos) * 100,
         Per_Vinculos = (Soma_Vinculos / TotalVinculos) * 100
         ) %>%
  arrange(OrdemArea) %>%
  select(
    Agregacao, OrdemArea, NomeAreaSubArea,
    Soma_Vinculos_SemSup, Per_Vinculos_SemSup,
    Soma_Vinculos_Sup, Per_Vinculos_Sup,
    Soma_Vinculos, Per_Vinculos
  )


# Checa dados
# View(NVincAreaSubAreaEsc_OSC)
# names(NVincAreaSubAreaEsc_OSC)

# Salva Tabela
saveRDS(NVincAreaSubAreaEsc_OSC, "tables/NVincAreaSubAreaEsc_OSC.RDS")

# Tabela Inteira:
NVincAreaSubAreaEsc_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_SemSup = "N",
    Per_Vinculos_SemSup = "(%)",
    Soma_Vinculos_Sup = "N",
    Per_Vinculos_Sup = "(%)",
    Soma_Vinculos = "N",
    Per_Vinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_SemSup, Soma_Vinculos_Sup, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_Vinculos_SemSup, Per_Vinculos_Sup, Per_Vinculos),
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
    label = "Sem nível superior",
    columns = c(Soma_Vinculos_SemSup, Per_Vinculos_SemSup)
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(Soma_Vinculos_Sup, Per_Vinculos_Sup)
  ) %>%


  tab_spanner(
    label = "Total",
    columns = c(Soma_Vinculos, Per_Vinculos)
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(Per_Vinculos_SemSup, Per_Vinculos_Sup, Per_Vinculos)
    )
  )


rm(NVincAreaSubAreaEsc_OSC, TotalVinculos)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Escolaridade dos vínculos por Área de atuação e Região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Escolaridade das pessoas com vínculos de trabalho, por finalidade e
# região: Brasil (2015)

TotalVinculos = sum(OscAtiva$n_vinculos, na.rm = TRUE)

# Dados por UF
NVincEscAreaUF_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao, cd_subarea_atuacao)%>%
  summarise(
    Soma_Vinculos_Sup = sum(n_vinculos_esc_sup_completo +
                              n_vinculos_esc_mestrado +
                              n_vinculos_esc_doutorado,
                            na.rm = TRUE),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Soma_Vinculos_SemSup = Soma_Vinculos - Soma_Vinculos_Sup) %>%
  select(-Soma_Vinculos) %>%
  ungroup() %>%
  gather("key", "value", Soma_Vinculos_SemSup, Soma_Vinculos_Sup) %>%
  mutate(key2 = paste0(UF_Regiao, "_", key)) %>%
  select(-key, -UF_Regiao) %>%
  arrange(key2) %>%
  spread(key2, value) %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_area_atuacao, tx_subarea_atuacao,
                   OrdemArea),
            by = "cd_subarea_atuacao") %>%
  mutate(Agregacao = "SubArea",
         tempOrdemArea = as.numeric(str_sub(OrdemArea, 1, 2)),
         NomeArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, tx_area_atuacao, tx_subarea_atuacao,
         everything()) %>%
  bind_rows(.,
            summarise(.,
                      N_Soma_Vinculos_SemSup = sum(N_Soma_Vinculos_SemSup, na.rm = TRUE),
                      N_Soma_Vinculos_Sup = sum(N_Soma_Vinculos_Sup, na.rm = TRUE),
                      NE_Soma_Vinculos_SemSup = sum(NE_Soma_Vinculos_SemSup, na.rm = TRUE),
                      NE_Soma_Vinculos_Sup = sum(NE_Soma_Vinculos_Sup, na.rm = TRUE),
                      CO_Soma_Vinculos_SemSup = sum(CO_Soma_Vinculos_SemSup, na.rm = TRUE),
                      CO_Soma_Vinculos_Sup = sum(CO_Soma_Vinculos_Sup, na.rm = TRUE),
                      S_Soma_Vinculos_SemSup = sum(S_Soma_Vinculos_SemSup, na.rm = TRUE),
                      S_Soma_Vinculos_Sup = sum(S_Soma_Vinculos_Sup, na.rm = TRUE),
                      SD_Soma_Vinculos_SemSup = sum(SD_Soma_Vinculos_SemSup, na.rm = TRUE),
                      SD_Soma_Vinculos_Sup = sum(SD_Soma_Vinculos_Sup, na.rm = TRUE),

                      tempOrdemArea = mean(tempOrdemArea, na.rm = TRUE),
                      Agregacao = "Area",
                      .by = tx_area_atuacao)
            ) %>%
  bind_rows(.,
            summarise(dplyr::filter(., Agregacao == "SubArea"),
                      N_Soma_Vinculos_SemSup = sum(N_Soma_Vinculos_SemSup, na.rm = TRUE),
                      N_Soma_Vinculos_Sup = sum(N_Soma_Vinculos_Sup, na.rm = TRUE),
                      NE_Soma_Vinculos_SemSup = sum(NE_Soma_Vinculos_SemSup, na.rm = TRUE),
                      NE_Soma_Vinculos_Sup = sum(NE_Soma_Vinculos_Sup, na.rm = TRUE),
                      CO_Soma_Vinculos_SemSup = sum(CO_Soma_Vinculos_SemSup, na.rm = TRUE),
                      CO_Soma_Vinculos_Sup = sum(CO_Soma_Vinculos_Sup, na.rm = TRUE),
                      S_Soma_Vinculos_SemSup = sum(S_Soma_Vinculos_SemSup, na.rm = TRUE),
                      S_Soma_Vinculos_Sup = sum(S_Soma_Vinculos_Sup, na.rm = TRUE),
                      SD_Soma_Vinculos_SemSup = sum(SD_Soma_Vinculos_SemSup, na.rm = TRUE),
                      SD_Soma_Vinculos_Sup = sum(SD_Soma_Vinculos_Sup, na.rm = TRUE),

                      NomeArea = "Total",
                      OrdemArea = "00",
                      Agregacao = "Total")
  ) %>%
  mutate(
    OrdemArea = ifelse(Agregacao == "Area",
                       paste0(str_pad(tempOrdemArea,
                                      width = 2,
                                      pad = "0",
                                      side = "left"),
                              "_00"),
                       OrdemArea),
    NomeArea = ifelse(Agregacao == "Area", tx_area_atuacao, NomeArea),

    N_Per_Vinculos_SemSup = (N_Soma_Vinculos_SemSup / TotalVinculos) * 100,
    N_Per_Vinculos_Sup = (N_Soma_Vinculos_Sup / TotalVinculos) * 100,
    NE_Per_Vinculos_SemSup = (NE_Soma_Vinculos_SemSup / TotalVinculos) * 100,
    NE_Per_Vinculos_Sup = (NE_Soma_Vinculos_Sup / TotalVinculos) * 100,
    CO_Per_Vinculos_SemSup = (CO_Soma_Vinculos_SemSup / TotalVinculos) * 100,
    CO_Per_Vinculos_Sup = (CO_Soma_Vinculos_Sup / TotalVinculos) * 100,
    S_Per_Vinculos_SemSup = (S_Soma_Vinculos_SemSup / TotalVinculos) * 100,
    S_Per_Vinculos_Sup = (S_Soma_Vinculos_Sup / TotalVinculos) * 100,
    SD_Per_Vinculos_SemSup = (SD_Soma_Vinculos_SemSup / TotalVinculos) * 100,
    SD_Per_Vinculos_Sup = (SD_Soma_Vinculos_Sup / TotalVinculos) * 100

    ) %>%

  select(
    Agregacao, OrdemArea, NomeArea,
    N_Soma_Vinculos_SemSup, N_Per_Vinculos_SemSup, N_Soma_Vinculos_Sup, N_Per_Vinculos_Sup,
    NE_Soma_Vinculos_SemSup, NE_Per_Vinculos_SemSup, NE_Soma_Vinculos_Sup, NE_Per_Vinculos_Sup,
    CO_Soma_Vinculos_SemSup, CO_Per_Vinculos_SemSup, CO_Soma_Vinculos_Sup, CO_Per_Vinculos_Sup,
    S_Soma_Vinculos_SemSup, S_Per_Vinculos_SemSup, S_Soma_Vinculos_Sup, S_Per_Vinculos_Sup,
    SD_Soma_Vinculos_SemSup, SD_Per_Vinculos_SemSup, SD_Soma_Vinculos_Sup, SD_Per_Vinculos_Sup
    ) %>%
  arrange(OrdemArea)


# Checa dados
# View(NVincEscAreaUF_OSC)
# names(NVincEscAreaUF_OSC)

# Salva Tabela
saveRDS(NVincEscAreaUF_OSC, "tables/NVincEscAreaUF_OSC.RDS")

# Tabela Inteira:
NVincEscAreaUF_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeArea = "Finalidade de atuação",
    N_Soma_Vinculos_SemSup = "N",
    N_Per_Vinculos_SemSup = "(%)",
    N_Soma_Vinculos_Sup = "N",
    N_Per_Vinculos_Sup = "(%)",
    NE_Soma_Vinculos_SemSup = "N",
    NE_Per_Vinculos_SemSup = "(%)",
    NE_Soma_Vinculos_Sup = "N",
    NE_Per_Vinculos_Sup = "(%)",
    CO_Soma_Vinculos_SemSup = "N",
    CO_Per_Vinculos_SemSup = "(%)",
    CO_Soma_Vinculos_Sup = "N",
    CO_Per_Vinculos_Sup = "(%)",
    S_Soma_Vinculos_SemSup = "N",
    S_Per_Vinculos_SemSup = "(%)",
    S_Soma_Vinculos_Sup = "N",
    S_Per_Vinculos_Sup = "(%)",
    SD_Soma_Vinculos_SemSup = "N",
    SD_Per_Vinculos_SemSup = "(%)",
    SD_Soma_Vinculos_Sup = "N",
    SD_Per_Vinculos_Sup = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(N_Soma_Vinculos_SemSup, N_Soma_Vinculos_Sup,
                NE_Soma_Vinculos_SemSup, NE_Soma_Vinculos_Sup,
                CO_Soma_Vinculos_SemSup, CO_Soma_Vinculos_Sup,
                S_Soma_Vinculos_SemSup, S_Soma_Vinculos_Sup,
                SD_Soma_Vinculos_SemSup, SD_Soma_Vinculos_Sup),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(N_Per_Vinculos_SemSup, N_Per_Vinculos_Sup,
                NE_Per_Vinculos_SemSup, NE_Per_Vinculos_Sup,
                CO_Per_Vinculos_SemSup, CO_Per_Vinculos_Sup,
                S_Per_Vinculos_SemSup, S_Per_Vinculos_Sup,
                SD_Per_Vinculos_SemSup, SD_Per_Vinculos_Sup),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

    # Cria grupos de variáveis (spanners)
# Norte
tab_spanner(
  label = "Sem nível superior",
  columns = c(N_Soma_Vinculos_SemSup, N_Per_Vinculos_SemSup),
  id = "span_N_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(N_Soma_Vinculos_Sup, N_Per_Vinculos_Sup),
    id = "span_N_Sup"
  ) %>%

  tab_spanner(
    label = "Norte",
    spanners  = c("span_N_SemSup", "span_N_Sup")
  ) %>%


  # Nordeste
  tab_spanner(
    label = "Sem nível superior",
    columns = c(NE_Soma_Vinculos_SemSup, NE_Per_Vinculos_SemSup),
    id = "span_NE_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(NE_Soma_Vinculos_Sup, NE_Per_Vinculos_Sup),
    id = "span_NE_Sup"
  ) %>%

  tab_spanner(
    label = "Nordeste",
    spanners  = c("span_NE_SemSup", "span_NE_Sup")
  ) %>%

  # Centro-Oeste
  tab_spanner(
    label = "Sem nível superior",
    columns = c(CO_Soma_Vinculos_SemSup, CO_Per_Vinculos_SemSup),
    id = "span_CO_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(CO_Soma_Vinculos_Sup, CO_Per_Vinculos_Sup),
    id = "span_CO_Sup"
  ) %>%

  tab_spanner(
    label = "Centro-Oeste",
    spanners  = c("span_CO_SemSup", "span_CO_Sup")
  ) %>%

  # Sul
  tab_spanner(
    label = "Sem nível superior",
    columns = c(S_Soma_Vinculos_SemSup, S_Per_Vinculos_SemSup),
    id = "span_S_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(S_Soma_Vinculos_Sup, S_Per_Vinculos_Sup),
    id = "span_S_Sup"
  ) %>%

  tab_spanner(
    label = "Sul",
    spanners  = c("span_S_SemSup", "span_S_Sup")
  ) %>%

  # Sudeste
  tab_spanner(
    label = "Sem nível superior",
    columns = c(SD_Soma_Vinculos_SemSup, SD_Per_Vinculos_SemSup),
    id = "span_SD_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(SD_Soma_Vinculos_Sup, SD_Per_Vinculos_Sup),
    id = "span_SD_Sup"
  ) %>%

  tab_spanner(
    label = "Sudeste",
    spanners  = c("span_SD_SemSup", "span_SD_Sup")
  )  %>%
  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(N_Per_Vinculos_Sup, NE_Per_Vinculos_Sup, CO_Per_Vinculos_Sup,
                  S_Per_Vinculos_Sup, SD_Per_Vinculos_Sup)
    )
  )

# names(NVincEscAreaUF_OSC)

# Tabela 1:
NVincEscAreaUF_OSC %>%
  select(Agregacao:CO_Per_Vinculos_Sup) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeArea = "Finalidade de atuação",
    N_Soma_Vinculos_SemSup = "N",
    N_Per_Vinculos_SemSup = "(%)",
    N_Soma_Vinculos_Sup = "N",
    N_Per_Vinculos_Sup = "(%)",
    NE_Soma_Vinculos_SemSup = "N",
    NE_Per_Vinculos_SemSup = "(%)",
    NE_Soma_Vinculos_Sup = "N",
    NE_Per_Vinculos_Sup = "(%)",
    CO_Soma_Vinculos_SemSup = "N",
    CO_Per_Vinculos_SemSup = "(%)",
    CO_Soma_Vinculos_Sup = "N",
    CO_Per_Vinculos_Sup = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(N_Soma_Vinculos_SemSup, N_Soma_Vinculos_Sup,
                NE_Soma_Vinculos_SemSup, NE_Soma_Vinculos_Sup,
                CO_Soma_Vinculos_SemSup, CO_Soma_Vinculos_Sup),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(N_Per_Vinculos_SemSup, N_Per_Vinculos_Sup,
                NE_Per_Vinculos_SemSup, NE_Per_Vinculos_Sup,
                CO_Per_Vinculos_SemSup, CO_Per_Vinculos_Sup),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)
  # Norte
  tab_spanner(
    label = "Sem nível superior",
    columns = c(N_Soma_Vinculos_SemSup, N_Per_Vinculos_SemSup),
    id = "span_N_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(N_Soma_Vinculos_Sup, N_Per_Vinculos_Sup),
    id = "span_N_Sup"
  ) %>%

  tab_spanner(
    label = "Norte",
    spanners  = c("span_N_SemSup", "span_N_Sup")
  ) %>%


  # Nordeste
  tab_spanner(
    label = "Sem nível superior",
    columns = c(NE_Soma_Vinculos_SemSup, NE_Per_Vinculos_SemSup),
    id = "span_NE_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(NE_Soma_Vinculos_Sup, NE_Per_Vinculos_Sup),
    id = "span_NE_Sup"
  ) %>%

  tab_spanner(
    label = "Nordeste",
    spanners  = c("span_NE_SemSup", "span_NE_Sup")
  ) %>%

  # Centro-Oeste
  tab_spanner(
    label = "Sem nível superior",
    columns = c(CO_Soma_Vinculos_SemSup, CO_Per_Vinculos_SemSup),
    id = "span_CO_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(CO_Soma_Vinculos_Sup, CO_Per_Vinculos_Sup),
    id = "span_CO_Sup"
  ) %>%

  tab_spanner(
    label = "Centro-Oeste",
    spanners  = c("span_CO_SemSup", "span_CO_Sup")
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(N_Per_Vinculos_Sup, NE_Per_Vinculos_Sup, CO_Per_Vinculos_Sup)
    )
  )


# names(NVincEscAreaUF_OSC)

# Tabela 2:
NVincEscAreaUF_OSC %>%
  select(Agregacao:NomeArea, S_Soma_Vinculos_SemSup:SD_Per_Vinculos_Sup) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeArea = "Finalidade de atuação",
    S_Soma_Vinculos_SemSup = "N",
    S_Per_Vinculos_SemSup = "(%)",
    S_Soma_Vinculos_Sup = "N",
    S_Per_Vinculos_Sup = "(%)",
    SD_Soma_Vinculos_SemSup = "N",
    SD_Per_Vinculos_SemSup = "(%)",
    SD_Soma_Vinculos_Sup = "N",
    SD_Per_Vinculos_Sup = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(S_Soma_Vinculos_SemSup, S_Soma_Vinculos_Sup,
                SD_Soma_Vinculos_SemSup, SD_Soma_Vinculos_Sup),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(S_Per_Vinculos_SemSup, S_Per_Vinculos_Sup,
                SD_Per_Vinculos_SemSup, SD_Per_Vinculos_Sup),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeArea )
  ) %>%

  # Torna as Áreas de Atuação mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeArea,
      rows = (Agregacao ==  "SubArea") )
  ) %>%

  # Cria grupos de variáveis (spanners)

  # Sul
  tab_spanner(
    label = "Sem nível superior",
    columns = c(S_Soma_Vinculos_SemSup, S_Per_Vinculos_SemSup),
    id = "span_S_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(S_Soma_Vinculos_Sup, S_Per_Vinculos_Sup),
    id = "span_S_Sup"
  ) %>%

  tab_spanner(
    label = "Sul",
    spanners  = c("span_S_SemSup", "span_S_Sup")
  ) %>%

  # Sudeste
  tab_spanner(
    label = "Sem nível superior",
    columns = c(SD_Soma_Vinculos_SemSup, SD_Per_Vinculos_SemSup),
    id = "span_SD_SemSup"
  ) %>%

  tab_spanner(
    label = "Com nível superior",
    columns = c(SD_Soma_Vinculos_Sup, SD_Per_Vinculos_Sup),
    id = "span_SD_Sup"
  ) %>%

  tab_spanner(
    label = "Sudeste",
    spanners  = c("span_SD_SemSup", "span_SD_Sup")
  )  %>%
  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(S_Per_Vinculos_Sup, SD_Per_Vinculos_Sup)
    )
  )


rm(NVincEscAreaUF_OSC, TotalVinculos)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Pessoal ocupado com nível superior por região e área ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# GRÁFICO - Pessoal ocupado com nível superior, por região e finalidades de
# atuação (2015) (Em %)

# names(dc_area_subarea_atuacao)

dc_area_atuacao <- dc_area_subarea_atuacao %>%
  mutate(OrdemArea = str_sub(OrdemArea, 1, 2)) %>%
  distinct(cd_area_atuacao, tx_area_atuacao, OrdemArea)


TotalVinculos = sum(OscAtiva$n_vinculos, na.rm = TRUE)

# Dados por UF
NVincEscAreaReg_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao, cd_area_atuacao)%>%
  summarise(
    Soma_Vinculos_Sup = sum(n_vinculos_esc_sup_completo +
                              n_vinculos_esc_mestrado +
                              n_vinculos_esc_doutorado,
                            na.rm = TRUE),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Per_Sup = Soma_Vinculos_Sup / Soma_Vinculos) %>%
  ungroup() %>%
  left_join(dc_area_atuacao,
            by = "cd_area_atuacao") %>%
  select(OrdemArea, UF_Regiao, tx_area_atuacao, Per_Sup) %>%
  arrange(OrdemArea, UF_Regiao)


# View(NVincEscAreaReg_OSC)
# names(NVincEscAreaReg_OSC)

# Salva Tabela
saveRDS(NVincEscAreaReg_OSC, "tables/NVincEscAreaReg_OSC.RDS")

# Gráfico
NVincEscAreaReg_OSC %>%
  ggplot(aes(x = tx_area_atuacao, y = Per_Sup, fill = UF_Regiao)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = label_wrap(25)) +
  ylim(0, 0.7) +
  geom_text(
    aes(
      label = format(round(Per_Sup * 100, 1), big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = Per_Sup
    ),
    position = position_dodge(width=0.9),
    vjust=0.25,
    hjust = -0.3,
    angle = 90) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust=1),
    axis.title.y = element_blank()
    )


rm(dc_area_atuacao, NVincEscAreaReg_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Pessoal ocupado com nível superior área (BR) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# GRÁFICO - Vínculos que possuem escolaridade superior completa, por
# finalidade de atuação:  Brasil (Em %)

dc_area_atuacao <- dc_area_subarea_atuacao %>%
  mutate(OrdemArea = str_sub(OrdemArea, 1, 2)) %>%
  distinct(cd_area_atuacao, tx_area_atuacao, OrdemArea)

# Dados por área de atuação
NVincSupBR <- OscAtiva %>%
  group_by(cd_area_atuacao)%>%
  summarise(
    Soma_Vinculos_Sup = sum(n_vinculos_esc_sup_completo +
                              n_vinculos_esc_mestrado +
                              n_vinculos_esc_doutorado,
                            na.rm = TRUE),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Per_Sup = Soma_Vinculos_Sup / Soma_Vinculos) %>%
  ungroup() %>%
  left_join(dc_area_atuacao,
            by = "cd_area_atuacao") %>%
  select(OrdemArea, tx_area_atuacao, Per_Sup) %>%
  arrange(OrdemArea)


# View(NVincSupBR)
# names(NVincSupBR)

# Salva Tabela
saveRDS(NVincSupBR, "tables/NVincSupBR.RDS")

# Gráfico
NVincSupBR %>%
  ggplot(aes(x = tx_area_atuacao, y = Per_Sup)) +
  geom_bar(stat="identity", color="black", fill = "blue",
           position=position_dodge())+
  theme_minimal() +
  ylim(0, 0.65) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = label_wrap(25)) +
  geom_text(
    aes(
      label = format(round(Per_Sup * 100, 1), big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = Per_Sup
    ),
    position = position_dodge(width=0.9),
    vjust= -0.25) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust=1),
    axis.title.y = element_blank()
  )



# Fim ####
