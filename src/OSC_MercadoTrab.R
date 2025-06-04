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

OrdemUF <- fread("data/OrdemUF.csv", encoding = "Latin-1") %>%
  mutate(UF_Id = as.character(UF_Id))


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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por UF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual do pessoal ocupado em empregos formais das OSCs
# por região e  Unidades da Federação.

# Dados por UF
NVincUF_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Sigla) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
    ) %>%
  mutate(Agregacao = "UF") %>%
  ungroup() %>%
  left_join(OrdemUF, by = "UF_Sigla") %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Freq, Soma_Vinculos)

# Checa dados
# View(NVincUF_OSC)

# Dados por Região
NVincReg_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Regiao",
         UF_Regiao = ifelse(UF_Regiao == "SE", "SD", UF_Regiao)) %>%
  ungroup() %>%
  left_join(OrdemUF, by = c("UF_Regiao" = "UF_Sigla") ) %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Freq, Soma_Vinculos)

# Checa dados
# View(NVincReg_OSC)


# Dados do Brasil como um todo
NVincBR_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "BR",
         NomeRegUF = "Brasil",
         UF_Ordem = 0) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Freq, Soma_Vinculos)


# Checa dados
# View(NVincBR_OSC)

# Calcula totais
Freq_OSC <- NVincBR_OSC$Freq[[1]]
Total_Soma_Vinculos <- NVincBR_OSC$Soma_Vinculos[[1]]


# Junta tudo
Join_NVinc_OSC <- bind_rows(NVincUF_OSC, NVincReg_OSC, NVincBR_OSC) %>%
  arrange(UF_Ordem) %>%
  mutate(
    PerFreqOSC = (Freq / Freq_OSC) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Freq, PerFreqOSC,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVinc_OSC)
# names(Join_NVinc_OSC)

# Salva Tabela
saveRDS(Join_NVinc_OSC, "tables/Join_NVinc_OSC.RDS")

# Tabela Inteira:
Join_NVinc_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "Região geográfica",
    Freq = "N",
    PerFreqOSC = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerFreqOSC, PerVinculos),
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
    label = "OSCs",
    columns = c(Freq, PerFreqOSC)
  ) %>%

  tab_spanner(
    label = "Pessoal ocupado",
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
      columns = c(PerFreqOSC, PerVinculos)
    )
  )


rm(Join_NVinc_OSC, NVincBR_OSC, NVincUF_OSC)
rm(Freq_OSC, Total_Soma_Vinculos)
# ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Percentual de pessoal Ocupado por Região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gráfico - Total e percentual do pessoal ocupado em empregos formais das
# OSCs por região e  Unidades da Federação

NVincReg_OSC

# Dados por Região
GrafData_NVincReg_OSC <- NVincReg_OSC %>%
  mutate(PerVinculos = (Freq / sum(Freq) ) * 100 )

# Checa dados
# View(GrafData_NVincReg_OSC)
# names(GrafData_NVincReg_OSC)

# Salva Tabela
saveRDS(GrafData_NVincReg_OSC, "tables/GrafData_NVincReg_OSC.RDS")


# Gera Gráfico:
GrafData_NVincReg_OSC %>%
  ggplot(aes(x = NomeRegUF)) +

  geom_bar(aes(y = Soma_Vinculos), stat = "identity", fill = "blue3") +
  geom_text(
    aes(
      label = format(Soma_Vinculos, big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = Soma_Vinculos
    ),
    position = position_dodge(width=0.9),
    vjust=-0.25,
    hjust = 1.1) +

  geom_point(aes(y = PerVinculos * 17000), color = "red", size = 3) +
  geom_label(
    aes(
      y = PerVinculos * 17000,
      label = format(round(PerVinculos, 1),
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
    sec.axis = sec_axis(~./17000, name="Porcentagem do Total (%)")
    ) +

  theme(
    axis.title.x = element_blank(),
  )

rm(NVincReg_OSC, GrafData_NVincReg_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por Area de Atuação - Geral ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de OSCs em empregos formais, por finalidades de
# atuação

# Dados por UF
NVincArea_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincArea_OSC)

# Dados por Região
NVincSubArea_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincSubArea_OSC)


# Calcula totais
Freq_OSC <- sum(NVincArea_OSC$Freq, na.rm = TRUE)
Total_Soma_Vinculos <- sum(NVincArea_OSC$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubArea_OSC <- bind_rows(NVincArea_OSC, NVincSubArea_OSC) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerFreqOSC = (Freq / Freq_OSC) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Freq, PerFreqOSC,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubArea_OSC)
# names(Join_NVincAreaSubArea_OSC)

# Salva Tabela
saveRDS(Join_NVincAreaSubArea_OSC, "tables/Join_NVincAreaSubArea_OSC.RDS")

# Tabela Inteira:
Join_NVincAreaSubArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Freq = "N",
    PerFreqOSC = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerFreqOSC, PerVinculos),
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
    label = "OSCs",
    columns = c(Freq, PerFreqOSC)
  ) %>%

  tab_spanner(
    label = "Pessoal ocupado",
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
      columns = c(PerFreqOSC, PerVinculos)
    )
  )


rm(NVincSubArea_OSC, NVincArea_OSC, Join_NVincAreaSubArea_OSC)
rm(Freq_OSC, Total_Soma_Vinculos)
# ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por Area de Atuação - Região Norte ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de OSCs em empregos formais, por finalidades de
# atuação

# Dados por UF
NVincArea_OSC_N <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "N") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincArea_OSC_N)

# Dados por Região
NVincSubArea_OSC_N <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "N") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincSubArea_OSC_N)


# Calcula totais
Freq_OSC <- sum(NVincArea_OSC_N$Freq, na.rm = TRUE)
Total_Soma_Vinculos <- sum(NVincArea_OSC_N$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubArea_OSC_N <- bind_rows(NVincArea_OSC_N, NVincSubArea_OSC_N) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerFreqOSC = (Freq / Freq_OSC) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Freq, PerFreqOSC,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubArea_OSC_N)
# names(Join_NVincAreaSubArea_OSC_N)

# Salva Tabela
saveRDS(Join_NVincAreaSubArea_OSC_N, "tables/Join_NVincAreaSubArea_OSC_N.RDS")

# Tabela Inteira:
Join_NVincAreaSubArea_OSC_N %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Freq = "N",
    PerFreqOSC = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerFreqOSC, PerVinculos),
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
    label = "OSCs",
    columns = c(Freq, PerFreqOSC)
  ) %>%

  tab_spanner(
    label = "Pessoal ocupado",
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
      columns = c(PerFreqOSC, PerVinculos)
    )
  )


rm(NVincSubArea_OSC_N, NVincArea_OSC_N, Join_NVincAreaSubArea_OSC_N)
rm(Freq_OSC, Total_Soma_Vinculos)
# ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por Area de Atuação - Região Nordeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de OSCs em empregos formais, por finalidades de
# atuação


# Dados por UF
NVincArea_OSC_NE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "NE") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincArea_OSC_NE)

# Dados por Região
NVincSubArea_OSC_NE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "NE") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincSubArea_OSC_NE)


# Calcula totais
Freq_OSC <- sum(NVincArea_OSC_NE$Freq, na.rm = TRUE)
Total_Soma_Vinculos <- sum(NVincArea_OSC_NE$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubArea_OSC_NE <- bind_rows(NVincArea_OSC_NE, NVincSubArea_OSC_NE) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerFreqOSC = (Freq / Freq_OSC) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Freq, PerFreqOSC,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubArea_OSC_NE)
# names(Join_NVincAreaSubArea_OSC_NE)

# Salva Tabela
saveRDS(Join_NVincAreaSubArea_OSC_NE, "tables/Join_NVincAreaSubArea_OSC_NE.RDS")

# Tabela Inteira:
Join_NVincAreaSubArea_OSC_NE %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Freq = "N",
    PerFreqOSC = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerFreqOSC, PerVinculos),
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
    label = "OSCs",
    columns = c(Freq, PerFreqOSC)
  ) %>%

  tab_spanner(
    label = "Pessoal ocupado",
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
      columns = c(PerFreqOSC, PerVinculos)
    )
  )


rm(NVincSubArea_OSC_NE, NVincArea_OSC_NE, Join_NVincAreaSubArea_OSC_NE)
rm(Freq_OSC, Total_Soma_Vinculos)
# ls()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por Area de Atuação - Região Centro-Oeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de OSCs em empregos formais, por finalidades de
# atuação


# Dados por UF
NVincArea_OSC_CO <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "CO") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincArea_OSC_CO)

# Dados por Região
NVincSubArea_OSC_CO <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "CO") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincSubArea_OSC_CO)


# Calcula totais
Freq_OSC <- sum(NVincArea_OSC_CO$Freq, na.rm = TRUE)
Total_Soma_Vinculos <- sum(NVincArea_OSC_CO$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubArea_OSC_CO <- bind_rows(NVincArea_OSC_CO, NVincSubArea_OSC_CO) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerFreqOSC = (Freq / Freq_OSC) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Freq, PerFreqOSC,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubArea_OSC_CO)
# names(Join_NVincAreaSubArea_OSC_CO)

# Salva Tabela
saveRDS(Join_NVincAreaSubArea_OSC_CO, "tables/Join_NVincAreaSubArea_OSC_CO.RDS")

# Tabela Inteira:
Join_NVincAreaSubArea_OSC_CO %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Freq = "N",
    PerFreqOSC = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerFreqOSC, PerVinculos),
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
    label = "OSCs",
    columns = c(Freq, PerFreqOSC)
  ) %>%

  tab_spanner(
    label = "Pessoal ocupado",
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
      columns = c(PerFreqOSC, PerVinculos)
    )
  )


rm(NVincSubArea_OSC_CO, NVincArea_OSC_CO, Join_NVincAreaSubArea_OSC_CO)
rm(Freq_OSC, Total_Soma_Vinculos)
# ls()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por Area de Atuação - Região Sudeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de OSCs em empregos formais, por finalidades de
# atuação


# Dados por UF
NVincArea_OSC_SE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "SE") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincArea_OSC_SE)

# Dados por Região
NVincSubArea_OSC_SE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "SE") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincSubArea_OSC_SE)


# Calcula totais
Freq_OSC <- sum(NVincArea_OSC_SE$Freq, na.rm = TRUE)
Total_Soma_Vinculos <- sum(NVincArea_OSC_SE$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubArea_OSC_SE <- bind_rows(NVincArea_OSC_SE, NVincSubArea_OSC_SE) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerFreqOSC = (Freq / Freq_OSC) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Freq, PerFreqOSC,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubArea_OSC_SE)
# names(Join_NVincAreaSubArea_OSC_SE)

# Salva Tabela
saveRDS(Join_NVincAreaSubArea_OSC_SE, "tables/Join_NVincAreaSubArea_OSC_SE.RDS")

# Tabela Inteira:
Join_NVincAreaSubArea_OSC_SE %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Freq = "N",
    PerFreqOSC = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerFreqOSC, PerVinculos),
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
    label = "OSCs",
    columns = c(Freq, PerFreqOSC)
  ) %>%

  tab_spanner(
    label = "Pessoal ocupado",
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
      columns = c(PerFreqOSC, PerVinculos)
    )
  )


rm(NVincSubArea_OSC_SE, NVincArea_OSC_SE, Join_NVincAreaSubArea_OSC_SE)
rm(Freq_OSC, Total_Soma_Vinculos)
# ls()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por Area de Atuação - Região Sul ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de OSCs em empregos formais, por finalidades de
# atuação


# Dados por UF
NVincArea_OSC_S <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "S") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincArea_OSC_S)

# Dados por Região
NVincSubArea_OSC_S <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "S") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE)
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
# View(NVincSubArea_OSC_S)


# Calcula totais
Freq_OSC <- sum(NVincArea_OSC_S$Freq, na.rm = TRUE)
Total_Soma_Vinculos <- sum(NVincArea_OSC_S$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubArea_OSC_S <- bind_rows(NVincArea_OSC_S, NVincSubArea_OSC_S) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerFreqOSC = (Freq / Freq_OSC) * 100,
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Freq, PerFreqOSC,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubArea_OSC_S)
# names(Join_NVincAreaSubArea_OSC_S)

# Salva Tabela
saveRDS(Join_NVincAreaSubArea_OSC_S, "tables/Join_NVincAreaSubArea_OSC_S.RDS")

# Tabela Inteira:
Join_NVincAreaSubArea_OSC_S %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Freq = "N",
    PerFreqOSC = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerFreqOSC, PerVinculos),
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
    label = "OSCs",
    columns = c(Freq, PerFreqOSC)
  ) %>%

  tab_spanner(
    label = "Pessoal ocupado",
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
      columns = c(PerFreqOSC, PerVinculos)
    )
  )


rm(NVincSubArea_OSC_S, NVincArea_OSC_S, Join_NVincAreaSubArea_OSC_S)
rm(Freq_OSC, Total_Soma_Vinculos)
# ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Faixa etária e sexo do pessoal ocupado - Geral ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Distribuição etária e por sexo de pessoal ocupado em empregos
# formais, por finalidade  de atuação.


TotalOcupado <- sum(OscAtiva$n_vinculos, na.rm = TRUE)

FaixaEtariaOrdem <- str_subset(names(OscAtiva), "^n_vinculos_idade_") %>%
  enframe(name = NULL, value = "FaixaEtaria") %>%
  mutate(FaixaEtaria = str_remove(FaixaEtaria, "n_vinculos_")) %>%
  arrange(FaixaEtaria) %>%
  mutate(FaixaOrdem = row_number())

# Dados por Região
PreparaDados <- OscAtiva %>%
  select(matches("n_vinculos_mulher_"), matches("n_vinculos_homem_")) %>%
  gather("key", "value") %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(
    sexo = ifelse(str_detect(key, "_mulher_"), "f", "m"),
    FaixaEtaria = str_remove(key, "n_vinculos_mulher_"),
    FaixaEtaria = str_remove(FaixaEtaria, "n_vinculos_homem_"),
  ) %>%
  group_by(FaixaEtaria, sexo) %>%
  summarise(Freq = sum(value, na.rm = TRUE)) %>%
  ungroup()

GrafIdadeSexo <- PreparaDados %>%
  left_join(FaixaEtariaOrdem, by = "FaixaEtaria") %>%
  mutate(sexo = ifelse(sexo == "f", "Feminino", "Masculino"),
         sexo = fct_inorder(sexo),
         FaixaEtaria = str_remove(FaixaEtaria, "idade_"),
         FaixaEtaria = str_replace(FaixaEtaria, "a", " a "),
         FaixaEtaria = ifelse(FaixaEtaria == "80m a is", "80+", FaixaEtaria),
         FaixaEtaria = fct_reorder(FaixaEtaria, FaixaOrdem),
         Per = Freq / sum(Freq),
         Per = ifelse(is.nan(Per), 0, Per),
         Per = ifelse(sexo == "Masculino", -Per, Per)
         )

# Checa dados
# View(GrafIdadeSexo)
# names(GrafIdadeSexo)

# Salva Tabela
saveRDS(GrafIdadeSexo, "tables/GrafIdadeSexo.RDS")

# Gera Gráfico:
GrafIdadeSexo %>%
  ggplot(aes(x = Per, y = FaixaEtaria, fill = sexo)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  # theme_classic() +
  ylab("Grupo Etário") +
  theme(
    legend.title = element_blank()
  )

rm(TotalOcupado, PreparaDados, GrafIdadeSexo)
ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Faixa etária e sexo do pessoal ocupado - Assistencia Social ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Distribuição etária e por sexo de pessoal ocupado em empregos
# formais, por finalidade  de atuação.

TotalOcupado_AS <- sum(OscAtiva$n_vinculos[OscAtiva$cd_area_atuacao == 5], na.rm = TRUE)

# Dados por Região
PreparaDados_AS <- OscAtiva %>%
  dplyr::filter(cd_area_atuacao == 5) %>%
  select(matches("n_vinculos_mulher_"), matches("n_vinculos_homem_")) %>%
  gather("key", "value") %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(
    sexo = ifelse(str_detect(key, "_mulher_"), "f", "m"),
    FaixaEtaria = str_remove(key, "n_vinculos_mulher_"),
    FaixaEtaria = str_remove(FaixaEtaria, "n_vinculos_homem_"),
  ) %>%
  group_by(FaixaEtaria, sexo) %>%
  summarise(Freq = sum(value, na.rm = TRUE)) %>%
  ungroup()

GrafIdadeSexo_AS <- PreparaDados_AS %>%
  left_join(FaixaEtariaOrdem, by = "FaixaEtaria") %>%
  mutate(sexo = ifelse(sexo == "f", "Feminino", "Masculino"),
         sexo = fct_inorder(sexo),
         FaixaEtaria = str_remove(FaixaEtaria, "idade_"),
         FaixaEtaria = str_replace(FaixaEtaria, "a", " a "),
         FaixaEtaria = ifelse(FaixaEtaria == "80m a is", "80+", FaixaEtaria),
         FaixaEtaria = fct_reorder(FaixaEtaria, FaixaOrdem),
         Per = Freq / sum(Freq),
         Per = ifelse(is.nan(Per), 0, Per),
         Per = ifelse(sexo == "Masculino", -Per, Per)
  )

# Checa dados
# View(GrafIdadeSexo_AS)
# names(GrafIdadeSexo_AS)

# Salva Tabela
saveRDS(GrafIdadeSexo_AS, "tables/GrafIdadeSexo_AS.RDS")

# Gera Gráfico:
GrafIdadeSexo_AS %>%
  ggplot(aes(x = Per, y = FaixaEtaria, fill = sexo)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  # theme_classic() +
  ylab("Grupo Etário") +
  theme(
    legend.title = element_blank()
  )

rm(TotalOcupado_AS, PreparaDados_AS, GrafIdadeSexo_AS)
ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Faixa etária e sexo do pessoal ocupado - Educação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Distribuição etária e por sexo de pessoal ocupado em empregos
# formais, por finalidade  de atuação.

TotalOcupado_ED <- sum(OscAtiva$n_vinculos[OscAtiva$cd_area_atuacao == 4], na.rm = TRUE)

# Dados por Região
PreparaDados_ED <- OscAtiva %>%
  dplyr::filter(cd_area_atuacao == 4) %>%
  select(matches("n_vinculos_mulher_"), matches("n_vinculos_homem_")) %>%
  gather("key", "value") %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(
    sexo = ifelse(str_detect(key, "_mulher_"), "f", "m"),
    FaixaEtaria = str_remove(key, "n_vinculos_mulher_"),
    FaixaEtaria = str_remove(FaixaEtaria, "n_vinculos_homem_"),
  ) %>%
  group_by(FaixaEtaria, sexo) %>%
  summarise(Freq = sum(value, na.rm = TRUE)) %>%
  ungroup()

GrafIdadeSexo_ED <- PreparaDados_ED %>%
  left_join(FaixaEtariaOrdem, by = "FaixaEtaria") %>%
  mutate(sexo = ifelse(sexo == "f", "Feminino", "Masculino"),
         sexo = fct_inorder(sexo),
         FaixaEtaria = str_remove(FaixaEtaria, "idade_"),
         FaixaEtaria = str_replace(FaixaEtaria, "a", " a "),
         FaixaEtaria = ifelse(FaixaEtaria == "80m a is", "80+", FaixaEtaria),
         FaixaEtaria = fct_reorder(FaixaEtaria, FaixaOrdem),
         Per = Freq / sum(Freq),
         Per = ifelse(is.nan(Per), 0, Per),
         Per = ifelse(sexo == "Masculino", -Per, Per)
  )

# Checa dados
# View(GrafIdadeSexo_ED)
# names(GrafIdadeSexo_ED)

# Salva Tabela
saveRDS(GrafIdadeSexo_ED, "tables/GrafIdadeSexo_ED.RDS")

# Gera Gráfico:
GrafIdadeSexo_ED %>%
  ggplot(aes(x = Per, y = FaixaEtaria, fill = sexo)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  ylab("Grupo Etário") +
  theme(
    legend.title = element_blank()
  )

rm(TotalOcupado_ED, PreparaDados_ED, GrafIdadeSexo_ED)
ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Faixa etária e sexo do pessoal ocupado - Defesa de Direitos ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Distribuição etária e por sexo de pessoal ocupado em empregos
# formais, por finalidade  de atuação.

TotalOcupado_DD <- sum(OscAtiva$n_vinculos[OscAtiva$cd_area_atuacao == 9], na.rm = TRUE)

# Dados por Região
PreparaDados_DD <- OscAtiva %>%
  dplyr::filter(cd_area_atuacao == 9) %>%
  select(matches("n_vinculos_mulher_"), matches("n_vinculos_homem_")) %>%
  gather("key", "value") %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(
    sexo = ifelse(str_detect(key, "_mulher_"), "f", "m"),
    FaixaEtaria = str_remove(key, "n_vinculos_mulher_"),
    FaixaEtaria = str_remove(FaixaEtaria, "n_vinculos_homem_"),
  ) %>%
  group_by(FaixaEtaria, sexo) %>%
  summarise(Freq = sum(value, na.rm = TRUE)) %>%
  ungroup()

GrafIdadeSexo_DD <- PreparaDados_DD %>%
  left_join(FaixaEtariaOrdem, by = "FaixaEtaria") %>%
  mutate(sexo = ifelse(sexo == "f", "Feminino", "Masculino"),
         sexo = fct_inorder(sexo),
         FaixaEtaria = str_remove(FaixaEtaria, "idade_"),
         FaixaEtaria = str_replace(FaixaEtaria, "a", " a "),
         FaixaEtaria = ifelse(FaixaEtaria == "80m a is", "80+", FaixaEtaria),
         FaixaEtaria = fct_reorder(FaixaEtaria, FaixaOrdem),
         Per = Freq / sum(Freq),
         Per = ifelse(is.nan(Per), 0, Per),
         Per = ifelse(sexo == "Masculino", -Per, Per)
  )

# Checa dados
# View(GrafIdadeSexo_DD)
# names(GrafIdadeSexo_DD)

# Salva Tabela
saveRDS(GrafIdadeSexo_DD, "tables/GrafIdadeSexo_DD.RDS")

# Gera Gráfico:
GrafIdadeSexo_DD %>%
  ggplot(aes(x = Per, y = FaixaEtaria, fill = sexo)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  ylab("Grupo Etário") +
  theme(
    legend.title = element_blank()
  )

rm(TotalOcupado_DD, PreparaDados_DD, GrafIdadeSexo_DD)
ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Faixa etária e sexo do pessoal ocupado - Religião ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Distribuição etária e por sexo de pessoal ocupado em empregos
# formais, por finalidade  de atuação.

TotalOcupado_RL <- sum(OscAtiva$n_vinculos[OscAtiva$cd_area_atuacao == 6], na.rm = TRUE)

# Dados por Região
PreparaDados_RL <- OscAtiva %>%
  dplyr::filter(cd_area_atuacao == 6) %>%
  select(matches("n_vinculos_mulher_"), matches("n_vinculos_homem_")) %>%
  gather("key", "value") %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(
    sexo = ifelse(str_detect(key, "_mulher_"), "f", "m"),
    FaixaEtaria = str_remove(key, "n_vinculos_mulher_"),
    FaixaEtaria = str_remove(FaixaEtaria, "n_vinculos_homem_"),
  ) %>%
  group_by(FaixaEtaria, sexo) %>%
  summarise(Freq = sum(value, na.rm = TRUE)) %>%
  ungroup()

GrafIdadeSexo_RL <- PreparaDados_RL %>%
  left_join(FaixaEtariaOrdem, by = "FaixaEtaria") %>%
  mutate(sexo = ifelse(sexo == "f", "Feminino", "Masculino"),
         sexo = fct_inorder(sexo),
         FaixaEtaria = str_remove(FaixaEtaria, "idade_"),
         FaixaEtaria = str_replace(FaixaEtaria, "a", " a "),
         FaixaEtaria = ifelse(FaixaEtaria == "80m a is", "80+", FaixaEtaria),
         FaixaEtaria = fct_reorder(FaixaEtaria, FaixaOrdem),
         Per = Freq / sum(Freq),
         Per = ifelse(is.nan(Per), 0, Per),
         Per = ifelse(sexo == "Masculino", -Per, Per)
  )

# Checa dados
# View(GrafIdadeSexo_RL)
# names(GrafIdadeSexo_RL)

# Salva Tabela
saveRDS(GrafIdadeSexo_RL, "tables/GrafIdadeSexo_RL.RDS")

# Gera Gráfico:
GrafIdadeSexo_RL %>%
  ggplot(aes(x = Per, y = FaixaEtaria, fill = sexo)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  ylab("Grupo Etário") +
  theme(
    legend.title = element_blank()
  )

rm(TotalOcupado_RL, PreparaDados_RL, GrafIdadeSexo_RL)
ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Faixa etária e sexo do pessoal ocupado - Saúde ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Distribuição etária e por sexo de pessoal ocupado em empregos
# formais, por finalidade  de atuação.

TotalOcupado_SD <- sum(OscAtiva$n_vinculos[OscAtiva$cd_area_atuacao == 2], na.rm = TRUE)

# Prepara Dados básicos
PreparaDados_SD <- OscAtiva %>%
  dplyr::filter(cd_area_atuacao == 2) %>%
  select(matches("n_vinculos_mulher_"), matches("n_vinculos_homem_")) %>%
  gather("key", "value") %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(
    sexo = ifelse(str_detect(key, "_mulher_"), "f", "m"),
    FaixaEtaria = str_remove(key, "n_vinculos_mulher_"),
    FaixaEtaria = str_remove(FaixaEtaria, "n_vinculos_homem_"),
  ) %>%
  group_by(FaixaEtaria, sexo) %>%
  summarise(Freq = sum(value, na.rm = TRUE)) %>%
  ungroup()

GrafIdadeSexo_SD <- PreparaDados_SD %>%
  left_join(FaixaEtariaOrdem, by = "FaixaEtaria") %>%
  mutate(sexo = ifelse(sexo == "f", "Feminino", "Masculino"),
         sexo = fct_inorder(sexo),
         FaixaEtaria = str_remove(FaixaEtaria, "idade_"),
         FaixaEtaria = str_replace(FaixaEtaria, "a", " a "),
         FaixaEtaria = ifelse(FaixaEtaria == "80m a is", "80+", FaixaEtaria),
         FaixaEtaria = fct_reorder(FaixaEtaria, FaixaOrdem),
         Per = Freq / sum(Freq),
         Per = ifelse(is.nan(Per), 0, Per),
         Per = ifelse(sexo == "Masculino", -Per, Per)
  )

# Checa dados
# View(GrafIdadeSexo_SD)
# names(GrafIdadeSexo_SD)

# Salva Tabela
saveRDS(GrafIdadeSexo_SD, "tables/GrafIdadeSexo_SD.RDS")

# Gera Gráfico:
GrafIdadeSexo_SD %>%
  ggplot(aes(x = Per, y = FaixaEtaria, fill = sexo)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  ylab("Grupo Etário") +
  theme(
    legend.title = element_blank()
  )

rm(TotalOcupado_SD, PreparaDados_SD, GrafIdadeSexo_SD)
ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por sexo e região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual do pessoal ocupado em empregos formais das
# OSCs das Grandes  Regiões e Unidades da Federação, por sexo

# names(OscAtiva)

# Dados por UF
NVincUFSex_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Sigla) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "UF") %>%
  ungroup() %>%
  left_join(OrdemUF, by = "UF_Sigla") %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Soma_Vinculos_f, Soma_Vinculos_m,
         Soma_Vinculos)

# Checa dados
# View(NVincUFSex_OSC)

# Dados por Região
NVincRegSex_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Regiao",
         UF_Regiao = ifelse(UF_Regiao == "SE", "SD", UF_Regiao)) %>%
  ungroup() %>%
  left_join(OrdemUF, by = c("UF_Regiao" = "UF_Sigla")) %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Soma_Vinculos_f, Soma_Vinculos_m,
         Soma_Vinculos)

# Checa dados
# View(NVincRegSex_OSC)


# Dados do Brasil como um todo
NVincSexBR_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  summarise(
    Freq = n(),
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "BR",
         NomeRegUF = "Brasil",
         UF_Ordem = 0) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, Soma_Vinculos_f, Soma_Vinculos_m,
         Soma_Vinculos)


# Checa dados
# View(NVincSexBR_OSC)

# Junta tudo
Join_NVincSex_OSC <- bind_rows(NVincUFSex_OSC, NVincRegSex_OSC, NVincSexBR_OSC) %>%
  arrange(UF_Ordem) %>%
  mutate(
    PerVinculos = (Soma_Vinculos / Soma_Vinculos) * 100,
    PerVinculos_f = (Soma_Vinculos_f / Soma_Vinculos) * 100,
    PerVinculos_m = (Soma_Vinculos_m / Soma_Vinculos) * 100
  ) %>%
  select(Agregacao, UF_Ordem, NomeRegUF,
         Soma_Vinculos_f, PerVinculos_f,
         Soma_Vinculos_m, PerVinculos_m,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincSex_OSC)
# names(Join_NVincSex_OSC)

# Salva Tabela
saveRDS(Join_NVincSex_OSC, "tables/Join_NVincSex_OSC.RDS")

# Tabela Inteira:
Join_NVincSex_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "Região geográfica",
    Soma_Vinculos_f = "N",
    PerVinculos_f = "(%)",
    Soma_Vinculos_m = "N",
    PerVinculos_m = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_f, Soma_Vinculos_m, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerVinculos_f, PerVinculos_m, PerVinculos),
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
    label = "Mulher",
    columns = c(Soma_Vinculos_f, PerVinculos_f)
  ) %>%

  tab_spanner(
    label = "Homem",
    columns = c(Soma_Vinculos_m, PerVinculos_m)
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
      columns = c(PerVinculos_f, PerVinculos_m, PerVinculos)
    )
  )


rm(NVincUFSex_OSC, NVincRegSex_OSC, NVincSexBR_OSC, Join_NVincSex_OSC)
# ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e sexo  - Geral ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais em
# OSCs por finalidades  de atuação, na região Nordeste, por sexo

# Dados por UF
NVincAreaSex_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m,Soma_Vinculos)

# Checa dados
# View(NVincAreaSex_OSC)

# Dados por Região
NVincSubAreaSex_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m, Soma_Vinculos)

# Checa dados
# View(NVincSubAreaSex_OSC)


# Calcula totais
Total_Soma_Vinculos <- sum(NVincAreaSex_OSC$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubAreaSex_OSC <- bind_rows(NVincAreaSex_OSC,
                                          NVincSubAreaSex_OSC) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
    PerVinculos_f = (Soma_Vinculos_f / Total_Soma_Vinculos) * 100,
    PerVinculos_m = (Soma_Vinculos_m / Total_Soma_Vinculos) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_f, PerVinculos_f,
         Soma_Vinculos_m, PerVinculos_m,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubAreaSex_OSC)
# names(Join_NVincAreaSubAreaSex_OSC)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaSex_OSC, "tables/Join_NVincAreaSubAreaSex_OSC.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaSex_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_f = "N",
    PerVinculos_f = "(%)",
    Soma_Vinculos_m = "N",
    PerVinculos_m = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_f, Soma_Vinculos_m, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerVinculos_f, PerVinculos_m, PerVinculos),
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
    label = "Homem",
    columns = c(Soma_Vinculos_f, PerVinculos_f)
  ) %>%

  tab_spanner(
    label = "Mulher",
    columns = c(Soma_Vinculos_m, PerVinculos_m)
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
      columns = c(Soma_Vinculos_f, Soma_Vinculos_m, PerVinculos)
    )
  )


rm(NVincAreaSex_OSC, NVincSubAreaSex_OSC, Join_NVincAreaSubAreaSex_OSC)
rm(Total_Soma_Vinculos)
# ls()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Tab - Pessoal Ocupado por área de atuação e sexo  - Nordeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais em
# OSCs por finalidades  de atuação, na região Nordeste, por sexo

# Dados por UF
NVincAreaSex_OSC_NE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "NE") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m,Soma_Vinculos)

# Checa dados
# View(NVincAreaSex_OSC_NE)

# Dados por Região
NVincSubAreaSex_OSC_NE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "NE") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m, Soma_Vinculos)

# Checa dados
# View(NVincSubAreaSex_OSC_NE)


# Calcula totais
Total_Soma_Vinculos <- sum(NVincAreaSex_OSC_NE$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubAreaSex_OSC_NE <- bind_rows(NVincAreaSex_OSC_NE,
                                          NVincSubAreaSex_OSC_NE) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
    PerVinculos_f = (Soma_Vinculos_f / Total_Soma_Vinculos) * 100,
    PerVinculos_m = (Soma_Vinculos_m / Total_Soma_Vinculos) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_f, PerVinculos_f,
         Soma_Vinculos_m, PerVinculos_m,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubAreaSex_OSC_NE)
# names(Join_NVincAreaSubAreaSex_OSC_NE)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaSex_OSC_NE, "tables/Join_NVincAreaSubAreaSex_OSC_NE.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaSex_OSC_NE %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_f = "N",
    PerVinculos_f = "(%)",
    Soma_Vinculos_m = "N",
    PerVinculos_m = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_f, Soma_Vinculos_m, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerVinculos_f, PerVinculos_m, PerVinculos),
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
    label = "Homem",
    columns = c(Soma_Vinculos_f, PerVinculos_f)
  ) %>%

  tab_spanner(
    label = "Mulher",
    columns = c(Soma_Vinculos_m, PerVinculos_m)
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
      columns = c(Soma_Vinculos_f, Soma_Vinculos_m, PerVinculos)
    )
  )


rm(NVincAreaSex_OSC_NE, NVincSubAreaSex_OSC_NE, Join_NVincAreaSubAreaSex_OSC_NE)
rm(Total_Soma_Vinculos)
# ls()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e sexo  - Norte ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais em
# OSCs por finalidades  de atuação, na região Norte, por sexo

# Dados por UF
NVincAreaSex_OSC_N <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "N") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m,Soma_Vinculos)

# Checa dados
# View(NVincAreaSex_OSC_N)

# Dados por Região
NVincSubAreaSex_OSC_N <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "N") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m, Soma_Vinculos)

# Checa dados
# View(NVincSubAreaSex_OSC_N)


# Calcula totais
Total_Soma_Vinculos <- sum(NVincAreaSex_OSC_N$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubAreaSex_OSC_N <- bind_rows(NVincAreaSex_OSC_N,
                                             NVincSubAreaSex_OSC_N) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
    PerVinculos_f = (Soma_Vinculos_f / Total_Soma_Vinculos) * 100,
    PerVinculos_m = (Soma_Vinculos_m / Total_Soma_Vinculos) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_f, PerVinculos_f,
         Soma_Vinculos_m, PerVinculos_m,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubAreaSex_OSC_N)
# names(Join_NVincAreaSubAreaSex_OSC_N)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaSex_OSC_N, "tables/Join_NVincAreaSubAreaSex_OSC_N.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaSex_OSC_N %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade",
    Soma_Vinculos_f = "N",
    PerVinculos_f = "(%)",
    Soma_Vinculos_m = "N",
    PerVinculos_m = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_f, Soma_Vinculos_m, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerVinculos_f, PerVinculos_m, PerVinculos),
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
    label = "Homem",
    columns = c(Soma_Vinculos_f, PerVinculos_f)
  ) %>%

  tab_spanner(
    label = "Mulher",
    columns = c(Soma_Vinculos_m, PerVinculos_m)
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
      columns = c(Soma_Vinculos_f, Soma_Vinculos_m, PerVinculos)
    )
  )

rm(NVincAreaSex_OSC_N, NVincSubAreaSex_OSC_N, Join_NVincAreaSubAreaSex_OSC_N)
rm(Total_Soma_Vinculos)
# ls()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e sexo  - Centro-Oeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais em
# OSCs por finalidades  de atuação, na região Centro-Oeste, por sexo

# Dados por UF
NVincAreaSex_OSC_CO <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "CO") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m,Soma_Vinculos)

# Checa dados
# View(NVincAreaSex_OSC_CO)

# Dados por Região
NVincSubAreaSex_OSC_CO <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "CO") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m, Soma_Vinculos)

# Checa dados
# View(NVincSubAreaSex_OSC_CO)


# Calcula totais
Total_Soma_Vinculos <- sum(NVincAreaSex_OSC_CO$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubAreaSex_OSC_CO <- bind_rows(NVincAreaSex_OSC_CO,
                                            NVincSubAreaSex_OSC_CO) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
    PerVinculos_f = (Soma_Vinculos_f / Total_Soma_Vinculos) * 100,
    PerVinculos_m = (Soma_Vinculos_m / Total_Soma_Vinculos) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_f, PerVinculos_f,
         Soma_Vinculos_m, PerVinculos_m,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubAreaSex_OSC_CO)
# names(Join_NVincAreaSubAreaSex_OSC_CO)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaSex_OSC_CO, "tables/Join_NVincAreaSubAreaSex_OSC_CO.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaSex_OSC_CO %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_f = "N",
    PerVinculos_f = "(%)",
    Soma_Vinculos_m = "N",
    PerVinculos_m = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_f, Soma_Vinculos_m, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerVinculos_f, PerVinculos_m, PerVinculos),
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
    label = "Homem",
    columns = c(Soma_Vinculos_f, PerVinculos_f)
  ) %>%

  tab_spanner(
    label = "Mulher",
    columns = c(Soma_Vinculos_m, PerVinculos_m)
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
      columns = c(Soma_Vinculos_f, Soma_Vinculos_m, PerVinculos)
    )
  )


rm(NVincAreaSex_OSC_CO, NVincSubAreaSex_OSC_CO, Join_NVincAreaSubAreaSex_OSC_CO)
rm(Total_Soma_Vinculos)
# ls()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e sexo  - Sudeste ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais em
# OSCs por finalidades  de atuação, na região Sudeste, por sexo

# Dados por UF
NVincAreaSex_OSC_SE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "SE") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m,Soma_Vinculos)

# Checa dados
# View(NVincAreaSex_OSC_SE)

# Dados por Região
NVincSubAreaSex_OSC_SE <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "SE") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m, Soma_Vinculos)

# Checa dados
# View(NVincSubAreaSex_OSC_SE)


# Calcula totais
Total_Soma_Vinculos <- sum(NVincAreaSex_OSC_SE$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubAreaSex_OSC_SE <- bind_rows(NVincAreaSex_OSC_SE,
                                             NVincSubAreaSex_OSC_SE) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
    PerVinculos_f = (Soma_Vinculos_f / Total_Soma_Vinculos) * 100,
    PerVinculos_m = (Soma_Vinculos_m / Total_Soma_Vinculos) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_f, PerVinculos_f,
         Soma_Vinculos_m, PerVinculos_m,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubAreaSex_OSC_SE)
# names(Join_NVincAreaSubAreaSex_OSC_SE)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaSex_OSC_SE, "tables/Join_NVincAreaSubAreaSex_OSC_SE.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaSex_OSC_SE %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_f = "N",
    PerVinculos_f = "(%)",
    Soma_Vinculos_m = "N",
    PerVinculos_m = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_f, Soma_Vinculos_m, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerVinculos_f, PerVinculos_m, PerVinculos),
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
    label = "Homem",
    columns = c(Soma_Vinculos_f, PerVinculos_f)
  ) %>%

  tab_spanner(
    label = "Mulher",
    columns = c(Soma_Vinculos_m, PerVinculos_m)
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
      columns = c(Soma_Vinculos_f, Soma_Vinculos_m, PerVinculos)
    )
  )


rm(NVincAreaSex_OSC_SE, NVincSubAreaSex_OSC_SE, Join_NVincAreaSubAreaSex_OSC_SE)
rm(Total_Soma_Vinculos)
# ls()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Pessoal Ocupado por área de atuação e sexo  - Sul ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de pessoal ocupado em empregos formais em
# OSCs por finalidades  de atuação, na região Sul, por sexo

# Dados por UF
NVincAreaSex_OSC_S <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "S") %>%
  group_by(cd_area_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "Area") %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m,Soma_Vinculos)

# Checa dados
# View(NVincAreaSex_OSC_S)

# Dados por Região
NVincSubAreaSex_OSC_S <- OscAtiva %>%
  dplyr::filter(UF_Regiao == "S") %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(
    Soma_Vinculos = sum(n_vinculos, na.rm = TRUE),
    Soma_Vinculos_f = sum(n_vinculos_mulheres, na.rm = TRUE),
    Soma_Vinculos_m = sum(n_vinculos_homens, na.rm = TRUE)
  ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, Soma_Vinculos_f,
         Soma_Vinculos_m, Soma_Vinculos)

# Checa dados
# View(NVincSubAreaSex_OSC_S)


# Calcula totais
Total_Soma_Vinculos <- sum(NVincAreaSex_OSC_S$Soma_Vinculos, na.rm = TRUE)

# Junta tudo
Join_NVincAreaSubAreaSex_OSC_S <- bind_rows(NVincAreaSex_OSC_S,
                                             NVincSubAreaSex_OSC_S) %>%
  arrange(OrdemArea) %>%
  mutate(
    PerVinculos = (Soma_Vinculos / Total_Soma_Vinculos) * 100,
    PerVinculos_f = (Soma_Vinculos_f / Total_Soma_Vinculos) * 100,
    PerVinculos_m = (Soma_Vinculos_m / Total_Soma_Vinculos) * 100
  ) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea,
         Soma_Vinculos_f, PerVinculos_f,
         Soma_Vinculos_m, PerVinculos_m,
         Soma_Vinculos, PerVinculos)


# Checa dados
# View(Join_NVincAreaSubAreaSex_OSC_S)
# names(Join_NVincAreaSubAreaSex_OSC_S)

# Salva Tabela
saveRDS(Join_NVincAreaSubAreaSex_OSC_S, "tables/Join_NVincAreaSubAreaSex_OSC_S.RDS")

# Tabela Inteira:
Join_NVincAreaSubAreaSex_OSC_S %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    Soma_Vinculos_f = "N",
    PerVinculos_f = "(%)",
    Soma_Vinculos_m = "N",
    PerVinculos_m = "(%)",
    Soma_Vinculos = "N",
    PerVinculos = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Soma_Vinculos_f, Soma_Vinculos_m, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerVinculos_f, PerVinculos_m, PerVinculos),
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
    label = "Homem",
    columns = c(Soma_Vinculos_f, PerVinculos_f)
  ) %>%

  tab_spanner(
    label = "Mulher",
    columns = c(Soma_Vinculos_m, PerVinculos_m)
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
      columns = c(Soma_Vinculos_f, Soma_Vinculos_m, PerVinculos)
    )
  )


rm(NVincAreaSex_OSC_S, NVincSubAreaSex_OSC_S, Join_NVincAreaSubAreaSex_OSC_S)
rm(Total_Soma_Vinculos)
# ls()

# Fim ####
