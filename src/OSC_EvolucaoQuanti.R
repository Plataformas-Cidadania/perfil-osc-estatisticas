# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo:
# "EVOLUÇÃO QUANTITATIVA DAS OSCs"

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
print(keys)
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

# Dados básicos das Unidades Federativas:
UFs <- fread("data/UFs.csv", encoding = "Latin-1") %>%
  mutate(UF_Id = as.character(UF_Id))

# Informações Básicas dos Municípios:
Municipios <- fread("data/Municipios.csv", encoding = "Latin-1")


dc_area_subarea_atuacao <- fread("data/dc_area_subarea_atuacao.csv",
                                 encoding = "Latin-1") %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_",
                            str_pad(cd_subarea_atuacao, 2, pad = "0") ) ) %>%
  distinct(cd_subarea_atuacao, .keep_all = TRUE)


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

# names(tb_dados_gerais)

# OSC do banco (não só as ativas)
OscAtiva <- tb_dados_gerais %>%
  # Adiciona a variável de OSC ativa
  left_join(
    select(tb_osc,
           id_osc, cd_identificador_osc, bo_osc_ativa),
    by = "id_osc"
  ) %>%

  # Para esse capítulo, é imporante não deletar as OSC inativas
  # dplyr::filter(bo_osc_ativa == TRUE) %>%
  # Remove a variável de OSC ativa.
  # select(-bo_osc_ativa) %>%

  # Excluir casos estranhos (OSC fechadas e sem data de fechamento, cerca de 4.4k)
  dplyr::filter(!(!bo_osc_ativa & is.na(dt_fechamento_osc)),
                # Vou deletar casos sem data de fundação (681 casos)
                !is.na(dt_fundacao_osc)
                ) %>%
  select(id_osc, cd_identificador_osc, cd_natureza_juridica_osc,
         dt_fundacao_osc, dt_fechamento_osc, bo_osc_ativa) %>%

  # Área de atuação
  left_join(area_atuacao_clean, by = "id_osc") %>%

  # Evita problemas de pading
  mutate(cd_identificador_osc = str_pad(cd_identificador_osc,
                                        width = 14, side = "left", pad = 0),
         nr_ano_fundacao_osc = year(ymd(dt_fundacao_osc)),
         nr_ano_fechamento_osc = year(ymd(dt_fechamento_osc)),

         ) %>%
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

# table(OscAtiva$nr_ano_fechamento_osc, useNA = "always")

rm(tb_osc, tb_dados_gerais, tb_localizacao)
rm(Municipios, UFs, area_atuacao_clean)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Evolução Quantitativa das OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total de OSCs, por ano (2010-2015)

# names(OscAtiva)

QtdAno_OSC <- tibble(Ano = 2010:2024) %>%
  mutate(N_OSC = NA_integer_,
         Aberturas = NA_integer_,
         Fechamentos = NA_integer_,
         DataRef = ymd(paste0(Ano, "-12-31")))

for (i in seq_along(QtdAno_OSC$Ano)) {
  # i <- 1
  message(QtdAno_OSC$Ano[i])

  # Soma abertura de OSC
  QtdAno_OSC$Aberturas[i] <- sum(OscAtiva$nr_ano_fundacao_osc == QtdAno_OSC$Ano[i],
                                na.rm = TRUE)

  # Soma Fechamento de OSC
  QtdAno_OSC$Fechamentos[i] <- sum(OscAtiva$nr_ano_fechamento_osc == QtdAno_OSC$Ano[i],
                                  na.rm = TRUE)

  # Calcula o estoque de OSC (N de OSC)
  QtdAno_OSC$N_OSC[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAno_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAno_OSC$DataRef[i],
        na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAno_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc),
        na.rm = TRUE)
}
rm(i)

QtdAno_OSC <- QtdAno_OSC %>%
  mutate(Crescimento = ( ( N_OSC/dplyr::lag(N_OSC) ) - 1) * 100 )


# Checa dados
# View(QtdAno_OSC)
# names(QtdAno_OSC)

# Salva Tabela
saveRDS(QtdAno_OSC, "tables/QtdAno_OSC.RDS")

# Tabela Inteira:
QtdAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Aberturas, Fechamentos, DataRef)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    Ano = "Ano",
    N_OSC = "Número de OSCs",
    Crescimento = "Crescimento (%)"
  ) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    columns = Crescimento,
    missing_text = "-"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(N_OSC),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Crescimento),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !Ano )
  )


rm(QtdAno_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - OSCs por faixas de ano de criação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - OSCs por faixas de ano de criação: Brasil

# names(OscAtiva)

FaixaFundacao_OSC <- OscAtiva %>%
  select(nr_ano_fundacao_osc) %>%
  dplyr::filter(nr_ano_fundacao_osc < 2025) %>%
  mutate(FaixaAnoFund = case_when(
    is.na(nr_ano_fundacao_osc) ~ "não disponível",
    nr_ano_fundacao_osc <= 1970 ~ "Até 1970",
    nr_ano_fundacao_osc <= 1980 ~ "De 1971 a 1980",
    nr_ano_fundacao_osc <= 1990 ~ "De 1981 a 1990",
    nr_ano_fundacao_osc <= 2000 ~ "De 1991 a 2000",
    nr_ano_fundacao_osc <= 2010 ~ "De 2001 a 2010",
    nr_ano_fundacao_osc >= 2011 ~ as.character(nr_ano_fundacao_osc),
    TRUE ~ "erro"
  )) %>%
  group_by(FaixaAnoFund) %>%
  summarise(Freq = n()) %>%
  mutate(Per = (Freq / sum(Freq)) * 100 ,
         Ordem = case_when(
           FaixaAnoFund == "não disponível" ~ 0,
           FaixaAnoFund == "Até 1970" ~ 1,
           FaixaAnoFund == "De 1971 a 1980" ~ 2,
           FaixaAnoFund == "De 1981 a 1990" ~ 3,
           FaixaAnoFund == "De 1991 a 2000" ~ 4,
           FaixaAnoFund == "De 2001 a 2010" ~ 5,
           TRUE ~ as.numeric(FaixaAnoFund)
         ) ) %>%
  arrange(Ordem)

# Checa dados
# View(FaixaFundacao_OSC)
# names(FaixaFundacao_OSC)

# Salva Tabela
saveRDS(FaixaFundacao_OSC, "tables/FaixaFundacao_OSC.RDS")

# Tabela Inteira:
FaixaFundacao_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    FaixaAnoFund = "Faixas de ano de criação",
    Freq = "Total",
    Per = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Freq),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !FaixaAnoFund )
  ) %>%

  # Cria grupos de variáveis (spanners)

  tab_spanner(
    label = "OSCs",
    columns = c(Freq, Per)
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - OSCs por faixas de ano de criação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - OSCs, por década de fundação (2016) (Em %)

# names(OscAtiva)

OscAtiva <- OscAtiva

FaixaFundacao_OSC %>%
  mutate(FaixaAnoFund2 = ifelse(str_sub(FaixaAnoFund, 1, 2) == "20",
                                "De 2011 a 2022", FaixaAnoFund) ) %>%
  group_by(FaixaAnoFund2) %>%
  summarise(Per = sum(Per, na.rm = TRUE)) %>%
  ggplot(aes(x = "",y = Per, fill = FaixaAnoFund2)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +

  geom_text(aes(x = 1.65, label=percent(Per / 100, 1)),
            position = position_stack(vjust=0.5)) +

  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    legend.position="bottom",
    axis.text.x=element_blank()
  )

rm(FaixaFundacao_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Crescimento OSC por região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA -  Evolução do número e percentual de OSCs por
# Grande Região


# names(OscAtiva)

sum(is.na(OscAtiva$UF_Regiao))

QtdAnoReg_OSC <- tibble(Ano = 2010:2024) %>%
  mutate(N_OSC = NA_integer_,
         N_OSC_N = NA_integer_,
         N_OSC_NE = NA_integer_,
         N_OSC_SE = NA_integer_,
         N_OSC_S = NA_integer_,
         N_OSC_CO = NA_integer_,
         DataRef = ymd(paste0(Ano, "-12-31")))


for (i in seq_along(QtdAnoReg_OSC$Ano)) {
  # i <- 1
  message(QtdAnoReg_OSC$Ano[i])

  # Calcula o estoque de OSC (N de OSC)
  QtdAnoReg_OSC$N_OSC[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoReg_OSC$DataRef[i] &
        !is.na(OscAtiva$UF_Regiao),
        na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc) &
          !is.na(OscAtiva$UF_Regiao),
        na.rm = TRUE)


  # Calcula o estoque de OSC no Centro-Oeste
  QtdAnoReg_OSC$N_OSC_CO[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$UF_Regiao == "CO", na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$UF_Regiao == "CO", na.rm = TRUE)

  # Calcula o estoque de OSC no Nordeste
  QtdAnoReg_OSC$N_OSC_NE[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$UF_Regiao == "NE", na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$UF_Regiao == "NE", na.rm = TRUE)

  # Calcula o estoque de OSC no Sul
  QtdAnoReg_OSC$N_OSC_S[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$UF_Regiao == "S", na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$UF_Regiao == "S", na.rm = TRUE)


  # Calcula o estoque de OSC no Norte
  QtdAnoReg_OSC$N_OSC_N[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$UF_Regiao == "N", na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$UF_Regiao == "N", na.rm = TRUE)

  # Calcula o estoque de OSC no Sudeste
  QtdAnoReg_OSC$N_OSC_SE[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoReg_OSC$DataRef[i] &
          OscAtiva$UF_Regiao == "SE", na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoReg_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$UF_Regiao == "SE", na.rm = TRUE)
}
rm(i)

QtdAnoReg_OSC <- QtdAnoReg_OSC %>%
  mutate(
    Crescimento = ((N_OSC_N/lag(N_OSC_N) ) - 1) * 100 ,
    Crescimento_N = ((N_OSC_N/lag(N_OSC_N) ) - 1) * 100 ,
    Crescimento_NE = ((N_OSC_NE/lag(N_OSC_NE) ) - 1) * 100 ,
    Crescimento_SE = ((N_OSC_SE/lag(N_OSC_SE) ) - 1) * 100 ,
    Crescimento_S = ((N_OSC_S/lag(N_OSC_S) ) - 1) * 100 ,
    Crescimento_CO = ((N_OSC_CO/lag(N_OSC_CO) ) - 1) * 100,

    Per_N = (N_OSC_N/N_OSC ) * 100 ,
    Per_NE = (N_OSC_NE/N_OSC ) * 100 ,
    Per_SE = (N_OSC_SE/N_OSC ) * 100 ,
    Per_S = (N_OSC_S/N_OSC ) * 100 ,
    Per_CO = (N_OSC_CO/N_OSC ) * 100
  ) %>%
  select(Ano, N_OSC, Crescimento,
         N_OSC_N, Crescimento_N, Per_N,
         N_OSC_NE, Crescimento_NE, Per_NE,
         N_OSC_SE, Crescimento_SE, Per_SE,
         N_OSC_S, Crescimento_S, Per_S,
         N_OSC_CO, Crescimento_CO, Per_CO  )

# Checa dados
# View(QtdAnoReg_OSC)
# names(QtdAnoReg_OSC)

# Salva Tabela
saveRDS(QtdAnoReg_OSC, "tables/QtdAnoReg_OSC.RDS")

# Tabela Inteira:
QtdAnoReg_OSC %>%
  gt(locale = "pt-BR") %>%

  # Oculta algumas colunas
  cols_hide(c(Per_N, Per_NE, Per_SE, Per_S, Per_CO)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    Ano = "Ano",
    N_OSC = "N",
    Crescimento = "(%)",
    N_OSC_N = "N",
    Crescimento_N = "(%)",
    N_OSC_NE = "N",
    Crescimento_NE = "(%)",
    N_OSC_SE = "N",
    Crescimento_SE = "(%)",
    N_OSC_S = "N",
    Crescimento_S = "(%)",
    N_OSC_CO = "N",
    Crescimento_CO = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(N_OSC, N_OSC_N, N_OSC_NE, N_OSC_SE, N_OSC_S, N_OSC_CO),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Crescimento, Crescimento_N, Crescimento_NE, Crescimento_SE,
                Crescimento_S, Crescimento_CO),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !Ano )
  ) %>%

  # Cria grupos de variáveis (spanners)

  tab_spanner(
    label = "Brasil",
    columns = c(N_OSC, Crescimento)
  ) %>%

  tab_spanner(
    label = "Norte",
    columns = c(N_OSC_N, Crescimento_N)
  ) %>%

  tab_spanner(
    label = "Nordeste",
    columns = c(N_OSC_NE, Crescimento_NE)
  ) %>%

  tab_spanner(
    label = "Sudeste",
    columns = c(N_OSC_SE, Crescimento_SE)
  ) %>%

  tab_spanner(
    label = "Sul",
    columns = c(N_OSC_S, Crescimento_S)
  ) %>%

  tab_spanner(
    label = "Centro-Oeste",
    columns = c(N_OSC_CO, Crescimento_CO)
  ) %>%

  # Negrito nos spanners
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
      columns = c(Crescimento, Crescimento_N, Crescimento_NE, Crescimento_SE,
                  Crescimento_S, Crescimento_CO)
    )
  )

rm(QtdAnoReg_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Porcentagem OSC por região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - OSCs, por faixas de ano de fundação, segundo as
# Grandes Regiões: Brasil

# names(OscAtiva)

PerOSCDecadaFund <- OscAtiva %>%
  select(nr_ano_fundacao_osc, UF_Regiao) %>%

  dplyr::filter(
    nr_ano_fundacao_osc < 2025,
    !is.na(UF_Regiao)
    ) %>%

  mutate(FaixaAnoFund = case_when(
    is.na(nr_ano_fundacao_osc) ~ "não disponível",
    nr_ano_fundacao_osc <= 1970 ~ "Até 1970",
    nr_ano_fundacao_osc <= 1980 ~ "De 1971 a 1980",
    nr_ano_fundacao_osc <= 1990 ~ "De 1981 a 1990",
    nr_ano_fundacao_osc <= 2000 ~ "De 1991 a 2000",
    nr_ano_fundacao_osc <= 2010 ~ "De 2001 a 2010",
    nr_ano_fundacao_osc <= 2020 ~ "De 2011 a 2020",
    nr_ano_fundacao_osc >= 2021 ~ "De 2021 a 2024",
    TRUE ~ "erro"
  )) %>%
  group_by(UF_Regiao) %>%
  summarise(
    N_OSC = n(),
    Ate1970 = sum(FaixaAnoFund == "Até 1970", na.rm = TRUE),
    De71a80 = sum(FaixaAnoFund == "De 1971 a 1980", na.rm = TRUE),
    De81a90 = sum(FaixaAnoFund == "De 1981 a 1990", na.rm = TRUE),
    De90a2000 = sum(FaixaAnoFund == "De 1991 a 2000", na.rm = TRUE),
    De2000a2010 = sum(FaixaAnoFund == "De 2001 a 2010", na.rm = TRUE),
    De2010a2020 = sum(FaixaAnoFund == "De 2011 a 2020", na.rm = TRUE),
    De2021a2024 = sum(FaixaAnoFund == "De 2021 a 2024", na.rm = TRUE)
  ) %>%
  mutate(
    PerAte1970 = (Ate1970 / sum(Ate1970) ) * 100,
    PerDe71a80 = (De71a80 / sum(De71a80) ) * 100,
    PerDe81a90 = (De81a90 / sum(De81a90) ) * 100,
    PerDe90a2000 = (De90a2000 / sum(De90a2000) ) * 100,
    PerDe2000a2010 = (De2000a2010 / sum(De2000a2010) ) * 100,
    PerDe2010a2020 = (De2010a2020 / sum(De2010a2020) ) * 100,
    PerDe2021a2024 = (De2021a2024 / sum(De2021a2024) ) * 100,
    UF_Regiao = ifelse(UF_Regiao == "SD", "SE", UF_Regiao)
  ) %>%
  select(
    UF_Regiao, N_OSC,
    Ate1970, PerAte1970,
    De71a80, PerDe71a80,
    De81a90, PerDe81a90,
    De90a2000, PerDe90a2000,
    De2000a2010, PerDe2000a2010,
    De2010a2020, PerDe2010a2020,
    De2021a2024, PerDe2021a2024
  )

# Checa dados
# View(PerOSCDecadaFund)
# names(PerOSCDecadaFund)

# Salva Tabela
saveRDS(PerOSCDecadaFund, "tables/PerOSCDecadaFund.RDS")

# Tabela Inteira:
PerOSCDecadaFund %>%
  gt(locale = "pt-BR") %>%

  # Oculta algumas colunas
  cols_hide(c(N_OSC)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    UF_Regiao = html("Grandes<br>Regiões"),
    Ate1970 = "N",
    PerAte1970 = "(%)",
    De71a80 = "N",
    PerDe71a80 = "(%)",
    De81a90 = "N",
    PerDe81a90 = "(%)",
    De90a2000 = "N",
    PerDe90a2000 = "(%)",
    De2000a2010 = "N",
    PerDe2000a2010 = "(%)",
    De2010a2020 = "N",
    PerDe2010a2020 = "(%)",
    De2021a2024 = "N",
    PerDe2021a2024 = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Ate1970, De71a80, De81a90, De90a2000, De2000a2010, De2010a2020,
                De2010a2020),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerAte1970, PerDe71a80, PerDe81a90, PerDe90a2000,
                PerDe2000a2010, PerDe2010a2020, PerDe2021a2024),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !UF_Regiao )
  ) %>%

  # Cria grupos de variáveis (spanners)

  tab_spanner(
    label = "Até 1970",
    columns = c(Ate1970, PerAte1970)
  ) %>%

  tab_spanner(
    label = html("De 1971<br>a 1980"),
    columns = c(De71a80, PerDe71a80)
  ) %>%

  tab_spanner(
    label = html("De 1981<br>a 1990"),
    columns = c(De81a90, PerDe81a90)
  ) %>%

  tab_spanner(
    label = html("De 1991<br>a 2000"),
    columns = c(De90a2000, PerDe90a2000)
  ) %>%

  tab_spanner(
    label = html("De 2001<br>a 2010"),
    columns = c(De2000a2010, PerDe2000a2010)
  ) %>%

  tab_spanner(
    label = html("De 2011<br>a 2020"),
    columns = c(De2010a2020, PerDe2010a2020)
  ) %>%

  tab_spanner(
    label = html("De 2021<br>a 2024"),
    columns = c(De2021a2024, PerDe2021a2024)
  ) %>%

  # Negrito nos spanners
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
      columns = c(PerAte1970, PerDe71a80, PerDe81a90, PerDe90a2000,
                  PerDe2000a2010, PerDe2010a2020, PerDe2021a2024)
    )
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Porcentagem OSC por década de fundação e região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - OSCs ativas, por região e década de criação (Em %)

# names(PerOSCDecadaFund)

grafPerOSCDecadaFund <- PerOSCDecadaFund %>%
  select(UF_Regiao, starts_with("Per")) %>%
  gather(key = "key", value = "value", PerAte1970:PerDe2021a2024) %>%
  mutate(key = case_when(
    key == "PerAte1970" ~ "Até 1970",
    key == "PerDe71a80" ~ "De 1971 a 1980",
    key == "PerDe81a90" ~ "De 1981 a 1990",
    key == "PerDe90a2000" ~ "De 1991 a 2000",
    key == "PerDe2000a2010" ~ "De 2001 a 2010",
    key == "PerDe2010a2020" ~ "De 2011 a 2020",
    key == "PerDe2021a2024" ~ "De 2021 a 2024"
  ))

# Checa dados
# View(grafPerOSCDecadaFund)
# names(grafPerOSCDecadaFund)

# Salva Tabela
saveRDS(grafPerOSCDecadaFund, "tables/grafPerOSCDecadaFund.RDS")

grafPerOSCDecadaFund %>%
  ggplot(aes(x = key, y = value, group = UF_Regiao, fill = UF_Regiao)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("blue4", "blue1", "lightblue", "purple", "magenta")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 35, hjust=1)
  )


rm(PerOSCDecadaFund, grafPerOSCDecadaFund)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - OSC por década de fundação e área de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total e percentual de OSCs por finalidades de
# atuação, por década de fundação

# names(OscAtiva)

TotalOSC <- sum(OscAtiva$bo_osc_ativa, na.rm = TRUE)

PerOSCDecFundAreaSubArea <- OscAtiva %>%

  # Para essa tabela, só usamos OSC ativas
  dplyr::filter(bo_osc_ativa) %>%

  select(nr_ano_fundacao_osc, cd_subarea_atuacao) %>%

  mutate(FaixaAnoFund = case_when(
    is.na(nr_ano_fundacao_osc) ~ "não disponível",
    nr_ano_fundacao_osc <= 1970 ~ "Até 1970",
    nr_ano_fundacao_osc <= 1980 ~ "De 1971 a 1980",
    nr_ano_fundacao_osc <= 1990 ~ "De 1981 a 1990",
    nr_ano_fundacao_osc <= 2000 ~ "De 1991 a 2000",
    nr_ano_fundacao_osc <= 2010 ~ "De 2001 a 2010",
    nr_ano_fundacao_osc <= 2020 ~ "De 2011 a 2020",
    nr_ano_fundacao_osc >= 2021 ~ "De 2021 a 2024",
    TRUE ~ "erro"
  )) %>%

  group_by(cd_subarea_atuacao) %>%

  summarise(
    Ate1970 = sum(FaixaAnoFund == "Até 1970", na.rm = TRUE),
    De71a80 = sum(FaixaAnoFund == "De 1971 a 1980", na.rm = TRUE),
    De81a90 = sum(FaixaAnoFund == "De 1981 a 1990", na.rm = TRUE),
    De90a2000 = sum(FaixaAnoFund == "De 1991 a 2000", na.rm = TRUE),
    De2000a2010 = sum(FaixaAnoFund == "De 2001 a 2010", na.rm = TRUE),
    De2010a2020 = sum(FaixaAnoFund == "De 2011 a 2020", na.rm = TRUE),
    De2021a2024 = sum(FaixaAnoFund == "De 2021 a 2024", na.rm = TRUE)
  ) %>%

  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%

  # Adiciona os rótulos das subáreas
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, cd_area_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%

  # Calcula os Dados por Area:
  bind_rows(.,
            summarise(.,
                      Ate1970 = sum(Ate1970, na.rm = TRUE),
                      De71a80 = sum(De71a80, na.rm = TRUE),
                      De81a90 = sum(De81a90, na.rm = TRUE),
                      De90a2000 = sum(De90a2000, na.rm = TRUE),
                      De2000a2010 = sum(De2000a2010, na.rm = TRUE),
                      De2010a2020 = sum(De2010a2020, na.rm = TRUE),
                      De2021a2024 = sum(De2021a2024, na.rm = TRUE),
                      Agregacao = "Area",
                      .by = cd_area_atuacao
                      )
            ) %>%
  # Adiciona os rótulos das áreas
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%

  mutate(OrdemArea = ifelse(Agregacao == "Area",
                            paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_0"),
                            OrdemArea)) %>%

  # Calcula os Dados do Total:
  bind_rows(.,
            summarise(dplyr::filter(., Agregacao == "SubArea"),

                      Ate1970 = sum(Ate1970, na.rm = TRUE),
                      De71a80 = sum(De71a80, na.rm = TRUE),
                      De81a90 = sum(De81a90, na.rm = TRUE),
                      De90a2000 = sum(De90a2000, na.rm = TRUE),
                      De2000a2010 = sum(De2000a2010, na.rm = TRUE),
                      De2010a2020 = sum(De2010a2020, na.rm = TRUE),
                      De2021a2024 = sum(De2021a2024, na.rm = TRUE),

                      Agregacao = "Todas",
                      tx_area_atuacao = "Total",
                      OrdemArea = "0")
            ) %>%
  # Arruma a ordem das linhas
  arrange(OrdemArea) %>%

  mutate(
    NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                             tx_subarea_atuacao,
                             tx_area_atuacao),

    PerAte1970 = (Ate1970 /TotalOSC ) * 100,
    PerDe71a80 = (De71a80 / TotalOSC ) * 100,
    PerDe81a90 = (De81a90 / TotalOSC ) * 100,
    PerDe90a2000 = (De90a2000 / TotalOSC ) * 100,
    PerDe2000a2010 = (De2000a2010 / TotalOSC ) * 100,
    PerDe2010a2020 = (De2010a2020 / TotalOSC ) * 100,
    PerDe2021a2024 = (De2021a2024 / TotalOSC ) * 100,
  ) %>%

  select(
    NomeAreaSubArea, OrdemArea, Agregacao,
    Ate1970, PerAte1970,
    De71a80, PerDe71a80,
    De81a90, PerDe81a90,
    De90a2000, PerDe90a2000,
    De2000a2010, PerDe2000a2010,
    De2010a2020, PerDe2010a2020,
    De2021a2024, PerDe2021a2024
  )

# Checa dados
# View(PerOSCDecFundAreaSubArea)
# names(PerOSCDecFundAreaSubArea)

# Salva Tabela
saveRDS(PerOSCDecFundAreaSubArea, "tables/PerOSCDecFundAreaSubArea.RDS")

# Tabela Inteira:
PerOSCDecFundAreaSubArea %>%
  gt(locale = "pt-BR") %>%

  # Oculta algumas colunas
  cols_hide(c(OrdemArea, Agregacao)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = html("Finalidade de atuação"),
    Ate1970 = "N",
    PerAte1970 = "(%)",
    Ate1970 = "N",
    PerAte1970 = "(%)",
    De71a80 = "N",
    PerDe71a80 = "(%)",
    De81a90 = "N",
    PerDe81a90 = "(%)",
    De90a2000 = "N",
    PerDe90a2000 = "(%)",
    De2000a2010 = "N",
    PerDe2000a2010 = "(%)",
    De2010a2020 = "N",
    PerDe2010a2020 = "(%)",
    De2021a2024 = "N",
    PerDe2021a2024 = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(Ate1970,De71a80, De81a90, De90a2000, De2000a2010, De2010a2020,
                De2021a2024),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(PerAte1970, PerDe71a80, PerDe81a90, PerDe90a2000, PerDe2000a2010,
                PerDe2010a2020, PerDe2021a2024),
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
    label = "Até 1970",
    columns = c(Ate1970, PerAte1970)
  ) %>%

  tab_spanner(
    label = html("De 1971<br>a 1980"),
    columns = c(De71a80, PerDe71a80)
  ) %>%

  tab_spanner(
    label = html("De 1981<br>a 1990"),
    columns = c(De81a90, PerDe81a90)
  ) %>%

  tab_spanner(
    label = html("De 1991<br>a 2000"),
    columns = c(De90a2000, PerDe90a2000)
  ) %>%

  tab_spanner(
    label = html("De 2001<br>a 2010"),
    columns = c(De2000a2010, PerDe2000a2010)
  ) %>%

  tab_spanner(
    label = html("De 2011<br>a 2020"),
    columns = c(De2010a2020, PerDe2010a2020)
  ) %>%

  tab_spanner(
    label = html("De 2021<br>a 2024"),
    columns = c(De2021a2024, PerDe2021a2024)
  ) %>%

  # Negrito nos spanners
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
      columns = c(PerAte1970, PerDe71a80, PerDe81a90, PerDe90a2000,
                  PerDe2000a2010, PerDe2010a2020, PerDe2021a2024)
    )
  )

rm(PerOSCDecFundAreaSubArea, TotalOSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - OSCs criadas a partir de 2001, por finalidades ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - OSCs criadas a partir de 2001, por finalidades de atuação (Em %)

# names(OscAtiva)

grafRecenteOSCArea <- OscAtiva %>%

  # Para essa tabela, só usamos OSC ativas
  dplyr::filter(bo_osc_ativa,
                nr_ano_fundacao_osc >= 2001
                ) %>%

  group_by(cd_area_atuacao) %>%

  summarise(
    Freq = n()
  ) %>%
  mutate(
    Per = ( Freq / sum(Freq) ) * 100
  ) %>%
  # Adiciona os rótulos das áreas
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao")

# Checa dados
grafRecenteOSCArea
# names(grafRecenteOSCArea)

# Salva Tabela
saveRDS(grafRecenteOSCArea, "tables/grafRecenteOSCArea.RDS")

# Gráfico
grafRecenteOSCArea %>%
  ggplot(aes(x = tx_area_atuacao, y = Per)) +
  geom_bar(stat="identity", color="black", position=position_dodge(),
           fill = "blue") +
  theme_minimal() +
  ylim(0, 40) +
  geom_text(
    aes(
      label = format(round(Per, 0), big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = Per
    ),
    vjust = -0.25
  ) +
  scale_x_discrete(labels = label_wrap(25)) +

  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust=1)
  )


rm(grafRecenteOSCArea)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Total de OSCs, por naturezas jurídicas ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total de OSCs, por naturezas jurídicas


# names(OscAtiva)

unique(OscAtiva$cd_natureza_juridica_osc)

QtdAnoNatJur_OSC <- tibble(Ano = 2010:2024) %>%
  mutate(N_OSC = NA_integer_,
         N_OSC_3999 = NA_integer_,
         N_OSC_3220 = NA_integer_,
         N_OSC_3069 = NA_integer_,
         N_OSC_3301 = NA_integer_,
         DataRef = ymd(paste0(Ano, "-12-31")))


for (i in seq_along(QtdAnoNatJur_OSC$Ano)) {
  # i <- 1
  message(QtdAnoNatJur_OSC$Ano[i])

  # Calcula o estoque de OSC (N de OSC)
  QtdAnoNatJur_OSC$N_OSC[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoNatJur_OSC$DataRef[i] &
          !is.na(OscAtiva$UF_Regiao),
        na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc) &
          !is.na(OscAtiva$UF_Regiao),
        na.rm = TRUE)


  # Calcula o estoque de OSC com natureza jurídica 3999
  QtdAnoNatJur_OSC$N_OSC_3999[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$cd_natureza_juridica_osc == 3999, na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$cd_natureza_juridica_osc == 3999, na.rm = TRUE)

  # Calcula o estoque de OSC com natureza jurídica 3220
  QtdAnoNatJur_OSC$N_OSC_3220[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$cd_natureza_juridica_osc == 3220, na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$cd_natureza_juridica_osc == 3220, na.rm = TRUE)

  # Calcula o estoque de OSC com natureza jurídica 3069
  QtdAnoNatJur_OSC$N_OSC_3069[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$cd_natureza_juridica_osc == 3069, na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$cd_natureza_juridica_osc == 3069, na.rm = TRUE)

  # Calcula o estoque de OSC com natureza jurídica 3301
  QtdAnoNatJur_OSC$N_OSC_3301[i] <-
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$dt_fechamento_osc > QtdAnoNatJur_OSC$DataRef[i] &
          OscAtiva$cd_natureza_juridica_osc == 3301, na.rm = TRUE) +
    sum(OscAtiva$dt_fundacao_osc <= QtdAnoNatJur_OSC$DataRef[i] &
          is.na(OscAtiva$dt_fechamento_osc)  &
          OscAtiva$cd_natureza_juridica_osc == 3301, na.rm = TRUE)

}
rm(i)

map_chr(QtdAnoNatJur_OSC, class)

QtdAnoNatJur_OSC <- QtdAnoNatJur_OSC %>%
  mutate(
    Per_OSC_3999 = (N_OSC_3999 / N_OSC) * 100,
    Per_OSC_3220 = (N_OSC_3220 / N_OSC) * 100,
    Per_OSC_3069 = (N_OSC_3069 / N_OSC) * 100,
    Per_OSC_3301 = (N_OSC_3301 / N_OSC) * 100
  ) %>%
  select(Ano, N_OSC,
         N_OSC_3999, Per_OSC_3999,
         N_OSC_3220, Per_OSC_3220,
         N_OSC_3069, Per_OSC_3069,
         N_OSC_3301, Per_OSC_3301
         )

# Checa dados
# View(QtdAnoNatJur_OSC)
# names(QtdAnoNatJur_OSC)

# Salva Tabela
saveRDS(QtdAnoNatJur_OSC, "tables/QtdAnoNatJur_OSC.RDS")

# Tabela Inteira:
QtdAnoNatJur_OSC %>%
  gt(locale = "pt-BR") %>%

  # Oculta algumas colunas
  cols_hide(c(N_OSC)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    Ano = html("Ano"),
    N_OSC_3999 = "N",
    Per_OSC_3999 = "(%)",
    N_OSC_3220 = "N",
    Per_OSC_3220 = "(%)",
    N_OSC_3069 = "N",
    Per_OSC_3069 = "(%)",
    N_OSC_3301 = "N",
    Per_OSC_3301 = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(N_OSC_3999, N_OSC_3220, N_OSC_3069, N_OSC_3301),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(Per_OSC_3999, Per_OSC_3220, Per_OSC_3069, Per_OSC_3301),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !Ano )
  ) %>%

  # Cria grupos de variáveis (spanners)

  tab_spanner(
    label = html("Associação<br>Privada"),
    columns = c(N_OSC_3999, Per_OSC_3999)
  ) %>%

  tab_spanner(
    label = html("Organização<br>Religiosa"),
    columns = c(N_OSC_3220, Per_OSC_3220)
  ) %>%

  tab_spanner(
    label = html("Fundação<br>Privada"),
    columns = c(N_OSC_3069, Per_OSC_3069)
  ) %>%

  tab_spanner(
    label = html("Organização<br>Social (OS)"),
    columns = c(N_OSC_3301, Per_OSC_3301)
  ) %>%

  # Negrito nos spanners
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
      columns = c(Per_OSC_3999, Per_OSC_3220, Per_OSC_3069, Per_OSC_3301)
    )
  )

rm(QtdAnoNatJur_OSC)


# Fim ####

