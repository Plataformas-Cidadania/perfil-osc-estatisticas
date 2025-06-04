# Instituto de Economia Aplicada - IPEA

# Objetivo do Script: criar tabelas e gráficos para a aula apresentada para a FioCruz

# Autor do Script: Murilo Junqueira
# (m.junqueira@yahoo.com.br; murilo.junqueira@ipea.gov.br)

# Data de Criação do Scrip: 2025-05-06

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
                            str_pad(cd_subarea_atuacao, 2, pad = "0") ) ) %>%
  distinct(cd_subarea_atuacao, .keep_all = TRUE)


dc_area_atuacao <- dc_area_subarea_atuacao %>%
  mutate(OrdemArea = str_sub(OrdemArea, 1, 2)) %>%
  distinct(cd_area_atuacao, tx_area_atuacao, OrdemArea)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas de uso comum ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Vou criar aqui alguns tratamentos que vão se repetir em muitas tabelas:


OrdemUF <- tribble(
  ~UF_Sigla, ~UF_Nome,                  ~UF_Ordem,
  "BR",       "Brasil",                 1,
  "N",        "Norte",                  2,
  "AC",       "Acre",                   3,
  "AP",       "Amapá",                  4,
  "AM",       "Amazônas",               5,
  "PA",       "Pará",                   6,
  "RO",       "Rondônia",               7,
  "RR",       "Roraima",                8,
  "TO",       "Tocantis",               9,
  "NE",       "Nordeste",               10,
  "AL",       "Alagoas",                11,
  "BA",       "Bahia",                  12,
  "CE",       "Ceará",                  13,
  "MA",       "Maranhão",               14,
  "PB",       "Paraiba",                15,
  "PE",       "Pernambuco",             16,
  "PI",       "Piauí",                  17,
  "RN",       "Rio grande do Norte",    18,
  "SE",       "Sergipe",                19,
  "SD",       "Sudeste",                20,
  "ES",       "Espírito Santo",         21,
  "MG",       "Minas Gerais",           22,
  "RJ",       "Rio de Janeiro",         23,
  "SP",       "São Paulo",              24,
  "S",        "Sul",                    25,
  "PR",       "Paraná",                 26,
  "RS",       "Rio Grande do Sul",      27,
  "SC",       "Santa Catarina",         28,
  "CO",       "Centro-Oeste",           29,
  "DF",       "Distrito Federal",       30,
  "GO",       "Goiás",                  31,
  "MT",       "Mato Grosso",            32,
  "MS",       "Mato Grosso do Sul",     33
)

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
names(tb_localicazao)

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
    select(tb_localicazao,
           id_osc, cd_municipio, tx_bairro),
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

rm(tb_osc, tb_dados_gerais, tb_localicazao)
rm(Municipios, UFs, area_atuacao_clean)
ls()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas BR todo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Gráfico - Evolução Quantitativa das OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Gráfico - Total de OSCs, por ano (2001-2015)

names(OscAtiva)

QtdAno_OSC <- tibble(Ano = 2001:2024) %>%
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

# Checa dados
View(QtdAno_OSC)
names(QtdAno_OSC)


# Gráfico
QtdAno_OSC %>%
  ggplot(aes(y = N_OSC, x = Ano)) +
  geom_bar(width = 1, stat = "identity", fill = "blue", color = "black") +
  geom_text(aes(
    y = N_OSC,
    label = format(N_OSC,
                   big.mark = ".",
                   decimal.mark = ",",
                   scientific = FALSE)
    ),
    position = position_stack(vjust=0.6),
    angle = 90,
    size = 4,
    color = "yellow"
    ) +

  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  scale_x_continuous(breaks = unique(QtdAno_OSC$Ano)) +
  ylab("Número de OSC") +
  ylim(0, 1000000) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust=1)
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Gráfico - OSC por área de Atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(OscAtiva)

OSCArea <- OscAtiva %>%
  dplyr::filter(bo_osc_ativa) %>%
  group_by(cd_area_atuacao) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  left_join(dc_area_atuacao,
            by = "cd_area_atuacao") %>%
  dplyr::filter(!is.na(OrdemArea)) %>%
  mutate(Per = Freq / sum(Freq),
         tx_area_atuacao = fct_reorder(tx_area_atuacao, OrdemArea))

OSCArea
names(OSCArea)

OSCArea %>%
  ggplot(aes(x = tx_area_atuacao, y = Per)) +
  geom_bar(stat="identity", color="black", fill = "blue",
           position=position_dodge())+
  theme_classic() +
  # ylim(0, 0.65) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = label_wrap(25)) +
  geom_text(
    aes(
      label = percent(Per, accuracy = 0.1, decimal.mark = ","),
      y = Per
    ),
    position = position_dodge(width=0.9),
    vjust= -0.25) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust=1),
    axis.title.y = element_blank()
  )

rm(OSCArea)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas RJ ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Gráfico - Evolução Quantitativa das OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Gráfico - Total de OSCs, por ano (2001-2015)

names(OscAtiva)
map_chr(OscAtiva, class)

QtdAno_OSC_RJ <- tibble(Ano = 2001:2024) %>%
  mutate(N_OSC = NA_integer_,
         N_OSC_ERJ = NA_integer_,
         N_OSC_CRJ = NA_integer_,
         N_OSC_Rocinha = NA_integer_,
         N_OSC_Vidigal = NA_integer_,
         N_OSC_VigarioG = NA_integer_,
         N_OSC_CidadeDeus = NA_integer_,
         DataRef = ymd(paste0(Ano, "-12-31")))

plotDataNOSC_RJ <- OscAtiva %>%
  mutate(
    ERJ = ifelse(UF_Sigla == "RJ", 1, 0),
    CRJ = ifelse(cd_municipio == 3304557, 1, 0),
    Rocinha = ifelse(CRJ == 1 & str_detect(tx_bairro, "ROCINHA"), 1, 0),
    Vidigal = ifelse(CRJ == 1 & str_detect(tx_bairro, "VIDIGAL"), 1, 0),
    VigarioG = ifelse(CRJ == 1 & str_detect(tx_bairro, "VIGARIO GERAL"), 1, 0),
    CidadeDeus = ifelse(CRJ == 1 & str_detect(tx_bairro, "CIDADE DE DEUS"), 1, 0),
    ) %>%
  select(dt_fundacao_osc, dt_fechamento_osc,
         ERJ, CRJ, Rocinha, Vidigal, VigarioG, CidadeDeus)

# names(plotDataNOSC_RJ)

for (i in seq_along(QtdAno_OSC_RJ$Ano)) {
  # i <- 1
  message(QtdAno_OSC_RJ$Ano[i])

  NewLine <- plotDataNOSC_RJ %>%
    mutate(
      flag =
        ( dt_fundacao_osc <= QtdAno_OSC_RJ$DataRef[i] &
          dt_fechamento_osc > QtdAno_OSC_RJ$DataRef[i] ) |
        ( dt_fundacao_osc <= QtdAno_OSC_RJ$DataRef[i] &
            is.na(dt_fechamento_osc))
      ) %>%
    summarise(
      N_OSC = sum(flag),
      N_OSC_ERJ = sum(ifelse(flag & ERJ == 1, 1, 0), na.rm = TRUE),
      N_OSC_CRJ = sum(ifelse(flag & CRJ == 1, 1, 0), na.rm = TRUE),
      N_OSC_Rocinha = sum(ifelse(flag & Rocinha == 1, 1, 0), na.rm = TRUE),
      N_OSC_Vidigal = sum(ifelse(flag & Vidigal == 1, 1, 0), na.rm = TRUE),
      N_OSC_VigarioG = sum(ifelse(flag & VigarioG == 1, 1, 0), na.rm = TRUE),
      N_OSC_CidadeDeus = sum(ifelse(flag & CidadeDeus == 1, 1, 0), na.rm = TRUE),
      Ano = QtdAno_OSC_RJ$Ano[i],
      DataRef = QtdAno_OSC_RJ$DataRef[i]
    )

  # NewLine
  # names(QtdAno_OSC_RJ)

  QtdAno_OSC_RJ$N_OSC[i] <- NewLine$N_OSC[[1]]
  QtdAno_OSC_RJ$N_OSC_ERJ[i] <- NewLine$N_OSC_ERJ[[1]]
  QtdAno_OSC_RJ$N_OSC_CRJ[i] <- NewLine$N_OSC_CRJ[[1]]
  QtdAno_OSC_RJ$N_OSC_Rocinha[i] <- NewLine$N_OSC_Rocinha[[1]]
  QtdAno_OSC_RJ$N_OSC_Vidigal[i] <- NewLine$N_OSC_Vidigal[[1]]
  QtdAno_OSC_RJ$N_OSC_VigarioG[i] <- NewLine$N_OSC_VigarioG[[1]]
  QtdAno_OSC_RJ$N_OSC_CidadeDeus[i] <- NewLine$N_OSC_CidadeDeus[[1]]

  rm(NewLine)
}
rm(i)
rm(plotDataNOSC_RJ)

# Checa dados
View(QtdAno_OSC_RJ)
names(QtdAno_OSC_RJ)


# Gráfico

# Brasil
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 1000000) +
  ggtitle("Brasil") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )

# Estado do Rio
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC_ERJ, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 80000) +
  ggtitle("Estado do RJ") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )

# Cidade do Rio
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC_CRJ, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 35000) +
  ggtitle("Cidade do RJ") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )


# Rocinha
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC_Rocinha, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 115) +
  ggtitle("Rocinha") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )


# Vidigal
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC_Vidigal, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 50) +
  ggtitle("Vidigal") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )


# Vidigal
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC_Vidigal, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 50) +
  ggtitle("Vidigal") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )


# Vigário Geral
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC_VigarioG, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 115) +
  ggtitle("Vigário Geral") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )

# Cidade de Deus
QtdAno_OSC_RJ %>%
  ggplot(aes(y = N_OSC_CidadeDeus, x = Ano)) +
  geom_line(color = "black") +
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
  ) +
  ylab("Número de OSC") +
  ylim(0, 115) +
  ggtitle("Cidade de Deus") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust=1)
  )

rm(QtdAno_OSC_RJ)
ls()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Gráfico - Onde estão as OSC do RJ ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(OscAtiva)

OSCporBairro <- OscAtiva %>%
  dplyr::filter(bo_osc_ativa) %>%
  dplyr::filter(cd_municipio == 3304557) %>%
  mutate(str_trim(tx_bairro)) %>%
  group_by(tx_bairro) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq)) %>%
  mutate(per = round(Freq / sum(Freq)*100, 1)) %>%
  ungroup()

View(OSCporBairro)
names(OSCporBairro)

# Salva Tabela
fwrite(OSCporBairro, "tables/OSCporBairro.csv",
       sep = ";", dec = ",")

rm(OSCporBairro)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Gráfico - Área de Atuação das OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(OscAtiva)

dc_area_atuacao

DataAreaAtuacao_RJ <- OscAtiva %>%
  dplyr::filter(bo_osc_ativa) %>%
  mutate(
    ERJ = ifelse(UF_Sigla == "RJ", 1, 0),
    CRJ = ifelse(cd_municipio == 3304557, 1, 0),
    Rocinha = ifelse(CRJ == 1 & str_detect(tx_bairro, "ROCINHA"), 1, 0),
    Vidigal = ifelse(CRJ == 1 & str_detect(tx_bairro, "VIDIGAL"), 1, 0),
    VigarioG = ifelse(CRJ == 1 & str_detect(tx_bairro, "VIGARIO GERAL"), 1, 0),
    CidadeDeus = ifelse(CRJ == 1 & str_detect(tx_bairro, "CIDADE DE DEUS"), 1, 0),
  ) %>%
  select(cd_area_atuacao,
         ERJ, CRJ, Rocinha, Vidigal, VigarioG, CidadeDeus ) %>%
  group_by(cd_area_atuacao) %>%
  summarise(Brasil = n(),
            ERJ = sum(ERJ, na.rm = TRUE),
            CRJ = sum(CRJ, na.rm = TRUE),
            Rocinha = sum(Rocinha, na.rm = TRUE),
            Vidigal = sum(Vidigal, na.rm = TRUE),
            VigarioG = sum(VigarioG, na.rm = TRUE),
            CidadeDeus = sum(CidadeDeus, na.rm = TRUE),
            ) %>%
  ungroup() %>%
  left_join(dc_area_atuacao,
            by = "cd_area_atuacao") %>%
  dplyr::filter(!is.na(OrdemArea)) %>%
  select(tx_area_atuacao, Brasil:CidadeDeus)

View(DataAreaAtuacao_RJ)
names(DataAreaAtuacao_RJ)

# Salva Tabela
fwrite(DataAreaAtuacao_RJ, "tables/DataAreaAtuacao_RJ.csv",
       sep = ";", dec = ",")

rm(DataAreaAtuacao_RJ)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Gráfico - Transferências para as OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(OscAtiva)

raw_loa <- readRDS("temp/transferencias/mosc_2661_loa_despesa_execucao_extrato_osc_v3.RDS")

FuncoesOrcamentarias <- read_xlsx("temp/transferencias/FuncoesOrcamentarias.xlsx",
                                  sheet = 1) %>%
  mutate(FuncaoCod = str_pad(FuncaoCod, side = "left", pad = "0", width = 2),
         SubFuncaoCod = str_pad(SubFuncaoCod, side = "left", pad = "0", width = 3))

# Creio que para a estrutura do capítulo, é melhor adicionar os dados das OSC
# na tabela de transferências e não o contrário
loa_despesa <- raw_loa %>%
  mutate(
    FuncOr = str_sub(funcional, 1, 2) ,
    SubFuncOr = str_sub(funcional, 4, 6),
    cd_identificador_osc = str_pad(cd_identificador_osc,
                                   width = 14, side = "left", pad = 0)
  ) %>%
  select(ano, mes, id_osc, cd_identificador_osc, razao_social, FuncOr,
         SubFuncOr, empenhado, liquidado, pago) %>%
  left_join(OscAtiva, by = "id_osc")

rm(raw_loa)
gc()
# ls()

names(loa_despesa)

teste  <- loa_despesa %>%
  dplyr::filter(bo_osc_ativa,
                ano >= 2017,
                !( cd_area_atuacao %in% c(4, 11) )
                ) %>%
  mutate(
    ERJ = ifelse(UF_Sigla == "RJ", 1, 0) ) %>%
  dplyr::filter(ERJ == 1)


TransOSCAno_RJ <- loa_despesa %>%
  dplyr::filter(bo_osc_ativa,
                ano >= 2017,
                !( cd_area_atuacao %in% c(4, 11) )
  ) %>%
  mutate(
    ERJ = ifelse(UF_Sigla == "RJ", 1, 0),
    CRJ = ifelse(cd_municipio == 3304557, 1, 0),
    Rocinha = ifelse(CRJ == 1 & str_detect(tx_bairro, "ROCINHA"), 1, 0),
    Vidigal = ifelse(CRJ == 1 & str_detect(tx_bairro, "VIDIGAL"), 1, 0),
    VigarioG = ifelse(CRJ == 1 & str_detect(tx_bairro, "VIGARIO GERAL"), 1, 0),
    CidadeDeus = ifelse(CRJ == 1 & str_detect(tx_bairro, "CIDADE DE DEUS"), 1, 0),
  ) %>%
  select(ano, pago,
         ERJ, CRJ, Rocinha, Vidigal, VigarioG, CidadeDeus ) %>%
  group_by(ano) %>%
  summarise(Brasil = n(),
            ERJ = mean(pago[ERJ == 1], na.rm = TRUE),
            CRJ = mean(pago[CRJ == 1], na.rm = TRUE),
            Rocinha = mean(pago[Rocinha == 1], na.rm = TRUE),
            Vidigal = mean(pago[Vidigal == 1], na.rm = TRUE),
            VigarioG = mean(pago[VigarioG == 1], na.rm = TRUE),
            CidadeDeus = mean(pago[CidadeDeus == 1], na.rm = TRUE),
  ) %>%
  ungroup()

for (i in seq_along(TransOSCAno_RJ)) {
  # i <- 6
  variavel <- names(TransOSCAno_RJ)[[i]]
  print(variavel)
  vetor <- TransOSCAno_RJ[[variavel]]
  TransOSCAno_RJ[[variavel]][is.nan(vetor)] <- 0
  rm(variavel, vetor)
}
rm(i)

# Salva Tabela
fwrite(TransOSCAno_RJ, "tables/TransOSCAno_RJ.csv",
       sep = ";", dec = ",")

rm(TransOSCAno_RJ, loa_despesa)


# Fim ####
