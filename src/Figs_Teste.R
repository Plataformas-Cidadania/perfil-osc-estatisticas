# Instituto de Economia Aplicada - IPEA

# Objetivo do Script: estudar e testar as figuras para atualização do livro
# perfil das OSC

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

# Pacotes de leitura de dados externos
library(data.table)
library(readxl)

# Manipulação de datas
library(lubridate)

# Manipulação de Textos
library(stringr)
library(glue)

# Pacotes de Bancos de dados (ex: PostgreSQL)
library(DBI)
library(RODBC)
library(RPostgres)
library(dbplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OSCs no Territorio ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tb_osc <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_osc.RDS")
tb_localicazao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_localizacao.RDS")
tb_dados_gerais <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_dados_gerais.RDS")
Municipios <- fread("data/Municipios.csv", encoding = "Latin-1")

Pop2022 <- fread("data/Pop2022.csv", encoding = "UTF-8") %>%
  magrittr::set_names(c("cd_municipio", "Munic_Nome", "Pop2022"))

UFs <- fread("data/UFs.csv", encoding = "Latin-1") %>%
  mutate(UF_Id = as.character(UF_Id))

# Junta as informações de município e população aqui.
PopMunic <- Municipios %>%
  left_join(select(Pop2022, cd_municipio, Pop2022),
            by = c("Munic_Id" = "cd_municipio") ) %>%
  rename(cd_municipio = Munic_Id)

rm(Municipios, Pop2022)


# Tabela 1 - Número e percentual de OSCs, segundo as Grandes Regiões e as
# Unidades da Federação: Brasil

# Status: dados prontos

PopUF <- PopMunic %>%
  mutate(UF_Id = str_sub(cd_municipio, 1, 2)) %>%
  group_by(UF_Id) %>%
  summarise(PopUF = sum(Pop2022, na.rm = TRUE)) %>%
  dplyr::filter(!is.na(UF_Id)) %>%
  ungroup()


OscMunic <- tb_localicazao %>%
  select(id_osc, cd_municipio) %>%
  # Adiciona ID e OSC ativa:
  left_join(
    select(tb_osc,
           id_osc, bo_osc_ativa),
    by = "id_osc"
  ) %>%
  # Somente trabalhar com OSC ativa
  dplyr::filter(bo_osc_ativa == TRUE,
                cd_municipio != "0") %>%
  select(-bo_osc_ativa)


OScPerUF <- OscMunic %>%
  mutate(UF_Id = str_sub(cd_municipio, 1, 2)) %>%
  left_join(UFs, by = "UF_Id") %>%
  group_by(UF_Regiao, UF_Sigla, UF_Id) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  left_join(PopUF, by = "UF_Id") %>%
  mutate(Per = ( Freq / sum(Freq) ) * 100 ,
         PerPop = ( PopUF / sum(PopUF) ) * 100 ,
         PropOsc = (Per / PerPop) * 100 ,
         OscP1000 = (Freq / PopUF) * 1000 )

OScPerUF

OScPerReg <- OScPerUF %>%
  group_by(UF_Regiao) %>%
  summarise(Freq = sum(Freq, na.rm = TRUE),
            PopReg = sum(PopUF, na.rm = TRUE) ) %>%
  mutate(Per = ( Freq / sum(Freq) ) * 100 ,
         PerPop = ( PopReg / sum(PopReg) ) * 100 ,
         OscP1000 = (Freq / PopReg) * 1000 )

OScPerReg

rm(OScPerUF, OScPerReg)

# TABELA 2 - Número de OSCs, OSCs por mil habitantes e percentual de OSCs nas
# capitais do país

# Status: dados prontos, falta checar bug

# TO DO: checar bug da população de Maceió

OscCap <- OscMunic %>%
  dplyr::filter(cd_municipio %in% UFs$Cd_Capital) %>%
  group_by(cd_municipio) %>%
  summarise(FreqCap = n() ) %>%
  ungroup() %>%
  left_join(select(PopMunic, cd_municipio, Pop2022),
            by = "cd_municipio") %>%
  mutate(UF_Id = str_sub(cd_municipio, 1, 2) ) %>%
  left_join(UFs, by = "UF_Id") %>%
  rename(PopCap = Pop2022,
         CapNome = Munic_Nome,
         cd_capital = cd_municipio) %>%
  select(UF_Sigla, cd_capital, CapNome, FreqCap, PopCap)

OscCap

OscPerCap <- OscCap %>%
  left_join(OScPerUF, by = "UF_Sigla") %>%
  mutate(PerCap = (FreqCap / Freq ) * 100,
         PerPopCap = (PopCap / PopUF ) * 100 ,
         OscCapP1000 = (FreqCap / PopCap) * 1000 )

OscPerCap


# GRÁFICO 2 - Habitantes e OSCs nas capitais em relação ao total da
# população e OSCs da Unidade Federativa

# TO DO: colocar população na tabela




# TABELA 3 - Número de OSCs, densidade de organizações por mil habitantes
# e IDHM nas capitais dos estados (2016)

# TO DO: colocar IDH
# TO DO: colocar População




# FIGURA 1- IDHM e número de OSCs por mil habitantes, por municípios

# TO DO: colocar População
# TO DO: colocar mapa


# Fim ####
