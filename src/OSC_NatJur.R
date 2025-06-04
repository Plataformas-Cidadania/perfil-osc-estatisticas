# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo:
# "A Natureza Jurídica das OSCs"

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

# Filtra as OSCs ativas em tb_dados_gerais
OscAtiva <- tb_dados_gerais %>%
  select(id_osc, cd_natureza_juridica_osc, dt_fundacao_osc,
         dt_fechamento_osc) %>%
  # Adiciona a variável de OSC ativa
  left_join(
    select(tb_osc,
           id_osc, bo_osc_ativa),
    by = "id_osc"
  ) %>%
  # Filtra somente OSC ativas
  dplyr::filter(bo_osc_ativa == TRUE) %>%
  # Remove a variável de OSC ativa.
  select(-bo_osc_ativa) %>%

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
  )

OscAtiva2 <- OscAtiva %>%
  # Área de atuação
  left_join(area_atuacao_clean, by = "id_osc")

rm(tb_osc, tb_dados_gerais, tb_localicazao)
rm(area_atuacao_clean, tb_area_atuacao)
gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Naturezas jurídicas:
# grupo 3: Entidades sem fins lucrativos
## 306-9	Fundação Privada
## 322-0	Organização Religiosa
## 330-1	Organização Social (OS)
## 399-9	Associação Privada
# Fonte: https://concla.ibge.gov.br/estrutura/natjur-estrutura/natureza-juridica-2021


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Freq OSCs por natureza jurídica e UF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - OSCs por natureza jurídica, Grandes Regiões e as Unidades da Federação

# OSCs por UF
OScPerUF <- OscAtiva %>%
  group_by(UF_Regiao, UF_Sigla, UF_Nome, UF_Id,
           cd_natureza_juridica_osc) %>%
  # Calcula frequência das OSC por UF
  summarise(Freq = n()) %>%
  ungroup() %>%
  # Percentual das UF na UF
  mutate(Agregacao = "UF" ) %>%
  # Remove casos de localização missing
  dplyr::filter(!is.na(UF_Regiao))

# Checa dados
# View(OScPerUF)

# OSCs por região
OScPerReg <- OScPerUF %>%
  # Da tabela anterior, agrupa por região
  mutate(Reg_Id = str_sub(UF_Id, 1, 1)) %>%
  group_by(UF_Regiao, Reg_Id, cd_natureza_juridica_osc) %>%
  # Frequência da região
  summarise(Freq = sum(Freq, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Agregacao = "Região",
         # Variáveis necessárias para unir os bancos
         UF_Sigla = UF_Regiao,
         # Adiciona o nome da região na variável 'UF_Nome'
         UF_Nome = case_when(
           UF_Regiao == "N" ~ "Região Norte",
           UF_Regiao == "S" ~ "Região Sul",
           UF_Regiao == "CO" ~ "Região Centro-Oeste",
           UF_Regiao == "SE" ~ "Região Sudeste",
           UF_Regiao == "NE" ~ "Região Nordeste",
           TRUE ~ "Erro" )
  )

# Checa dados
# View(OScPerReg)

NOSC_UF <- sum(OScPerUF$Freq, na.rm = TRUE)

# Dados do Brasil como um todo
OscNatJur_BR <- OScPerUF %>%
  group_by(cd_natureza_juridica_osc) %>%
  summarise(Freq = sum(Freq) ) %>%
  mutate(Per = 100,
         UF_Regiao = "BR",
         UF_Sigla = "BR",
         UF_Nome = "Brasil",
         UF_Id = "0",
         Reg_Id = NA,
         Agregacao = "Região")

# Checa dados
# View(OscNatJur_BR)


# Une dados das UFs e das Regiões (formato longo):
FreqOSC_NatJur <- bind_rows(OScPerUF, OScPerReg) %>%
  # Porcentagem de OSC da região
  mutate(Per = ( Freq / NOSC_UF ) * 100 ) %>%
  bind_rows(OscNatJur_BR) %>%
  left_join(select(OrdemUF, UF_Sigla, UF_Ordem),
            by = "UF_Sigla") %>%
  arrange(as.integer(UF_Ordem)) %>%
  select(-UF_Ordem)


# Checa dados
# View(FreqOSC_NatJur)

# mutate(
#   # Adiciona o nome da região na variável 'UF_Nome'
#   NatJur = case_when(
#     cd_natureza_juridica_osc == 3069 ~ "Fundação Privada",
#     cd_natureza_juridica_osc == 3220 ~ "Organização Religiosa",
#     cd_natureza_juridica_osc == 3301 ~ "Organização Social (OS)",
#     cd_natureza_juridica_osc == 3999 ~ "Associação Privada",
#     TRUE ~ "Erro" )
# )


# Organiza os dados para ficar igual à tabela final
tblFreqOSC_NatJur <-  FreqOSC_NatJur %>%
  select(UF_Nome, Freq, Per, Agregacao, cd_natureza_juridica_osc,
         UF_Sigla)%>%
  gather("key", "value", Freq, Per) %>%
  mutate(key2 = paste0(key, "_", cd_natureza_juridica_osc) ) %>%
  select(UF_Sigla, UF_Nome, Agregacao, key2, value) %>%
  spread(key2, value) %>%
  rowwise() %>%
  mutate(TotalFreq = sum(Freq_3999, Freq_3069, Freq_3220, Freq_3301),
         TotalPer = sum(Per_3999, Per_3069, Per_3220, Per_3301),

         PerReg_3999 = (Freq_3999 / TotalFreq) * 100,
         PerReg_3069 = (Freq_3069 / TotalFreq) * 100,
         PerReg_3220 = (Freq_3220 / TotalFreq) * 100,
         PerReg_3301 = (Freq_3301 / TotalFreq) * 100,
         TotalPerReg = sum(PerReg_3999, PerReg_3069, PerReg_3220, PerReg_3301),

         ) %>%
  ungroup() %>%
  mutate(TotalPer = ifelse(UF_Nome == "Brasil", 100, TotalPer)) %>%
  select(UF_Nome,
         Freq_3999, Freq_3069, Freq_3220, Freq_3301, TotalFreq,
         Per_3999, Per_3069, Per_3220, Per_3301, TotalPer,
         PerReg_3999, PerReg_3069, PerReg_3220, PerReg_3301, TotalPerReg,
         Agregacao, UF_Sigla) %>%
  left_join(select(OrdemUF, UF_Sigla, UF_Ordem),
            by = "UF_Sigla") %>%
  arrange(as.integer(UF_Ordem))

# Checa dados
# View(tblFreqOSC_NatJur)

# Salva Tabela
# saveRDS(tblFreqOSC_NatJur, "tables/tblFreqOSC_NatJur.RDS")

# Formata tabela:
tblFreqOSC_NatJur_gt <- tblFreqOSC_NatJur %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem, UF_Sigla)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    UF_Nome = html("Regiões e Unidades da Federação"),
    Freq_3999 = "Associação Privada",
    Freq_3069 = "Fundação Privada",
    Freq_3220 = "Organização Religiosa",
    Freq_3301 = "Organização Social (OS)",
    TotalFreq = "Total",

    Per_3999 = "Associação Privada",
    Per_3069 = "Fundação Privada",
    Per_3220 = "Organização Religiosa",
    Per_3301 = "Organização Social (OS)",
    TotalPer = "Total",

    PerReg_3999 = "Associação Privada",
    PerReg_3069 = "Fundação Privada",
    PerReg_3220 = "Organização Religiosa",
    PerReg_3301 = "Organização Social (OS)",
    TotalPerReg = "Total"

  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels(columns = c(TotalFreq, TotalPer, TotalPerReg))
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(Per_3999, Per_3069, Per_3220, Per_3301, TotalPer,
                PerReg_3999, PerReg_3069, PerReg_3220, PerReg_3301,
                TotalPerReg),
    decimals = 1,
    dec_mark = ",",
  ) %>%
  fmt_number(
    columns = c(Freq_3999, Freq_3069, Freq_3220, Freq_3301, TotalFreq),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !UF_Nome )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Quantidade de OSCs",
    columns = c(Freq_3999, Freq_3069, Freq_3220, Freq_3301, TotalFreq)
  ) %>%

  tab_spanner(
    label = "Total (%)",
    columns = c(Per_3999, Per_3069, Per_3220, Per_3301, TotalPer)
  ) %>%

  tab_spanner(
    label = "Região geográfica (%)",
    columns = c(PerReg_3999, PerReg_3069, PerReg_3220, PerReg_3301, TotalPerReg)
  ) %>%

  # Torna as regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = UF_Nome,
      rows = Agregacao ==  "UF")
  ) %>%
  # tab_style(
  #   style = cell_text(weight = "bold"), # Negrito
  #   locations = cells_body(
  #     columns = everything(),
  #     rows = Agregacao !=  "UF")
  # ) %>%

  tab_style(
    style = cell_text(weight = "bold"), # Negrito
    locations = cells_body(
      columns = c(TotalFreq, TotalPer, TotalPerReg) )
  )

# Checa tabela
tblFreqOSC_NatJur_gt


# Tabela 1
tblFreqOSC_NatJur %>%
  select(UF_Nome:TotalFreq, Agregacao:UF_Ordem) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem, UF_Sigla)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    UF_Nome = html("Regiões e Unidades da Federação"),
    Freq_3999 = "Associação Privada",
    Freq_3069 = "Fundação Privada",
    Freq_3220 = "Organização Religiosa",
    Freq_3301 = "Organização Social (OS)",
    TotalFreq = "Total"

  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels(columns = c(TotalFreq))
  ) %>%

  fmt_number(
    columns = c(Freq_3999, Freq_3069, Freq_3220, Freq_3301, TotalFreq),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !UF_Nome )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Quantidade de OSCs",
    columns = c(Freq_3999, Freq_3069, Freq_3220, Freq_3301, TotalFreq)
  ) %>%

  # Torna as regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = UF_Nome,
      rows = Agregacao ==  "UF")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"), # Negrito
    locations = cells_body(
      columns = c(TotalFreq) )
  )



# Tabela 2
tblFreqOSC_NatJur %>%
  select(UF_Nome, Per_3999:TotalPer, Agregacao:UF_Ordem) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem, UF_Sigla)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    UF_Nome = html("Regiões e Unidades da Federação"),
    Per_3999 = "Associação Privada",
    Per_3069 = "Fundação Privada",
    Per_3220 = "Organização Religiosa",
    Per_3301 = "Organização Social (OS)",
    TotalPer = "Total",

  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels(columns = c(TotalPer))
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(Per_3999, Per_3069, Per_3220, Per_3301, TotalPer),
    decimals = 1,
    dec_mark = ",",
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !UF_Nome )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Total (%)",
    columns = c(Per_3999, Per_3069, Per_3220, Per_3301, TotalPer)
  ) %>%

  # Torna as regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = UF_Nome,
      rows = Agregacao ==  "UF")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"), # Negrito
    locations = cells_body(
      columns = c(TotalPer) )
  )



# Tabela 3
tblFreqOSC_NatJur %>%
  select(UF_Nome, PerReg_3999:UF_Ordem) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem, UF_Sigla)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    UF_Nome = html("Regiões e Unidades da Federação"),

    PerReg_3999 = "Associação Privada",
    PerReg_3069 = "Fundação Privada",
    PerReg_3220 = "Organização Religiosa",
    PerReg_3301 = "Organização Social (OS)",
    TotalPerReg = "Total"

  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels(columns = c(TotalPerReg))
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(PerReg_3999, PerReg_3069, PerReg_3220, PerReg_3301,
                TotalPerReg),
    decimals = 1,
    dec_mark = ",",
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !UF_Nome )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Região geográfica (%)",
    columns = c(PerReg_3999, PerReg_3069, PerReg_3220, PerReg_3301, TotalPerReg)
  ) %>%

  # Torna as regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = UF_Nome,
      rows = Agregacao ==  "UF")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"), # Negrito
    locations = cells_body(
      columns = c(TotalPerReg) )
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Freq OSCs por natureza jurídica e Área de Atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - OSCs, por natureza jurídica e finalidade

# OSCs por UF
OScNatJurArea <- OscAtiva2 %>%
  group_by(cd_area_atuacao,  cd_natureza_juridica_osc) %>%
  # Calcula frequência das OSC por UF
  summarise(Freq = n()) %>%
  ungroup() %>%
  mutate(cd_natureza_juridica_osc = paste0("NatJur_",
                                           cd_natureza_juridica_osc)) %>%
  spread(cd_natureza_juridica_osc, Freq, fill = 0) %>%
  # Percentual das UF na UF
  mutate(Agregacao = "Area" ) %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(OScNatJurArea)

# OSCs por região
OScNatJurSubArea <- OscAtiva2 %>%
  group_by(cd_subarea_atuacao, cd_natureza_juridica_osc) %>%
  summarise(Freq = n() ) %>%
  mutate(Agregacao = "SubArea") %>%
  ungroup() %>%
  mutate(cd_natureza_juridica_osc = paste0("NatJur_",
                                           cd_natureza_juridica_osc)) %>%
  spread(cd_natureza_juridica_osc, Freq, fill = 0) %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(OScNatJurSubArea)

# Dados do Brasil como um todo
OScNatJurAreaBR <- OscAtiva2 %>%
  group_by(cd_natureza_juridica_osc) %>%
  summarise(Freq  = n() ) %>%
  ungroup() %>%
  mutate(cd_natureza_juridica_osc = paste0("NatJur_", cd_natureza_juridica_osc)) %>%
  spread(cd_natureza_juridica_osc, Freq, fill = 0) %>%
  mutate(Agregacao = "Região",
         OrdemArea = "0",
         NomeAreaSubArea = "Brasil")

# Checa dados
OScNatJurAreaBR


# Número total de OSC
N_TotalOSC <- nrow(OscAtiva2)

# Une dados
JoinNatJurArea_OSC <- bind_rows(OScNatJurArea, OScNatJurSubArea,
                                   OScNatJurAreaBR) %>%
  arrange(OrdemArea) %>%

  rowwise() %>%
  mutate(
    TotalNatJur = sum(NatJur_3069, NatJur_3220, NatJur_3301, NatJur_3999,
                      na.rm = TRUE),
    PerNatJur_3069 = (NatJur_3069 / TotalNatJur) * 100,
    PerNatJur_3220 = (NatJur_3220 / TotalNatJur) * 100,
    PerNatJur_3301 = (NatJur_3301 / TotalNatJur) * 100,
    PerNatJur_3999 = (NatJur_3999 / TotalNatJur) * 100,
    PerTotalNatJur = 100,

    PerTot_3069 = (NatJur_3069 / N_TotalOSC) * 100,
    PerTot_3220 = (NatJur_3220 / N_TotalOSC) * 100,
    PerTot_3301 = (NatJur_3301 / N_TotalOSC) * 100,
    PerTot_3999 = (NatJur_3999 / N_TotalOSC) * 100,
    PerTot_OSC = (TotalNatJur / N_TotalOSC) * 100

  ) %>%

  select(Agregacao, OrdemArea, NomeAreaSubArea,
         NatJur_3069, NatJur_3220, NatJur_3301, NatJur_3999, TotalNatJur,
         PerTot_3069, PerTot_3220, PerTot_3301, PerTot_3999, PerTot_OSC,
         PerNatJur_3069, PerNatJur_3220, PerNatJur_3301, PerNatJur_3999,
         PerTotalNatJur)

# Checa dados
# View(JoinNatJurArea_OSC)
# names(JoinNatJurArea_OSC)

# Salva Tabela
# saveRDS(JoinNatJurArea_OSC, "tables/JoinNatJurArea_OSC.RDS")


# names(JoinNatJurArea_OSC)

# Tabela Inteira:
JoinNatJurArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",

    NatJur_3069 = "Fundação Privada",
    NatJur_3220 = "Organização Religiosa",
    NatJur_3301 = "Organização Social",
    NatJur_3999 = "Associação Privada",
    TotalNatJur = "Total",

    PerTot_3069 = "Fundação Privada",
    PerTot_3220 = "Organização Religiosa",
    PerTot_3301 = "Organização Social",
    PerTot_3999 = "Associação Privada",
    PerTot_OSC = "Total",

    PerNatJur_3069 = "Fundação Privada",
    PerNatJur_3220 = "Organização Religiosa",
    PerNatJur_3301 = "Organização Social",
    PerNatJur_3999 = "Associação Privada",
    PerTotalNatJur = "Total"
    ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(NatJur_3069, NatJur_3220, NatJur_3301, NatJur_3999, TotalNatJur),
    sep_mark = ".",
    decimals = 0
  ) %>%

  fmt_number(
    columns = c(PerTot_3069, PerTot_3220, PerTot_3301, PerTot_3999, PerTot_OSC,
                PerNatJur_3069, PerNatJur_3220, PerNatJur_3301, PerNatJur_3999,
                PerTotalNatJur),
    decimals = 1,
    dec_mark = ",",
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
      rows = Agregacao ==  "SubArea")
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Quantidade de OSCs",
    columns = c(NatJur_3069, NatJur_3220, NatJur_3301, NatJur_3999, TotalNatJur)
  ) %>%

  tab_spanner(
    label = "Total (%)",
    columns = c(PerTot_3069, PerTot_3220, PerTot_3301, PerTot_3999, PerTot_OSC)
  ) %>%

  tab_spanner(
    label = "Natureza jurídica (%)",
    columns = c(PerNatJur_3069, PerNatJur_3220, PerNatJur_3301, PerNatJur_3999,
                PerTotalNatJur)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Freq OSCs religiosas ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# OSCs de finalidade religiosa por Grandes Regiões e Unidades da Federação


table(OscAtiva$cd_natureza_juridica_osc)


# OSCs por UF
OSCReligPerUF <- OscAtiva %>%
  # Marca OSC religiosas
  mutate(OSC_Religiao = ifelse(cd_natureza_juridica_osc == 3220, 1, 0) ) %>%
  group_by(UF_Regiao, UF_Sigla, UF_Nome, UF_Id,
           OSC_Religiao) %>%

  # Calcula frequência das OSC por UF
  summarise(Freq = n()) %>%
  ungroup() %>%
  # Percentual das UF na UF
  mutate(Agregacao = "UF" ) %>%
  # Remove casos de localização missing
  dplyr::filter(!is.na(UF_Regiao))

# Checa dados
# View(OSCReligPerUF)

# OSCs por região
OScReligPerReg <- OSCReligPerUF %>%
  # Da tabela anterior, agrupa por região
  mutate(Reg_Id = str_sub(UF_Id, 1, 1)) %>%
  group_by(UF_Regiao, Reg_Id, OSC_Religiao) %>%
  # Frequência da região
  summarise(Freq = sum(Freq, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Agregacao = "Região",
         # Variáveis necessárias para unir os bancos
         UF_Sigla = UF_Regiao,
         # Adiciona o nome da região na variável 'UF_Nome'
         UF_Nome = case_when(
           UF_Regiao == "N" ~ "Região Norte",
           UF_Regiao == "S" ~ "Região Sul",
           UF_Regiao == "CO" ~ "Região Centro-Oeste",
           UF_Regiao == "SE" ~ "Região Sudeste",
           UF_Regiao == "NE" ~ "Região Nordeste",
           TRUE ~ "Erro" )
  )

# Checa dados
# View(OScReligPerReg)

# Dados do Brasil como um todo
OscRelig_BR <- OSCReligPerUF %>%
  group_by(OSC_Religiao) %>%
  summarise(Freq = sum(Freq) ) %>%
  mutate(UF_Regiao = "BR",
         UF_Sigla = "BR",
         UF_Nome = "Brasil",
         UF_Id = "0",
         Reg_Id = NA,
         Agregacao = "Região")

# Checa dados
# View(OscRelig_BR)

NOSCRelig_UF <- sum(OSCReligPerUF$Freq, na.rm = TRUE)

# names(OSCReligPerUF)
# names(FreqOSC_Relig)

# Une dados das UFs e das Regiões (formato longo):
FreqOSC_Relig <- bind_rows(OSCReligPerUF, OScReligPerReg, OscRelig_BR) %>%
  mutate(UF_Sigla = ifelse(UF_Regiao == "SE" & UF_Sigla == "SE",
                           "Sud", UF_Sigla) ) %>%
  group_by(UF_Sigla) %>%
  mutate(Freq2 = ifelse(OSC_Religiao == 1, Freq, sum(Freq) ) ) %>%
  select(UF_Sigla, UF_Nome, Agregacao, OSC_Religiao, Freq2) %>%
  # Porcentagem de OSC da região

  mutate(OSC_Religiao = ifelse(OSC_Religiao == 1, "OSC_Relig", "OSC_Total")) %>%
  spread(OSC_Religiao, Freq2) %>%

  mutate(
    PerOSC = ( OSC_Total / NOSCRelig_UF ) * 100,
    PerRelig = ( OSC_Relig / NOSCRelig_UF ) * 100,
    PerOSCRelig = (PerRelig / PerOSC) * 100,

    UF_Sigla = ifelse(UF_Sigla == "Sud", "SE", UF_Sigla)

  ) %>%

  left_join(select(OrdemUF, UF_Sigla, UF_Ordem),
            by = "UF_Sigla") %>%
  arrange(as.integer(UF_Ordem)) %>%

  select(UF_Sigla, UF_Nome, Agregacao, OSC_Total, PerOSC,
         OSC_Relig, PerRelig, PerOSCRelig) %>%
  ungroup()

# Checa dados
# names(FreqOSC_Relig)
# View(FreqOSC_Relig)

# Salva Tabela
saveRDS(FreqOSC_Relig, "tables/FreqOSC_Relig.RDS")



# Tabela Inteira
tblFreqOSC_Relig <- FreqOSC_Relig %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(UF_Sigla, Agregacao)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    UF_Nome = html("Regiões e Unidades da Federação"),
    OSC_Total = "N",
    PerOSC = "(%)",
    OSC_Relig = "N",
    PerRelig = "(%)",
    PerOSCRelig = "Unidades da  Federação (%)"

  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(PerOSC, PerRelig, PerOSCRelig),
    decimals = 1,
    dec_mark = ",",
  ) %>%
  fmt_number(
    columns = c(OSC_Total, OSC_Relig),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !UF_Nome )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Universo de OSCs",
    columns = c(OSC_Total, PerOSC)
  ) %>%

  tab_spanner(
    label = "Organizações de finalidade religiosa",
    columns = c(OSC_Relig, PerRelig, PerOSCRelig)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Torna as regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = UF_Nome,
      rows = Agregacao ==  "UF")
  )


# Checa tabela
tblFreqOSC_Relig



# Tabela 1
JoinNatJurArea_OSC %>%
  select(Agregacao:TotalNatJur) %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",

    NatJur_3069 = "Fundação Privada",
    NatJur_3220 = "Organização Religiosa",
    NatJur_3301 = "Organização Social",
    NatJur_3999 = "Associação Privada",
    TotalNatJur = "Total"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(NatJur_3069, NatJur_3220, NatJur_3301, NatJur_3999, TotalNatJur),
    sep_mark = ".",
    decimals = 0
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
      rows = Agregacao ==  "SubArea")
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Quantidade de OSCs",
    columns = c(NatJur_3069, NatJur_3220, NatJur_3301, NatJur_3999, TotalNatJur)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )


# Tabela 2
JoinNatJurArea_OSC %>%
  select(Agregacao:NomeAreaSubArea, PerTot_3069:PerTot_OSC) %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",

    PerTot_3069 = "Fundação Privada",
    PerTot_3220 = "Organização Religiosa",
    PerTot_3301 = "Organização Social",
    PerTot_3999 = "Associação Privada",
    PerTot_OSC = "Total"
    ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  fmt_number(
    columns = c(PerTot_3069, PerTot_3220, PerTot_3301, PerTot_3999, PerTot_OSC),
    decimals = 1,
    dec_mark = ",",
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
      rows = Agregacao ==  "SubArea")
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Total (%)",
    columns = c(PerTot_3069, PerTot_3220, PerTot_3301, PerTot_3999, PerTot_OSC)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )


# names(JoinNatJurArea_OSC)

# Tabela 3
JoinNatJurArea_OSC %>%
  select(Agregacao:NomeAreaSubArea, PerNatJur_3069:PerTotalNatJur) %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",

    PerNatJur_3069 = "Fundação Privada",
    PerNatJur_3220 = "Organização Religiosa",
    PerNatJur_3301 = "Organização Social",
    PerNatJur_3999 = "Associação Privada",
    PerTotalNatJur = "Total"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela

  fmt_number(
    columns = c(PerNatJur_3069, PerNatJur_3220, PerNatJur_3301, PerNatJur_3999,
                PerTotalNatJur),
    decimals = 1,
    dec_mark = ",",
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
      rows = Agregacao ==  "SubArea")
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "Natureza jurídica (%)",
    columns = c(PerNatJur_3069, PerNatJur_3220, PerNatJur_3301, PerNatJur_3999,
                PerTotalNatJur)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )


# Fim ####
