# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo:
# "AS OSCs E AS REMUNERAÇÕES"

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

dados_RAIS <- readRDS("temp/RAIS/RAIS_2022.RDS")

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

  # Evita problemas de pading
  mutate(cd_identificador_osc = str_pad(cd_identificador_osc,
                                        width = 14, side = "left", pad = 0)) %>%
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



# Neste capítulo, acho melhor colocar os dados das OSC no dados RAIS e não o
# Contrário
Vinculos_OSC <- dados_RAIS %>%
  select(-cd_municipio, -UF_Id) %>%
  # Seleciona apenas as OSC
  dplyr::filter(cd_identificador_osc %in% OscAtiva$cd_identificador_osc) %>%
  left_join(OscAtiva, by = "cd_identificador_osc") %>%
  select(everything())

# names(Vinculos_OSC)
# # View(Vinculos_OSC)

SM2022 <- 1212 # Salário mínimo no Brasil em 2022 (R$)

rm(tb_osc, tb_dados_gerais, tb_localicazao)
rm(Municipios, UFs, area_atuacao_clean, OscAtiva, dados_RAIS)
gc()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Remuneração por área e subarea de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# names(Vinculos_OSC)

# Total geral de vínculos
TotalVinculos = nrow(Vinculos_OSC)

# Dados por Área e SubÁrea de atuação
MedRemAreaSubArea_OSC <- Vinculos_OSC %>%

  # Calcula os Dados por SubArea:
  group_by(cd_subarea_atuacao) %>%

  summarise(
    Soma_Vinculos = n(),
    media_salarial = mean(rem_media_reais, na.rm = TRUE)
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
                      media_salarial = weighted.mean(media_salarial,
                                                     Soma_Vinculos,
                                                     na.rm = TRUE),
                      Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
                      Agregacao = "Area",
                      .by = cd_area_atuacao)
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
                      media_salarial = weighted.mean(media_salarial,
                                                     Soma_Vinculos,
                                                     na.rm = TRUE),
                      Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
                      Agregacao = "Todas",
                      tx_area_atuacao = "Média Geral",
                      OrdemArea = "0")
  ) %>%
  mutate(NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                                  tx_subarea_atuacao,
                                  tx_area_atuacao),
         media_salarialSM = round(media_salarial / SM2022, 1),
         Per_Vinculos = (Soma_Vinculos / TotalVinculos) * 100
  ) %>%
  # Arruma a ordem das linhas
  arrange(OrdemArea) %>%

  # Seleciona os dados e a ordem das colunas
  select(
    Agregacao, OrdemArea, NomeAreaSubArea,
    media_salarial, media_salarialSM, Soma_Vinculos, Per_Vinculos
    )

# Checa dados
# View(MedRemAreaSubArea_OSC)
# names(MedRemAreaSubArea_OSC)

# Salva Tabela
saveRDS(MedRemAreaSubArea_OSC, "tables/MedRemAreaSubArea_OSC.RDS")

# Tabela Inteira:
MedRemAreaSubArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    media_salarial = "R$ 1,00 corrente",
    media_salarialSM = "Salários Mínimos",
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
    columns = c(media_salarial, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(media_salarialSM, Per_Vinculos),
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
    label = "Salário Médio Mensal",
    columns = c(media_salarial, media_salarialSM),
    id = "span_N_SemSup"
  ) %>%

  tab_spanner(
    label = "Número de Vínculos",
    columns = c(Soma_Vinculos, Per_Vinculos),
    id = "span_N_Sup"
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(media_salarialSM)
    )
  )


rm(TotalVinculos)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Remuneração por área de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Remuneração média do pessoal ocupado, por finalidade
# de atuação (Em R$ e SMs)

# names(MedRemAreaSubArea_OSC)

grafMedRemAreaSubArea_OSC <- MedRemAreaSubArea_OSC %>%
  dplyr::filter(Agregacao == "Area")

# Checa dados
# View(grafMedRemAreaSubArea_OSC)
# names(grafMedRemAreaSubArea_OSC)

# Salva Tabela
saveRDS(grafMedRemAreaSubArea_OSC, "tables/grafMedRemAreaSubArea_OSC.RDS")

# Gráfico
grafMedRemAreaSubArea_OSC %>%
  dplyr::filter(NomeAreaSubArea != "Associações patronais e profissionais") %>%
  ggplot(aes(x = NomeAreaSubArea, y = media_salarial)) +
  geom_bar(stat="identity", color="black", position=position_dodge(),
           fill = "blue")+
  theme_minimal() +
  scale_x_discrete(labels = label_wrap(25)) +
  ylim(0, 6500) +
  geom_text(
    aes(
      label = paste("R$",
                    format(round(media_salarial, 0), big.mark = ".",
                           decimal.mark = ",",
                           scientific = FALSE)),
      y = media_salarial
    ),
    vjust = -0.25
  ) +

  geom_point(aes(y = media_salarialSM * 1500), color = "red", size = 3) +
  geom_label(
    aes(
      y = media_salarialSM * 1500,
      label = format(round(media_salarialSM, 1),
                     big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE)
    ),
    fill = "pink",
    label.size = 0,
    position = position_dodge(width=0.9),
    vjust = 0.25,
    hjust = -0.2) +

  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE),
    # Features of the first axis
    name = "Remumeração (R$)",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1500, name="SMs")
  ) +

  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust=1)
  )

rm(MedRemAreaSubArea_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Remuneração por região e UF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Salário médio mensal do pessoal ocupado assalariado,
# por Grande Região e Unidades  da Federação: Brasil


# names(Vinculos_OSC)

TotalVinculos = nrow(
  dplyr::filter(Vinculos_OSC,
                !is.na(UF_Regiao),
                !is.na(UF_Sigla))
  )

# Dados por Região
MedRemRegUF_OSC <- Vinculos_OSC %>%

  # Calcula os Dados por UF:
  dplyr::filter(!is.na(UF_Regiao),
                !is.na(UF_Sigla)) %>%
  group_by(UF_Regiao, UF_Sigla) %>%

  summarise(
    Soma_Vinculos = n(),
    media_salarial = mean(rem_media_reais, na.rm = TRUE)
  ) %>%

  mutate(Agregacao = "UF") %>%
  ungroup() %>%

  # Calcula os Dados por Area:
  bind_rows(.,
            summarise(.,
                      media_salarial = weighted.mean(media_salarial,
                                                     Soma_Vinculos,
                                                     na.rm = TRUE),
                      Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
                      Agregacao = "Regiao",
                      .by = UF_Regiao)
  ) %>%

  # Calcula os Dados do Total:
  bind_rows(.,
            summarise(dplyr::filter(., Agregacao == "UF"),
                      media_salarial = weighted.mean(media_salarial,
                                                     Soma_Vinculos,
                                                     na.rm = TRUE),
                      Soma_Vinculos = sum(Soma_Vinculos, na.rm = TRUE),
                      Agregacao = "BR",
                      UF_Sigla = "BR")
  ) %>%
  mutate(
    UF_Regiao = ifelse(UF_Regiao == "SE", "SD", UF_Regiao),
    UF_Sigla = ifelse(is.na(UF_Sigla), UF_Regiao, UF_Sigla),
    UF_Sigla = ifelse(UF_Regiao == "SE" & Agregacao == "Regiao",
                      "SD", UF_Sigla),
    media_salarialSM = round(media_salarial / SM2022, 1),
    Per_Vinculos = (Soma_Vinculos / TotalVinculos) * 100
  ) %>%

  left_join(OrdemUF, by = c("UF_Sigla")) %>%
  # Arruma a ordem das linhas
  arrange(UF_Ordem) %>%
  ungroup() %>%
  rename(NomeRegUF = UF_Nome) %>%

  # Seleciona os dados e a ordem das colunas
  select(
    Agregacao, UF_Ordem, NomeRegUF,
    media_salarial, media_salarialSM, Soma_Vinculos, Per_Vinculos
  )

# Checa dados
# View(MedRemRegUF_OSC)
# names(MedRemRegUF_OSC)

# Salva Tabela
saveRDS(MedRemRegUF_OSC, "tables/MedRemRegUF_OSC.RDS")

# Tabela Inteira:
MedRemRegUF_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "Região geográfica",
    media_salarial = "R$ 1,00 corrente",
    media_salarialSM = "Salários Mínimos",
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
    columns = c(media_salarial, Soma_Vinculos),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Decimais
  fmt_number(
    columns = c(media_salarialSM, Per_Vinculos),
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
    label = "Salário Médio Mensal",
    columns = c(media_salarial, media_salarialSM),
    id = "span_N_SemSup"
  ) %>%

  tab_spanner(
    label = "Número de Vínculos",
    columns = c(Soma_Vinculos, Per_Vinculos),
    id = "span_N_Sup"
  ) %>%

  # Coloca linha separando Spanners
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(media_salarialSM)
    )
  )


rm(TotalVinculos)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Remuneração por região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Remuneração média do pessoal ocupado, por finalidade
# de atuação (Em R$ e SMs)

# names(MedRemRegUF_OSC)

grafMedRemReg_OSC <- MedRemRegUF_OSC %>%
  dplyr::filter(Agregacao == "Regiao")

# Checa dados
# View(grafMedRemReg_OSC)
# names(grafMedRemReg_OSC)

# Salva Tabela
saveRDS(grafMedRemReg_OSC, "tables/grafMedRemReg_OSC.RDS")

# Gráfico
grafMedRemReg_OSC %>%
  ggplot(aes(x = NomeRegUF, y = media_salarial)) +
  geom_bar(stat="identity", color="black", position=position_dodge(),
           fill = "blue") +
  theme_minimal() +
  ylim(0, 6000) +
  geom_text(
    aes(
      label = paste("R$",
                    format(round(media_salarial, 0), big.mark = ".",
                           decimal.mark = ",",
                           scientific = FALSE)),
      y = media_salarial
    ),
    vjust = -0.25
  ) +

  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE),
    # Features of the first axis
    name = "Remumeração (R$)",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1400, name = "SMs")
  ) +

  geom_point(aes(y = media_salarialSM * 1400), color = "red", size = 3) +
  geom_label(
    aes(
      y = media_salarialSM * 1400,
      label = format(round(media_salarialSM, 1),
                     big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE)
    ),
    fill = "pink",
    label.size = 0,
    position = position_dodge(width=0.9),
    vjust = 0.25,
    hjust = -0.2) +

  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank()
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Remuneração por UF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Salário médio mensal do pessoal ocupado assalariado,
# por Unidades da Federação (2015) (Em R$)

# Checa dados
# View(MedRemRegUF_OSC)
# names(MedRemRegUF_OSC)

# Gráfico
MedRemRegUF_OSC %>%
  mutate(NomeRegUF = fct_reorder(NomeRegUF, UF_Ordem, .desc = TRUE)) %>%
  ggplot(aes(y = NomeRegUF, x = media_salarial, fill = Agregacao)) +
  geom_col() +
  theme_minimal() +
  scale_x_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
    ) +
  geom_text(
    aes(
      label = paste("R$",
                    format(round(media_salarial, 0), big.mark = ".",
                           decimal.mark = ",",
                           scientific = FALSE)),
      x = media_salarial
    ),
    hjust = -0.25,
    size = 3
  ) +
  xlim(0, 8500) +
  xlab("Remuneração (R$)") +
  geom_label(
    aes(
      x = media_salarialSM * 800,
      label = format(round(media_salarialSM, 1),
                     big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE)
    ),
    fill = "pink",
    label.size = 0,
    position = position_dodge(width=0.9),
    size = 2.5) +

  theme(
    legend.title = element_blank(),
    axis.title.y = element_blank()
  )

rm(MedRemRegUF_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Remuneração por gênero, área e subarea de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Salário médio mensal do pessoal ocupado assalariado,
# por sexo e finalidade: Brasil

# names(Vinculos_OSC)

# Total geral de vínculos
TotalVinculos = nrow(Vinculos_OSC)

# Dados por Área e SubÁrea de atuação
MedRemSexAreaSubArea_OSC <- Vinculos_OSC %>%

  dplyr::filter(genero %in% c(1, 2)) %>%

  # Calcula os Dados por SubArea:
  group_by(cd_subarea_atuacao) %>%

  summarise(
    # Soma_Vinculos = n(),
    Soma_Vinculos_F = sum(genero == 2, na.rm = TRUE),
    media_salarial_F = mean(rem_media_reais[genero == 2], na.rm = TRUE),
    Soma_Vinculos_M = sum(genero == 1, na.rm = TRUE),
    media_salarial_M = mean(rem_media_reais[genero == 1], na.rm = TRUE)
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
                      media_salarial_F = weighted.mean(media_salarial_F,
                                                       Soma_Vinculos_F,
                                                       na.rm = TRUE),

                      media_salarial_M = weighted.mean(media_salarial_M,
                                                       Soma_Vinculos_M,
                                                       na.rm = TRUE),

                      Soma_Vinculos_F = sum(Soma_Vinculos_F, na.rm = TRUE),
                      Soma_Vinculos_M = sum(Soma_Vinculos_M, na.rm = TRUE),

                      Agregacao = "Area",
                      .by = cd_area_atuacao)
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

                      media_salarial_F = weighted.mean(media_salarial_F,
                                                       Soma_Vinculos_F,
                                                       na.rm = TRUE),

                      media_salarial_M = weighted.mean(media_salarial_M,
                                                       Soma_Vinculos_M,
                                                       na.rm = TRUE),

                      Soma_Vinculos_F = sum(Soma_Vinculos_F, na.rm = TRUE),
                      Soma_Vinculos_M = sum(Soma_Vinculos_M, na.rm = TRUE),

                      Agregacao = "Todas",
                      tx_area_atuacao = "Média Geral",
                      OrdemArea = "0")
  ) %>%
  mutate(NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                                  tx_subarea_atuacao,
                                  tx_area_atuacao),

         media_salarialSM_F = round(media_salarial_F / SM2022, 1),
         media_salarialSM_M = round(media_salarial_M / SM2022, 1),

         Per_Vinculos_F = (Soma_Vinculos_F / TotalVinculos) * 100,
         Per_Vinculos_M = (Soma_Vinculos_M / TotalVinculos) * 100
  ) %>%
  # Arruma a ordem das linhas
  arrange(OrdemArea) %>%

  # Seleciona os dados e a ordem das colunas
  select(
    Agregacao, OrdemArea, NomeAreaSubArea,
    media_salarial_F, media_salarial_M,
    media_salarialSM_F, media_salarialSM_M,
    Soma_Vinculos_F, Soma_Vinculos_M,
    Per_Vinculos_F, Per_Vinculos_M,
  )

# Checa dados
# View(MedRemSexAreaSubArea_OSC)
# names(MedRemSexAreaSubArea_OSC)

# Salva Tabela
saveRDS(MedRemSexAreaSubArea_OSC, "tables/MedRemSexAreaSubArea_OSC.RDS")

# Tabela Inteira:
MedRemSexAreaSubArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea,
              Soma_Vinculos_F, Soma_Vinculos_M,
              Per_Vinculos_F, Per_Vinculos_M)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    media_salarial_F = html("R$ 1,00 corrente<br>(Mulher)"),
    media_salarial_M = html("R$ 1,00 corrente<br>(Homem)"),
    media_salarialSM_F = html("Salários Mínimos<br>(Mulher)"),
    media_salarialSM_M = html("Salários Mínimos<br>(Homem)")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  # fmt_number(
  #   columns = c(media_salarial, Soma_Vinculos),
  #   sep_mark = ".",
  #   decimals = 0
  # ) %>%

  # Decimais
  fmt_number(
    columns = c(media_salarial_F, media_salarial_M,
                media_salarialSM_F, media_salarialSM_M),
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
    label = "Salário Médio Mensal",
    columns = c(media_salarial_F, media_salarial_M)
  ) %>%

  tab_spanner(
    label = "Salários Mínimos",
    columns = c(media_salarialSM_F, media_salarialSM_M)
  )


rm(TotalVinculos)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Remuneração por gênero e área de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Salário médio mensal do pessoal ocupado assalariado,
# por sexo e finalidade: Brasil (Em R$ e SMs)

# names(MedRemSexAreaSubArea_OSC)

# Gráfico
MedRemSexAreaSubArea_OSC %>%
  dplyr::filter(NomeAreaSubArea != "Associações patronais e profissionais",
                Agregacao == "Area") %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, media_salarialSM_F,
         media_salarialSM_M) %>%
  gather("genero", "SMs", media_salarialSM_F:media_salarialSM_M) %>%
  mutate(genero = ifelse(genero == "media_salarialSM_F", "Mulher", "Homem")) %>%
  ggplot(aes(x = NomeAreaSubArea, y = SMs, group = genero, fill = genero)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("blue1", "purple")) +
  theme_minimal() +
  scale_x_discrete(labels = label_wrap(25)) +
  ylim(0, 6) +
  geom_text(
    aes(
      label = format(round(SMs, 1), big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = SMs
    ),
    vjust = -0.4,
    hjust = 0.5,
    position = position_dodge(width = 1)
  ) +

  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust=1)
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Remuneração por escolaridade, área e subarea de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Salário médio mensal do pessoal ocupado assalariado, por
# escolaridade e finalidade: Brasil

# names(Vinculos_OSC)

# Total geral de vínculos
TotalVinculos = nrow(Vinculos_OSC)

# Dados por Área e SubÁrea de atuação
MedRemEscAreaSubArea_OSC <- Vinculos_OSC %>%

  mutate(
    dummy_esc_sup = ifelse(grau_instr %in% c(9L, 10L, 11L), 1, 0)
  ) %>%

  # Calcula os Dados por SubArea:
  group_by(cd_subarea_atuacao) %>%

  summarise(
    # Soma_Vinculos = n(),
    Soma_Vinculos_Sup = sum(dummy_esc_sup == 1, na.rm = TRUE),
    media_salarial_Sup = mean(rem_media_reais[dummy_esc_sup == 1], na.rm = TRUE),
    Soma_Vinculos_SemSup = sum(dummy_esc_sup == 0, na.rm = TRUE),
    media_salarial_SemSup = mean(rem_media_reais[dummy_esc_sup == 0], na.rm = TRUE)
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
                      media_salarial_Sup = weighted.mean(media_salarial_Sup,
                                                       Soma_Vinculos_Sup,
                                                       na.rm = TRUE),

                      media_salarial_SemSup = weighted.mean(media_salarial_SemSup,
                                                       Soma_Vinculos_SemSup,
                                                       na.rm = TRUE),

                      Soma_Vinculos_Sup = sum(Soma_Vinculos_Sup, na.rm = TRUE),
                      Soma_Vinculos_SemSup = sum(Soma_Vinculos_SemSup, na.rm = TRUE),

                      Agregacao = "Area",
                      .by = cd_area_atuacao)
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

                      media_salarial_Sup = weighted.mean(media_salarial_Sup,
                                                       Soma_Vinculos_Sup,
                                                       na.rm = TRUE),

                      media_salarial_SemSup = weighted.mean(media_salarial_SemSup,
                                                       Soma_Vinculos_SemSup,
                                                       na.rm = TRUE),

                      Soma_Vinculos_Sup = sum(Soma_Vinculos_Sup, na.rm = TRUE),
                      Soma_Vinculos_SemSup = sum(Soma_Vinculos_SemSup, na.rm = TRUE),

                      Agregacao = "Todas",
                      tx_area_atuacao = "Brasil",
                      OrdemArea = "0")
  ) %>%
  mutate(NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                                  tx_subarea_atuacao,
                                  tx_area_atuacao),

         media_salarialSM_Sup = round(media_salarial_Sup / SM2022, 1),
         media_salarialSM_SemSup = round(media_salarial_SemSup / SM2022, 1),

         Per_Vinculos_Sup = (Soma_Vinculos_Sup / TotalVinculos) * 100,
         Per_Vinculos_SemSup = (Soma_Vinculos_SemSup / TotalVinculos) * 100
  ) %>%
  # Arruma a ordem das linhas
  arrange(OrdemArea) %>%

  # Seleciona os dados e a ordem das colunas
  select(
    Agregacao, OrdemArea, NomeAreaSubArea,
    media_salarial_Sup, media_salarial_SemSup,
    media_salarialSM_Sup, media_salarialSM_SemSup,
    Soma_Vinculos_Sup, Soma_Vinculos_SemSup,
    Per_Vinculos_Sup, Per_Vinculos_SemSup,
  )

# Checa dados
# View(MedRemEscAreaSubArea_OSC)
# names(MedRemEscAreaSubArea_OSC)

# Salva Tabela
saveRDS(MedRemEscAreaSubArea_OSC, "tables/MedRemEscAreaSubArea_OSC.RDS")

# Tabela Inteira:
MedRemEscAreaSubArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea,
              Soma_Vinculos_Sup, Soma_Vinculos_SemSup,
              Per_Vinculos_Sup, Per_Vinculos_SemSup)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    media_salarial_Sup = html("R$ 1,00 corrente<br>(Com Nível Superior)"),
    media_salarial_SemSup = html("R$ 1,00 corrente<br>(Sem Nível Superior)"),
    media_salarialSM_Sup = html("Salários Mínimos<br>(Com Nível Superior)"),
    media_salarialSM_SemSup = html("Salários Mínimos<br>(Sem Nível Superior)")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  # fmt_number(
  #   columns = c(media_salarial, Soma_Vinculos),
  #   sep_mark = ".",
  #   decimals = 0
  # ) %>%

  # Decimais
  fmt_number(
    columns = c(media_salarial_Sup, media_salarial_SemSup,
                media_salarialSM_Sup, media_salarialSM_SemSup),
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
    label = "Salário Médio Mensal",
    columns = c(media_salarial_Sup, media_salarial_SemSup)
  ) %>%

  tab_spanner(
    label = "Salários Mínimos",
    columns = c(media_salarialSM_Sup, media_salarialSM_SemSup)
  )


rm(TotalVinculos)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Remuneração por escolaridade e área de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Salário médio mensal do pessoal ocupado assalariado,
# por escolaridade e finalidade de atuação (Em R$ e SMs)

# names(MedRemEscAreaSubArea_OSC)

# Gráfico
MedRemEscAreaSubArea_OSC %>%
  dplyr::filter(NomeAreaSubArea != "Associações patronais e profissionais",
                Agregacao == "Area") %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, media_salarialSM_Sup,
         media_salarialSM_SemSup) %>%
  gather("EnsinoSup", "SMs", media_salarialSM_Sup:media_salarialSM_SemSup) %>%
  mutate(EnsinoSup = ifelse(EnsinoSup == "media_salarialSM_Sup",
                         "Com ensino superior",
                         "Sem ensino superior")) %>%
  ggplot(aes(x = NomeAreaSubArea, y = SMs, group = EnsinoSup, fill = EnsinoSup)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("blue1", "purple")) +
  theme_minimal() +
  scale_x_discrete(labels = label_wrap(25)) +
  ylim(0, 8) +
  geom_text(
    aes(
      label = format(round(SMs, 1), big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = SMs
    ),
    vjust = -0.4,
    hjust = 0.5,
    position = position_dodge(width = 1)
  ) +

  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust=1)
  )

rm(MedRemEscAreaSubArea_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Remuneração por raça, área e subarea de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA  - Salário médio mensal do pessoal ocupado assalariado,
# por raça e finalidade: Brasil

# names(Vinculos_OSC)

# Total geral de vínculos
TotalVinculos = nrow(Vinculos_OSC)

# Dados por Área e SubÁrea de atuação
MedRemRacaAreaSubArea_OSC <- Vinculos_OSC %>%

  mutate(
    dummy_branco = ifelse(raca_cor == 2L, 1, 0)
  ) %>%

  # Calcula os Dados por SubArea:
  group_by(cd_subarea_atuacao) %>%

  summarise(
    # Soma_Vinculos = n(),
    Soma_Vinculos_branco = sum(dummy_branco == 1, na.rm = TRUE),
    media_salarial_branco = mean(rem_media_reais[dummy_branco == 1], na.rm = TRUE),
    Soma_Vinculos_nbranco = sum(dummy_branco == 0, na.rm = TRUE),
    media_salarial_nbranco = mean(rem_media_reais[dummy_branco == 0], na.rm = TRUE)
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
                      media_salarial_branco = weighted.mean(media_salarial_branco,
                                                         Soma_Vinculos_branco,
                                                         na.rm = TRUE),

                      media_salarial_nbranco = weighted.mean(media_salarial_nbranco,
                                                            Soma_Vinculos_nbranco,
                                                            na.rm = TRUE),

                      Soma_Vinculos_branco = sum(Soma_Vinculos_branco, na.rm = TRUE),
                      Soma_Vinculos_nbranco = sum(Soma_Vinculos_nbranco, na.rm = TRUE),

                      Agregacao = "Area",
                      .by = cd_area_atuacao)
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

                      media_salarial_branco = weighted.mean(media_salarial_branco,
                                                         Soma_Vinculos_branco,
                                                         na.rm = TRUE),

                      media_salarial_nbranco = weighted.mean(media_salarial_nbranco,
                                                            Soma_Vinculos_nbranco,
                                                            na.rm = TRUE),

                      Soma_Vinculos_branco = sum(Soma_Vinculos_branco, na.rm = TRUE),
                      Soma_Vinculos_nbranco = sum(Soma_Vinculos_nbranco, na.rm = TRUE),

                      Agregacao = "Todas",
                      tx_area_atuacao = "Brasil",
                      OrdemArea = "0")
  ) %>%
  mutate(NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                                  tx_subarea_atuacao,
                                  tx_area_atuacao),

         media_salarialSM_branco = round(media_salarial_branco / SM2022, 1),
         media_salarialSM_nbranco = round(media_salarial_nbranco / SM2022, 1),

         Per_Vinculos_branco = (Soma_Vinculos_branco / TotalVinculos) * 100,
         Per_Vinculos_nbranco = (Soma_Vinculos_nbranco / TotalVinculos) * 100
  ) %>%
  # Arruma a ordem das linhas
  arrange(OrdemArea) %>%

  # Seleciona os dados e a ordem das colunas
  select(
    Agregacao, OrdemArea, NomeAreaSubArea,
    media_salarial_branco, media_salarial_nbranco,
    media_salarialSM_branco, media_salarialSM_nbranco,
    Soma_Vinculos_branco, Soma_Vinculos_nbranco,
    Per_Vinculos_branco, Per_Vinculos_nbranco,
  )

# Checa dados
# View(MedRemRacaAreaSubArea_OSC)
# names(MedRemRacaAreaSubArea_OSC)

# Salva Tabela
saveRDS(MedRemRacaAreaSubArea_OSC, "tables/MedRemRacaAreaSubArea_OSC.RDS")

# Tabela Inteira:
MedRemRacaAreaSubArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea,
              Soma_Vinculos_branco, Soma_Vinculos_nbranco,
              Per_Vinculos_branco, Per_Vinculos_nbranco)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    media_salarial_branco = html("R$ 1,00 corrente<br>(Brancos)"),
    media_salarial_nbranco = html("R$ 1,00 corrente<br>(Não Brancos)"),
    media_salarialSM_branco = html("Salários Mínimos<br>(Brancos)"),
    media_salarialSM_nbranco = html("Salários Mínimos<br>(Não Brancos)")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  # fmt_number(
  #   columns = c(media_salarial, Soma_Vinculos),
  #   sep_mark = ".",
  #   decimals = 0
  # ) %>%

  # Decimais
  fmt_number(
    columns = c(media_salarial_branco, media_salarial_nbranco,
                media_salarialSM_branco, media_salarialSM_nbranco),
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
    label = "Salário Médio Mensal",
    columns = c(media_salarial_branco, media_salarial_nbranco)
  ) %>%

  tab_spanner(
    label = "Salários Mínimos",
    columns = c(media_salarialSM_branco, media_salarialSM_nbranco)
  )


rm(TotalVinculos)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Remuneração por raça e área de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# names(MedRemRacaAreaSubArea_OSC)

# Gráfico
MedRemRacaAreaSubArea_OSC %>%
  dplyr::filter(NomeAreaSubArea != "Associações patronais e profissionais",
                Agregacao == "Area") %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, media_salarialSM_branco,
         media_salarialSM_nbranco) %>%
  gather("Raca", "SMs", media_salarialSM_branco:media_salarialSM_nbranco) %>%
  mutate(Raca = ifelse(Raca == "media_salarialSM_branco",
                            "Brancos",
                            "Não Brancos")) %>%
  ggplot(aes(x = NomeAreaSubArea, y = SMs, group = Raca, fill = Raca)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("lightgreen", "pink")) +
  theme_minimal() +
  scale_x_discrete(labels = label_wrap(25)) +
  ylim(0, 7) +
  geom_text(
    aes(
      label = format(round(SMs, 1), big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = SMs
    ),
    vjust = -0.4,
    hjust = 0.5,
    position = position_dodge(width = 1)
  ) +

  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust=1)
  )

rm(MedRemEscAreaSubArea_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Remuneração por raça e escolaridade ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Salário médio mensal do pessoal ocupado assalariado,
# por raça e escolaridade (Em R$)

MedRemRacaEsc_OSC <- Vinculos_OSC %>%
  mutate(
    raca = ifelse(raca_cor == 2L, "Brancos", "Não Brancos"),
    escolaridade = ifelse(grau_instr %in% c(9L, 10L, 11L),
                           "Com Ensino Superior",
                           "Sem Ensino Superior"),
  ) %>%
  group_by(raca, escolaridade) %>%
  summarise(
    Soma_Vinculos = n(),
    media_salarial = mean(rem_media_reais, na.rm = TRUE)
  )

# Checa dados
# View(MedRemRacaEsc_OSC)
# names(MedRemRacaEsc_OSC)

# Salva Tabela
saveRDS(MedRemRacaEsc_OSC, "tables/MedRemRacaEsc_OSC.RDS")

# Gráfico
MedRemRacaEsc_OSC %>%
  ggplot(aes(x = raca, y = media_salarial, group = escolaridade, fill = escolaridade)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("blue1", "purple")) +
  theme_minimal() +
  ylim(0, 9000) +
  geom_text(
    aes(
      label = format(round(media_salarial, 0), big.mark = ".",
                     decimal.mark = ",",
                     scientific = FALSE),
      y = media_salarial
    ),
    vjust = -0.4,
    hjust = 0.5,
    position = position_dodge(width = 1)
  ) +

  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
  )

rm(MedRemEscAreaSubArea_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Remuneração por Região, UF, área e subarea de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA 7 Salário médio mensal do pessoal ocupado assalariado,
# por Grandes Regiões e finalidade: Brasil (2015)

# names(Vinculos_OSC)

# Total geral de vínculos
TotalVinculos = nrow(Vinculos_OSC)

# names(MedRemRegUFAreaSubArea_OSC)

# Dados por Área e SubÁrea de atuação, Região e UF
MedRemRegUFAreaSubArea_OSC <- Vinculos_OSC %>%

  dplyr::filter(!is.na(cd_subarea_atuacao),
                !is.na(UF_Regiao)) %>%

  mutate(media_salarialSM = rem_media_reais / SM2022) %>%

  # Calcula os Dados por SubArea:
  group_by(cd_subarea_atuacao) %>%

  summarise(
    media_salarial_BR = mean(media_salarialSM, na.rm = TRUE),

    media_salarial_N = mean(media_salarialSM[UF_Regiao == "N"], na.rm = TRUE),
    media_salarial_NE = mean(media_salarialSM[UF_Regiao == "NE"], na.rm = TRUE),
    media_salarial_S = mean(media_salarialSM[UF_Regiao == "S"], na.rm = TRUE),
    media_salarial_SE = mean(media_salarialSM[UF_Regiao == "SE"], na.rm = TRUE),
    media_salarial_CO = mean(media_salarialSM[UF_Regiao == "CO"], na.rm = TRUE),

    Soma_Vinculos_BR = n(),

    Soma_Vinculos_N = sum(UF_Regiao == "N", na.rm = TRUE),
    Soma_Vinculos_NE = sum(UF_Regiao == "NE", na.rm = TRUE),
    Soma_Vinculos_S = sum(UF_Regiao == "S", na.rm = TRUE),
    Soma_Vinculos_SE = sum(UF_Regiao == "SE", na.rm = TRUE),
    Soma_Vinculos_CO = sum(UF_Regiao == "CO", na.rm = TRUE),

    ) %>%
  ungroup() %>%

  mutate(Agregacao = "SubArea") %>%
  # Adiciona os rótulos das subáreas
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, cd_area_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%

  # Calcula os Dados por Area:
  bind_rows(.,
            summarise(.,

                      media_salarial_BR = weighted.mean(media_salarial_BR,
                                                        Soma_Vinculos_BR,
                                                        na.rm = TRUE),

                      media_salarial_N = weighted.mean(media_salarial_N,
                                                       Soma_Vinculos_N,
                                                       na.rm = TRUE),

                      media_salarial_NE = weighted.mean(media_salarial_NE,
                                                        Soma_Vinculos_NE,
                                                        na.rm = TRUE),

                      media_salarial_S = weighted.mean(media_salarial_S,
                                                       Soma_Vinculos_S,
                                                       na.rm = TRUE),

                      media_salarial_SE = weighted.mean(media_salarial_SE,
                                                        Soma_Vinculos_SE,
                                                        na.rm = TRUE),

                      media_salarial_CO = weighted.mean(media_salarial_CO,
                                                        Soma_Vinculos_CO,
                                                        na.rm = TRUE),

                      Soma_Vinculos_BR = sum(Soma_Vinculos_BR, na.rm = TRUE),

                      Soma_Vinculos_N = sum(Soma_Vinculos_N, na.rm = TRUE),
                      Soma_Vinculos_NE = sum(Soma_Vinculos_NE, na.rm = TRUE),
                      Soma_Vinculos_S = sum(Soma_Vinculos_S, na.rm = TRUE),
                      Soma_Vinculos_SE = sum(Soma_Vinculos_SE, na.rm = TRUE),
                      Soma_Vinculos_CO = sum(Soma_Vinculos_CO, na.rm = TRUE),

                      Agregacao = "Area",
                      .by = cd_area_atuacao)
            ) %>%
  # Adiciona os rótulos das áreas
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%

  mutate(OrdemArea = ifelse(Agregacao == "Area",
                            paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_0"),
                            OrdemArea)) %>%

  mutate(NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                                  tx_subarea_atuacao,
                                  tx_area_atuacao)
         ) %>%
  # Arruma a ordem das linhas
  arrange(OrdemArea) %>%

  # Seleciona os dados e a ordem das colunas
  select(
    Agregacao, OrdemArea, NomeAreaSubArea,
    media_salarial_BR, media_salarial_N, media_salarial_NE, media_salarial_S,
    media_salarial_SE, media_salarial_CO,
    Soma_Vinculos_BR, Soma_Vinculos_N, Soma_Vinculos_NE, Soma_Vinculos_S,
    Soma_Vinculos_SE, Soma_Vinculos_CO,
  )


# Checa dados
# View(MedRemRegUFAreaSubArea_OSC)
# names(MedRemRegUFAreaSubArea_OSC)

# Salva Tabela
saveRDS(MedRemRegUFAreaSubArea_OSC, "tables/MedRemRegUFAreaSubArea_OSC.RDS")

# Tabela Inteira:
MedRemRegUFAreaSubArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea,
              Soma_Vinculos_BR, Soma_Vinculos_N, Soma_Vinculos_NE,
              Soma_Vinculos_S, Soma_Vinculos_SE, Soma_Vinculos_CO)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade de atuação",
    media_salarial_BR = "Brasil",
    media_salarial_N = "Norte",
    media_salarial_NE = "Nordeste",
    media_salarial_S = "Sul",
    media_salarial_SE = "Sudeste",
    media_salarial_CO = "Centro-Oeste"
    # media_salarial_F = html("R$ 1,00 corrente<br>(Mulher)"),
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  # fmt_number(
  #   columns = c(media_salarial, Soma_Vinculos),
  #   sep_mark = ".",
  #   decimals = 0
  # ) %>%

  # Decimais
  fmt_number(
    columns = c(media_salarial_BR, media_salarial_N, media_salarial_NE,
                media_salarial_S, media_salarial_SE, media_salarial_CO),
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

  # Destaca os dados agregados
  tab_style(
    style = cell_text(weight = "bold"), # Intentação
    locations = cells_body(
      columns = media_salarial_BR )
  ) %>%

  # Cria grupos de variáveis (spanners)

  tab_spanner(
    label = html("Salário médio mensal (salário mínimo)"),
    columns = c(media_salarial_BR, media_salarial_N, media_salarial_NE,
                media_salarial_S, media_salarial_SE, media_salarial_CO)
  )


rm(TotalVinculos)



# Fim ####
