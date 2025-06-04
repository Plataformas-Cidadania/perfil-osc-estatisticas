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


tb_relacoes_trabalho <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_relacoes_trabalho.RDS")



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
  select(id_osc, dt_fundacao_osc, dt_fechamento_osc) %>%
  dplyr::filter(id_osc %in% tb_relacoes_trabalho$id_osc) %>%

  # Área de atuação
  left_join(area_atuacao_clean, by = "id_osc") %>%

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
  # Insere Dados de Vínculos de Trabalho
  left_join(
    select(tb_relacoes_trabalho,
           id_osc, nr_trabalhadores_vinculo, nr_trabalhadores_deficiencia),
    by = "id_osc"
  ) %>%
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
  rename(n_vinculos = nr_trabalhadores_vinculo) %>%
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
      TRUE ~ -1 )
  ) %>%
  select(everything())

rm(tb_osc, tb_dados_gerais, tb_localicazao, tb_relacoes_trabalho)
rm(Municipios, UFs, tb_area_atuacao, area_atuacao_clean)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Porte das OSC por categorias ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Total e percentual de OSCs, por faixas de vínculos
FreqPorte_OSC <- OscAtiva %>%
  group_by(PorteOSC, OrdemPorte) %>%
  summarise(Freq = n()) %>%
  mutate(Per = (Freq / sum(Freq)) * 100) %>%
  arrange(OrdemPorte) %>%
  ungroup()

# Checa dados
FreqPorte_OSC

# Salva Tabela
saveRDS(FreqPorte_OSC, "tables/FreqPorte_OSC.RDS")

FreqPorte_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(OrdemPorte)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    PorteOSC = "Faixas de vínculos",
    Freq = "N",
    Per = "(%)"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(Per),
    decimals = 1,
    dec_mark = ",",
  ) %>%
  fmt_number(
    columns = c(Freq),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !PorteOSC )
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "OSCs",
    columns = c(Freq, Per)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Porte das OSC por categorias e áreas de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Total e percentual de OSCs por finalidades de atuação e por faixas de
# número de vínculos

FreqPorteArea_OSC <- OscAtiva %>%
  group_by(cd_area_atuacao, PorteOSC, OrdemPorte) %>%
  summarise(Freq = n() ) %>%
  mutate(Agregacao = "Area") %>%
  arrange(OrdemPorte, cd_area_atuacao) %>%
  select(-OrdemPorte) %>%
  spread(PorteOSC, Freq, fill = 0) %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(FreqPorteArea_OSC)


FreqPorteSubArea_OSC <- OscAtiva %>%
  group_by(cd_subarea_atuacao, PorteOSC, OrdemPorte) %>%
  summarise(Freq = n() ) %>%
  mutate(Agregacao = "SubArea") %>%
  arrange(OrdemPorte, cd_subarea_atuacao) %>%
  select(-OrdemPorte) %>%
  spread(PorteOSC, Freq, fill = 0) %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(FreqPorteSubArea_OSC)

JoinFreqPorteArea_OSC <- bind_rows(FreqPorteArea_OSC, FreqPorteSubArea_OSC) %>%

  rename(SemVinculos = "Sem vínculos",
         De1a2 = "De 1 a 2",
         De3a4 = "De 3 a 4",
         De5a9 = "De 5 a 9",
         De10a49 = "De 10 a 49",
         De50a99 = "De 50 a 99",
         De100a499 = "De 100 a 499",
         Mais500 = "500 ou mais" ) %>%

  arrange(OrdemArea, NomeAreaSubArea) %>%

  rowwise() %>%
  mutate(
    TotalArea = sum(SemVinculos, De1a2, De3a4, De5a9, De10a49, De50a99,
                    De100a499, Mais500, na.rm = TRUE),
    PerSemVinculos = (SemVinculos / TotalArea) * 100,
    PerDe1a2 = (De1a2 / TotalArea) * 100,
    PerDe3a4 = (De3a4 / TotalArea) * 100,
    PerDe5a9 = (De5a9 / TotalArea) * 100,
    PerDe10a49 = (De10a49 / TotalArea) * 100,
    PerDe50a99 = (De50a99 / TotalArea) * 100,
    PerDe100a499 = (De100a499 / TotalArea) * 100,
    PerMais500 = (Mais500 / TotalArea) * 100,
    PerTotalArea = (TotalArea / TotalArea) * 100
  ) %>%

  select(Agregacao, OrdemArea, NomeAreaSubArea,
         SemVinculos, PerSemVinculos,
         De1a2, PerDe1a2,
         De3a4, PerDe3a4,
         De5a9, PerDe5a9,
         De10a49, PerDe10a49,
         De50a99, PerDe50a99,
         De100a499, PerDe100a499,
         Mais500, PerMais500,
         TotalArea, PerTotalArea)

# Checa dados
# View(JoinFreqPorteArea_OSC)
# names(JoinFreqPorteArea_OSC)

# Salva Tabela
saveRDS(JoinFreqPorteArea_OSC, "tables/JoinFreqPorteArea_OSC.RDS")

# Tabela Inteira:
JoinFreqPorteArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade das OSCs",
    SemVinculos = "N",
    PerSemVinculos = "(%)",
    De1a2 = "N",
    PerDe1a2 = "(%)",
    De3a4 = "N",
    PerDe3a4 = "(%)",
    De5a9 = "N",
    PerDe5a9 = "(%)",
    De10a49 = "N",
    PerDe10a49 = "(%)",
    De50a99 = "N",
    PerDe50a99 = "(%)",
    De100a499 = "N",
    PerDe100a499 = "(%)",
    Mais500 = "N",
    PerMais500 = "(%)",
    TotalArea = "N",
    PerTotalArea = "(%)"  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(SemVinculos, De1a2, De3a4, De5a9, De10a49, De50a99, De100a499,
                TotalArea),
    sep_mark = ".",
    decimals = 0
  ) %>%

  fmt_number(
    columns = c(PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9, PerDe10a49,
                PerDe50a99, PerDe100a499, PerTotalArea),
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
    label = "Sem pessoal",
    columns = c(SemVinculos, PerSemVinculos)
  ) %>%

  tab_spanner(
    label = "De 1 a 2",
    columns = c(De1a2, PerDe1a2)
  ) %>%

  tab_spanner(
    label = "De 3 a 4",
    columns = c(De3a4, PerDe3a4)
  ) %>%

  tab_spanner(
    label = "De 5 a 9",
    columns = c(De5a9, PerDe5a9)
  ) %>%

  tab_spanner(
    label = "De 10 a 49",
    columns = c(De10a49, PerDe10a49)
  ) %>%

  tab_spanner(
    label = "De 50 a 99",
    columns = c(De50a99, PerDe50a99)
  ) %>%

  tab_spanner(
    label = "De 100 a 499",
    columns = c(De100a499, PerDe100a499)
  ) %>%

  tab_spanner(
    label = "500 e mais",
    columns = c(Mais500, PerMais500)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(TotalArea, PerTotalArea)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )



# Tabela 1
JoinFreqPorteArea_OSC %>%
  # Seleciona somente primeira metade
  select(Agregacao:PerDe5a9, TotalArea, PerTotalArea) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade das OSCs",
    SemVinculos = "N",
    PerSemVinculos = "(%)",
    De1a2 = "N",
    PerDe1a2 = "(%)",
    De3a4 = "N",
    PerDe3a4 = "(%)",
    De5a9 = "N",
    PerDe5a9 = "(%)",
    TotalArea = "N",
    PerTotalArea = "(%)"  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(SemVinculos, De1a2, De3a4, De5a9,
                TotalArea),
    sep_mark = ".",
    decimals = 0
  ) %>%

  fmt_number(
    columns = c(PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9,
                PerTotalArea),
    decimals = 1,
    dec_mark = ",",
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9, PerTotalArea)
    )
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
    label = html("Sem<br>pessoal"),
    columns = c(SemVinculos, PerSemVinculos)
  ) %>%

  tab_spanner(
    label = "1 a 2",
    columns = c(De1a2, PerDe1a2)
  ) %>%

  tab_spanner(
    label = "3 a 4",
    columns = c(De3a4, PerDe3a4)
  ) %>%

  tab_spanner(
    label = "5 a 9",
    columns = c(De5a9, PerDe5a9)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(TotalArea, PerTotalArea)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )


# Tabela 2
JoinFreqPorteArea_OSC %>%
  # Seleciona somente segunda metade
  select(Agregacao, OrdemArea, NomeAreaSubArea, De10a49:PerTotalArea) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade das OSCs",
    De10a49 = "N",
    PerDe10a49 = "(%)",
    De50a99 = "N",
    PerDe50a99 = "(%)",
    De100a499 = "N",
    PerDe100a499 = "(%)",
    Mais500 = "N",
    PerMais500 = "(%)",
    TotalArea = "N",
    PerTotalArea = "(%)"  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(De10a49, De50a99, De100a499, Mais500, TotalArea),
    sep_mark = ".",
    decimals = 0
  ) %>%

  fmt_number(
    columns = c(PerDe10a49, PerDe50a99, PerDe100a499, PerMais500, PerTotalArea),
    decimals = 1,
    dec_mark = ",",
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeAreaSubArea )
  ) %>%

  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(PerDe10a49, PerDe50a99, PerDe100a499, PerMais500,
                  PerTotalArea)
    )
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
    label = "10 a 49",
    columns = c(De10a49, PerDe10a49)
  ) %>%

  tab_spanner(
    label = "50 a 99",
    columns = c(De50a99, PerDe50a99)
  ) %>%

  tab_spanner(
    label = "100 a 499",
    columns = c(De100a499, PerDe100a499)
  ) %>%

  tab_spanner(
    label = "500+",
    columns = c(Mais500, PerMais500)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(TotalArea, PerTotalArea)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Porte das OSC por categorias e áreas de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gráfico - Total e percentual do número de OSCs por finalidades de atuação e
# por faixas de  número de vínculos

PlotFreqPorteArea_OSC <- JoinFreqPorteArea_OSC %>%
  dplyr::filter(Agregacao == "Area") %>%
  select(NomeAreaSubArea, PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9,
         PerDe10a49, PerDe50a99, PerDe100a499, PerMais500) %>%
  gather(key = "key", value = "value", PerSemVinculos:PerMais500) %>%
  mutate(
    value = value/100,
    OrdemPorte = case_when(
      key == "PerSemVinculos" ~ 8,
      key == "PerDe1a2" ~ 7,
      key == "PerDe3a4" ~ 6,
      key == "PerDe5a9" ~ 5,
      key == "PerDe10a49" ~ 4,
      key == "PerDe50a99" ~ 3,
      key == "PerDe100a499" ~ 2,
      key == "PerMais500" ~ 1 ),

    key = case_when(
      key == "PerSemVinculos" ~ "Sem Vínculos",
      key == "PerDe1a2" ~ "1 a 2",
      key == "PerDe3a4" ~ "3 a 4",
      key == "PerDe5a9" ~ "5 a 9",
      key == "PerDe10a49" ~ "10 a 49",
      key == "PerDe50a99" ~ "50 a 99",
      key == "PerDe100a499" ~ "100 a 499",
      key == "PerMais500" ~ "Mais 500" ),

    key = fct_reorder(key, OrdemPorte)
    )

# Checa dados
# View(PlotFreqPorteArea_OSC)

# Salva Tabela
saveRDS(PlotFreqPorteArea_OSC, "tables/PlotFreqPorteArea_OSC.RDS")


PlotFreqPorteArea_OSC %>%
  ggplot(aes(x = NomeAreaSubArea, y = value, fill = key)) +
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(labels = label_wrap(25)) +
  scale_fill_manual(
    values=c(
      "darkgreen",
      "green",
      "beige",
      "darkorchid1",
      "darkviolet",
      "cyan",
      "blue",
      "blue4"
    )) +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = 'Número de Vínculos:') +
  guides(fill = guide_legend(reverse = TRUE) ) +
  coord_flip() +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

PlotFreqPorteArea_OSC %>%
  dplyr::filter(key != "Sem Vínculos") %>%
  ggplot(aes(x = NomeAreaSubArea, y = value, fill = key)) +
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(labels = label_wrap(25)) +
  scale_fill_manual(
    values=c(
      "darkgreen",
      "green",
      "beige",
      "darkorchid1",
      "darkviolet",
      "cyan",
      "blue",
      "blue4"
    )) +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = 'Número de Vínculos:') +
  guides(fill = guide_legend(reverse = TRUE) ) +
  coord_flip() +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Porte por Região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA 3 Número médio de vínculos de emprego em OSCs em diferentes
# finalidades de atuação,  por região

MeanVincRegArea_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_area_atuacao, UF_Regiao) %>%
  summarise(Media_Vinculos = mean(n_vinculos, na.rm = TRUE) ) %>%
  mutate(Agregacao = "Area") %>%
  spread(UF_Regiao, Media_Vinculos, fill = 0) %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  rename(NomeAreaSubArea = tx_area_atuacao) %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(MeanVincRegArea_OSC)

MeanVincRegSubArea_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_subarea_atuacao, UF_Regiao) %>%
  summarise(Media_Vinculos = mean(n_vinculos, na.rm = TRUE) ) %>%
  mutate(Agregacao = "SubArea") %>%
  spread(UF_Regiao, Media_Vinculos, fill = 0) %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  rename(NomeAreaSubArea = tx_subarea_atuacao) %>%
  select(-cd_subarea_atuacao) %>%
  select(Agregacao, OrdemArea, NomeAreaSubArea, everything())

# Checa dados
# View(MeanVincRegSubArea_OSC)

# Dados do Brasil como um todo
MeanVincBRArea_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_area_atuacao) %>%
  summarise(BR = mean(n_vinculos, na.rm = TRUE) ) %>%
  ungroup() %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_00")) %>%
  select(-cd_area_atuacao) %>%
  select(OrdemArea, everything())

# View(MeanVincBRArea_OSC)

MeanVincBRSubArea_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(cd_subarea_atuacao) %>%
  summarise(BR = mean(n_vinculos, na.rm = TRUE) ) %>%
  ungroup() %>%
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%
  select(-cd_subarea_atuacao) %>%
  select(OrdemArea, everything())

# View(MeanVincBRSubArea_OSC)

JoinBRArea_OSC <- bind_rows(MeanVincBRArea_OSC, MeanVincBRSubArea_OSC) %>%
  distinct(OrdemArea, .keep_all = TRUE)

# Checa dados
# View(JoinBRArea_OSC)

#Junta tudo
JoinMeanVincRegArea_OSC <- bind_rows(MeanVincRegArea_OSC, MeanVincRegSubArea_OSC) %>%
  distinct(OrdemArea, .keep_all = TRUE) %>%
  left_join(JoinBRArea_OSC,
            by = "OrdemArea") %>%
  arrange(OrdemArea, NomeAreaSubArea)


# Checa dados
# View(JoinMeanVincRegArea_OSC)
# names(JoinMeanVincRegArea_OSC)


# Salva Tabela
saveRDS(JoinMeanVincRegArea_OSC, "tables/JoinMeanVincRegArea_OSC.RDS")

# Tabela Inteira:
JoinMeanVincRegArea_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = "Finalidade das OSCs",
    CO = "Centro-Oeste",
    N = "Norte",
    NE = "Nordeste",
    S = "Sul",
    SE = "Sudeste",
    BR = "Brasil"
   ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(CO, N, NE, S, SE, BR),
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
      rows = Agregacao ==  "SubArea")
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Média de vínculos por áreas de atuação ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO 2 Número médio de vínculos de emprego em OSCs em diferentes
# finalidades de atuação:  Brasil

MeanVincArea <- OscAtiva %>%
  group_by(cd_area_atuacao) %>%
  summarise(Media_Vinculos = mean(n_vinculos, na.rm = TRUE) ) %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  select(everything())

# Checa Dados:
MeanVincArea

# Mesmo gráfico, mas agora com as organizações que tem pelo menos um vínculo.
MeanVincArea_1mais <- OscAtiva %>%
  dplyr::filter(n_vinculos >= 1) %>%
  group_by(cd_area_atuacao) %>%
  summarise(Media_Vinculos = mean(n_vinculos, na.rm = TRUE) ) %>%
  ungroup() %>%
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%
  select(everything())

# Checa Dados:
MeanVincArea_1mais

# Salva Tabela
saveRDS(MeanVincArea, "tables/MeanVincArea.RDS")
saveRDS(MeanVincArea_1mais, "tables/MeanVincArea_1mais.RDS")

# Gráficos:
MeanVincArea_1mais %>%
  ggplot(aes(x = tx_area_atuacao, y = Media_Vinculos)) +
  geom_bar(stat="identity", color = "blue3", fill = "blue3") +
  scale_x_discrete(labels = label_wrap(25)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 40, hjust=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

MeanVincArea_1mais %>%
  ggplot(aes(x = tx_area_atuacao, y = Media_Vinculos)) +
  geom_bar(stat="identity", color = "blue3", fill = "blue3") +
  scale_x_discrete(labels = label_wrap(25)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 40, hjust=1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Porte das OSC por porte e UF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA 4 Total e percentual do número de OSCs por regiões geográficas e por
# faixas de número de vínculos

FreqPorteUF_OSC <- OscAtiva %>%
  group_by(UF_Nome, UF_Regiao, PorteOSC, OrdemPorte) %>%
  summarise(Freq = n() ) %>%
  mutate(Agregacao = "UF") %>%
  arrange(OrdemPorte, UF_Regiao, UF_Nome) %>%
  select(-OrdemPorte) %>%
  spread(PorteOSC, Freq, fill = 0) %>%
  ungroup() %>%
  select(Agregacao, UF_Regiao, UF_Nome, everything()) %>%
  dplyr::filter(!is.na(UF_Regiao))

# Checa dados
# View(FreqPorteUF_OSC)

# Dados por região
FreqPorteReg_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(UF_Regiao, PorteOSC, OrdemPorte) %>%
  summarise(Freq = n() ) %>%
  mutate(Agregacao = "Regiao",
         UF_Nome = "0") %>%
  arrange(OrdemPorte, UF_Regiao) %>%
  select(-OrdemPorte) %>%
  spread(PorteOSC, Freq, fill = 0) %>%
  ungroup() %>%
  select(Agregacao, UF_Regiao, UF_Nome, everything())

# Checa dados
# View(FreqPorteReg_OSC)

# Dados BR
FreqPorteBR_OSC <- OscAtiva %>%
  dplyr::filter(!is.na(UF_Regiao)) %>%
  group_by(PorteOSC, OrdemPorte) %>%
  summarise(Freq = n() ) %>%
  mutate(Agregacao = "BR",
         UF_Regiao = "BR",
         UF_Nome = "0") %>%
  select(-OrdemPorte) %>%
  spread(PorteOSC, Freq, fill = 0) %>%
  ungroup() %>%
  select(Agregacao, UF_Regiao, UF_Nome, everything())

# Checa dados
# View(FreqPorteBR_OSC)

# Junta tudo
JoinFreqPorteUF_OSC <- bind_rows(FreqPorteUF_OSC, FreqPorteReg_OSC,
                                 FreqPorteBR_OSC) %>%

  rename(SemVinculos = "Sem vínculos",
         De1a2 = "De 1 a 2",
         De3a4 = "De 3 a 4",
         De5a9 = "De 5 a 9",
         De10a49 = "De 10 a 49",
         De50a99 = "De 50 a 99",
         De100a499 = "De 100 a 499",
         Mais500 = "500 ou mais" ) %>%

  mutate(
    Ordem = paste0(UF_Regiao, "_", UF_Nome) ,

    NomeRegUF = case_when(
      Agregacao == "UF" ~ UF_Nome,
      UF_Regiao == "BR" ~ "Brasil",
      UF_Regiao == "S" ~ "Sul",
      UF_Regiao == "N" ~ "Norte",
      UF_Regiao == "CO" ~ "Centro-Oeste",
      UF_Regiao == "NE" ~ "Nordeste",
      UF_Regiao == "SE" ~ "Sudeste",
      TRUE ~ "erro"  )
    ) %>%

  arrange(Ordem) %>%

  rowwise() %>%
  mutate(
    TotalUF = sum(SemVinculos, De1a2, De3a4, De5a9, De10a49, De50a99,
                    De100a499, Mais500, na.rm = TRUE),
    PerSemVinculos = (SemVinculos / TotalUF) * 100,
    PerDe1a2 = (De1a2 / TotalUF) * 100,
    PerDe3a4 = (De3a4 / TotalUF) * 100,
    PerDe5a9 = (De5a9 / TotalUF) * 100,
    PerDe10a49 = (De10a49 / TotalUF) * 100,
    PerDe50a99 = (De50a99 / TotalUF) * 100,
    PerDe100a499 = (De100a499 / TotalUF) * 100,
    PerMais500 = (Mais500 / TotalUF) * 100,
    PerUFArea = (TotalUF / TotalUF) * 100
  ) %>%

  select(Agregacao, UF_Regiao, UF_Nome,
         NomeRegUF,
         SemVinculos, PerSemVinculos,
         De1a2, PerDe1a2,
         De3a4, PerDe3a4,
         De5a9, PerDe5a9,
         De10a49, PerDe10a49,
         De50a99, PerDe50a99,
         De100a499, PerDe100a499,
         Mais500, PerMais500,
         TotalUF, PerUFArea)

# Checa dados
# View(JoinFreqPorteUF_OSC)
# names(JoinFreqPorteUF_OSC)

# Salva Tabela
saveRDS(JoinFreqPorteUF_OSC, "tables/JoinFreqPorteUF_OSC.RDS")

# Tabela Inteira:
JoinFreqPorteUF_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide( c(Agregacao, UF_Regiao, UF_Nome) ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "",
    SemVinculos = "N",
    PerSemVinculos = "(%)",
    De1a2 = "N",
    PerDe1a2 = "(%)",
    De3a4 = "N",
    PerDe3a4 = "(%)",
    De5a9 = "N",
    PerDe5a9 = "(%)",
    De10a49 = "N",
    PerDe10a49 = "(%)",
    De50a99 = "N",
    PerDe50a99 = "(%)",
    De100a499 = "N",
    PerDe100a499 = "(%)",
    Mais500 = "N",
    PerMais500 = "(%)",
    TotalUF = "N",
    PerUFArea = "(%)"  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(SemVinculos, De1a2, De3a4, De5a9, De10a49, De50a99, De100a499,
                Mais500, TotalUF),
    sep_mark = ".",
    decimals = 0
  ) %>%

  fmt_number(
    columns = c(PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9, PerDe10a49,
                PerDe50a99, PerDe100a499, PerMais500, PerUFArea),
    decimals = 1,
    dec_mark = ",",
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeRegUF )
  ) %>%

  # Torna as Regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeRegUF,
      rows = Agregacao ==  "UF")
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = html("Sem<br>pessoal"),
    columns = c(SemVinculos, PerSemVinculos)
  ) %>%

  tab_spanner(
    label = "1 a 2",
    columns = c(De1a2, PerDe1a2)
  ) %>%

  tab_spanner(
    label = "3 a 4",
    columns = c(De3a4, PerDe3a4)
  ) %>%

  tab_spanner(
    label = "5 a 9",
    columns = c(De5a9, PerDe5a9)
  ) %>%

  tab_spanner(
    label = "10 a 49",
    columns = c(De10a49, PerDe10a49)
  ) %>%

  tab_spanner(
    label = "50 a 99",
    columns = c(De50a99, PerDe50a99)
  ) %>%

  tab_spanner(
    label = "100 a 499",
    columns = c(De100a499, PerDe100a499)
  ) %>%

  tab_spanner(
    label = "500 +",
    columns = c(Mais500, PerMais500)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(TotalUF, PerUFArea)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Separadores para facilitar a visualização
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9, PerDe10a49,
                  PerDe50a99, PerDe100a499, PerMais500, PerUFArea)
      )
    )

# Tabela 1:
JoinFreqPorteUF_OSC %>%
  select(Agregacao:PerDe5a9, TotalUF, PerUFArea) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide( c(Agregacao, UF_Regiao, UF_Nome) ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "",
    SemVinculos = "N",
    PerSemVinculos = "(%)",
    De1a2 = "N",
    PerDe1a2 = "(%)",
    De3a4 = "N",
    PerDe3a4 = "(%)",
    De5a9 = "N",
    PerDe5a9 = "(%)",
    TotalUF = "N",
    PerUFArea = "(%)"  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(SemVinculos, De1a2, De3a4, De5a9, TotalUF),
    sep_mark = ".",
    decimals = 0
  ) %>%

  fmt_number(
    columns = c(PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9, PerUFArea),
    decimals = 1,
    dec_mark = ",",
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeRegUF )
  ) %>%

  # Torna as Regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeRegUF,
      rows = Agregacao ==  "UF")
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = html("Sem<br>pessoal"),
    columns = c(SemVinculos, PerSemVinculos)
  ) %>%

  tab_spanner(
    label = "1 a 2",
    columns = c(De1a2, PerDe1a2)
  ) %>%

  tab_spanner(
    label = "3 a 4",
    columns = c(De3a4, PerDe3a4)
  ) %>%

  tab_spanner(
    label = "5 a 9",
    columns = c(De5a9, PerDe5a9)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(TotalUF, PerUFArea)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Separadores para facilitar a visualização
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(PerSemVinculos, PerDe1a2, PerDe3a4, PerDe5a9, PerUFArea)
    )
  )

# Tabela 2:
JoinFreqPorteUF_OSC %>%
  select(Agregacao:NomeRegUF, De10a49:PerUFArea) %>%

  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide( c(Agregacao, UF_Regiao, UF_Nome) ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = "",
    De10a49 = "N",
    PerDe10a49 = "(%)",
    De50a99 = "N",
    PerDe50a99 = "(%)",
    De100a499 = "N",
    PerDe100a499 = "(%)",
    Mais500 = "N",
    PerMais500 = "(%)",
    TotalUF = "N",
    PerUFArea = "(%)"  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(De10a49, De50a99, De100a499, Mais500, TotalUF),
    sep_mark = ".",
    decimals = 0
  ) %>%

  fmt_number(
    columns = c(PerDe10a49,PerDe50a99, PerDe100a499, PerMais500, PerUFArea),
    decimals = 1,
    dec_mark = ",",
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NomeRegUF )
  ) %>%

  # Torna as Regiões mais destacadas
  tab_style(
    style = cell_text(indent = px(20)), # Intentação
    locations = cells_body(
      columns = NomeRegUF,
      rows = Agregacao ==  "UF")
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "10 a 49",
    columns = c(De10a49, PerDe10a49)
  ) %>%

  tab_spanner(
    label = "50 a 99",
    columns = c(De50a99, PerDe50a99)
  ) %>%

  tab_spanner(
    label = "100 a 499",
    columns = c(De100a499, PerDe100a499)
  ) %>%

  tab_spanner(
    label = "500 +",
    columns = c(Mais500, PerMais500)
  ) %>%

  tab_spanner(
    label = "Total",
    columns = c(TotalUF, PerUFArea)
  ) %>%

  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_spanners()
  ) %>%

  # Separadores para facilitar a visualização
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "dashed",
      weight = px(1)),
    locations = cells_body(
      columns = c(PerDe10a49, PerDe50a99, PerDe100a499, PerMais500, PerUFArea)
    )
  )


# Fim ####
