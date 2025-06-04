# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo:
# "TRANSFERÊNCIAS DE RECURSOS PÚBLICOS PARA AS OSCs"

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



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Coneção à Base ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Baixa a chave secreta do código

keys_file <- "keys/psql12-usr_manutencao_mapa.json"
assert_that(file.exists(keys_file))
keys <- jsonlite::read_json(keys_file)


# Verifica se pode conectar
TestConexao <- dbCanConnect(RPostgres::Postgres(),
                            dbname = keys$dbname,
                            host = keys$host,
                            port = keys$port,
                            user = keys$username,
                            password = keys$password)

assert_that(TestConexao,
            msg = paste("O teste de coneção falhou, ",
                        "verificar nome da base, host, ",
                        "porta, usuário e senha."))

# conencta à base
connec <- dbConnect(RPostgres::Postgres(),
                    dbname = keys$dbname,
                    host = keys$host,
                    port = keys$port,
                    user = keys$username,
                    password = keys$password,
                    options="-c search_path=public")

# Verifica a coneção com a base
assert_that(dbIsValid(connec))

rm(keys, TestConexao, keys_file)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Monta os dados ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Area de Atuação das OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Áreas de Atuaçãpo
# tb_area_atuacao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_area_atuacao.RDS")
tb_area_atuacao <- dbGetQuery(connec, "SELECT * FROM osc.tb_area_atuacao;")

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

dc_area_subarea_atuacao <- fread("data/dc_area_subarea_atuacao.csv",
                                 encoding = "Latin-1") %>%
  mutate(OrdemArea = paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_",
                            str_pad(cd_subarea_atuacao, 2, pad = "0") ) ) %>%
  distinct(cd_subarea_atuacao, .keep_all = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dados Gerais e Localização das OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Informações sobre as OSC ativas:
# tb_osc <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_osc.RDS")
tb_osc <- dbGetQuery(connec, "SELECT * FROM osc.tb_osc;")

# Localização das OSC:
# tb_localicazao <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_localizacao.RDS")
tb_localicazao <- dbGetQuery(connec, "SELECT * FROM osc.tb_localizacao;")

# Dados Gerais OSC:
# tb_dados_gerais <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_dados_gerais.RDS")
tb_dados_gerais <- dbGetQuery(connec, "SELECT * FROM osc.tb_dados_gerais;")

# Dados básicos das Unidades Federativas:
UFs <- fread("data/UFs.csv", encoding = "Latin-1") %>%
  mutate(UF_Id = as.character(UF_Id))

# Informações Básicas dos Municípios:
Municipios <- fread("data/Municipios.csv", encoding = "Latin-1")


# OSC do banco (não só as ativas)
DadosOSCs <- tb_dados_gerais %>%
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

  select(id_osc, cd_identificador_osc, cd_natureza_juridica_osc,
         dt_fundacao_osc, dt_fechamento_osc, bo_osc_ativa) %>%

  # Área de atuação
  left_join(area_atuacao_clean, by = "id_osc") %>%

  # Evita problemas de pading
  mutate(nr_ano_fundacao_osc = year(ymd(dt_fundacao_osc)),
         nr_ano_fechamento_osc = year(ymd(dt_fechamento_osc)),

  ) %>%
  # Insere dados de município e UF.
  left_join(
    select(tb_localicazao,
           id_osc, cd_municipio),
    by = "id_osc"
  ) %>%
  mutate(
    UF_Id = str_sub(cd_municipio, 1, 2),
    cd_identificador_osc = str_pad(cd_identificador_osc,
                                   width = 14,
                                   side = "left",
                                   pad = 0)

    ) %>%
  left_join(
    select(UFs,
           UF_Id, UF_Nome, UF_Sigla, UF_Regiao),
    by = "UF_Id"
  ) %>%
  select(everything())

OrdemUF <- fread("data/OrdemUF.csv", encoding = "Latin-1")


# Limpa a memória
rm(tb_osc, tb_dados_gerais, tb_localicazao)
rm(Municipios, UFs, area_atuacao_clean)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Porte das OSC ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# tb_relacoes_trabalho <- readRDS("temp/dataset/2025-03-06 extract-homolog/tb_relacoes_trabalho.RDS")
tb_relacoes_trabalho <- dbGetQuery(connec, "SELECT * FROM osc.tb_relacoes_trabalho;")

# Filtra as OSCs ativas em tb_dados_gerais
PorteOSC <- tb_relacoes_trabalho %>%
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
  select(id_osc, n_vinculos, PorteOSC, OrdemPorte)

# Junta os dados:
DadosOSCs <- DadosOSCs %>%
  left_join(PorteOSC, by = "id_osc")

rm(PorteOSC, tb_relacoes_trabalho)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dados de Transferências ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FuncoesOrcamentarias <- read_xlsx("temp/transferencias/FuncoesOrcamentarias.xlsx",
                                  sheet = 1) %>%
  mutate(FuncaoCod = str_pad(FuncaoCod, side = "left", pad = "0", width = 2),
         SubFuncaoCod = str_pad(SubFuncaoCod, side = "left", pad = "0", width = 3))


# raw_loa <- readRDS("temp/transferencias/mosc_2661_loa_despesa_execucao_extrato_osc_v3.RDS")
raw_loa <- dbGetQuery(connec, "SELECT * FROM public.mosc_2661_loa_tb_orcamento_def_v7;")

# Creio que para a estrutura do capítulo, é melhor adicionar os dados das OSC
# na tabela de transferências e não o contrário
loa_despesa <- raw_loa %>%
  # mutate(
  #   FuncOr = str_sub(funcional, 1, 2) ,
  #   SubFuncOr = str_sub(funcional, 4, 6),
  #   cd_identificador_osc = str_pad(cd_identificador_osc,
  #                                  width = 14, side = "left", pad = 0)
  # ) %>%
  # select(ano, mes, id_osc, cd_identificador_osc, razao_social, FuncOr,
  #        SubFuncOr, empenhado, liquidado, pago) %>%

  rename(
    ano = nr_orcamento_ano,
    cd_identificador_osc = nr_orcamento_cnpj,
    pago = soma_pago_inpc_jan_2025,
    ) %>%

  mutate(
    cd_identificador_osc = str_pad(cd_identificador_osc,
                                   width = 14,
                                   side = "left",
                                   pad = 0)
  ) %>%

  select(ano, cd_identificador_osc, pago) %>%
  left_join(DadosOSCs, by = "cd_identificador_osc")

rm(DadosOSCs, raw_loa)
dbDisconnect(connec) # Desconecta do banco de dados.
rm(connec)
gc()
# ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Soma de Transferências por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Valores do orçamento geral da União e das transferências
# para ESFLs e percentual desta em relação à primeira

# names(loa_despesa)

TransAno_OSC <- loa_despesa %>%
  group_by(ano) %>%
  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  # Muda a escala para milhões:
  mutate(transPago_osc = transPago_osc / 1000000) %>%
  arrange(ano)

# Checa dados
# View(TransAno_OSC)
# names(TransAno_OSC)

# Salva Tabela
saveRDS(TransAno_OSC, "tables/TransAno_OSC.RDS")

# Tabela Inteira:
TransAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Nomes amigáveis das Colunas
  cols_label(
    ano = "Ano",
    transPago_osc = html("Transferências para OSCs<br>(em milhões)"),
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(transPago_osc),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !ano )
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Transferências por ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Valores reais das transferências anuais para ESFLs
# e percentual dos valores no orçamento geral da União

# names(TransAno_OSC)

# Gráfico
TransAno_OSC %>%
  ggplot(aes(x = ano, y = transPago_osc)) +
  geom_bar(stat="identity", color="black", position=position_dodge(),
           fill = "lightblue")+
  theme_classic() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ".",
                                decimal.mark = ",",
                                scientific = FALSE)
    ) +

  scale_x_continuous(breaks = unique(TransAno_OSC$ano)) +

  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust=1)
  )

rm(TransAno_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Transferências por Função Orçamentária por Ano (suspenso) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Valores reais anualmente transferidos para OSCs,
# por função orçamentária

# names(loa_despesa)
# names(FuncoesOrcamentarias)

# TransFuncOrAno_OSC <- loa_despesa %>%
#   # Não tem como colocar tantos anos na tabela
#   dplyr::filter(ano >= 2017) %>%
#   group_by(FuncOr, ano) %>%
#   summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
#   ungroup() %>%
#   left_join(distinct(FuncoesOrcamentarias, FuncaoCod, FuncaDesc),
#             by = c("FuncOr" = "FuncaoCod")) %>%
#   # select(-FuncOr) %>%
#   bind_rows(.,
#             summarise(.,
#                       FuncaDesc = "Total",
#                       FuncOr = "999",
#                       transPago_osc = sum(transPago_osc, na.rm = TRUE),
#                       .by = ano
#                       )
#             ) %>%
#   # Muda a escala para milhões:
#   mutate(transPago_osc = transPago_osc / 1000000) %>%
#   arrange(ano) %>%
#   spread(ano, transPago_osc) %>%
#   select(FuncOr, FuncaDesc, everything())
#
# # Checa dados
# # View(TransFuncOrAno_OSC)
# # names(TransFuncOrAno_OSC)
#
# # Salva Tabela
# saveRDS(TransFuncOrAno_OSC, "tables/TransFuncOrAno_OSC.RDS")
#
# # Tabela Inteira:
# TransFuncOrAno_OSC %>%
#   gt(locale = "pt-BR") %>%
#
#   # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
#   cols_hide(c(FuncOr)) %>%
#
#   # Coloca o valor missing de crescimento no primeiro ano em formato amigável
#   sub_missing(
#     missing_text = "-"
#   ) %>%
#
#   # Nomes amigáveis das Colunas
#   cols_label(
#     FuncaDesc = html("Função<br>Orçamentária")
#   ) %>%
#
#   # Deixa o Nome das Colunas em negrito e centralizado.
#   tab_style(
#     style = cell_text(align = "center", weight = "bold"),
#     locations = cells_column_labels()
#   ) %>%
#
#   # Formata os números da tabela
#   # Inteiros:
#   fmt_number(
#     columns = !FuncaDesc,
#     sep_mark = ".",
#     decimals = 0
#   ) %>%
#
#   # Centraliza os dados das tabelas
#   tab_style(
#     style = cell_text(align = "center"),
#     locations = cells_body(
#       columns = !FuncaDesc )
#   ) %>%
#
#   # Deixa o total em negrito
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_body(
#       rows = FuncaDesc == "Total")
#   )
#
# rm(TransFuncOrAno_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Transferências por Função Orçamentária (suspenso) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Valores reais anualmente transferidos para OSCs,
# por função orçamentária


# names(loa_despesa)
# names(FuncoesOrcamentarias)

# TransFuncOr_OSC <- loa_despesa %>%
#   # Não tem como colocar tantos anos na tabela
#   dplyr::filter(ano == 2024) %>%
#   group_by(FuncOr) %>%
#   summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
#   ungroup() %>%
#   # Muda a escala para milhões:
#   mutate(transPago_osc = transPago_osc / 1000000,
#          Per = (transPago_osc / sum(transPago_osc, na.rm = TRUE)) * 100
#          ) %>%
#   left_join(distinct(FuncoesOrcamentarias, FuncaoCod, FuncaDesc),
#             by = c("FuncOr" = "FuncaoCod")) %>%
#   arrange(desc(Per)) %>%
#   mutate(Ordem = row_number()) %>%
#   # Agrega todos as funções com menos de 1% no "outros"
#   bind_rows(.,
#             summarise(dplyr::filter(., Per <= 1),
#                       transPago_osc = sum(transPago_osc, na.rm = TRUE),
#                       Per = sum(Per, na.rm = TRUE),
#                       FuncaDesc = "Outros",
#                       Ordem = 1000
#                       )
#             ) %>%
#   dplyr::filter(Per > 1) %>%
#   arrange(Ordem) %>%
#   mutate(FuncaDesc = fct_reorder(FuncaDesc, Ordem))
#
# # Checa dados
# # View(TransFuncOr_OSC)
# # names(TransFuncOr_OSC)
#
# # Salva Tabela
# saveRDS(TransFuncOr_OSC, "tables/TransFuncOr_OSC.RDS")
#
# # Gráfico
# TransFuncOr_OSC %>%
#   ggplot(aes(x = FuncaDesc, y = transPago_osc)) +
#   geom_bar(stat="identity", color="black", position=position_dodge(),
#            fill = "blue")+
#   theme_classic() +
#   scale_y_continuous(
#     labels = function(x) format(x, big.mark = ".",
#                                 decimal.mark = ",",
#                                 scientific = FALSE)
#   ) +
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.x = element_text(angle = 90, hjust=1)
#   )
#
# rm(TransFuncOr_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Transferências por Região e UF por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total de recursos públicos federais transferidos
# anualmente para OSCs, por região e  Unidade da Federação (Em R$)

# names(loa_despesa)
# names(FuncoesOrcamentarias)

TransRegUFAno_OSC <- loa_despesa %>%
  dplyr::filter(
    ano >= 2017,
    !is.na(UF_Regiao)
  ) %>%

  select(ano, UF_Regiao, UF_Sigla, pago) %>%

  # Não tem como colocar tantos anos na tabela

  group_by(UF_Regiao, UF_Sigla, ano) %>%
  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  # Muda a escala para milhões:
  mutate(transPago_osc = transPago_osc / 1000000,
         Agregacao = "UF") %>%
  ungroup() %>%

  bind_rows(.,
            summarise(.,
                      transPago_osc = sum(transPago_osc, na.rm = TRUE),
                      Agregacao = "Regiao",
                      .by = c(UF_Regiao, ano)
                      )
            ) %>%

  # Dados do Brasil como um todo
  bind_rows(.,
            summarise(dplyr::filter(., Agregacao == "UF"),
                      transPago_osc = sum(transPago_osc, na.rm = TRUE),
                      UF_Sigla = "BR",
                      Agregacao = "Brasil",
                      .by = ano
            )
            ) %>%

  mutate(UF_Sigla = ifelse(is.na(UF_Sigla), UF_Regiao, UF_Sigla),
         UF_Sigla = ifelse(UF_Regiao == "SE" & Agregacao == "Regiao",
                           "SD", UF_Sigla)
         ) %>%
  left_join(OrdemUF, by = c("UF_Sigla")) %>%
  rename(NomeRegUF = UF_Nome) %>%
  select(Agregacao, UF_Ordem, NomeRegUF, ano, transPago_osc) %>%
  spread(ano, transPago_osc) %>%
  # Arruma a ordem das linhas
  arrange(UF_Ordem)


# Checa dados
# View(TransRegUFAno_OSC)
# names(TransRegUFAno_OSC)

# Salva Tabela
saveRDS(TransRegUFAno_OSC, "tables/TransRegUFAno_OSC.RDS")

# Tabela Inteira:
TransRegUFAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, UF_Ordem)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeRegUF = html("Região e UF")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = !NomeRegUF,
    sep_mark = ".",
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
  )

rm(TransRegUFAno_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Transferências por Região ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gráfico - Total de transferências voluntárias da União para OSCs,
# por região da OSC (Em %)

# names(loa_despesa)
# names(FuncoesOrcamentarias)

grafTransReg_OSC <- loa_despesa %>%
  dplyr::filter(
    # Não tem como colocar tantos anos na tabela
    ano >= 2017,
    !is.na(UF_Regiao)
  ) %>%

  group_by(UF_Regiao) %>%
  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  # Muda a escala para milhões:
  mutate(
    # Muda a escala para milhões:
    transPago_osc = transPago_osc / 1000000,
    Per = transPago_osc / sum(transPago_osc),
    UF_Regiao = ifelse(UF_Regiao == "SD", "SE", UF_Regiao),
    UF_Regiao = fct_reorder(UF_Regiao, Per)
    ) %>%
  ungroup()

# Checa dados
# View(grafTransReg_OSC)
# names(grafTransReg_OSC)

# Salva Tabela
saveRDS(grafTransReg_OSC, "tables/grafTransReg_OSC.RDS")

# Gráfico
grafTransReg_OSC %>%
  ggplot(aes(x = UF_Regiao, y = Per)) +
  geom_bar(stat="identity", color="black", position=position_dodge(),
           fill = "blue")+
  theme_classic() +
  scale_y_continuous(
    labels = scales::percent
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  )

rm(grafTransReg_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Transferências por Região por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Transferências voluntárias da União anuais para
# OSCs, por região das OSCs

# names(loa_despesa)
# names(FuncoesOrcamentarias)

grafTransRegAno_OSC <- loa_despesa %>%
  dplyr::filter(
    # Não tem como colocar tantos anos na tabela
    ano >= 2017,
    !is.na(UF_Regiao)
  ) %>%

  group_by(UF_Regiao, ano) %>%
  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  # Muda a escala para milhões:
  mutate(
    # Muda a escala para bilhões:
    transPago_osc = transPago_osc / 1000000000,
    Per = transPago_osc / sum(transPago_osc),
    UF_Regiao = ifelse(UF_Regiao == "SD", "SE", UF_Regiao),
    UF_Regiao = fct_reorder(UF_Regiao, Per)
  ) %>%
  ungroup()

# Checa dados
# View(grafTransRegAno_OSC)
# names(grafTransRegAno_OSC)

# Salva Tabela
saveRDS(grafTransRegAno_OSC, "tables/grafTransRegAno_OSC.RDS")

# Gráfico
grafTransRegAno_OSC %>%
  ggplot(aes(y = transPago_osc, x = ano, group = UF_Regiao, color = UF_Regiao)) +
  geom_line(size = 1.2) +
  theme_classic() +
  ylab("Bilhões") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line()
  )

rm(grafTransRegAno_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Transferências por Área e Subárea de atuação por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total de transferências voluntárias anuais para OSCs,
# por finalidade de atuação.

# names(loa_despesa)
# names(FuncoesOrcamentarias)

TransAreaSubAreaAno_OSC <- loa_despesa %>%

  # Não tem como colocar tantos anos na tabela
  dplyr::filter(
    ano >= 2017,
    !is.na(cd_subarea_atuacao),
    !is.na(ano)
    ) %>%

  # Calcula os Dados por SubArea:
  group_by(cd_subarea_atuacao, ano) %>%
  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  ungroup() %>%

  # Muda a escala para milhões:
  mutate(transPago_osc = transPago_osc / 1000000,
         Agregacao = "SubArea") %>%

  # Adiciona os rótulos das subáreas
  left_join(select(dc_area_subarea_atuacao,
                   cd_subarea_atuacao, cd_area_atuacao, tx_subarea_atuacao, OrdemArea),
            by = "cd_subarea_atuacao") %>%

  dplyr::filter(
    !is.na(tx_subarea_atuacao),
    !is.na(OrdemArea)
  ) %>%

  bind_rows(.,
            summarise(.,
                      transPago_osc = sum(transPago_osc, na.rm = TRUE),
                      Agregacao = "Area",
                      .by = c(cd_area_atuacao, ano)
            )
  ) %>%

  # Adiciona os rótulos das áreas
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%

  mutate(OrdemArea = ifelse(Agregacao == "Area",
                            paste0(str_pad(cd_area_atuacao, 2, pad = "0"), "_0"),
                            OrdemArea)) %>%

  # Dados do Brasil como um todo
  bind_rows(.,
            summarise(dplyr::filter(., Agregacao == "SubArea"),
                      transPago_osc = sum(transPago_osc, na.rm = TRUE),
                      Agregacao = "Brasil",
                      tx_area_atuacao = "Total",
                      OrdemArea = "0",
                      .by = ano
            )
  ) %>%

  mutate(NomeAreaSubArea = ifelse(Agregacao == "SubArea",
                                  tx_subarea_atuacao,
                                  tx_area_atuacao)
         ) %>%
  select(OrdemArea, Agregacao, NomeAreaSubArea, ano, transPago_osc) %>%

  spread(ano, transPago_osc) %>%

  # Arruma a ordem das linhas
  arrange(OrdemArea)



# Checa dados
# View(TransAreaSubAreaAno_OSC)
# names(TransAreaSubAreaAno_OSC)

# Salva Tabela
saveRDS(TransAreaSubAreaAno_OSC, "tables/TransAreaSubAreaAno_OSC.RDS")

# Tabela Inteira:
TransAreaSubAreaAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(Agregacao, OrdemArea)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NomeAreaSubArea = html("Finalidade de atuação")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = !NomeAreaSubArea,
    sep_mark = ".",
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
  )

rm(TransAreaSubAreaAno_OSC)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Transferências por Area de Atuação por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Transferências voluntárias da União anuais para
# OSCs, por região das OSCs

# names(loa_despesa)
# names(FuncoesOrcamentarias)

grafTransAreaAno_OSC <- loa_despesa %>%
  dplyr::filter(
    # Não tem como colocar tantos anos na tabela
    ano >= 2017,
    !is.na(cd_area_atuacao)
  ) %>%

  group_by(cd_area_atuacao, ano) %>%
  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  ungroup() %>%

  # Adiciona os rótulos das áreas
  left_join(distinct(dc_area_subarea_atuacao,
                     cd_area_atuacao, tx_area_atuacao),
            by = "cd_area_atuacao") %>%


  mutate(
    # Muda a escala para bilhões:
    transPago_osc = transPago_osc / 1000000000,
    Per = transPago_osc / sum(transPago_osc),
    tx_area_atuacao = fct_reorder(tx_area_atuacao, Per)
  ) %>%
  dplyr::filter(!is.na(tx_area_atuacao)  )


# Checa dados
# View(grafTransAreaAno_OSC)
# names(grafTransAreaAno_OSC)

# Salva Tabela
saveRDS(grafTransAreaAno_OSC, "tables/grafTransAreaAno_OSC.RDS")

# Gráfico
grafTransAreaAno_OSC %>%
  ggplot(aes(y = transPago_osc, x = ano, group = tx_area_atuacao,
             color = tx_area_atuacao)) +
  geom_line(size = 1.2) +
  theme_classic() +
  scale_x_continuous(breaks = unique(grafTransAreaAno_OSC$ano)) +
  ylab("Bilhões") +
  # scale_color_manual(labels = label_wrap(25)) +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(),
    legend.position = "bottom"
  )

rm(grafTransAreaAno_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Transferências por porte e por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Total de recursos públicos federais anualmente
# transferido para OSCs, por porte da  organização


# names(loa_despesa)
# names(FuncoesOrcamentarias)

TransPorteAno_OSC <- loa_despesa %>%

  dplyr::filter(
    !is.na(PorteOSC),
    ano >= 2017
    ) %>%

  group_by(PorteOSC, OrdemPorte, ano) %>%

  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%

  mutate(
    # Muda a escala para milhões:
    transPago_osc = transPago_osc / 1000000,
    PorteOSC = fct_reorder(PorteOSC, OrdemPorte)
  ) %>%
  ungroup() %>%
  spread(ano, transPago_osc)


# Dados do Brasil todo:
TransPorteAno_OSC <- TransPorteAno_OSC %>%
  summarise_at(
    .vars = 3:10,
    sum,
    na.rm = TRUE
  ) %>%
  mutate(
    OrdemPorte = 100,
    PorteOSC = "Total"
  ) %>%
  bind_rows(TransPorteAno_OSC) %>%
  arrange(OrdemPorte) %>%
  select(OrdemPorte, PorteOSC, everything())


# Checa dados
# View(TransPorteAno_OSC)
# names(TransPorteAno_OSC)

# Salva Tabela
saveRDS(TransPorteAno_OSC, "tables/TransPorteAno_OSC.RDS")

# Tabela Inteira:
TransPorteAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(OrdemPorte)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    PorteOSC = html("Pessoal Ocupado")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = !PorteOSC,
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !PorteOSC )
  ) %>%

  # Total em negrito
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = PorteOSC == "Total"
    )
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Transferências por porte e por Ano (porcentagem) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - OSCs que receberam recursos públicos federais,
# por porte (Em %)


# names(TransPorteAno_OSC)


TransPerPorteAno_OSC <- tibble(.rows = nrow(TransPorteAno_OSC)) %>%
  add_column(
    OrdemPorte = TransPorteAno_OSC$OrdemPorte,
    PorteOSC = TransPorteAno_OSC$PorteOSC
  )

for (i in seq_along(TransPorteAno_OSC)) {

  if(i >= 3) {
    # i <- 3

    # Nome da coluna a calcular a porcentagem
    nomeCol <- names(TransPorteAno_OSC)[i]

    # Imprime o nome da coluna
    print(nomeCol)

    # Total do ano
    TotalAno <- TransPorteAno_OSC[TransPorteAno_OSC$PorteOSC == "Total" , i] %>%
      as.numeric()

    # Cria a coluna da porcentagem
    TransPerPorteAno_OSC[[nomeCol]] <- (TransPorteAno_OSC[[nomeCol]] / TotalAno) * 100


    rm(TotalAno, nomeCol)
  }
}
rm(i)

# Checa dados
# View(TransPerPorteAno_OSC)
# names(TransPerPorteAno_OSC)

# Salva Tabela
saveRDS(TransPerPorteAno_OSC, "tables/TransPerPorteAno_OSC.RDS")

# Tabela Inteira:
TransPerPorteAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(OrdemPorte)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    PorteOSC = html("Pessoal Ocupado")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = !PorteOSC,
    sep_mark = ".",
    decimals = 1
  ) %>%

  # Inteiros (sem decimal no total)
  fmt_number(
    columns = !PorteOSC,
    rows = PorteOSC == "Total",
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !PorteOSC )
  ) %>%

  # Total em negrito
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = PorteOSC == "Total"
    )
  )

rm(TransPerPorteAno_OSC)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Transferências (percentuais) por Porte por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - GRÁFICO 10 Transferências voluntárias federais
# anuais para OSCs, por porte (Em %)

# names(TransPorteAno_OSC)

grafTransPorteAno_OSC <- TransPorteAno_OSC %>%
  dplyr::filter(PorteOSC != "Total") %>%
  mutate(PorteOSC = fct_reorder(PorteOSC, OrdemPorte)) %>%
  gather("ano", "Trans", dplyr::matches("^20") )


# Checa dados
# View(grafTransPorteAno_OSC)
# names(grafTransPorteAno_OSC)

# Salva Tabela
saveRDS(grafTransPorteAno_OSC, "tables/grafTransPorteAno_OSC.RDS")

# Gráfico
grafTransPorteAno_OSC %>%
  ggplot(aes(y = Trans, x = ano, group = PorteOSC, color = PorteOSC)) +
  geom_line(size = 1.2) +
  theme_classic() +
  # scale_x_continuous(breaks = unique(grafTransPorteAno_OSC$ano)) +
  ylab("Milhões") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(),
    legend.position = "bottom"
  )

rm(grafTransPorteAno_OSC, TransPorteAno_OSC)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Transferências por Natureza Jurídica e por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - TABELA 9 Total em relação ao total das transferências
# voluntárias de recursos para OSCs, por  natureza jurídica

# names(loa_despesa)

TransNatJurAno_OSC <- loa_despesa %>%

  dplyr::filter(
    !is.na(cd_natureza_juridica_osc),
    ano >= 2017
  ) %>%

  group_by(cd_natureza_juridica_osc, ano) %>%

  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    # Muda a escala para milhões:
    transPago_osc = transPago_osc / 1000000,
    transPago_osc = ifelse(transPago_osc == 0, NA, transPago_osc),
    ) %>%

  # Dados do Brasil como um todo
  bind_rows(.,
            summarise(.,
                      transPago_osc = sum(transPago_osc, na.rm = TRUE),

                      cd_natureza_juridica_osc = 9999,
                      .by = ano
            )
  ) %>%
  spread(ano, transPago_osc) %>%
  mutate(
    # Adiciona o nome da Natureza Jurídica
    NatJur = case_when(
      cd_natureza_juridica_osc == 3069 ~ "Fundação Privada",
      cd_natureza_juridica_osc == 3220 ~ "Organização Religiosa",
      cd_natureza_juridica_osc == 3301 ~ "Organização Social (OS)",
      cd_natureza_juridica_osc == 3999 ~ "Associação Privada",
      cd_natureza_juridica_osc == 9999 ~ "Total",
      TRUE ~ "Erro" ),

    NatJurOrdem = case_when(
      cd_natureza_juridica_osc == 3069 ~ 2,
      cd_natureza_juridica_osc == 3220 ~ 3,
      cd_natureza_juridica_osc == 3301 ~ 4,
      cd_natureza_juridica_osc == 3999 ~ 1,
      cd_natureza_juridica_osc == 9999 ~ 100,
      TRUE ~ -1 ),

    NatJur = fct_reorder(NatJur, NatJurOrdem)

  ) %>%
  ungroup() %>%
  select(-cd_natureza_juridica_osc) %>%
  arrange(NatJurOrdem) %>%
  select(NatJurOrdem, NatJur, everything())

# Checa dados
# View(TransNatJurAno_OSC)
# names(TransNatJurAno_OSC)

# Salva Tabela
saveRDS(TransNatJurAno_OSC, "tables/TransNatJurAno_OSC.RDS")

# Tabela Inteira:
TransNatJurAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(NatJurOrdem)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NatJur = html("Natureza jurídica")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = !NatJur,
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NatJur )
  ) %>%

  # Total em negrito
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = NatJur == "Total"
    )
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Transferências por Natureza Jurídica e por Ano (porcentagem) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Percentual em relação ao total das transferências
# voluntárias de recursos para OSCs,  por natureza jurídica

# names(TransNatJurAno_OSC)

TransPerNatJurAno_OSC <- tibble(.rows = nrow(TransNatJurAno_OSC)) %>%
  add_column(
    NatJurOrdem = TransNatJurAno_OSC$NatJurOrdem,
    NatJur = TransNatJurAno_OSC$NatJur
  )

for (i in seq_along(TransNatJurAno_OSC)) {

  if(i >= 3) {
    # i <- 3

    # Nome da coluna a calcular a porcentagem
    nomeCol <- names(TransNatJurAno_OSC)[i]

    # Imprime o nome da coluna
    print(nomeCol)

    # Total do ano
    TotalAno <- TransNatJurAno_OSC[TransNatJurAno_OSC$NatJur == "Total" , i] %>%
      as.numeric()

    # Cria a coluna da porcentagem
    TransPerNatJurAno_OSC[[nomeCol]] <- (TransNatJurAno_OSC[[nomeCol]] / TotalAno) * 100


    rm(TotalAno, nomeCol)
  }
}
rm(i)

# Checa dados
# View(TransPerNatJurAno_OSC)
# names(TransPerNatJurAno_OSC)

# Salva Tabela
saveRDS(TransPerNatJurAno_OSC, "tables/TransPerNatJurAno_OSC.RDS")

# Tabela Inteira:
TransPerNatJurAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(c(NatJurOrdem)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    NatJur = html("Natureza jurídica")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = !NatJur,
    sep_mark = ".",
    decimals = 1
  ) %>%

  # Inteiros:
  fmt_number(
    columns = !NatJur,
    rows = NatJur == "Total",
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !NatJur )
  ) %>%

  # Total em negrito
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = NatJur == "Total"
    )
  )


rm(TransPerNatJurAno_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Transferências (percentuais) por Natureza Jurídica por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - GRÁFICO 10 Transferências voluntárias federais
# anuais para OSCs, por porte (Em %)

# names(TransNatJurAno_OSC)

grafTransNatJurAno_OSC <- TransNatJurAno_OSC %>%
  dplyr::filter(NatJur != "Total",
                NatJurOrdem < 3) %>%
  mutate(NatJur = fct_reorder(NatJur, NatJurOrdem)) %>%
  gather("ano", "Trans", dplyr::matches("^20") )


# Checa dados
# View(grafTransNatJurAno_OSC)
# names(grafTransNatJurAno_OSC)

# Salva Tabela
saveRDS(grafTransNatJurAno_OSC, "tables/grafTransNatJurAno_OSC.RDS")

# Gráfico
grafTransNatJurAno_OSC %>%
  ggplot(aes(y = Trans, x = ano, group = NatJur, color = NatJur)) +
  geom_line(size = 1.2) +
  theme_classic() +
  # scale_x_continuous(breaks = unique(grafTransPorteAno_OSC$ano)) +
  ylab("Milhões") +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(),
    legend.position = "bottom"
  )

rm(grafTransNatJurAno_OSC, TransNatJurAno_OSC)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Número de OSC beneficiadas por ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - TABELA 11  Evolução do número de OSCs recipientes
# de transferências federais no orçamento  geral da União

# names(loa_despesa)

TransFreqAno_OSC <- loa_despesa %>%
  group_by(ano) %>%
  summarise(transPago_osc = n()) %>%
  arrange(ano)

# Checa dados
# View(TransFreqAno_OSC)
# names(TransFreqAno_OSC)

# Salva Tabela
saveRDS(TransFreqAno_OSC, "tables/TransFreqAno_OSC.RDS")

# Tabela Inteira:
TransFreqAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Nomes amigáveis das Colunas
  cols_label(
    ano = "Ano",
    transPago_osc = html("Número de OSCs que<br>receberam transferências federais"),
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(transPago_osc),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !ano )
  )

rm(TransFreqAno_OSC)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Valor Médio de Transferências por Ano ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Valores do orçamento geral da União e das transferências
# para ESFLs e percentual desta em relação à primeira

# names(loa_despesa)

TransMeanAno_OSC <- loa_despesa %>%
  group_by(ano, id_osc) %>%
  summarise(transPago_osc = sum(pago, na.rm = TRUE)) %>%
  dplyr::filter(transPago_osc > 0) %>%
  group_by(ano) %>%
  summarise(MeantransPago_osc = mean(transPago_osc, na.rm = TRUE)) %>%
  # Muda a escala para milhões:
  mutate(MeantransPago_osc = MeantransPago_osc / 1000000) %>%
  arrange(ano)

# Checa dados
# View(TransMeanAno_OSC)
# names(TransMeanAno_OSC)

# Salva Tabela
saveRDS(TransMeanAno_OSC, "tables/TransMeanAno_OSC.RDS")

# Tabela Inteira:
TransMeanAno_OSC %>%
  gt(locale = "pt-BR") %>%

  # Nomes amigáveis das Colunas
  cols_label(
    ano = "Ano",
    MeantransPago_osc = html("Transferências média para OSCs<br>(em milhões)"),
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  # Inteiros:
  fmt_number(
    columns = c(MeantransPago_osc),
    sep_mark = ".",
    decimals = 1
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !ano )
  )

rm(TransMeanAno_OSC)



# Limpa a memória e finaliza a rotina
rm(loa_despesa, dc_area_subarea_atuacao, OrdemUF, FuncoesOrcamentarias)
# ls()
gc()

# Fim ####
