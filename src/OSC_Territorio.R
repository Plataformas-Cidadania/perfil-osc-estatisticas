# Instituto de Economia Aplicada - IPEA

# Projeto: livro 'Perfil das OSC 2025'

# Objetivo do Script: criar e testar tabelas e figuras do capítulo "OSC no
# Território"

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
library(ggpmisc) # informações estatísticas adicionais aos gráficos

# Mapas
library(geobr)

# Pacotes de leitura de dados externos
library(data.table)
library(readxl)

# Manipulação de datas
library(lubridate)

# Manipulação de Textos
library(stringr)
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

# População Municipal (censo 2022):
Pop2022 <- fread("data/Pop2022.csv", encoding = "UTF-8") %>%
  magrittr::set_names(c("cd_municipio", "Munic_Nome", "Pop2022"))

# IDH Municipal
IDHMunic <- fread("data/IDHMunic2010.csv", encoding = "UTF-8") %>%
  magrittr::set_names(c("Sigla", "cd_municipio", "Municipio", "IDH2010",
                        "V5")) %>%
  select(cd_municipio, IDH2010)

# Vou juntar dados dos municípios em uma única tabela:
MunicSocialData <- Municipios %>%
  left_join(select(Pop2022, cd_municipio, Pop2022),
            by = c("Munic_Id" = "cd_municipio") ) %>%
  rename(cd_municipio = Munic_Id) %>%
  left_join(IDHMunic, by = "cd_municipio")

rm(Municipios, Pop2022, IDHMunic)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas de uso comum no capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Vou criar aqui alguns tratamentos que vão se repetir em muitas tabelas:

# População por UF:
PopUF <- MunicSocialData %>%
  # Extrai o código UF do código municial (dois primeiros dígitos)
  mutate(UF_Id = str_sub(cd_municipio, 1, 2)) %>%
  # Agrupa por UF
  group_by(UF_Id) %>%
  # Soma população dos municípios
  summarise(PopUF = sum(Pop2022, na.rm = TRUE)) %>%
  # Soma casos de UF faltante (?)
  dplyr::filter(!is.na(UF_Id)) %>%
  # Remove o agrupamento para evitar alertas chatos abaixo.
  ungroup()

# Filtra as OSCs ativas em tb_localicazao
OscAtiva <- tb_localicazao %>%
  select(id_osc, cd_municipio) %>%
  # Adiciona a variável de OSC ativa
  left_join(
    select(tb_osc,
           id_osc, bo_osc_ativa),
    by = "id_osc"
  ) %>%
  # Filtra somente OSC ativas
  dplyr::filter(bo_osc_ativa == TRUE,
                # Vou retirar também casos de OSC com código 0 (exterior)
                cd_municipio != "0") %>%
  # Remove a variável de OSC ativa.
  select(-bo_osc_ativa)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tabelas do Capítulo ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Número de OSC nas Regiões e UFs Brasileiras ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Tabela - Número e percentual de OSCs, segundo as Grandes Regiões e as
# Unidades da Federação: Brasil

# Status: dados prontos


# OSCs por UF
OScPerUF <- OscAtiva %>%
  # Extrai código UF
  mutate(UF_Id = str_sub(cd_municipio, 1, 2)) %>%
  # Dados da UF (região)
  left_join(UFs, by = "UF_Id") %>%
  # Agrupo por UF e região
  group_by(UF_Regiao, UF_Sigla, UF_Nome, UF_Id) %>%
  # Calcula frequência das OSC por UF
  summarise(Freq = n()) %>%
  ungroup() %>%
  # Acrescenta dados da população das UFs
  left_join(PopUF, by = "UF_Id") %>%
  rename(Pop = PopUF) %>%
  # Percentual das UF na UF
  mutate(Agregacao = "UF" )

# Checa dados
# View(OScPerUF)
# names(OScPerUF)

# OSCs por região
OScPerReg <- OScPerUF %>%
  # Da tabela anterior, agrupa por região
  mutate(UF_Id = str_sub(UF_Id, 1, 1)) %>%
  group_by(UF_Regiao, UF_Id) %>%
  # Frequência da região
  summarise(Freq = sum(Freq, na.rm = TRUE),
            # População da região
            Pop = sum(Pop, na.rm = TRUE) ) %>%
  ungroup() %>%
  mutate(Agregacao = "Região",
         # Variáveis necessárias para unir os bancos
         UF_Sigla = NA_character_,
         # Adiciona o nome da região na variável 'UF_Nome'
         UF_Nome = case_when(
           UF_Regiao == "N" ~ "Região Norte",
           UF_Regiao == "S" ~ "Região Sul",
           UF_Regiao == "CO" ~ "Região Centro-Oeste",
           UF_Regiao == "SE" ~ "Região Sudeste",
           UF_Regiao == "NE" ~ "Região Nordeste",
           TRUE ~ "Erro" )
         ) %>%

  bind_rows(.,
            summarise(.,
                      UF_Id = "999",
                      UF_Sigla = "BR",
                      UF_Regiao = "Região",
                      Agregacao = "BR",
                      UF_Nome = "Total",
                      Freq = sum(Freq),
                      Pop = sum(Pop)
                      )
  )

# Checa dados
# View(OScPerReg)
# names(OScPerReg)

# Une dados das UFs e das Regiões:
JoinFreqOSC <- bind_rows(OScPerUF, OScPerReg) %>%
  arrange(UF_Id) %>%
  # Porcentagem de OSC da região
  mutate(Per = ( Freq / sum(Freq) ) * 100 ,
         # Porcentagem da população da região
         PerPop = ( Pop / sum(Pop) ) * 100 ,
         # Razão entre a porcentagem das OSC e a porcentagem da população
         PropOsc = (Per / PerPop) * 100 ,
         # OSC por mil habitantes
         OscP1000 = (Freq / Pop) * 1000)

# Checa dados
# View(JoinFreqOSC)
# names(JoinFreqOSC)

# Salva Tabela
saveRDS(JoinFreqOSC, "tables/tblJoinFreqOSC.RDS")

# Formata tabela:
JoinFreqOSC %>%
  select(UF_Nome, Freq, Per, Pop, PerPop, OscP1000, Agregacao) %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(Agregacao) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    UF_Nome = html("Grandes Regiões e Unidades da Federação"),
    Freq = "Total",
    Per = "(%)",
    Pop = "Total",
    PerPop = "(%)",
    OscP1000 = html("OSCs por mil habitantes")
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(Per, PerPop, OscP1000),
    decimals = 1
    ) %>%
  fmt_number(
    columns = c(Freq, Pop),
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
    label = "OSC",
    columns = c(Freq, Per)
  ) %>%
  tab_spanner(
    label = "População",
    columns = c(Pop, PerPop)
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
      columns = everything(),
      rows = Agregacao !=  "UF")
  ) %>%

  # Largura das colunas
  cols_width(c(Freq, Pop) ~ px(100),
             c(Per, PerPop, OscP1000) ~ px(85),
             UF_Nome ~ px(170))

rm(JoinFreqOSC, OScPerUF, OScPerReg)
# ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - OSCs por 1000 habs UF ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Número de OSCs por mil habitantes, por Grandes Regiões e Unidades
# da Federação: Brasil

# Status: dados prontos

OscP1000MeanBR <- sum( (JoinFreqOSC$PerPop * JoinFreqOSC$OscP1000) ) / 100

Graf_OSCpor1000UF <- JoinFreqOSC %>%
  select(UF_Nome, UF_Id, OscP1000, Agregacao) %>%
  dplyr::filter(UF_Nome != "Total") %>%
  arrange(UF_Id) %>%
  mutate(UF_Nome = fct_reorder(UF_Nome, UF_Id) ) %>%
  ggplot(aes(x = UF_Nome, y = OscP1000, fill = Agregacao)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("lightgreen", "steelblue")) +
  geom_hline(yintercept = OscP1000MeanBR, linetype="dashed",
             color = "red", linewidth= 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) ,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")

Graf_OSCpor1000UF

rm(OscP1000MeanBR, Graf_OSCpor1000UF)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Comparacao entre frequência de OSC nas capitais e no interior ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Número de OSCs, OSCs por mil habitantes e percentual de OSCs nas
# capitais do país

# Status: dados prontos

OscCap <- OscAtiva %>%
  # Filtra apenas as capitais
  dplyr::filter(cd_municipio %in% UFs$Cd_Capital) %>%
  # Agrupa por capital
  group_by(cd_municipio) %>%
  # Frequencia
  summarise(FreqCap = n() ) %>%
  ungroup() %>%
  # Adiciona dados da população da capital.
  left_join(select(MunicSocialData, cd_municipio, Pop2022),
            by = "cd_municipio") %>%
  # Extrai código da UF
  mutate(UF_Id = str_sub(cd_municipio, 1, 2) ) %>%
  # Dados da UF (região)
  left_join(UFs, by = "UF_Id") %>%
  # Renomeia variáveis
  rename(PopCap = Pop2022,
         CapNome = Munic_Nome,
         cd_capital = cd_municipio) %>%
  # Seleciona somente variáveis relevantes
  select(UF_Sigla, cd_capital, CapNome, FreqCap, PopCap)

# Checa dados
# View(OscCap)


# Junta dados das UFs e das capitais
OscPerCap <- OscCap %>%
  # Junta dados das UFs calculados acima
  left_join(OScPerUF, by = "UF_Sigla") %>%
  # Calcula as mesmas variáveis dos estados para as capitais
  mutate(PerCap = (FreqCap / Freq ) * 100,
         PerPopCap = (PopCap / Pop ) * 100 ,
         # OSC por mil habitantes
         OscP1000 = (Freq / Pop) * 1000,
         # OSC por mil habitantes (capitais)
         OscCapP1000 = (FreqCap / PopCap) * 1000 ,
         CapitalUF = paste0(CapNome, " (", UF_Sigla, ")") ) %>%
  select(CapitalUF, FreqCap, Freq, PerCap, PopCap, Pop, PerPopCap,
         OscCapP1000, OscP1000, CapNome, cd_capital) %>%
  arrange(desc(PerCap)) %>%
  bind_rows(.,
            summarise(.,
                      CapitalUF = "Totais",
                      FreqCap = sum(FreqCap, na.rm = TRUE),
                      Freq = sum(Freq, na.rm = TRUE),
                      PerCap = sum(PerCap, na.rm = TRUE),
                      PopCap = sum(PopCap, na.rm = TRUE),
                      Pop = sum(Pop, na.rm = TRUE)
                      )
            )

# Checa dados
# View(OscPerCap)

# Salva Tabela
saveRDS(OscPerCap, "tables/OscPerCap.RDS")

# Formata tabela:
OscPerCap %>%
  gt(locale = "pt-BR") %>%

  # Omite colunas
  cols_hide(c(CapNome, cd_capital)) %>%

  # Coloca o valor missing de crescimento no primeiro ano em formato amigável
  sub_missing(
    missing_text = "-"
  ) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    CapitalUF = "Capitais",
    FreqCap = "Capital",
    Freq = "UF",
    PerCap = "Capital (%)",
    PopCap = "Capital",
    Pop = "UF",
    PerPopCap = "Capital (%)",
    OscCapP1000 = "Capital",
    OscP1000 = "UF"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels()
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !CapitalUF )
  ) %>%

  # Destaca os totais
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows =  CapitalUF == "Totais")
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(PerCap, PerPopCap, OscCapP1000, OscP1000),
    decimals = 1
  ) %>%

  fmt_number(
    columns = c(FreqCap, Freq, Pop),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Cria grupos de variáveis (spanners)
  tab_spanner(
    label = "OSCs",
    columns = c(FreqCap, Freq, PerCap)
  ) %>%
  tab_spanner(
    label = "População",
    columns = c(PopCap, Pop, PerPopCap)
  ) %>%
  tab_spanner(
    label = "OSCs por mil habitantes",
    columns = c(OscCapP1000, OscP1000)
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graf - Comparacao entre frequência de OSC nas capitais e no interior ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GRÁFICO - Habitantes e OSCs nas capitais em relação ao total da
# população e OSCs da Unidade Federativa

# Status: dados prontos

GrafPerOscPop <- OscPerCap %>%
  dplyr::filter(CapitalUF != "Totais") %>%
  select(CapNome, PerCap, PerPopCap) %>%
  mutate(CapNome = fct_reorder(CapNome, PerCap, .desc = TRUE) ) %>%
  gather(Agregacao, Percentual, PerCap:PerPopCap) %>%
  mutate(Agregacao = ifelse(Agregacao == "PerCap", "OSCs na Capital",
                             "População da UF na capital") ) %>%
  ggplot(aes(x = CapNome, y = Percentual, fill = Agregacao)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_brewer(palette="Blues") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1) ,
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom" )

GrafPerOscPop

rm(GrafPerOscPop)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Tab - Frequencia de OSC e IDH nas capitais ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABELA - Número de OSCs, densidade de organizações por mil habitantes
# e IDHM nas capitais dos estados

# Status: dados prontos

# names(MunicSocialData)

OSCCapIDH <- OscPerCap %>%
  select(cd_capital, CapNome, FreqCap, OscCapP1000) %>%
  left_join(select(MunicSocialData, cd_municipio, IDH2010),
            by = c("cd_capital" = "cd_municipio")) %>%
  arrange(FreqCap)


# Checa dados
# View(OSCCapIDH)

# Salva Tabela
saveRDS(OSCCapIDH, "tables/OSCCapIDH.RDS")

# names(OSCCapIDH)

# Formata tabela:
OSCCapIDH %>%
  gt(locale = "pt-BR") %>%

  # Esta coluna não aparece nos dados, só serve para diferenciar as regiões
  cols_hide(cd_capital) %>%

  # Nomes amigáveis das Colunas
  cols_label(
    CapNome = "Nome da Capital",
    FreqCap = "OSCs",
    OscCapP1000 = "OSCs por mil habitantes",
    IDH2010 = "IDHM"
  ) %>%

  # Deixa o Nome das Colunas em negrito e centralizado.
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels()
  ) %>%

  # Formata os números da tabela
  fmt_number(
    columns = c(OscCapP1000, IDH2010),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(FreqCap),
    sep_mark = ".",
    decimals = 0
  ) %>%

  # Centraliza os dados das tabelas
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(
      columns = !CapNome )
  )

# Modelo simples que correlaciona IDH e presença de OSC
summary(lm(OscCapP1000 ~ IDH2010, data = OSCCapIDH))

# Correlação em gráfico
OSCCapIDH %>%
  ggplot(aes(x = IDH2010, y = OscCapP1000)) +
  geom_point() +
  ggpmisc::stat_poly_line() +
  ggpmisc::stat_poly_eq(use_label(c("eq", "R2", "p"))) +
  theme_classic() +
  xlab("IDH Municipal das Capitais (2010)") +
  ylab("OSC por 1000 Habitantes")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapa - IDHM e número de OSCs por mil habitantes, por municípios ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FIGURA- IDHM e número de OSCs por mil habitantes, por municípios
# Parte 1: IDHM

municipios_geo <- geobr::read_municipality(year=2022)

mapa_dados <- municipios_geo %>%
  left_join(select(MunicSocialData, cd_municipio, IDH2010),
            by = c("code_muni" = "cd_municipio")) %>%
  dplyr::filter( !is.na(IDH2010) )%>%

  mutate(
    # Agrupa o IDH em grupos
    IDHGrupo = case_when(
      IDH2010 < 0.55 ~ "Muito Baixo",
      IDH2010 < 0.62 ~ "Baixo",
      IDH2010 < 0.7 ~ "Médio",
      IDH2010 < 0.8 ~ "Alto",
      IDH2010 < 0.95 ~ "Muito Alto",
      TRUE ~ "Erro" ),

    # Ordena os grupos
    IDHOrdem = case_when(
      IDHGrupo == "Muito Baixo" ~ 1,
      IDHGrupo == "Baixo" ~ 2,
      IDHGrupo == "Médio" ~ 3,
      IDHGrupo == "Alto" ~ 4,
      IDHGrupo == "Muito Alto" ~ 5,
      IDHGrupo == "Erro" ~ 6 ),

    # Transforma IDHGrupo em fator ordenado
    IDHGrupo = fct_reorder(IDHGrupo, IDHOrdem, .desc = TRUE)
    )

# Distribuição de frequência dos grupos de IDH
table(mapa_dados$IDHGrupo)

mapa_dados_clean <- mapa_dados %>%
  select(name_muni, IDHGrupo, geom)


# Checa dados
# View(mapa_dados_clean)

# Salva Tabela
saveRDS(mapa_dados_clean, "tables/mapa_dados_clean.RDS")


HeatMapIDHMunic <- ggplot() +
  geom_sf(data = mapa_dados_clean, aes(fill = IDHGrupo), color=NA, size=.15) +
  scale_fill_manual(values = c(
    "blue",
    "lightgreen",
    "yellow",
    "orange",
    "pink"
  )
  ) +

  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.text  = element_text(size = 6),
        legend.position = "bottom" )

HeatMapIDHMunic

ggsave(plot = HeatMapIDHMunic, filename = "plots/HeatMapIDHMunic.png",
       width = 12,
       height = 12,
       units = "cm")

# Parte 2: Frequência OSC

OSCFreqMunic <- OscAtiva %>%
  group_by(cd_municipio) %>%
  summarise(FreqMunic = n()) %>%
  left_join(select(MunicSocialData, cd_municipio, Pop2022),
            by = "cd_municipio") %>%
  dplyr::filter( !is.na(FreqMunic),
                 !is.na(Pop2022) ) %>%

  mutate(

    OscP1000 = (FreqMunic / Pop2022) * 1000,

    # Agrupa o IDH em grupos
    FreqOSCGrupo = case_when(
      OscP1000 < 3 ~ "Muito Baixo",
      OscP1000 < 4.5 ~ "Baixo",
      OscP1000 < 5.5 ~ "Médio",
      OscP1000 < 7 ~ "Alto",
      OscP1000 < 40 ~ "Muito Alto",
      TRUE ~ "Erro" ),

    # Ordena os grupos
    FreqOSCOrdem = case_when(
      FreqOSCGrupo == "Muito Baixo" ~ 1,
      FreqOSCGrupo == "Baixo" ~ 2,
      FreqOSCGrupo == "Médio" ~ 3,
      FreqOSCGrupo == "Alto" ~ 4,
      FreqOSCGrupo == "Muito Alto" ~ 5,
      FreqOSCGrupo == "Erro" ~ 6 ),

    # Transforma FreqOSCGrupo em fator ordenado
    FreqOSCGrupo = fct_reorder(FreqOSCGrupo, FreqOSCOrdem, .desc = TRUE)

  )

# Distribuição de frequência dos grupos de IDH
table(OSCFreqMunic$FreqOSCGrupo)



mapa_dados2 <- municipios_geo %>%
  left_join(select(OSCFreqMunic, cd_municipio, FreqOSCGrupo),
            by = c("code_muni" = "cd_municipio")) %>%
  dplyr::filter( !is.na(FreqOSCGrupo) )




HeatMapFreqOSC <- ggplot() +
  geom_sf(data = mapa_dados2, aes(fill = FreqOSCGrupo), color=NA, size=.15) +
  scale_fill_manual(values = c(
    "blue",
    "lightgreen",
    "yellow",
    "orange",
    "pink"
  )
  ) +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        legend.text  = element_text(size = 6),
        legend.position = "bottom" )

HeatMapFreqOSC

ggsave(plot = HeatMapFreqOSC, filename = "plots/HeatMapFreqOSC.png",
       width = 12,
       height = 12,
       units = "cm")

# Fim ####
