# Projeto: livro de estatísticas "Perfil das OSC"

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2025-06-05

# Set Working Directory
# setwd(rstudioapi::getActiveProject())


# Main packages
library(tidyverse)
library(data.table)
library(stringr)
library(assertthat)
library(lubridate)
library(rsconnect)
library(bookdown)



# Options
options(stringsAsFactors = FALSE)
options(encoding = "utf-8")
options(dplyr.summarise.inform = FALSE)

# Chaves do banco de dados:
DadosHomologacao <- FALSE # Determina se os dados virão do banco de Homologação

if(DadosHomologacao){
  keys <- "keys/psql12-homolog_key.json" # Banco de homologação  
} else {
  keys <- "keys/psql12-usr_manutencao_mapa.json" # Banco de produção  
}
rm(DadosHomologacao)


cat("Projeto: Importar e livro de estatísticas 'Perfil das OSC'
    ")
cat(keys)


