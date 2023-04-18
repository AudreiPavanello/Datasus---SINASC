# Carregando bibliotecas necessárias--------------------------------------------

pacman::p_load("microdatasus", # Carregar dados datasus
               "tidyverse",    # manipulação de dados
               "openxlsx",     # exportar dados excel
               'readxl',       # carregar dados excel
               'tidytext',     # processamento linguagem natural
               'rio',          # importar/exportar dados
               'janitor',      # limpeza de nomes
               'gtsummary',    # estatísticas descritivas
               'here',         # sincronizar working directory 
               'rstatix',      # estatísticas descritivas
               'CI')           # Cálculo do intervalo de confiança

## Carregando os dados do DATASUS-----------------------------------------------
## Dados do Sistema Nacional de Nascidos Vivos

dadospr <- fetch_datasus(year_start = 2013, 
                         year_end = 2020,
                         uf = 'PR', 
                         information_system = 'SINASC')

dadossp <- fetch_datasus(year_start = 2013, 
                       year_end = 2020,
                       uf = 'SP', 
                       information_system = 'SINASC')

dadosba <- fetch_datasus(year_start = 2013, 
                       year_end = 2020,
                       uf = 'BA', 
                       information_system = 'SINASC')

dadosmt <- fetch_datasus(year_start = 2013, 
                         year_end = 2020,
                         uf = 'MT', 
                         information_system = 'SINASC')


dadospa <- fetch_datasus(year_start = 2013, 
                         year_end = 2020,
                         uf = 'PA', 
                         information_system = 'SINASC')

## Processando os dados --> Essa função processa e corrige formatos dos dados---
dadospr <- process_sinasc(dadospr)
dadosba <- process_sinasc(dadosba)
dadospa <- process_sinasc(dadospa)
dadossp <- process_sinasc(dadossp)
dadosmt <- process_sinasc(dadosmt)

export(dados, "dados_sinasc_2013_2021_pr_full.xlsx")

# Selecionado váriaveis de interesse --------------------------------------
dados2 <- select(dadospr, IDADEMAE, ESCMAE, QTDFILVIVO, QTDFILMORT,
                 SEXO, RACACOR, GRAVIDEZ, PARTO, PESO, IDANOMAL,
                 GESTACAO, RACACORMAE, SEMAGESTAC, ESTCIVMAE, 
                 QTDPARTCES, QTDGESTANT, QTDPARTNOR
                 )


# Retirando NAs -----------------------------------------------------------
dados2 <- drop_na(dados2)

# Criando planilha excel para armazenar dados -----------------------------
export(dados2, "dados_sinasc_2013_2020_rs_alterado.csv")



