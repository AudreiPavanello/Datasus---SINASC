# Loading Packages--------------------------------------------------------------

# The pacman package allows the loading and install of many libraries. 

pacman::p_load("microdatasus", # Package to load the SUS data
               "tidyverse",    # Data manipulation
               'tidytext',     # Natural language processing
               'rio',          # Import/export of data
               'janitor',      # Data cleaning
               'gtsummary',    # Descriptive statistics
               'here',         # Working directory sincronization
               'rstatix',      # Descriptive statistics
               'CI')           # Confidence interval 

## Loading the data of DATASUS -------------------------------------------------
## Unified Health System Data from Brasil
## SINASC System - Information System on Live Births

datapr <- fetch_datasus(year_start = 2013, 
                         year_end = 2020,
                         uf = 'PR', 
                         information_system = 'SINASC')

## Data processing -------------------------------------------------------------
datapr <- process_sinasc(datapr)


# Selecting Variable of interest------------------------------------------------
data2 <- select(datapr, IDADEMAE, ESCMAE, QTDFILVIVO, QTDFILMORT,
                 SEXO, RACACOR, GRAVIDEZ, PARTO, PESO, IDANOMAL,
                 GESTACAO, RACACORMAE, SEMAGESTAC, ESTCIVMAE, 
                 QTDPARTCES, QTDGESTANT, QTDPARTNOR
)


# Withdrawing NAs --------------------------------------------------------------
data2 <- drop_na(data2)

# Saving the data --------------------------------------------------------------
export(dados2, "data_sinasc_2013_2020_pr.csv")

