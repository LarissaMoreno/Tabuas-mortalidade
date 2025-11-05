#install.packages("read.dbc", repos = "https://packagemanager.posit.co/cran/2024-07-05")
#remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(dplyr)
library(tidyr)
#dicionario: https://svs.aids.gov.br/daent/cgiae/sistemas-informacao/sim/documentacao/dicionario-de-dados-SIM-tabela-DO.pdf
setwd("C:/Users/User/Desktop/IBICT/LEO/Proposta Indicadores/demográficos/mortalidade")


vars=c("CODMUNRES","IDADE","SEXO","CAUSABAS")
sim2020<- fetch_datasus(year_start = 2020, year_end=2020, uf = "all", information_system="SIM-DO",vars=vars)
sim2020$ANO=2020

sim2021<- fetch_datasus(year_start = 2021, year_end=2021, uf = "all", information_system="SIM-DO",vars=vars)
sim2021$ANO=2021

sim2022<- fetch_datasus(year_start = 2022, year_end=2022, uf = "all", information_system="SIM-DO",vars=vars)
sim2022$ANO=2022

sim2023<- fetch_datasus(year_start = 2023, year_end=2023, uf = "all", information_system="SIM-DO",vars=vars)
sim2023$ANO=2023

sim2024<- fetch_datasus(year_start = 2024, year_end=2024, uf = "all", information_system="SIM-DO",vars=vars)
sim2024$ANO=2024

dados=rbind(sim2020,sim2021,sim2022,sim2023,sim2024)
rm(sim2020);rm(sim2021);rm(sim2022);rm(sim2023);rm(sim2024);
dados1=process_sim(dados)

saveRDS(dados1,"mortalidade.rds")


