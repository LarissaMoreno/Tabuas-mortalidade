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
#####################################################
dados2=dados1%>%filter(!is.na(IDADEdias) | !is.na(IDADEmeses) |!is.na(IDADEanos))

write.csv(dados2, "mortalidadeinfantil.csv", row.names = FALSE)
saveRDS(dados1,"mortalidade.rds")
##################################################
library(readr)
mortalidadeinfantil <- read_csv("mortalidadeinfantil.csv")
uf_map <- data.frame(
  CODMUNRES = c("11", "12", "13", "14", "15", "16", "17", "21", "22", "23", "24", "25", "26", "27", 
                "28", "29", "31", "32", "33", "35", "41", "42", "43", "50", "51", "52", "53"),
  UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", 
         "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
)
mortalidadeinfantil$CODMUNRES <- substr(mortalidadeinfantil$CODMUNRES, 1, 2)
mortalidadeinfantil<- mortalidadeinfantil%>%left_join(uf_map, by = "CODMUNRES")
mortalidadeinfantil=mortalidadeinfantil%>%select(-CODMUNRES)
mortalidadeinfantil=mortalidadeinfantil[,-c(10:17)]










##################################################3
dados1=readRDS("mortalidade.rds")
dados1=dados1[,-c(11:18,6:9,2)]
uf_map <- data.frame(
  CODMUNRES = c("11", "12", "13", "14", "15", "16", "17", "21", "22", "23", "24", "25", "26", "27", 
                "28", "29", "31", "32", "33", "35", "41", "42", "43", "50", "51", "52", "53"),
  UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", 
         "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
)
dados1$CODMUNRES <- substr(dados1$CODMUNRES, 1, 2)
dados1<- dados1%>%left_join(uf_map, by = "CODMUNRES")
dados1=dados1%>%select(-CODMUNRES)

#http://tabnet.datasus.gov.br/cgi/sih/mxcid10.htm

# Criar função para converter código CID-10 em capítulo
cid10_capitulo <- function(codigo) {
  codigo <- toupper(substr(codigo, 1, 3))  # padroniza e pega 3 primeiros caracteres
  
  dplyr::case_when(
    codigo >= "A00" & codigo <= "B99" ~ "I - Algumas doenças infecciosas e parasitárias",
    codigo >= "C00" & codigo <= "D48" ~ "II - Neoplasmas [tumores]",
    codigo >= "D50" & codigo <= "D89" ~ "III - Doenças do sangue e dos órgãos hematopoéticos e alguns transtornos imunitários",
    codigo >= "E00" & codigo <= "E90" ~ "IV - Doenças endócrinas, nutricionais e metabólicas",
    codigo >= "F00" & codigo <= "F99" ~ "V - Transtornos mentais e comportamentais",
    codigo >= "G00" & codigo <= "G99" ~ "VI - Doenças do sistema nervoso",
    codigo >= "H00" & codigo <= "H59" ~ "VII - Doenças do olho e anexos",
    codigo >= "H60" & codigo <= "H95" ~ "VIII - Doenças do ouvido e da apófise mastoide",
    codigo >= "I00" & codigo <= "I99" ~ "IX - Doenças do aparelho circulatório",
    codigo >= "J00" & codigo <= "J99" ~ "X - Doenças do aparelho respiratório",
    codigo >= "K00" & codigo <= "K93" ~ "XI - Doenças do aparelho digestivo",
    codigo >= "L00" & codigo <= "L99" ~ "XII - Doenças da pele e do tecido subcutâneo",
    codigo >= "M00" & codigo <= "M99" ~ "XIII - Doenças do sistema osteomuscular e do tecido conjuntivo",
    codigo >= "N00" & codigo <= "N99" ~ "XIV - Doenças do aparelho geniturinário",
    codigo >= "O00" & codigo <= "O99" ~ "XV - Gravidez, parto e puerpério",
    codigo >= "P00" & codigo <= "P96" ~ "XVI - Algumas afecções originadas no período perinatal",
    codigo >= "Q00" & codigo <= "Q99" ~ "XVII - Malformações congênitas, deformidades e anomalias cromossômicas",
    codigo >= "R00" & codigo <= "R99" ~ "XVIII - Sintomas, sinais e achados anormais de exames clínicos e de laboratório, não classificados em outra parte",
    codigo >= "S00" & codigo <= "T98" ~ "XIX - Lesões, envenenamentos e algumas outras consequências de causas externas",
    codigo >= "V01" & codigo <= "Y98" ~ "XX - Causas externas de morbidade e de mortalidade",
    codigo >= "Z00" & codigo <= "Z99" ~ "XXI - Fatores que influenciam o estado de saúde e o contato com os serviços de saúde",
    codigo >= "U00" & codigo <= "U99" ~ "** - CID não disponível ou inválido",
    TRUE ~ NA_character_
  )
}
rm(dados)
# Exemplo de uso no seu dataframe (chamado df)
teste <- dados1 %>%
  dplyr::mutate(CAPITULO_CID = cid10_capitulo(CAUSABAS))



breaks <- c(1, seq(5, 80, by = 5), Inf)   # 1,5,10,...,80,Inf
labels <- c(
  "1 a 4 anos",
  "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos",
  "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos",
  "50 a 54 anos", "55 a 59 anos", "60 a 64 anos", "65 a 69 anos", "70 a 74 anos",
  "75 a 79 anos", "80 anos e mais"
)
teste$IDADEanos=as.numeric(teste$IDADEanos)
teste <- teste %>%
  mutate(FAIXA_ETARIA = case_when(is.na(IDADEanos) ~ NA_character_,
                                  IDADEanos < 1 ~ "Menor 1 ano",# menores de 1 ano
                                  TRUE ~ as.character(
                                    cut(IDADEanos,breaks = breaks,labels = labels,
                                        right = FALSE  # inclui limite inferior, ex: 1<=idade<5 => "1 a 4"
                                    ))),
         FAIXA_ETARIA = factor(FAIXA_ETARIA,levels = c("Menor 1 ano", labels)))
teste=teste%>%group_by(ANO,UF,CAPITULO_CID,SEXO,FAIXA_ETARIA)%>%summarise(n=n())

teste <- teste %>%
  bind_rows(
    teste %>%
      group_by(ANO,CAPITULO_CID, SEXO,FAIXA_ETARIA) %>%
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
      mutate(UF = "Brasil")
  )

write.csv2(teste,"capituloscid.csv")
