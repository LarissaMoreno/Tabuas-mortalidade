#POPULAÇÃO: ftp://ftp.datasus.gov.br/dissemin/publicos/IBGE/POPSVS/
#OBITOS: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10uf.def

library(foreign)
library(dplyr)
library(tidyr)
library(readr)
setwd("C:/Users/User/Desktop/IBICT/LEO/Proposta Indicadores/demográficos/tabuas de vida")

##########Estimativa população#################
estimativapop2024 <- read.dbf("POP24.dbf", as.is = F)
estimativapop2023 <- read.dbf("POP23.dbf", as.is = F)
estimativapop2022 <- read.dbf("POP22.dbf", as.is = F)
estimativapop2021 <- read.dbf("pop21.dbf", as.is = F)
estimativapop2020 <- read.dbf("pop20.dbf", as.is = F)
estimativapop2024$ANO=2024
estimativapop2023$ANO=2023
estimativapop2022$ANO=2022
estimativapop2021$ANO=2021
estimativapop2020$ANO=2020

uf_map <- data.frame(
  UF_COD = c("11", "12", "13", "14", "15", "16", "17", "21", "22", "23", "24", "25", "26", "27", 
             "28", "29", "31", "32", "33", "35", "41", "42", "43", "50", "51", "52", "53"),
  UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", 
         "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF")
)
popestimativa=rbind(estimativapop2020,estimativapop2021,estimativapop2022,
                    estimativapop2023,estimativapop2024)

rm(estimativapop2020);rm(estimativapop2021);rm(estimativapop2022);rm(estimativapop2023);
rm(estimativapop2024)

popestimativa$UF_COD <- substr(popestimativa$COD_MUN, 1, 2)
popestimativa<- popestimativa%>%left_join(uf_map, by = "UF_COD")
popestimativa$IDADE <-as.numeric(as.character(popestimativa$IDADE))
# criar faixas etárias

breaks <- c(1, seq(5, 80, by = 5), Inf)   # 1,5,10,...,80,Inf
labels <- c(
  "1 a 4 anos",
  "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos",
  "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos",
  "50 a 54 anos", "55 a 59 anos", "60 a 64 anos", "65 a 69 anos", "70 a 74 anos",
  "75 a 79 anos", "80 anos e mais"
)

popestimativa <- popestimativa %>%
  mutate(FAIXA_ETARIA = case_when(is.na(IDADE) ~ NA_character_,
                                  IDADE < 1 ~ "Menor 1 ano",# menores de 1 ano
                                  TRUE ~ as.character(
                                    cut(IDADE,breaks = breaks,labels = labels,
                                     right = FALSE  # inclui limite inferior, ex: 1<=idade<5 => "1 a 4"
                                     ))),
         FAIXA_ETARIA = factor(FAIXA_ETARIA,levels = c("Menor 1 ano", labels)))

popestimativa<- popestimativa%>%
  group_by(FAIXA_ETARIA,UF,SEXO,ANO) %>%
  summarise(POP_TOTAL = sum(POP, na.rm = TRUE)) %>%
  arrange(FAIXA_ETARIA)
popestimativa$SEXO=as.character(popestimativa$SEXO)
popestimativa$SEXO=ifelse(popestimativa$SEXO=="1","Masculino","Feminino")

popestimativa$FAIXA_ETARIA=as.character(popestimativa$FAIXA_ETARIA)
popestimativa <- popestimativa %>%
  bind_rows(
    popestimativa %>%
      group_by(FAIXA_ETARIA, SEXO, ANO) %>%
      summarise(POP_TOTAL = sum(POP_TOTAL, na.rm = TRUE), .groups = "drop") %>%
      mutate(UF = "Brasil")
  )


###############Mortalidade################
ler_e_processar <- function(arquivo, sexo, ano, col_specs) {
  if (!file.exists(arquivo)) {
    warning(paste("Arquivo não encontrado:", arquivo))
    return(NULL)
  }
  dados <- read_delim(arquivo, 
                      delim = ";", 
                      escape_double = FALSE, 
                      locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE, 
                      skip = 4,
                      col_types = col_specs, # Usa as especificações de tipo
                      show_col_types = FALSE) 

  dados$Sexo <- sexo
  dados$ANO <- ano 
  return(dados)
}

processar_obitos_ano_tidy_corrigido <- function(ano) {
  # 1. Configuração dos arquivos
  arquivo_fem <- paste0("obitos", ano, "fem.csv")
  arquivo_masc <- paste0("obitos", ano, "masc.csv")
  arquivo_ign <- paste0("obitos", ano, "ign.csv")
  
  # 2. Define a especificação dos tipos de coluna para leitura
  col_specs <- cols(
    .default = col_character(), 
    `Faixa Etária` = col_character() 
  )
  
  # 3. Leitura e processamento usando a função corrigida
  dados_fem  <- ler_e_processar(arquivo_fem, "Feminino", ano, col_specs)
  dados_masc <- ler_e_processar(arquivo_masc, "Masculino", ano, col_specs)
  # Une os dados
  lista_dados <- list(dados_fem, dados_masc, dados_ign)
  lista_dados <- lista_dados[!sapply(lista_dados, is.null)]
  
  if (length(lista_dados) == 0) {
    message(paste("Nenhum dado encontrado para o ano:", ano))
    return(NULL)
  }
  
  obitos_ano_wide <- bind_rows(lista_dados)
  # 4. Aplica as transformações (TIDY)
  obitos_tidy <- obitos_ano_wide %>%
    mutate(across(c(RO:Total), ~as.numeric(gsub(",", ".", .)))) %>% 
    
    # B. Reestrutura os dados
    pivot_longer(
      cols = -c(`Faixa Etária det`, Sexo, ANO), 
      names_to = "UF",
      values_to = "OBITOS"
    )%>%
    
    # C. Trata valores e renomeia
    mutate(
      OBITOS = ifelse(is.na(OBITOS), 0, OBITOS), 
      UF = ifelse(UF == "Total", "Brasil", UF),
      `Faixa Etária det`=ifelse(
        `Faixa Etária det` %in% c("0 a 6 dias","7 a 27 dias","28 a 364 dias","Menor 1 ano (ign)"),
        "Menor 1 ano",`Faixa Etária det`)
    ) %>%group_by(`Faixa Etária det`,ANO,UF,Sexo) %>%
    summarise(OBITOS = sum(OBITOS, na.rm = TRUE), .groups = "drop")%>%
    # D. Filtra
    filter(
      `Faixa Etária det` %in% c(  "1 a 4 anos","5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos",
                                  "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", 
                                  "45 a 49 anos","50 a 54 anos", "55 a 59 anos", "60 a 64 anos", "65 a 69 anos",
                                  "70 a 74 anos", "75 a 79 anos", "80 anos e mais","Menor 1 ano",
                                  "Idade ignorada") )
  names(obitos_tidy)=c("Faixa Etária","ANO","UF","Sexo","OBITOS")
  return(obitos_tidy)
}
anos_a_processar <- 2020:2024
lista_obitos_tidy <- lapply(anos_a_processar, processar_obitos_ano_tidy_corrigido)
obitos <- bind_rows(lista_obitos_tidy)

###Unindo
obitos <- obitos %>%
  rename(Faixa_Etaria = `Faixa Etária`, Sexo = `Sexo`, ANO = `ANO`, UF = `UF`,D_corrigido=OBITOS)

popestimativa <- popestimativa %>%
  rename(Faixa_Etaria = `FAIXA_ETARIA`, Sexo = `SEXO`, ANO = `ANO`, UF = `UF`)

tabua1 <- left_join(obitos, popestimativa,
                    by = c("UF", "ANO", "Sexo", "Faixa_Etaria"))
tabua1=tabua1%>%filter(Faixa_Etaria!="Idade ignorada")%>%
  filter(Sexo!="Ignorado")

write.csv(tabua1, "tabua.csv", row.names = FALSE)

