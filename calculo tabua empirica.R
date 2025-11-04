setwd("C:/Users/User/Desktop/IBICT/LEO/Proposta Indicadores/demográficos/mortalidade")
dados1=readRDS("mortalidade.rds")
names(dados1)
dados1=dados1[,-c(1,4,11:13,15:18)]
names(dados1)
dados1$IDADEanos=as.numeric(dados1$IDADEanos)
dados1$IDADEmeses=as.numeric(dados1$IDADEmeses)
dados1$IDADEdias=as.numeric(dados1$IDADEdias)
dados1$IDADEhoras=as.numeric(dados1$IDADEhoras)
dados1$IDADEminutos=as.numeric(dados1$IDADEminutos)

library(dplyr)

# 1?????? Converter idades em anos decimais
dados1 <- dados1 %>%
  mutate(
    IDADE_decimal = case_when(
      !is.na(IDADEminutos) ~ IDADEminutos / (60 * 24 * 365),
      !is.na(IDADEhoras)   ~ IDADEhoras / (24 * 365),
      !is.na(IDADEdias)    ~ IDADEdias / 365,
      !is.na(IDADEmeses)   ~ IDADEmeses / 12,
      TRUE ~ as.numeric(IDADEanos)
    )
  )

# 2?????? Criar faixas etárias de 5 em 5 anos até 80+
dados1 <- dados1 %>%
  mutate(
    faixa = case_when(
      IDADE_decimal < 1 ~ "0",
      IDADE_decimal >= 1 & IDADE_decimal < 5 ~ "1",
      IDADE_decimal >= 5 & IDADE_decimal < 10 ~ "5",
      IDADE_decimal >= 10 & IDADE_decimal < 15 ~ "10",
      IDADE_decimal >= 15 & IDADE_decimal < 20 ~ "15",
      IDADE_decimal >= 20 & IDADE_decimal < 25 ~ "20",
      IDADE_decimal >= 25 & IDADE_decimal < 30 ~ "25",
      IDADE_decimal >= 30 & IDADE_decimal < 35 ~ "30",
      IDADE_decimal >= 35 & IDADE_decimal < 40 ~ "35",
      IDADE_decimal >= 40 & IDADE_decimal < 45 ~ "40",
      IDADE_decimal >= 45 & IDADE_decimal < 50 ~ "45",
      IDADE_decimal >= 50 & IDADE_decimal < 55 ~ "50",
      IDADE_decimal >= 55 & IDADE_decimal < 60 ~ "55",
      IDADE_decimal >= 60 & IDADE_decimal < 65 ~ "60",
      IDADE_decimal >= 65 & IDADE_decimal < 70 ~ "65",
      IDADE_decimal >= 70 & IDADE_decimal < 75 ~ "70",
      IDADE_decimal >= 75 & IDADE_decimal < 80 ~ "75",
      IDADE_decimal >= 80 ~ "80+",
      TRUE ~ NA_character_
    )
  )

# 3?????? Criar idade inicial e amplitude n
dados1 <- dados1 %>%
  mutate(
    idade_inicial = case_when(
      faixa == "0"   ~ 0,
      faixa == "1"   ~ 1,
      faixa == "5"   ~ 5,
      faixa == "10"  ~ 10,
      faixa == "15"  ~ 15,
      faixa == "20"  ~ 20,
      faixa == "25"  ~ 25,
      faixa == "30"  ~ 30,
      faixa == "35"  ~ 35,
      faixa == "40"  ~ 40,
      faixa == "45"  ~ 45,
      faixa == "50"  ~ 50,
      faixa == "55"  ~ 55,
      faixa == "60"  ~ 60,
      faixa == "65"  ~ 65,
      faixa == "70"  ~ 70,
      faixa == "75"  ~ 75,
      faixa == "80+" ~ 80
    ),
    n = case_when(
      faixa == "0" ~ 1,
      faixa == "1" ~ 4,
      faixa == "80+" ~ NA_real_,
      TRUE ~ 5
    )
  )

# 4?????? Calcular ax por sexo e UF usando reframe()
ax_por_sexo <- dados1 %>%
  group_by(ANO, SEXO, munResUf, faixa, idade_inicial, n) %>%
  reframe(
    n_obitos = n(),
    idade_media = mean(IDADE_decimal, na.rm = TRUE),
    ax = idade_media - idade_inicial
  )

# 5?????? Calcular também para ambos os sexos
ax_ambos <- dados1 %>%
  group_by(ANO, faixa, munResUf, idade_inicial, n) %>%
  reframe(
    n_obitos = n(),
    idade_media = mean(IDADE_decimal, na.rm = TRUE),
    ax = idade_media - idade_inicial
  ) %>%
  mutate(SEXO = "Ambos")

##Calcular para o Brasil
dados1$munResUf="Brasil"
# 4?????? Calcular ax por sexo e UF usando reframe()
ax_por_sexo1 <- dados1 %>%
  group_by(ANO, SEXO, munResUf, faixa, idade_inicial, n) %>%
  reframe(
    n_obitos = n(),
    idade_media = mean(IDADE_decimal, na.rm = TRUE),
    ax = idade_media - idade_inicial
  )

# 5?????? Calcular também para ambos os sexos
ax_ambos1 <- dados1 %>%
  group_by(ANO, faixa, munResUf, idade_inicial, n) %>%
  reframe(
    n_obitos = n(),
    idade_media = mean(IDADE_decimal, na.rm = TRUE),
    ax = idade_media - idade_inicial
  ) %>%
  mutate(SEXO = "Ambos")


# 6?????? Unir tudo
ax_empirico <- bind_rows(ax_por_sexo, ax_ambos,ax_por_sexo1, ax_ambos1)
ax_empirico <- ax_empirico %>%
  distinct(ANO, SEXO, munResUf, faixa, idade_inicial, n, .keep_all = TRUE)
ax_empirico=ax_empirico%>%filter(!is.na(faixa))

write.csv(ax_empirico, "ax_empirico.csv", row.names = FALSE)

#############################################
setwd("C:/Users/User/Desktop/IBICT/LEO/Proposta Indicadores/demográficos/mortalidade")
tabua1=read_csv("C:/Users/User/Desktop/IBICT/LEO/Proposta Indicadores/demográficos/tabuas de vida/tabua.csv")
tabua1$ordem=ifelse(tabua1$Faixa_Etaria=="Menor 1 ano",1,
                    ifelse(tabua1$Faixa_Etaria=="1 a 4 anos",2,
                    ifelse(tabua1$Faixa_Etaria=="5 a 9 anos",3,
                    ifelse(tabua1$Faixa_Etaria=="10 a 14 anos",4,
                    ifelse(tabua1$Faixa_Etaria=="15 a 19 anos",5,
                    ifelse(tabua1$Faixa_Etaria=="20 a 24 anos",6,
                    ifelse(tabua1$Faixa_Etaria=="25 a 29 anos",7,
                    ifelse(tabua1$Faixa_Etaria=="30 a 34 anos",8,
                    ifelse(tabua1$Faixa_Etaria=="35 a 39 anos",9,
                    ifelse(tabua1$Faixa_Etaria=="40 a 44 anos",10,
                    ifelse(tabua1$Faixa_Etaria=="45 a 49 anos",11,
                    ifelse(tabua1$Faixa_Etaria=="50 a 54 anos",12,      
                    ifelse(tabua1$Faixa_Etaria=="55 a 59 anos",13,
                    ifelse(tabua1$Faixa_Etaria=="60 a 64 anos",14,
                    ifelse(tabua1$Faixa_Etaria=="65 a 69 anos",15,
                    ifelse(tabua1$Faixa_Etaria=="70 a 74 anos",16,
                    ifelse(tabua1$Faixa_Etaria=="75 a 79 anos",17,
                    ifelse(tabua1$Faixa_Etaria=="80 anos e mais",18,0
           ))))))))))))))))))

tabua1=tabua1%>%arrange(ANO, UF, Sexo,ordem)%>%select(-ordem)
ax_empirico=read_csv("ax_empirico.csv")
tabua_ambos <- tabua1 %>%
  group_by(ANO, UF, Faixa_Etaria) %>%
  summarise(
    D_corrigido = sum(D_corrigido),
    POP_TOTAL = sum(POP_TOTAL),
    .groups = "drop"
  )%>%mutate(Sexo="Ambos")%>%
  select("Faixa_Etaria","ANO","UF","Sexo","D_corrigido","POP_TOTAL"  )
tabua1=rbind(tabua1,tabua_ambos)


tabua1 <- tabua1 %>%
  mutate(
    faixa = case_when(
      str_detect(Faixa_Etaria, "Menor|<1") ~ "0",
      str_detect(Faixa_Etaria, "1 a 4 anos") ~ "1",
      str_detect(Faixa_Etaria, "5 a 9 anos") ~ "5",
      str_detect(Faixa_Etaria, "10 a 14 anos") ~ "10",
      str_detect(Faixa_Etaria, "15 a 19 anos") ~ "15",
      str_detect(Faixa_Etaria, "20 a 24 anos") ~ "20",
      str_detect(Faixa_Etaria, "25 a 29 anos") ~ "25",
      str_detect(Faixa_Etaria, "30 a 34 anos") ~ "30",
      str_detect(Faixa_Etaria, "35 a 39 anos") ~ "35",
      str_detect(Faixa_Etaria, "40 a 44 anos") ~ "40",
      str_detect(Faixa_Etaria, "45 a 49 anos") ~ "45",
      str_detect(Faixa_Etaria, "50 a 54 anos") ~ "50",
      str_detect(Faixa_Etaria, "55 a 59 anos") ~ "55",
      str_detect(Faixa_Etaria, "60 a 64 anos") ~ "60",
      str_detect(Faixa_Etaria, "65 a 69 anos") ~ "65",
      str_detect(Faixa_Etaria, "70 a 74 anos") ~ "70",
      str_detect(Faixa_Etaria, "75 a 79 anos") ~ "75",
      str_detect(Faixa_Etaria, "80")  ~ "80+",
      TRUE ~ NA_character_
    )
  )


tabua1 <- tabua1 %>%
  mutate(
    munResUf = case_when(
      str_detect(UF, "AC") ~ "Acre",
      str_detect(UF, "AL") ~ "Alagoas",
      str_detect(UF, "AM") ~ "Amazonas",
      str_detect(UF, "AP") ~ "Amapá",
      str_detect(UF, "BA") ~ "Bahia",
      str_detect(UF, "Brasil") ~ "Brasil",
      str_detect(UF, "CE") ~ "Ceará",
      str_detect(UF, "DF") ~ "Distrito Federal",
      str_detect(UF, "ES") ~ "Espírito Santo",
      str_detect(UF, "GO") ~ "Goiás",
      str_detect(UF, "MA") ~ "Maranhão",
      str_detect(UF, "MG") ~ "Minas Gerais",
      str_detect(UF, "MS") ~ "Mato Grosso do Sul",
      str_detect(UF, "MT") ~ "Mato Grosso",
      str_detect(UF, "PA") ~ "Pará",
      str_detect(UF, "PB") ~ "Paraíba",
      str_detect(UF, "PE") ~ "Pernambuco",
      str_detect(UF, "PI")  ~ "Piauí",
      str_detect(UF, "PR")  ~ "Paraná",
      str_detect(UF, "RJ")  ~ "Rio de Janeiro",
      str_detect(UF, "RN")  ~ "Rio Grande do Norte",
      str_detect(UF, "RO")  ~ "Rondônia",
      str_detect(UF, "RR")  ~ "Roraima",
      str_detect(UF, "RS")  ~ "Rio Grande do Sul",
      str_detect(UF, "SC")  ~ "Santa Catarina",
      str_detect(UF, "SE")  ~ "Sergipe",
      str_detect(UF, "SP")  ~ "São Paulo",
      str_detect(UF, "TO")  ~ "Tocantins",
      TRUE ~ NA_character_
    )
  )

tabua_com_ax <- tabua1 %>%
  left_join(
    ax_empirico %>%
      select(ANO, munResUf, SEXO, faixa, ax) %>%
      rename(Sexo = SEXO),
    by = c("ANO", "munResUf", "Sexo", "faixa")
  )

##########################################################################################

tabua_com_ax$nMx=tabua_com_ax$D_corrigido/tabua_com_ax$POP_TOTAL
tabua_com_ax$n=ifelse(tabua_com_ax$faixa=="0",1,
                      ifelse(tabua_com_ax$faixa=="1",4,
                             ifelse(tabua_com_ax$faixa=="80+",NA,5)))

tabua_com_ax$nqx=(tabua_com_ax$n*tabua_com_ax$nMx)/
  (1+tabua_com_ax$n*(1-tabua_com_ax$ax)*tabua_com_ax$nMx)

library(dplyr)

# Definindo a raiz (Radix)
radix <- 100000 


tabua_com_ax <- tabua_com_ax %>%
  # 1. Agrupamento e Ordenação
  # Incluir ANO no agrupamento é crucial se houver dados de múltiplos anos.
  group_by(UF, ANO, Sexo) %>% 
  arrange(faixa, .by_group = TRUE) %>% 
  
  # 2. CÁLCULO DE lx e npx
  mutate(
    nqx_ajustado = ifelse(Faixa_Etaria == "80 anos e mais", 1.0, nqx),
    npx = 1 - nqx_ajustado, 
    lx_calculado = lag(cumprod(npx), default = 1) * radix
  ) %>%
  
  # 3. CÁLCULO DE dx e nLx
  mutate(
    dx_calculado = lx_calculado * nqx_ajustado,
    # Atenção: a coluna 'n' deve conter a amplitude de cada faixa (e.g., 1, 4, 5, 5, ...)
    nLx_calculado = ifelse(
      Faixa_Etaria == "80 anos e mais", 
      dx_calculado / nMx, # Usa a taxa de mortalidade na faixa aberta
      n * lead(lx_calculado, default = 0) + ax * dx_calculado
    )
  ) %>%
  
  # 4. CÁLCULO DE Tx e ex
  mutate(
    Tx_calculado = rev(cumsum(rev(nLx_calculado))), 
    ex_calculado = Tx_calculado / lx_calculado
  ) %>%
  ungroup()