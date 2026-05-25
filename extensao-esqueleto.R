# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

library(tidyverse)
dados_sinasc <- read.csv2("SINASC_2015.csv")


# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 <- dados_sinasc %>% select(c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61))
colnames(dados_sinasc)


# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF
dados_sinasc_2 <- dados_sinasc_1 %>% filter(substr(CODMUNRES, start = 0, stop = 2) == "22")


# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122

# Exportar o arquivo com o nome dados_sinasc_2.csv
write.csv(dados_sinasc_2, file = "dados_sinasc_2.csv")
dados_sinasc_2 <- read.csv("dados_sinasc_2.csv")

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, CONSPRENAT, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] <- NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] <- NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] <- NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] <- NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] <- NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] <- NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] <- NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] <- NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] <- NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] <- NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] <- NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] <- NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] <- NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] <- NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] <- NA
summary(dados_sinasc_2)

# Por curiosidade, verificando o tamanho dos banco de dados referente ao estado e aos municípios com e sem NAs
n_total_nasc_UF <- nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing <- sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN <- tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing <- tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5),
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$LOCNASC <- factor(dados_sinasc_2$LOCNASC, levels = c(1, 2, 3, 4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE, levels = c(1, 2, 3, 4, 5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO <- factor(dados_sinasc_2$GESTACAO, levels = c(1, 2, 3, 4, 5, 6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "32 a 36 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ, levels = c(1, 2, 3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO, levels = c(1, 2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO, levels = c(1, 2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR, levels = c(1, 2, 3, 4, 5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL, levels = c(1, 2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 <- factor(dados_sinasc_2$ESCMAE2010, levels = c(0, 1, 2, 3, 4, 5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE <- factor(dados_sinasc_2$RACACORMAE, levels = c(1, 2, 3, 4, 5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT <- factor(dados_sinasc_2$TPAPRESENT, levels = c(1, 2, 3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON <- factor(dados_sinasc_2$TPROBSON, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE <- factor(dados_sinasc_2$PARIDADE, levels = c(0, 1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK <- factor(dados_sinasc_2$KOTELCHUCK, levels = c(1, 2, 3, 4, 5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator


dados_sinasc_2$F_IDADE <- ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
       ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
              ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                     ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                            ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                   ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                          ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                 ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                        "50+"
                                                 )
                                          )
                                   )
                            )
                     )
              )
       )
)
dados_sinasc_2$F_IDADE <- factor(dados_sinasc_2$F_IDADE,
       levels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"), ordered = TRUE
)

dados_sinasc_2$F_PESO <- ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
       ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
              "Macrossomia"
       )
)
dados_sinasc_2$F_PESO <- factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso", "Peso normal", "Macrossomia"))

dados_sinasc_2$F_APGAR5 <- ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 <- factor(dados_sinasc_2$F_APGAR5, levels = c("Baixo", "Normal"))

dados_sinasc_2$PERIG <- ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
       ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim")
)
dados_sinasc_2$PERIG <- factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV <- ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
       ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA)
)
dados_sinasc_2$ESTCIV <- factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro", "Com companheiro"))


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

# criar nova variável referente ao deslocamento materno para realizar o parto, chamado de peregrinação
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES

tabela_pig <- read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep = ";")
tabela_pig$SEXO <- factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 <- merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = TRUE)
dados_sinasc_2$F_PIG <- ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
       ifelse(is.na(dados_sinasc_2$PESO) | is.na(dados_sinasc_2$PESO_P10) | is.na(dados_sinasc_2$PESO_P90),
              NA,
              ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                     ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG", "GIG")
              )
       )
)
dados_sinasc_2$F_PIG <- factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))

dados_sinasc_2 <- dados_sinasc_2 %>% mutate(PERIG = ifelse(CODMUNNASC == CODMUNRES, "Não", "Sim"))


# Tarefa 9. Obter as frequências das categorias das variáveis e medidas descritivas de variáveis e salvar os resultados em novas variáveis.
# Exemplo: freq_SEXO = table(dados_sinasc_2$SEXO)   media_peso = mean(dados_sinasc_2$PESO)
# Medidas descritivas a serem calculadas para variáveis QUANTITATIVAS: P25, P50, P75, média e desvio-padrão. Atenção: usar na.rm = TRUE, quando necessário.

# 1. Base inicial de municípios
municipios <- sort(unique(dados_sinasc_2$CODMUNRES))
base <- data.frame(CODMUNRES = municipios)

# Garante que todas as categorias apareçam, mesmo com contagem zero
gera_colunas <- function(var_nome, levels_vetor, nomes_finais) {
       tab <- table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2[[var_nome]], levels = levels_vetor))
       df <- as.data.frame.matrix(tab)
       names(df) <- nomes_finais
       df$CODMUNRES <- as.numeric(rownames(df))
       return(df)
}


# TN (4)
TN <- as.data.frame(table(factor(dados_sinasc_2$CODMUNRES, levels = municipios)))
names(TN) <- c("CODMUNRES", "TN")
base <- merge(base, TN, by = "CODMUNRES", all.x = TRUE)

# TNRC e TNRCR (5-6) - Ajustar critérios conforme sua base original
base$TNRC <- sapply(municipios, function(x) sum(complete.cases(dados_sinasc_2[dados_sinasc_2$CODMUNRES == x, ])))
base$TNRCR <- base$TNRC # Exemplo: se não houver filtro diferente para 22 variáveis

# Idade Materna (7-16)
base <- merge(base, gera_colunas(
       "F_IDADE",
       c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"),
       c("TGI_15", "TGI_15_19", "TGI_20_24", "TGI_25_29", "TGI_30_34", "TGI_35_39", "TGI_40_44", "TGI_45_49", "TGI_50")
), by = "CODMUNRES")
base$TGIF <- rowSums(base[, c("TGI_15_19", "TGI_20_24", "TGI_25_29", "TGI_30_34", "TGI_35_39", "TGI_40_44", "TGI_45_49")])

# Escolaridade (22-27)
base <- merge(base, gera_colunas(
       "ESCMAE2010",
       c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (9ª a 12ª série)", "Superior incompleto", "Superior completo"),
       c("EM_S", "EM_FI", "EM_FII", "EM_M", "EM_SI", "EM_SC")
), by = "CODMUNRES")

# Raça Mãe (28-32)
base <- merge(base, gera_colunas(
       "RACACORMAE", c("Branca", "Preta", "Amarela", "Parda", "Indígena"),
       c("TGRC_B", "TGRC_PT", "TGRC_A", "TGRC_PD", "TGRC_I")
), by = "CODMUNRES")

# Situação Conjugal (33-34)
base <- merge(base, gera_colunas(
       "ESTCIV", c("Sem companheiro", "Com companheiro"),
       c("TGSC", "TGCC")
), by = "CODMUNRES")

# Paridade e Gravidez (35-38)
base <- merge(base, gera_colunas("PARIDADE", c("Nulípara", "Multípara"), c("TGPRI", "TGNPRI")), by = "CODMUNRES")
base <- merge(base, gera_colunas("GRAVIDEZ", c("Única", "Dupla", "Tripla e mais"), c("TGU", "TGG1", "TGG2")), by = "CODMUNRES")
base$TGG <- base$TGG1 + base$TGG2

# Duração Gestação (39-47)
base <- merge(base, gera_colunas(
       "GESTACAO",
       c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"),
       c("TGD_22", "TGD_22_27", "TGD_28_31", "TGD_32_36", "TGD_37_41", "TGD_42")
), by = "CODMUNRES")
base$TGD_PRT <- base$TGD_22 + base$TGD_22_27 + base$TGD_28_31 + base$TGD_32_36
base$TGD_AT <- base$TGD_37_41
base$TGD_PST <- base$TGD_42

# Kotelchuck e Peregrinação (53-59)
base <- merge(base, gera_colunas(
       "KOTELCHUCK", c("Não realizou", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"),
       c("TKC_NR", "TKC_ID", "TKC_IT", "TKC_AD", "TKC_MAD")
), by = "CODMUNRES")
base <- merge(base, gera_colunas("PERIG", c("Sim", "Não"), c("TGPRG_S", "TGPRG_N")), by = "CODMUNRES")

# Parto e Apresentação (60-64)
base <- merge(base, gera_colunas("PARTO", c("Vaginal", "Cesário"), c("TPV", "TPC")), by = "CODMUNRES")
base <- merge(base, gera_colunas(
       "TPAPRESENT", c("Cefálico", "Pélvica ou podálica", "Transversa"),
       c("TRAP_C", "TRAP_P", "TRAP_T")
), by = "CODMUNRES")

# Robson (65-74) e Local (75-79)
base <- merge(base, gera_colunas("TPROBSON", paste0("Grupo ", 1:10), paste0("TGROB_", 1:10)), by = "CODMUNRES")
base <- merge(base, gera_colunas(
       "LOCNASC", c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena"),
       c("TNLOC_H", "TNLOC_ES", "TNLOC_D", "TNLOC_O", "TNLOC_AI")
), by = "CODMUNRES")

# Sexo e Raça RN (80-86)
base <- merge(base, gera_colunas("SEXO", c("Masculino", "Feminino"), c("TRS_M", "TRS_F")), by = "CODMUNRES")
base <- merge(base, gera_colunas(
       "RACACOR", c("Branca", "Preta", "Amarela", "Parda", "Indígena"),
       c("TRRC_B", "TRRC_PT", "TRRC_A", "TRRC_PD", "TRRC_I")
), by = "CODMUNRES")

# Peso e PIG (87-89, 95-97)
base <- merge(base, gera_colunas("F_PESO", c("Baixo peso", "Peso normal", "Macrossomia"), c("TRP_BP", "TRP_N", "TRP_M")), by = "CODMUNRES")
base <- merge(base, gera_colunas("F_PIG", c("PIG", "AIG", "GIG"), c("TRPIG_P", "TRPIG_A", "TRPIG_G")), by = "CODMUNRES")

# Apgar e Anomalia (98-99, 102-103)
base <- merge(base, gera_colunas("F_APGAR5", c("Baixo", "Normal"), c("TRAPG5_B", "TRAPG5_N")), by = "CODMUNRES")
base <- merge(base, gera_colunas("IDANOMAL", c("Sim", "Não"), c("TRAC", "TRSAC")), by = "CODMUNRES")

# --- 3. MEDIDAS DESCRITIVAS (ESTATÍSTICAS) ---
calc_estat <- function(var, prefixo) {
       df <- aggregate(dados_sinasc_2[[var]] ~ CODMUNRES, dados_sinasc_2, function(x) {
              c(P25 = quantile(x, 0.25, na.rm = T), P50 = quantile(x, 0.5, na.rm = T), P75 = quantile(x, 0.75, na.rm = T), MD = mean(x, na.rm = T), DP = sd(x, na.rm = T))
       })
       res <- data.frame(CODMUNRES = df[, 1], df[, 2])
       names(res) <- c("CODMUNRES", paste0(prefixo, c("_P25", "_P50", "_P75", "_MD", "_DP")))
       return(res)
}

base <- merge(base, calc_estat("IDADEMAE", "IM"), by = "CODMUNRES", all.x = T)
base <- merge(base, calc_estat("SEMAGESTAC", "DG"), by = "CODMUNRES", all.x = T)
base <- merge(base, calc_estat("PESO", "PESO"), by = "CODMUNRES", all.x = T)
base <- merge(base, calc_estat("APGAR5", "APG5"), by = "CODMUNRES", all.x = T)

# --- 4. CRIAÇÃO DA LINHA UF (AGREGADO TOTAL) ---
linha_uf <- data.frame(matrix(NA, nrow = 1, ncol = ncol(base)))
names(linha_uf) <- names(base)
linha_uf$CODMUNRES <- 22 # Piauí

# Soma das colunas de contagem
cols_soma <- setdiff(names(base), c("CODMUNRES", grep("_P|_MD|_DP", names(base), value = T)))
linha_uf[cols_soma] <- colSums(base[cols_soma], na.rm = T)

# Estatísticas para a UF
calc_uf_est <- function(var, prefixo) {
       x <- dados_sinasc_2[[var]]
       linha_uf[[paste0(prefixo, "_P25")]] <<- quantile(x, 0.25, na.rm = T)
       linha_uf[[paste0(prefixo, "_P50")]] <<- quantile(x, 0.5, na.rm = T)
       linha_uf[[paste0(prefixo, "_P75")]] <<- quantile(x, 0.75, na.rm = T)
       linha_uf[[paste0(prefixo, "_MD")]] <<- mean(x, na.rm = T)
       linha_uf[[paste0(prefixo, "_DP")]] <<- sd(x, na.rm = T)
}
calc_uf_est("IDADEMAE", "IM")
calc_uf_est("SEMAGESTAC", "DG")
calc_uf_est("PESO", "PESO")
calc_uf_est("APGAR5", "APG5")

# --- 5. UNIÃO E ORGANIZAÇÃO FINAL (103 COLUNAS) ---
final <- rbind(linha_uf, base)
final$ANO <- 2015
final$NIVEL <- c("UF", rep("MUNICIPIO", nrow(base)))


# Tarefa 10. Criar as colunas do novo banco de dados (de nome SINASC_UF.csv Exemplo: SINASC_RJ.csv) com base nas análises prévias, devendo as variáveis estar na ordem indicada abaixo
# ATENÇÃO aos nomes das variáveis e ordem das colunas
# 1. ANO: 2015  2. UFR (Estado de residência)   3. TN (total de nascimentos)   4. TNRC (total de nascimentos com registros completos, ou seja, sem NA em todas as variáveis do banco de dados)
# 5. TGI_15 (total de gestantes com idade inferior a 15 anos - F_IDADE)   6. TGI_15_19 (total de gestantes com idade >=15 e <=19 anos)
# 7: TGI_20_24 (total de gestantes com idade >=20 e <=24 anos)   8. TGI_25_29 (total de gestantes com idade >=25 e <=29 anos)
# 9: TGI_30_34 (total de gestantes com idade >=30 e <=34 anos)   10. TGI_35_39 (total de gestantes com idade >=35 e <=39 anos)
# 11: TGI_40_44 (total de gestantes com idade >=40 e <=44 anos)  12. TGI_45_49 (total de gestantes com idade >=45 e <=49 anos)
# 13: TGI_50 (total de gestantes com idade >=50)   14: TGIF (total de gestantes em idade fértil, idade >=15 e <=49 anos)
# 15: IM_P25 (percentil 25 da idade materna - IDADEMAE) 16: IM_P50 (percentil 50 da idade materna)   17: IM_P75 (percentil 75 da idade materna)
# 18. IM_MD (idade média materna)   19: IM_DP (desvio-padrão da idade materna)
# 20. EM_S (total de gestantes sem escolaridade, ESCMAE2010=0)   21: EM_FI (total de gestantes com escolaridade Fundamental I)
# 22. EM_FII (total de gestantes com escolaridade Fundamental II)   23. EM_M (total de gestantes com escolaridade Médio)
# 24. EM_SI (total de gestantes com escolaridade Superior Incompleto)   25. EM_SC (total de gestantes com escolaridade Superior Completo)
# 26. TGRC_B (total de gestantes da raça/cor branca - RACACORMAE)   27. TGRC_PT (total de gestantes da raça/cor preta)
# 28. TGRC_A (total de gestantes da raça/cor amarela)   29. TGRC_PD (total de gestantes da raça/cor parda)
# 30. TGRC_I (total de gestantes da raça/cor indígena)
# 31. TGPRI (total de gestantes primíparas - PARIDADE)     32. TGNPRI (total de gestantes não primíparas)
# 33. TGU (total de gestações única)   34. TGG (total de gestações gemelares)   35. TGD_22 (total de gestações com duração inferior a 22 semanas - GESTACAO)
# 36. TGD_22_27 (total de gestações com duração da gestação >=22 e <=27)   37. TGD_28_31 (total de gestações com duração da gestação >=28 e <=31)
# 38. TGD_32_36 (total de gestações com duração da gestação >=32 e <=36)   39. TGD_37_41 (total de gestações com duração da gestação >=37 e <=41)
# 40. TGD_42 (total de gestações com duração da gestação >=42)   41. TGD_PRT (total de gestações pre-termo, duração < 37 semanas)
# 42. TGD_AT (total de gestações a termo, duração >=37 e <=41)   43. TGD_PST  (total de gestações pós termo, duração >=42)
# 44. DG_P25 (percentil 25 da duração da gestação - SEMAGESTAC)  45. DG_P50 (percentil 50 da duração da gestação)
# 46. DG_P75 (percentil 75 da duração da gestação)   47. DG_MD (idade média da duração da gestação)   48. DG_DP (desvio-padrão da duração da gestação)
# 49. TKC_NR (total de consultas de pre-natal não realizado - KOTELCHUCK)   50. TKC_ID (total de consultas de pre-natal inadequado)
# 51. TKC_IT (total de consultas de pre-natal intermediário)   52. TKC_AD (total de consultas de pre-natal adequado)
# 53. TKC_MAD (total de consultas de pre-natal mais que adequado)   54. TGPRG_S (total de gestantes que peregrinaram)
# 55. TGPRG_N (total de gestantes que não peregrinaram)    56. TPV (total de partos vaginais)   57. TPC (total de partos cesáreos)
# 58. TRAP_C (total de recém-nascidos na posição cefálica)   59. TRAP_P (total de recém-nascidos na posição pélvica ou podálica)
# 60. TRAP_T (total de recém-nascidos na posição transversa)  61. TGROB_1 (total de gestantes do grupo de Robson 1 - TPROBSON)
# 62. TGROB_2 (total de gestantes do grupo de Robson 2)   63. TGROB_3 (total de gestantes do grupo de Robson 3)
# 64. TGROB_4 (total de gestantes do grupo de Robson 4)   65. TGROB_5 (total de gestantes do grupo de Robson 5)
# 66. TGROB_6 (total de gestantes do grupo de Robson 6)   67. TGROB_7 (total de gestantes do grupo de Robson 7)
# 68. TGROB_8 (total de gestantes do grupo de Robson 8)   69. TGROB_9 (total de gestantes do grupo de Robson 9)
# 70. TGROB_10 (total de gestantes do grupo de Robson 10)
# 71. TNLOC_H (total de nascimentos em hospital)   72. TNLOC_ES (total de nascimentos em outros estabelecimentos de saúde)
# 73. TNLOC_D (total de nascimentos em domicílio)  74. TNLOC_O (total de nascimentos em outros locais)
# 75. TNLOC_AI (total de nascimentos em aldeias indígenas)
# 76. TRRC_B (total de recém-nascidos da raça/cor branca - RACACOR)   77. TRRC_PT (total de recém-nascidos da raça/cor preta)
# 78. TRRC_A (total de recém-nascidos da raça/cor amarela)   79. TRRC_PD (total de recém-nascidos da raça/cor parda)
# 80. TRRC_I (total de recém-nascidos da raça/cor indígena)  81. TRP_BP (total de recém nascidos com baixo peso - FPESO)
# 82. TRP_N (total de recém nascidos com peso normal)   83. TRP_M (total de recém nascidos com macrossomia)
# 84. PESO_P25 (percentil 25 do peso dos recém-nascidos - PESO)  85. PESO_P50 (percentil 50 do peso dos recém-nascidos)
# 86. PESO_P75 (percentil 75 do peso dos recém-nascidos)  87. PESO_MD (peso médio dos recém-nascidos)
# 88. PESO_DP (desvio-padrão dos pesos dos recém-nascidos)    89. TRPIG_P (total de recém-nascidos de GESTAÇÕES ÚNICAS com PIG)
# 90. TRPIG_A (total de recém-nascidos de GESTAÇÕES ÚNICAS com AIG)   91. TRPIG_G (total de recém-nascidos de GESTAÇÕES ÚNICAS com GIG)
# 92: TRAPG5_B (total de recém-nascidos com Apgar5 baixo, ou seja, < 7)
# 93: TRAPG5_N (total de recém-nascidos com Apgar5 normal, ou seja, >= 7)   94. APG5_MD (Apgar5 médio dos recém-nascidos)
# 95. APG5_DP (desvio-padrão dos Apgar5 dos recém-nascidos) 96. TRAC (total de recém-nascidos com anomalia congênita - IDANOMAL)
# 97. TRSAC (total de recém-nascidos sem anomalia congênita)


# Ordenação exata conforme a lista de 1 a 103
ordem_103 <- c(
       "ANO", "NIVEL", "CODMUNRES", "TN", "TNRC", "TNRCR", # 1-6
       "TGI_15", "TGI_15_19", "TGI_20_24", "TGI_25_29", "TGI_30_34", "TGI_35_39", "TGI_40_44", "TGI_45_49", "TGI_50", "TGIF", # 7-16
       "IM_P25", "IM_P50", "IM_P75", "IM_MD", "IM_DP", # 17-21
       "EM_S", "EM_FI", "EM_FII", "EM_M", "EM_SI", "EM_SC", # 22-27
       "TGRC_B", "TGRC_PT", "TGRC_A", "TGRC_PD", "TGRC_I", # 28-32
       "TGSC", "TGCC", "TGPRI", "TGNPRI", "TGU", "TGG", # 33-38
       "TGD_22", "TGD_22_27", "TGD_28_31", "TGD_32_36", "TGD_37_41", "TGD_42", # 39-44
       "TGD_PRT", "TGD_AT", "TGD_PST", # 45-47
       "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP", # 48-52
       "TKC_NR", "TKC_ID", "TKC_IT", "TKC_AD", "TKC_MAD", # 53-57
       "TGPRG_S", "TGPRG_N", "TPV", "TPC", # 58-61
       "TRAP_C", "TRAP_P", "TRAP_T", # 62-64
       paste0("TGROB_", 1:10), # 65-74
       "TNLOC_H", "TNLOC_ES", "TNLOC_D", "TNLOC_O", "TNLOC_AI", # 75-79
       "TRS_M", "TRS_F", # 80-81
       "TRRC_B", "TRRC_PT", "TRRC_A", "TRRC_PD", "TRRC_I", # 82-86
       "TRP_BP", "TRP_N", "TRP_M", # 87-89
       "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP", # 90-94
       "TRPIG_P", "TRPIG_A", "TRPIG_G", # 95-97
       "TRAPG5_B", "TRAPG5_N", "APG5_MD", "APG5_DP", # 98-101
       "TRAC", "TRSAC" # 102-103
)

SINASC_PI <- final[, ordem_103]
SINASC_PI[, 7:103] <- round(SINASC_PI[, 7:103], 2)


# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv
write.csv(SINASC_FINAL, "SINASC_PI.csv", row.names = FALSE)


# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"


##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commite "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2
# Para realizar as tarefas da ETAPA 2, ABRIR ANTES uma branch de nome SIM no main de Extensao e ir para ela


# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim

dados_sim <- read.csv2("Mortalidade_Geral_2015.csv")


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas,
# nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO,
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
library(tidyverse)

dados_sim_1 <- dados_sim %>% select(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

dados_sim_2 <- dados_sim_1 %>% filter(substr(CODMUNRES, start = 0, stop = 2) == "22")


# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv

write.csv(dados_sim_2, file = "dados_sim_2.csv")


# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR,
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

dados_sim_2 <- read.csv("dados_sim_2.csv") %>% select(-1)

table(dados_sim_2$TIPOBITO)
table(dados_sim_2$SEXO)
table(dados_sim_2$RACACOR)
table(dados_sim_2$TPMORTEOCO)
table(dados_sim_2$OBITOGRAV)
table(dados_sim_2$OBITOPUERP)
table(dados_sim_2$CAUSABAS)
table(dados_sim_2$TPOBITOCOR)
table(dados_sim_2$MORTEPARTO)


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de
# "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA

dados_sim_2$SEXO[dados_sim_2$SEXO == 0] <- NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA
dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] <- NA
dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] <- NA
dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] <- NA
dados_sim_2$IDADE[dados_sim_2$IDADE == 999] <- NA
dados_sim_2$ESC2010[dados_sim_2$ESC2010 == 9] <- NA
dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] <- NA

summary(dados_sim_2)

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2),
# labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


summary(dados_sim_2)
dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR,
       levels = c(1, 2, 3, 4, 5),
       labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")
)
dados_sim_2$ESC2010 <- factor(dados_sim_2$ESC2010,
       levels = c(0, 1, 2, 3, 4, 5),
       labels = c(
              "Sem escolaridade", "Fundamental I (1ª a 4ª série)",
              "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º Grau)",
              "Superior incompleto", "Superior completo"
       )
)


dados_sim_2$TPMORTEOCO <- factor(dados_sim_2$TPMORTEOCO,
       levels = c(1, 2, 3, 4, 5, 8),
       labels = c(
              "Na gravidez", "No parto", "No abortamento",
              "Até 42 dias após o término do parto",
              "De 43 dias a 1 ano após o término da gestação ",
              "Não ocorreu nestes períodos"
       )
)
dados_sim_2$OBITOPUERP <- factor(dados_sim_2$OBITOPUERP,
       levels = c(1, 2, 3),
       labels = c(
              "Sim, até 42 dias após o parto", "Sim, de 43 dias a 1 ano",
              "Não"
       )
)

dados_sim_2$TPOBITOCOR <- factor(dados_sim_2$TPOBITOCOR,
       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
       labels = c(
              "Durante a gestação",
              "Durante o abortamento",
              "Após o abortamento",
              "No parto ou até 1 hora após o parto",
              "No puerpério - até 42 dias após o parto",
              "Entre 43 dias e até 1 ano após o parto",
              "A investigação não identificou o momento do óbito",
              "Mais de um ano após o parto",
              "O óbito não ocorreu nas circunstancias anteriores"
       )
)


dados_sim_2$MORTEPARTO <- factor(dados_sim_2$MORTEPARTO,
       levels = c(1, 2, 3),
       labels = c(
              "Antes", "Durante",
              "Após"
       )
)


dados_sim_2 <- dados_sim_2 %>% mutate(
       TIPOBITO = case_when(
              TIPOBITO == 2 ~ "Não fetal",
              TIPOBITO == 1 ~ "Fetal"
       ),
       SEXO = case_when(
              SEXO == 1 ~ "Masculino",
              SEXO == 2 ~ "Feminino"
       ),
       OBITOGRAV = case_when(
              OBITOGRAV == 1 ~ "Sim",
              SEXO == 2 ~ "Não"
       )
)


summary(dados_sim_2)


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo "Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf"
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

# -------------------------
# PREPARAÇÃO
# -------------------------

dados_sim_2$IDADE_STR <- sprintf("%03d", dados_sim_2$IDADE)
tipo_idade  <- substr(dados_sim_2$IDADE_STR, 1, 1)
valor_idade <- as.numeric(substr(dados_sim_2$IDADE_STR, 2, 3))
dados_sim_2$IDADE_DIAS <- ifelse(tipo_idade == "1", valor_idade / 1440,
       ifelse(tipo_idade == "2", valor_idade / 24,
              ifelse(tipo_idade == "3", valor_idade,
                     ifelse(tipo_idade == "4", valor_idade * 365, NA))))

dados_sim_2$CAUSA_INI <- substr(dados_sim_2$CAUSABAS, 1, 1)
idade_anos <- dados_sim_2$IDADE_DIAS / 365

# -------------------------
# BASE MUNICÍPIOS
# -------------------------

municipios <- sort(unique(dados_sim_2$CODMUNRES))
base <- data.frame(CODMUNRES = municipios)

# Função: soma TRUE por município (alinhada a dados_sim_2)
contagem <- function(cond, col_name) {
       res <- tapply(as.integer(cond), dados_sim_2$CODMUNRES, sum, na.rm = TRUE)
       df  <- data.frame(CODMUNRES = as.numeric(names(res)), V = as.integer(res))
       names(df)[2] <- col_name
       df
}

# =========================
# 1. TOTAIS
# =========================

TO_df <- as.data.frame(table(dados_sim_2$CODMUNRES))
names(TO_df) <- c("CODMUNRES", "TO")
TO_df$CODMUNRES <- as.numeric(as.character(TO_df$CODMUNRES))
base <- merge(base, TO_df, by = "CODMUNRES", all.x = TRUE)

torc_tab <- tapply(as.integer(complete.cases(dados_sim_2)), dados_sim_2$CODMUNRES, sum, na.rm = TRUE)
base <- merge(base, data.frame(CODMUNRES = as.numeric(names(torc_tab)), TORC = as.integer(torc_tab)), by = "CODMUNRES", all.x = TRUE)

vars_14 <- intersect(c("TIPOBITO","DTOBITO","DTNASC","IDADE","SEXO","RACACOR","ESC2010",
                       "CODMUNRES","TPMORTEOCO","OBITOGRAV","OBITOPUERP","CAUSABAS","TPOBITOCOR","MORTEPARTO"),
                     names(dados_sim_2))
torcr_tab <- tapply(as.integer(complete.cases(dados_sim_2[, vars_14])), dados_sim_2$CODMUNRES, sum, na.rm = TRUE)
base <- merge(base, data.frame(CODMUNRES = as.numeric(names(torcr_tab)), TORCR = as.integer(torcr_tab)), by = "CODMUNRES", all.x = TRUE)

# =========================
# 2. CAUSAS
# =========================

nn_cond <- dados_sim_2$CAUSA_INI %in% c("V", "W", "X", "Y")
base <- merge(base, contagem(nn_cond, "TO_NN"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!nn_cond & !is.na(dados_sim_2$CAUSA_INI), "TO_N"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(dados_sim_2$CAUSA_INI %in% c("A","B"), "TO_CB_I"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(dados_sim_2$CAUSA_INI %in% c("C","D"), "TO_CB_N"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$CAUSA_INI) & dados_sim_2$CAUSA_INI == "I", "TO_CB_C"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$CAUSA_INI) & dados_sim_2$CAUSA_INI == "J", "TO_CB_R"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!dados_sim_2$CAUSA_INI %in% c("A","B","C","D","I","J","V","W","X","Y") & !is.na(dados_sim_2$CAUSA_INI), "TO_CB_O"), by = "CODMUNRES", all.x = TRUE)

# =========================
# 3. SEXO
# =========================

base <- merge(base, contagem(!is.na(dados_sim_2$SEXO) & dados_sim_2$SEXO == "Masculino", "TO_M"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$SEXO) & dados_sim_2$SEXO == "Feminino",  "TO_F"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$SEXO) & dados_sim_2$SEXO == "Feminino" &
                             !is.na(idade_anos) & idade_anos >= 15 & idade_anos <= 49, "TO_F_IF"), by = "CODMUNRES", all.x = TRUE)

# =========================
# 4. FETAL E NEONATAL
# =========================

ft_cond  <- !is.na(dados_sim_2$TIPOBITO) & dados_sim_2$TIPOBITO == "Fetal"
nt_cond  <- !is.na(dados_sim_2$TIPOBITO) & dados_sim_2$TIPOBITO == "Não fetal" & !is.na(dados_sim_2$IDADE_DIAS) & dados_sim_2$IDADE_DIAS <= 27
ntp_cond <- !is.na(dados_sim_2$TIPOBITO) & dados_sim_2$TIPOBITO == "Não fetal" & !is.na(dados_sim_2$IDADE_DIAS) & dados_sim_2$IDADE_DIAS <= 6
ntt_cond <- !is.na(dados_sim_2$TIPOBITO) & dados_sim_2$TIPOBITO == "Não fetal" & !is.na(dados_sim_2$IDADE_DIAS) & dados_sim_2$IDADE_DIAS >= 7  & dados_sim_2$IDADE_DIAS <= 27
pnt_cond <- !is.na(dados_sim_2$TIPOBITO) & dados_sim_2$TIPOBITO == "Não fetal" & !is.na(dados_sim_2$IDADE_DIAS) & dados_sim_2$IDADE_DIAS >= 28 & dados_sim_2$IDADE_DIAS <= 364
mtg_cond <- !is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO == "Na gravidez"

base <- merge(base, contagem(ft_cond,  "TO_FT"),  by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(nt_cond,  "TO_NT"),  by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(ntp_cond, "TO_NT_P"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(ntt_cond, "TO_NT_T"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(pnt_cond, "TO_PNT"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(mtg_cond, "TO_MT_G"), by = "CODMUNRES", all.x = TRUE)

# Raça neonatal (apenas óbitos neonatais)
dados_nt <- dados_sim_2[nt_cond, ]
raca_nt <- function(label, col_name) {
       res <- tapply(as.integer(!is.na(dados_nt$RACACOR) & dados_nt$RACACOR == label),
                     dados_nt$CODMUNRES, sum, na.rm = TRUE)
       df <- data.frame(CODMUNRES = as.numeric(names(res)), V = as.integer(res))
       names(df)[2] <- col_name
       df
}
base <- merge(base, raca_nt("Branca",   "TONT_B"),  by = "CODMUNRES", all.x = TRUE)
base <- merge(base, raca_nt("Preta",    "TONT_PT"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, raca_nt("Amarela",  "TONT_A"),  by = "CODMUNRES", all.x = TRUE)
base <- merge(base, raca_nt("Parda",    "TONT_PD"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, raca_nt("Indígena", "TONT_I"),  by = "CODMUNRES", all.x = TRUE)

# =========================
# 5. MATERNO
# =========================

# TPMORTEOCO já é fator com labels após Tarefa 6
mt_todos <- c("Na gravidez", "No parto", "No abortamento",
              "Até 42 dias após o término do parto",
              "De 43 dias a 1 ano após o término da gestação ")
mt_prec  <- c("Na gravidez", "No parto", "No abortamento",
              "Até 42 dias após o término do parto")

mt_cond  <- !is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO %in% mt_todos
mtp_cond <- !is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO %in% mt_prec

base <- merge(base, contagem(mt_cond,  "TO_MT"),    by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO == "Na gravidez",                               "TO_MT_DG"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO == "No parto",                                   "TO_MT_PT"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO == "No abortamento",                             "TO_MT_AB"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO == "Até 42 dias após o término do parto",        "TO_MT_42"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(!is.na(dados_sim_2$TPMORTEOCO) & dados_sim_2$TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação ", "TO_MT_43"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(mtp_cond, "TO_MT_P"),  by = "CODMUNRES", all.x = TRUE)
base <- merge(base, contagem(mtp_cond & !is.na(idade_anos) & idade_anos >= 15 & idade_anos <= 49, "TO_MT_P_I"), by = "CODMUNRES", all.x = TRUE)

esc_mt <- function(label, col_name) {
       contagem(mtp_cond & !is.na(dados_sim_2$ESC2010) & as.character(dados_sim_2$ESC2010) == label, col_name)
}
base <- merge(base, esc_mt("Sem escolaridade",                "TO_MT_P_ES"),   by = "CODMUNRES", all.x = TRUE)
base <- merge(base, esc_mt("Fundamental I (1ª a 4ª série)",  "TO_MT_P_EFI"),  by = "CODMUNRES", all.x = TRUE)
base <- merge(base, esc_mt("Fundamental II (5ª a 8ª série)", "TO_MT_P_EFII"), by = "CODMUNRES", all.x = TRUE)
base <- merge(base, esc_mt("Médio (antigo 2º Grau)",         "TO_MT_P_EM"),   by = "CODMUNRES", all.x = TRUE)
base <- merge(base, esc_mt("Superior incompleto",            "TO_MT_P_ESI"),  by = "CODMUNRES", all.x = TRUE)
base <- merge(base, esc_mt("Superior completo",              "TO_MT_P_ESC"),  by = "CODMUNRES", all.x = TRUE)

# =========================
# 6. SUBSTITUIR NA por 0
# =========================

cols_count <- setdiff(names(base), "CODMUNRES")
base[cols_count][is.na(base[cols_count])] <- 0

# =========================
# 7. LINHA UF
# =========================

linha_uf <- as.data.frame(t(colSums(base[cols_count], na.rm = TRUE)))
linha_uf$CODMUNRES <- 22
SIM_PI <- rbind(linha_uf, base)

SIM_PI$ANO   <- 2015
SIM_PI$NIVEL <- c("UF", rep("MUNICIPIO", nrow(SIM_PI) - 1))

# =========================
# 8. ORDENAR AS 41 COLUNAS
# =========================

ordem_41 <- c(
       "ANO", "NIVEL", "CODMUNRES",
       "TO", "TORC", "TORCR",
       "TO_NN", "TO_N",
       "TO_CB_I", "TO_CB_N", "TO_CB_C", "TO_CB_R", "TO_CB_O",
       "TO_M", "TO_F", "TO_F_IF",
       "TO_FT", "TO_NT", "TO_NT_P", "TO_NT_T", "TO_PNT", "TO_MT_G",
       "TONT_B", "TONT_PT", "TONT_A", "TONT_PD", "TONT_I",
       "TO_MT", "TO_MT_DG", "TO_MT_PT", "TO_MT_AB", "TO_MT_42", "TO_MT_43",
       "TO_MT_P", "TO_MT_P_I",
       "TO_MT_P_ES", "TO_MT_P_EFI", "TO_MT_P_EFII", "TO_MT_P_EM", "TO_MT_P_ESI", "TO_MT_P_ESC"
)

SIM_PI <- SIM_PI[, ordem_41]

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv
write.csv(SIM_PI, "SIM_PI.csv", row.names = FALSE)

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main

#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Ao terminar a ETAPA 2 faça um merge de SIM para main
# Altere as orientações do script e commit (em main) "Script com orientações ETAPA 3 - SIDRA"
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos da Tarefa 1 abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv  
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

pop_est_2015 <- read.csv2("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv")
pop_censo_2010 <- read.csv2("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv")
pop_age_uf <- read.csv2("população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv")
pop_age_mun <- read.csv2("população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv")


# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

# Filtra municípios que começam com 22 e a linha da UF 22
pop_est_2015 <- pop_est_2015 %>% filter(substr(CODMUNRES, 1, 2) == "22")
pop_censo_2010 <- pop_censo_2010 %>% filter(substr(CODMUNRES, 1, 2) == "22")
pop_age_uf <- pop_age_uf %>% filter(CODMUNRES == 22)
pop_age_mun <- pop_age_mun %>% filter(substr(CODMUNRES, 1, 2) == "22")

# 4. Processamento das Faixas Etárias (Censo 2010)
# Agrupando as faixas etárias brutas em 0-14, 15-49 e 50+
faixa_15 <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")
faixa_15_49 <- c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", 
                 "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")

# Unindo dados de faixa etária de UF e Municípios
pop_age_all <- bind_rows(
  pop_age_uf %>% select(CODMUNRES, F_IDADE, POP, POPM, POPF),
  pop_age_mun %>% select(CODMUNRES, F_IDADE, POP, POPM, POPF)
)

pop_age_agg <- pop_age_all %>%
  mutate(
    cat = case_when(
      F_IDADE %in% faixa_15 ~ "15",
      F_IDADE %in% faixa_15_49 ~ "15_49",
      TRUE ~ "50"
    )
  ) %>%
  group_by(CODMUNRES, cat) %>%
  summarise(
    POPRC_age = sum(POP, na.rm = TRUE),
    POPRC_F_age = sum(POPF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Pivotagem corrigida: usamos {cat} que é o nome da nossa coluna de categorias
  pivot_wider(
    names_from = cat,
    values_from = c(POPRC_age, POPRC_F_age),
    names_glue = "{.value}_{cat}"
  ) %>%
  # Renomeando para os nomes exatos solicitados na tarefa
  rename(
    POPRC_15 = POPRC_age_15,
    POPRC_15_49 = POPRC_age_15_49,
    POPRC_50 = POPRC_age_50,
    POPRC_F_15 = POPRC_F_age_15,
    POPRC_F_15_49 = POPRC_F_age_15_49,
    POPRC_F_50 = POPRC_F_age_50
  )


# 5. Montagem do Banco de Dados Final (SIDRA_UF)
SIDRA_UF <- pop_est_2015 %>%
  select(CODMUNRES, POPRE_T) %>%
  left_join(pop_censo_2010 %>% select(CODMUNRES, POPRC_T, POPRC_M, POPRC_F), by = "CODMUNRES") %>%
  left_join(pop_age_agg, by = "CODMUNRES") %>%
  mutate(
    ANO = 2015,
    NIVEL = ifelse(nchar(as.character(CODMUNRES)) == 2, "UF", "MUNICIPIO")
  ) %>%
  select(
    ANO, NIVEL, CODMUNRES, 
    POPRE_T, POPRC_T, POPRC_M, POPRC_F, 
    POPRC_15, POPRC_15_49, POPRC_50, 
    POPRC_F_15, POPRC_F_15_49, POPRC_F_50
  )

# Exporte o arquivo em formato CSV
write.csv(SIDRA_UF, "SIDRA_PI.csv", row.names = FALSE)
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

#Tarefa 2: Acesso aos bancos de dados do SINISA e obtenção da informação
# Escreva os comandos da Tarefa 2 estando na branch OUTROS# Leia o arquivo agua e esgoto - município - 2015.csv 
library(tidyverse)
agua_esgoto = read.csv2("agua e esgoto - município - 2015.csv")

# A partir do arquivo acima gere o banco de dados de nome SINISA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPR_RA
# 5 POPR_RE

agua_esgoto = agua_esgoto %>% filter(substr(CODMUNRES, 1, 2) == "22")
agua_esgoto = agua_esgoto %>% rename(ANO =  Ano.de.Referência,
                                     POPRE_RA = POPR_RA,
                                     POPRE_RE= POPR_RE
)
agua_esgoto = agua_esgoto %>% mutate(NIVEL = "MUNICIPIO",
                                     POPRE_RA = as.numeric(POPRE_RA),
                                     POPRE_RE = as.numeric(POPRE_RE)) %>% 
  select(ANO,NIVEL,CODMUNRES,POPRE_RA,POPRE_RE)

linha_uf <- agua_esgoto %>%
  summarise(
    ANO = 2015,
    NIVEL = "UF",
    CODMUNRES = 22,
    POPRE_RA = sum(POPRE_RA, na.rm = TRUE),
    POPRE_RE = sum(POPRE_RE, na.rm = TRUE)
  )

SINISA_PI= bind_rows(linha_uf,agua_esgoto)

# Exporte o arquivo em formato CSV
write.csv(SINISA_PI , file = "SINISA_PI.csv")
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SINISA"

# Tarefa 3: Acesso aos bancos de dados do ATLAS  e obtenção da informação
# Escreva os comandos da Tarefa 3 estando na branch OUTROS
# Leia os arquivos:
# 1. códigos dos municípios - 2010.csv      
# 2. IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# 3. IDHM - 2010 - municípios - Atlas Brasil.csv

codmun = read_csv2("códigos dos municípios - 2010.csv") 
IDHM_tot_sex = read.csv2("IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv")
IDHM_mun = read.csv2("IDHM - 2010 - municípios - Atlas Brasil.csv")

# A partir do arquivo acima gere o banco de dados de nome ATLAS_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 IDHM_A
# 5 IDHM_CA
# 6 IDHM_CA_M
# 7 IDHM_CA_F

linha_uf <- IDHM_tot_sex %>%
  filter(UF == "Piauí") %>%
  mutate(
    ANO = 2015,
    NIVEL = "UF",
    CODMUNRES = "22", # Código do IBGE para o Piauí como caractere para bater com os municípios
    IDHM_A = IDHM_2015,
    IDHM_CA = IDHM_2010,
    IDHM_CA_M = IDHM_2010_M,
    IDHM_CA_F = IDHM_2010_F
  ) %>%
  select(ANO, NIVEL, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M, IDHM_CA_F)

IDHM_mun <- IDHM_mun %>% 
  filter(str_ends(município, " \\(PI\\)")) %>% 
  mutate(município = str_sub(município, start = 1, end = -6))

codmun <- codmun %>% 
  filter(str_starts(as.character(CODMUNRES), "22"))

ATLAS_MUN <- IDHM_mun%>%
  left_join(codmun, by = "município") %>%
  mutate(
    ANO = 2015,
    NIVEL = "MUNICIPIO",
    CODMUNRES = as.character(CODMUNRES),
    IDHM_A = NA_real_,      
    IDHM_CA = IDHM_2010,    
    IDHM_CA_M = NA_real_,   
    IDHM_CA_F = NA_real_    
  ) %>%
  select(ANO, NIVEL, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M, IDHM_CA_F)

ATLAS_PI <- bind_rows(linha_uf, ATLAS_MUN)

# Exporte o arquivo em formato CSV
write_csv(ATLAS_PI, "ATLAS_PI.csv")

# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"
#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora


############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes
