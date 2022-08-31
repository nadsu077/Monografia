# Funções e ligação com SQL ----
#rm(list = ls())
allDup <- function(x){
  duplicated(x) | duplicated(x, fromLast = TRUE)
}
pkg = c("dplyr", "odbc", "dbplyr", "tidyr", "lubridate", "GGally", "beepr")
install.packages(pkg[!pkg %in% rownames(installed.packages())], quiet = T)
sapply(pkg, require, character.only = T)

options(scipen = 999)

con = odbc::dbConnect(odbc::odbc(),
                      driver = "/usr/lib/x86_64-linux-gnu/odbc/libmyodbc8a.so", 
                      server = "localhost",
                      uid = "tanjiro", 
                      pwd = "charger69R$", 
                      port = 3306, 
                      database = "ANS_DB", 
                      timeout = 86400)

# Montar uma tabela com as informações dos planos de saúde como a abrangencia, quantidade de municípios,
# quantidade de beneficiários por faixa etária, porte da operadora, modalideade da ops, 
# cobertura assistencial, quantidade de beneficiários da operadora por faixa etária, 
# valor de comercialização, percentuais de carregamento (admin, comercial e lucro), 
# Fator moderador, tipo de contratacao

# Características dos produtos ----

carac_prod = tbl(con, "CARAC_PROD_SSP") %>%
  filter(
    # Trabalhar somente com os planos vigentes posteriores a lei 9656/98 por conta da obrigatoriedade do rol de procedimentos
    VIGENCIA_PLANO == "P", # Vigencia do Plano: A=Anterior 9656/98; P=Posterior 9656/98
    # Trabalhar com planos de no máximo abrangência estadual (inicial)
    ABRANGENCIA_COBERTURA %in% c(1:3), # 1-munic; 2-gr munic; 3-estad; 4-gr estad; 5-nac; 6-outras
    # Trabalhar com planos ativos (pois tem beneficiários e estão sendo comercializados)
    SITUACAO_PLANO == "A",
    # Trabalhar somente com planos que não sejam exclusivamente odontológicos
    SGMT_ASSISTENCIAL != 11, # Diferente de 11 = Odontológico
    # Trabalhar somente com planos de pre pagamento
    TIPO_FINANCIAMENTO == 2, # 1-pos; 2-pre; 3-misto; 0-nao identificado
    # Trabalhar somente com operadoras que não sejam exclusivamente odontologicas
    GR_MODALIDADE %in% c (1,2,4,5,7), # 1: Autogestão; 2: Coop. Médica; 3: Coop. Odontológica; 4: Filantropia; 5: Medicina de Grupo; 6: Odontologia de Grupo; 7: Seguradora; 8: Adm
    # Trabalhar somente com planos sem livre escolha
    LIVRE_ESCOLHA == 1, # 1-Ausente; 2-Total; 3-Parc c/ int; 4-Parc s/ int
    # Trabalhar com acomodação diferente de Não Identificado
    ACOMODACAO_HOSPITALAR != 0 # 1-Col; 2-Ind; 3-Nao se Aplica; 0-Nao Ident
    
    ) %>% 
  left_join(x = .,
            y = tbl(con, "TBL_IDPLAN_NMPLAN") %>% select(ID_PLANO, CD_PLANO),
            by = "ID_PLANO") %>% 
  select(ID_PLANO, CD_PLANO, SITUACAO_PLANO, CD_OPERADORA, GR_MODALIDADE, GR_CONTRATACAO, SGMT_ASSISTENCIAL, ABRANGENCIA_COBERTURA, 
         FATOR_MODERADOR, ACOMODACAO_HOSPITALAR, DT_SITUACAO, DT_REGISTRO_PLANO, DT_ATUALIZACAO)

# Quantidade de municípios por plano ----

carac_prod = carac_prod %>% 
  left_join(x = .,
            y = tbl(con, "NTRP_ABRAN_COM") %>% 
              mutate(CD_UF = str_sub(CD_MUNICIPIO, 1, 2)) %>% 
              select(CD_PLANO, CD_MUNICIPIO, CD_UF) %>% 
              filter(CD_UF == 23) %>% # Codigo 23 é Ceará
              group_by(CD_PLANO, CD_UF) %>% 
              summarise(QTD_MUNICIPIO = count(CD_MUNICIPIO)),
            by = c("CD_PLANO")) %>% 
  data.frame() 

# Mudando multiplas colunas usando a base do R

carac_prod[,1:4] = lapply(carac_prod[,1:4], as.character)
carac_prod[,c(5:10,14)] = lapply(carac_prod[,c(5:10,14)], as.factor)
carac_prod[,11:13] = lapply(carac_prod[,11:13], as.Date)
carac_prod[,15] = as.numeric(carac_prod[,15])

carac_prod = carac_prod[!is.na(carac_prod$CD_UF),]

# Quantidade de beneficiários ----

# Quantidade de beneficiários em cada plano por estado, por ops para o porte e faixa etária

benef_planos = tbl(con, "BENEF_2021") %>%
  filter(SG_UF != "XX") %>% 
  mutate(CD_UF = case_when(
    SG_UF == "RO" ~ 11, SG_UF == "AC" ~ 12, SG_UF == "AM" ~ 13,
    SG_UF == "RR" ~ 14, SG_UF == "PA" ~ 15, SG_UF == "AP" ~ 16,
    SG_UF == "TO" ~ 17, SG_UF == "MA" ~ 21, SG_UF == "PI" ~ 22,
    SG_UF == "CE" ~ 23, SG_UF == "RN" ~ 24, SG_UF == "PB" ~ 25,
    SG_UF == "PE" ~ 26, SG_UF == "AL" ~ 27, SG_UF == "SE" ~ 28,
    SG_UF == "BA" ~ 29, SG_UF == "MG" ~ 31, SG_UF == "ES" ~ 32,
    SG_UF == "RJ" ~ 33, SG_UF == "SP" ~ 35, SG_UF == "PR" ~ 41,
    SG_UF == "SC" ~ 42, SG_UF == "RS" ~ 43, SG_UF == "MS" ~ 50,
    SG_UF == "MT" ~ 51, SG_UF == "GO" ~ 52, SG_UF == "DF" ~ 53
    )) %>%
  filter(CD_UF == 23) %>% # Filtrando UF = 23 (Ceará)
  group_by(CD_UF, DE_FAIXA_ETARIA_REAJ, CD_PLANO, CD_OPERADORA) %>% 
  summarise(QT_BENEF_PLAN = sum(QT_BENEFICIARIO_ATIVO, na.rm = T)) %>% 
  data.frame()

benef_planos[,1:2] = lapply(benef_planos[,1:2], as.factor)
benef_planos[,3:4] = lapply(benef_planos[,3:4], as.character)
benef_planos[,5] = as.numeric(benef_planos[,5])

# Quantidade de beneficiários em cada OPS

benef_total = tbl(con, "BENEF_2021") %>% 
  group_by(CD_OPERADORA) %>% 
  summarise(QT_BENEF_OPS = sum(QT_BENEFICIARIO_ATIVO, na.rm = T)) %>% 
  data.frame()

benef_total[,1] = as.character(benef_total[,1])
benef_total[,2] = as.numeric(benef_total[,2])

# Valor de Comercialização dos Produtos e seus Percentuais ----

prod_vcm = tbl(con, "NT_VC") %>% 
  select(ID_PLANO, CD_NOTA, CD_FAIXA_ETARIA, ANO_MES, VCM, PCT_DESP_ASS,
         PCT_CARREG, PCT_CARREG_ADMIN, PCT_CARREG_COML, PCT_CARREG_LUCRO) %>%
  data.frame() 

prod_vcm[,1:2] = lapply(prod_vcm[,1:2], as.character)
prod_vcm[,3] = as.factor(prod_vcm[,3])
prod_vcm[,4:10] = lapply(prod_vcm[,4:10], as.numeric)

# Transformando os percentuais em valores decimais
prod_vcm[,6:10] = lapply(prod_vcm[,6:10], function(x){x/100})

# Filtrando os produtos que tem percentual de despesa assistencial ou de demais despesas
# igual a zero, pois é indicativo de erro de registro.

erros = unique(prod_vcm[prod_vcm$PCT_DESP_ASS == 0 | prod_vcm$PCT_CARREG == 0,1])

prod_vcm = prod_vcm[!prod_vcm$ID_PLANO %in% erros,]

# Somando os percentuais de despesa assistencial e demais carregamentos
# A soma tem que dar 1, porém há produtos que a soma da diferente de 1
prod_vcm[,"PCT_TOTAL"] = rowSums(prod_vcm[,6:7]) %>% round(4)

# Filtrando quem tá com a soma de despesa assistencial com demais carregamentos
# maior ou menor que 1 e diferente de 0
a = prod_vcm[prod_vcm$PCT_TOTAL != 1,]

a$PCT_DESP_ASS_CORR = a$PCT_DESP_ASS/a$PCT_TOTAL
a$PCT_CARREG_CORR = a$PCT_CARREG/a$PCT_TOTAL
a$PCT_CARREG_ADMIN_CORR = a$PCT_CARREG_ADMIN/a$PCT_CARREG * a$PCT_CARREG_CORR
a$PCT_CARREG_COML_CORR = a$PCT_CARREG_COML/a$PCT_CARREG * a$PCT_CARREG_CORR
a$PCT_CARREG_LUCRO_CORR = a$PCT_CARREG_CORR - (a$PCT_CARREG_ADMIN_CORR + a$PCT_CARREG_COML_CORR)

a = a[,c(1:5,12:16)]

a[,"PCT_TOTAL"] = rowSums(a[,6:7]) %>% round(0)

names(a) = names(prod_vcm)

# Unindo o que foi corrigo com o que estava igual a 1
prod_vcm = rbind(prod_vcm[prod_vcm$PCT_TOTAL == 1,], a)
rm(a, erros)

# Filtrando pelos produtos com registros mais recentes
comp = Sys.Date() %>% format(format = "%Y%m") %>% as.numeric()

prod_vcm$DIFF = comp - prod_vcm$ANO_MES

prod_vcm = prod_vcm %>% 
  group_by(ID_PLANO) %>% 
  filter(DIFF == min(DIFF), 
         CD_NOTA == max(as.numeric(CD_NOTA))
         )

prod_vcm = prod_vcm[,-c(4,11:12)]

prod_vcm$PCT_CARREG_LUCRO = round(prod_vcm$PCT_CARREG - (prod_vcm$PCT_CARREG_ADMIN + prod_vcm$PCT_CARREG_COML),5)

prod_vcm[prod_vcm$PCT_CARREG_LUCRO < 0,9] = -prod_vcm[prod_vcm$PCT_CARREG_LUCRO < 0,9]

# Unindo as Tabelas ----

tst = carac_prod %>% 
  left_join(x = ., 
            y = prod_vcm, 
            by = c("ID_PLANO")) %>% 
  left_join(x = ., 
            y = benef_planos, 
            by = c("CD_PLANO", "CD_UF", "CD_OPERADORA", "CD_FAIXA_ETARIA" = "DE_FAIXA_ETARIA_REAJ")) %>% 
  left_join(x = .,
            y = benef_total,
            by = "CD_OPERADORA")

nrow(tst[is.na(tst$QT_BENEF_PLAN),])

# Existem ocorrencias de NA's na quantidade de beneficiario por plano.
# Isso ocorre pois há planos que não tem beneficiários em alguma faixa etária.
# Nesse caso, basta atribuir o valor 0 onde tiver NA.

tst[is.na(tst$QT_BENEF_PLAN),24] = 0

nrow(tst[is.na(tst$QT_BENEF_PLAN),])

tst = tst[!is.na(tst$VCM),]

tst$Premio_Puro = tst$VCM*tst$PCT_DESP_ASS # Valor do premio puro

tst$QT_BENEF_PLAN = round(tst$QT_BENEF_PLAN/12) # Quantidade média de beneficiários no ano em cada plano
tst$QT_BENEF_OPS = round(tst$QT_BENEF_OPS/12) # Quantidade média de beneficiários no ano em cada OPS

tst = tst %>% 
  mutate(
    PorteOPS = case_when(
      QT_BENEF_OPS >= 10^5 ~ "G" ,
      QT_BENEF_OPS < 10^5 & QT_BENEF_OPS >= 2*10^4 ~ "M",
      QT_BENEF_OPS < 2*10^4 ~ "P"),
    PorteOPS = factor(PorteOPS)
    )

# Analise Descritiva ----

# Boxplots do Valor de Comercialização das Mensalidades (VCM) ----

# Boxplot do Valor de Comercialização das Mensalidades (VCM)
boxplot(tst$VCM)

# Boxplot do Valor de Comercialização das Mensalidades (VCM) segundo o Grupo de Modalidade da OPS
# 1: Autogestão; 2: Coop. Médica; 4: Filantropia; 5: Medicina de Grupo; 7: Seguradora
boxplot(tst$VCM ~ tst$GR_MODALIDADE) 

# Boxplot do Valor de Comercialização das Mensalidades (VCM) segundo o Grupo de Contratação
# Ind/Fam = 1; Col Emp. = 2, col ad. = 3 e NI = 0
boxplot(tst$VCM ~ tst$GR_CONTRATACAO)

# Boxplot do Valor de Comercialização das Mensalidades (VCM) segundo a Segmentação Assistencial
# 1 = "Ambulatorial", 
# 2 = "Ambulatorial + Hospitalar com obstetrícia", 
# 3 = "Ambulatorial + Hospitalar com obstetrícia + Odontológico", 
# 4 = "Ambulatorial + Hospitalar sem obstetrícia", 
# 5 = "Ambulatorial + Hospitalar sem obstetrícia + Odontológico", 
# 7 = "Hospitalar com obstetrícia", 
# 9 = "Hospitalar sem obstetrícia",
# 12 = "Referência"
boxplot(tst$VCM ~ tst$SGMT_ASSISTENCIAL)

# Boxplot do Valor de Comercialização das Mensalidades (VCM) segundo a Abrangência de Cobertura
# 1-municipal; 2-grupo de municipios; 3-estadual;
boxplot(tst$VCM ~ tst$ABRANGENCIA_COBERTURA)

# Boxplot do Valor de Comercialização das Mensalidades (VCM) segundo o Fator Moderador
# 1-Ausente; 2-Coparticipação; 3-Franquia; 4-Franquia+Coparticipação
boxplot(tst$VCM ~ tst$FATOR_MODERADOR)

# Boxplot do Valor de Comercialização das Mensalidades (VCM) segundo o Fator Moderador
# 1-Coletivo; 2-Individual; 3-Nao se Aplica
boxplot(tst$VCM ~ tst$ACOMODACAO_HOSPITALAR)

# Boxplot do Valor de Comercialização das Mensalidades (VCM) segundo o Porte da OPS
# P = Pequeno; M = Medio e G = Grande
boxplot(tst$VCM ~ tst$PorteOPS)

# Boxplots do Percentual de Despesa Assistencial ----

# Boxplot do Percentual de Despesa Assistencial
boxplot(tst$PCT_DESP_ASS)
boxplot(tst$DESP_ASS)

# Boxplot do Percentual de Despesa Assistencial segundo o Grupo de Modalidade da OPS
# 1: Autogestão; 2: Coop. Médica; 4: Filantropia; 5: Medicina de Grupo; 7: Seguradora
boxplot(tst$PCT_DESP_ASS ~ tst$GR_MODALIDADE) 
boxplot(tst$DESP_ASS ~ tst$GR_MODALIDADE)

# Boxplot do Percentual de Despesa Assistencial segundo o Grupo de Contratação
# Ind/Fam = 1; Col Emp. = 2, col ad. = 3 e NI = 0
boxplot(tst$PCT_DESP_ASS ~ tst$GR_CONTRATACAO)
boxplot(tst$DESP_ASS ~ tst$GR_CONTRATACAO)

# Boxplot do Percentual de Despesa Assistencial segundo a Segmentação Assistencial
# 1 = "Ambulatorial", 
# 2 = "Ambulatorial + Hospitalar com obstetrícia", 
# 3 = "Ambulatorial + Hospitalar com obstetrícia + Odontológico", 
# 4 = "Ambulatorial + Hospitalar sem obstetrícia", 
# 5 = "Ambulatorial + Hospitalar sem obstetrícia + Odontológico", 
# 7 = "Hospitalar com obstetrícia", 
# 9 = "Hospitalar sem obstetrícia",
# 12 = "Referência"
boxplot(tst$PCT_DESP_ASS ~ tst$SGMT_ASSISTENCIAL)
boxplot(tst$DESP_ASS ~ tst$SGMT_ASSISTENCIAL)

# Boxplot do Percentual de Despesa Assistencial segundo a Abrangência de Cobertura
# 1-municipal; 2-grupo de municipios; 3-estadual;
boxplot(tst$PCT_DESP_ASS ~ tst$ABRANGENCIA_COBERTURA)
boxplot(tst$DESP_ASS ~ tst$ABRANGENCIA_COBERTURA)

# Boxplot do Percentual de Despesa Assistencial segundo o Fator Moderador
# 1-Ausente; 2-Coparticipação; 3-Franquia; 4-Franquia+Coparticipação
boxplot(tst$PCT_DESP_ASS ~ tst$FATOR_MODERADOR)
boxplot(tst$DESP_ASS ~ tst$FATOR_MODERADOR)

# Boxplot do Percentual de Despesa Assistencial segundo o Fator Moderador
# 1-Coletivo; 2-Individual; 3-Nao se Aplica
boxplot(tst$PCT_DESP_ASS ~ tst$ACOMODACAO_HOSPITALAR)
boxplot(tst$DESP_ASS ~ tst$ACOMODACAO_HOSPITALAR)

# Boxplots do Percentual de Demais Carregamentos ----

# Boxplot do Percentual de Demais Carregamentos
boxplot(tst$PCT_CARREG)

# Boxplot do Percentual de Demais Carregamentos segundo o Grupo de Modalidade da OPS
# 1: Autogestão; 2: Coop. Médica; 4: Filantropia; 5: Medicina de Grupo; 7: Seguradora
boxplot(tst$PCT_CARREG ~ tst$GR_MODALIDADE) 

# Boxplot do Percentual de Demais Carregamentos segundo o Grupo de Contratação
# Ind/Fam = 1; Col Emp. = 2, col ad. = 3 e NI = 0
boxplot(tst$PCT_CARREG ~ tst$GR_CONTRATACAO)

# Boxplot do Percentual de Demais Carregamentos segundo a Segmentação Assistencial
# 1 = "Ambulatorial", 
# 2 = "Ambulatorial + Hospitalar com obstetrícia", 
# 3 = "Ambulatorial + Hospitalar com obstetrícia + Odontológico", 
# 4 = "Ambulatorial + Hospitalar sem obstetrícia", 
# 5 = "Ambulatorial + Hospitalar sem obstetrícia + Odontológico", 
# 7 = "Hospitalar com obstetrícia", 
# 9 = "Hospitalar sem obstetrícia",
# 12 = "Referência"
boxplot(tst$PCT_CARREG ~ tst$SGMT_ASSISTENCIAL)

# Boxplot do Percentual de Demais Carregamentos segundo a Abrangência de Cobertura
# 1-municipal; 2-grupo de municipios; 3-estadual;
boxplot(tst$PCT_CARREG ~ tst$ABRANGENCIA_COBERTURA)

# Boxplot do Percentual de Demais Carregamentos segundo o Fator Moderador
# 1-Ausente; 2-Coparticipação; 3-Franquia; 4-Franquia+Coparticipação
boxplot(tst$PCT_CARREG ~ tst$FATOR_MODERADOR)

# Boxplot do Percentual de Demais Carregamentos segundo o Fator Moderador
# 1-Coletivo; 2-Individual; 3-Nao se Aplica
boxplot(tst$PCT_CARREG ~ tst$ACOMODACAO_HOSPITALAR)

# Tabelas de Frequência ----

# Tabela de Frequência do Grupo de Modalidade da OPS
# 1: Autogestão; 2: Coop. Médica; 4: Filantropia; 5: Medicina de Grupo; 7: Seguradora
table(tst$GR_MODALIDADE)

# Tabela de Frequência do Grupo de Contratação
# Ind/Fam = 1; Col Emp. = 2, col ad. = 3 e NI = 0
table(tst$GR_CONTRATACAO)

# Tabela de Frequência da Segmentação Assistencial
# 1 = "Ambulatorial", 
# 2 = "Ambulatorial + Hospitalar com obstetrícia", 
# 3 = "Ambulatorial + Hospitalar com obstetrícia + Odontológico", 
# 4 = "Ambulatorial + Hospitalar sem obstetrícia", 
# 5 = "Ambulatorial + Hospitalar sem obstetrícia + Odontológico", 
# 7 = "Hospitalar com obstetrícia", 
# 9 = "Hospitalar sem obstetrícia",
# 12 = "Referência"
table(tst$SGMT_ASSISTENCIAL)

# Tabela de Frequência da Abrangência de Cobertura
# 1-municipal; 2-grupo de municipios; 3-estadual;
table(tst$ABRANGENCIA_COBERTURA)

# Boxplot do Percentual de Demais Carregamentos segundo o Fator Moderador
# 1-Ausente; 2-Coparticipação; 3-Franquia; 4-Franquia+Coparticipação
table(tst$FATOR_MODERADOR)

# Boxplot do Percentual de Demais Carregamentos segundo o Fator Moderador
# 1-Coletivo; 2-Individual; 3-Nao se Aplica
table(tst$ACOMODACAO_HOSPITALAR)

# Gráficos ----

hist(tst$VCM, breaks = 35, main = "Histograma dos Valores de Comercialização")

# Modelagem estatística dos dados ----

# Fazendo cópia dos dados
mydf = tst

'
# Normalizando as variáveis numéricas
tst$QTD_MUNICIPIO = scale(tst$QTD_MUNICIPIO)[,1]
tst$VCM = scale(tst$VCM)[,1]
tst$PCT_DESP_ASS = scale(tst$PCT_DESP_ASS)[,1]
tst$PCT_CARREG = scale(tst$PCT_CARREG)[,1]
tst$PCT_CARREG_ADMIN = scale(tst$PCT_CARREG_ADMIN)[,1]
tst$PCT_CARREG_COML = scale(tst$PCT_CARREG_COML)[,1]
tst$PCT_CARREG_LUCRO = scale(tst$PCT_CARREG_LUCRO)[,1]
tst$QT_BENEF_PLAN = scale(tst$QT_BENEF_PLAN)[,1]
tst$QT_BENEF_OPS = scale(tst$QT_BENEF_OPS)[,1]
'
# Arvore de decisao utilizando o pacote rpart ----
# link: https://www.r-bloggers.com/2021/04/decision-trees-in-r/
set.seed(77)

# Criando a variavel de identificação de treino(1) e teste(2)
ind = sample(2, nrow(tst), replace = T, prob = c(.5, .5))

# Separando os dados em Treino (ind == 1) e Teste (ind == 2)
treino = tst[ind == 1,-c(1:4, 11:14, 16, 18:23, 25)]
teste = tst[ind == 2,-c(1:4, 11:14, 16, 18:23, 25)]

# Resumo estatístico do treino e do teste
summary(treino)
summary(teste)

rpart::rpart.control()

# Criando a arvore de regressão a partir do conjunto de treino
tree = rpart::rpart(Premio_Puro ~., 
                    data = treino, 
                    method = "anova")

# Visualizando a árvore criada
rpart.plot::rpart.plot(tree)

# Resumo da árvore
rpart::printcp(tree)

# Regras da árvore
rpart.plot::rpart.rules(tree)

# 
rpart::plotcp(tree)

# Prevendo os valores baseado no treino
predito_treino = predict(tree, treino)

# Histograma da arvore predita e da matriz de treino
ggplot()+
  geom_histogram(aes(x = predito_treino, col = "Predito")) + 
  geom_histogram(aes(x = treino$VCM, col = "Treino")) +
  scale_fill_manual(values = c(Predito = "blue",
                               Treino = "red"))

par(mfrow = c(1,2))
boxplot(predito_treino, main = "Boxplot VCM Treino Predito", ylim = c(0,10^4))
boxplot(treino$VCM, main = "Boxplot VCM Treino", ylim = c(0,10^4))
par(mfrow = c(1,1))

# Raiz Quadrática Média do Erro (RMSE)
sqrt(mean((treino$VCM - predito_treino)^2))

# R-Quadrado (R²)
(cor(treino$VCM, predito_treino))^2*100

# Prevendo os valores baseado no teste
predito_teste = predict(tree, teste)

# Histograma da arvore predita e da matriz de teste
ggplot()+
  geom_histogram(aes(x = predito_teste, col = "Predito")) + 
  geom_histogram(aes(x = teste$VCM, col = "Teste")) +
  scale_fill_manual(values = c(Predito = "blue",
                               Teste = "red"))

# Boxplot
par(mfrow = c(1,2))
boxplot(predito_teste, main = "Boxplot VCM Teste Predito", ylim = c(0,10^4))
boxplot(teste$VCM, main = "Boxplot VCM Teste", ylim = c(0,10^4))
par(mfrow = c(1,1))

# Raiz Quadrática Média do Erro (RMSE)
sqrt(mean((teste$VCM - predito_teste)^2))

# R-Quadrado (R²)
(cor(teste$VCM, predito_teste))^2

# Random Forest (Pacote randomForest)----
# link: https://www.r-bloggers.com/2018/01/how-to-implement-random-forests-in-r/
library(randomForest)

rf = randomForest(Premio_Puro ~ ., data = treino)
rf

# Aplicando o modelo nos dados de treino
predito_treino.rf = predict(rf, treino)

# Histograma da floresta predita e da matriz de treino
ggplot()+
  geom_histogram(aes(x = predito_treino.rf, col = "Predito")) + 
  geom_histogram(aes(x = treino$Premio_Puro, col = "Treino")) +
  scale_fill_manual(values = c(Predito = "blue",
                               Treino = "red"))

par(mfrow = c(1,2))
boxplot(predito_treino.rf, main = "Boxplot Premio_Puro Treino Predito", ylim = c(0,3000))
boxplot(treino$Premio_Puro, main = "Boxplot Premio_Puro Treino", ylim = c(0,3000))
par(mfrow = c(1,1))

# RMSE
sqrt(mean((treino$Premio_Puro - predito_treino.rf)^2))

# R²
(cor(treino$Premio_Puro, predito_treino.rf))^2

# Aplicando o modelo nos dados de teste
predito_teste.rf = predict(rf, teste)

# Histograma da floresta predita e da matriz de teste
ggplot()+
  geom_histogram(aes(x = predito_teste.rf, col = "Predito")) + 
  geom_histogram(aes(x = teste$Premio_Puro, col = "Treino")) +
  scale_fill_manual(values = c(Predito = "blue",
                               Teste = "red"))

par(mfrow = c(1,2))
boxplot(predito_teste.rf, main = "Boxplot Premio_Puro Teste Predito", ylim = c(0,3000))
boxplot(teste$Premio_Puro, main = "Boxplot Premio_Puro Teste", ylim = c(0,3000))
par(mfrow = c(1,1))

# RMSE
sqrt(mean((teste$Premio_Puro - predito_teste.rf)^2))

# R²
(cor(teste$Premio_Puro, predito_teste.rf))^2

# Testando com um produto hipotetico

# Bootstrapping ----

# Elaborando um produto hipotetico ----

# Informções de entrada
n_entrada = teste %>% 
  filter(GR_MODALIDADE == 2,
         GR_CONTRATACAO == 2,
         SGMT_ASSISTENCIAL == 2,
         ABRANGENCIA_COBERTURA == 2,
         FATOR_MODERADOR == 2,
         ACOMODACAO_HOSPITALAR == 1,
         PorteOPS == "G") %>% 
  group_by(GR_MODALIDADE,
           GR_CONTRATACAO,
           SGMT_ASSISTENCIAL,
           ABRANGENCIA_COBERTURA,
           FATOR_MODERADOR,
           ACOMODACAO_HOSPITALAR,
           PorteOPS,
           CD_FAIXA_ETARIA) %>% 
  summarise(Premio_Puro = mean(Premio_Puro)) %>% 
  mutate(QTD_MUNICIPIO = 19, # Número de municípios que compõe a região metropolitana de Fortaleza
         QT_BENEF_PLAN = c(220, 37, 51, 53, 50, 42, 29, 21, 16, 34))

# Ordenando as colunas
n_entrada = n_entrada[,c(1:6, 10, 8, 11, 9, 7)]

# Produtos resultantes segundo cada modelo visto

# Decision Tree
produto_hipo.dt = predict(tree, n_entrada)

# Random Forest
produto_hipo.rf = predict(rf, n_entrada)

# Verificando a variação entre as faixas etárias do modelo dt
cat(
  "         VERIFICANDO AS VARIAÇÕES ENTRE AS FAIXAS ETÁRIAS \n                 Modelo por Decision Tree",
  "\nVariação entre a 10º e a 1º Faixa Etária: ",
  round(produto_hipo.dt[10] / produto_hipo.dt[1], 2),
  "\nVariação entre a 10º e a 7º Faixa Etária: ",
  round(produto_hipo.dt[10] / produto_hipo.dt[7], 2),
  "\nVariação entre a 7º e a 1º Faixa Etária: ",
  round(produto_hipo.dt[7] / produto_hipo.dt[1], 2)
)

# Verificando a variação entre as faixas etárias do modelo rf
cat(
  "         VERIFICANDO AS VARIAÇÕES ENTRE AS FAIXAS ETÁRIAS \n                 Modelo por Random Forest",
  "\nVariação entre a 10º e a 1º Faixa Etária: ",
  round(produto_hipo.rf[10] / produto_hipo.rf[1], 2),
  "\nVariação entre a 10º e a 7º Faixa Etária: ",
  round(produto_hipo.rf[10] / produto_hipo.rf[7], 2),
  "\nVariação entre a 7º e a 1º Faixa Etária: ",
  round(produto_hipo.rf[7] / produto_hipo.rf[1], 2)
)








