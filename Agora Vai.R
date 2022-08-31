# Funções e Pacotes ----
#rm(list = ls())
allDup <- function(x){
  duplicated(x) | duplicated(x, fromLast = TRUE)
}
pkg = c("dplyr", "odbc", "dbplyr", "tidyr", "lubridate", "GGally", "beepr")
install.packages(pkg[!pkg %in% rownames(installed.packages())], quiet = T)
sapply(pkg, require, character.only = T)

options(scipen = 999)

# Montar uma tabela com as informações dos planos de saúde como a abrangencia, quantidade de municípios,
# quantidade de beneficiários por faixa etária, porte da operadora, modalideade da ops, 
# cobertura assistencial, quantidade de beneficiários da operadora por faixa etária, 
# valor de comercialização, percentuais de carregamento (admin, comercial e lucro), 
# Fator moderador, tipo de contratacao

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








