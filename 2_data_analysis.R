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
               'CI',           # Cálculo do intervalo de confiança
               "rpart",        # Modelagem de árvore simples
               "caret",        # Várias modelagens
               "tidymodels",   # Várias modelagens
               "rpart.plot",
               "randomForest",
               "pROC",
               "neuralnet")   # Plotagem da árvore        


# Importando dados --------------------------------------------------------
dfpr <- import(setclass = "tibble", here("dados_sinasc_2013_2020_pr_alterado.xlsx"))
dfmt <- import(setclass = "tibble", here("dados_sinasc_2013_2020_mt_alterado.xlsx"))
dfes <- import(setclass = "tibble", here("dados_sinasc_2013_2020_es_alterado.xlsx"))
dfpa <- import(setclass = "tibble", here("dados_sinasc_2013_2020_pa_alterado.xlsx"))
dfba <- import(setclass = "tibble", here("dados_sinasc_2013_2020_ba_alterado.xlsx"))

df1 <- import(setclass = "tibble", here("dados_sinasc_2013_2020_pr_ok.csv"))


### Juntando dados

df1 <- full_join(dfba, dfmt)
df1 <- full_join(df1, dfpa)
df1 <- full_join(df1, dfpr)
df1 <- full_join(df1, dfes)


export(dados_sinasc_2013_2020_pr_alterado, "dados_sinasc_2013_2020_pr_ok_full.csv")


## Adicionando uma coluna com a classificação do peso

df1 <- df1 %>% 
  mutate(Cat_PESO = case_when(
    PESO < 2500 ~ "Baixo Peso",
    PESO >= 2500 & PESO <= 4000 ~ "Peso Normal",
    PESO > 4000 ~ "Macrossômico")
    )

# Descriptive statistics --------------------------------------------------

summary(df1)


tabyl(df2) %>% 
  flextable::flextable() %>% 
  flextable::autofit()

df3 %>%
  flextable::flextable() %>%   
  flextable::autofit()          


df1 %>% 
  get_summary_stats(type = "common",)  %>% 
  flextable::flextable() %>%    
  flextable::autofit()          

df1 %>% 
  tabyl() 


# Histogramas  das Variáveis Quantitativas --------------------------------


## ESCMAE
ggplot(data = df2, aes(x = factor(ESCMAE, levels = c("Nenhum", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos ou mais")))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Escolaridade Materna",
       x = "Valores",
       y = "Frequência") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +  
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## RACACOR
ggplot(data = df2, aes(x = factor(RACACOR))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Raça e Cor Neonato",
       x = "Raça e Cor",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +  
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## RACACORMAE

ggplot(data = df2, aes(x = factor(RACACORMAE))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Raça e Cor Materna",
       x = "Raça e Cor",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +  
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## Cat_PESO

ggplot(data = df2, aes(x = factor(Cat_PESO))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Classificação do peso ao Nascimento",
       x = "Categorias",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## IDANOMAL

ggplot(data = df2, aes(x = factor(IDANOMAL))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Presença de Anomalia no Nascimento",
       x = "Anomalia Identificada",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## Gravidez

ggplot(data = df2, aes(x = factor(GRAVIDEZ))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Tipo de Gravidez",
       x = "Categorias",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


## PARTO

ggplot(data = df2, aes(x = factor(PARTO))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Tipo de Parto",
       x = "Categorias",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## SEXO

ggplot(data = df2, aes(x = factor(SEXO))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Sexo ao Nascimento",
       x = "Categorias",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

## ESTCIVMAE

ggplot(data = df2, aes(x = factor(ESTCIVMAE))) +
  stat_count(color = "black", fill = "lightblue") +
  labs(title = "Estado Civil Materno",
       x = "Estado Civil",
       y = "Número de Indivíduos") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")), vjust = -0.5) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Data Wrangling ----------------------------------------------------------
glimpse(df1)

df1$PESO <- as.numeric(df1$PESO)
df1$QTDFILVIVO <- as.numeric(df1$QTDFILVIVO)
df1$QTDFILMORT <- as.numeric(df1$QTDFILMORT)
df1$IDADEMAE <- as.numeric(df1$IDADEMAE)
df1$SEMAGESTAC <- as.numeric(df1$SEMAGESTAC)
df1$QTDPARTCES <- as.numeric(df1$QTDPARTCES)
df1$QTDGESTANT <- as.numeric(df1$QTDGESTANT)
df1$QTDPARTNOR <- as.numeric(df1$QTDPARTNOR)
df1$MESPRENAT <- as.numeric(df1$MESPRENAT)

df1$ESCMAE <- as.factor(df1$ESCMAE)
df1$SEXO <- as.factor(df1$SEXO)
df1$RACACOR <- as.factor(df1$RACACOR)
df1$RACACORMAE <- as.factor(df1$RACACORMAE)
df1$GRAVIDEZ <- as.factor(df1$GRAVIDEZ)
df1$PARTO <- as.factor(df1$PARTO)
df1$IDANOMAL <- as.factor(df1$IDANOMAL)
df1$GESTACAO <- as.factor(df1$GESTACAO)
df1$RACACORMAE <- as.factor(df1$RACACORMAE)
df1$ESTCIVMAE <- as.factor(df1$ESTCIVMAE)
df1$CODOCUPMAE <- as.factor(df1$CODOCUPMAE)

df1$Cat_PESO <- as.factor(df1$Cat_PESO)


df2 <- select(df1, ESCMAE, RACACOR, GRAVIDEZ, IDANOMAL,
              RACACORMAE, Cat_PESO, IDADEMAE,  Cat_PESO, QTDFILVIVO,
              QTDFILMORT,  SEMAGESTAC, PARTO, SEXO, QTDPARTCES,
              QTDGESTANT, QTDPARTNOR, ESTCIVMAE
)

## Separado conjunto de treino e de teste
train <- sample(nrow(df2), 0.7*nrow(df2), replace = FALSE)
TrainSet <- df2[train,]
ValidSet <- df2[-train,]



# Modelo Árvore de classificação simples --> 0.7 usado de split-----------------


arvore <- rpart(Cat_PESO ~. ,
                data = TrainSet,  
                method = 'class',
                model = T,
                control = rpart.control(cp = 0,00001,
                                        maxdepth = 8, minsplit = 1000, 
                                        minbucket = 1000)
)


rpart.plot::rpart.plot(arvore, type = 2, extra = 106,
                       box.palette = "RdYlGn", digits = 2) # Paleta de cores

arvore$variable.importance
prp(arvore)
## gráfico para plotar importância das variáveis

barplot(arvore$variable.importance, 
        main = "Variable Importance",
        xlab = "Value", ylab = "",
        horiz = TRUE, las = 1, cex.names = 0.52)

## Matrix de confusão Rpart

ValidSet$Cat_PESO <- as.factor(ValidSet$Cat_PESO)

arvore_train <- predict(arvore, newdata = ValidSet, type = "class")

cm_arvore <- confusionMatrix(arvore_train, reference = ValidSet$Cat_PESO)

cm_arvore


## ROC Curve

arvore_roc <- predict(arvore, newdata = ValidSet, type = "prob")

tree.roc <- multiclass.roc(ValidSet$Cat_PESO, arvore_roc)

auc(tree.roc)


## Salvando modelo
saveRDS(arvore, "modelo_rpart_classificacao_pr_08958_last_.rds")


# Regressão Logística Multinomial -----------------------------------------

logistic_model <- multinom(formula = Cat_PESO ~. , 
                            data = TrainSet)


summary(logistic_model)

modelo_logistic$AIC

## Matrix de confusão Modelo Logístico Multinomial 0.8 split

ValidSet$Cat_PESO <- as.factor(ValidSet$Cat_PESO)

logistic_teste <- predict(modelo_logistic, newdata = ValidSet, type = "class")

cm_lgm <- confusionMatrix(logistic_teste, reference = ValidSet$Cat_PESO)

cm_lgm

saveRDS(logistic_model, "modelo_logistic_multinomial_classificacao.rds")


## ROC Curve

arvore_roc <- predict(modelo_logistic, newdata = ValidSet, type = "prob")

tree.roc <- multiclass.roc(ValidSet$Cat_PESO, arvore_roc)

auc(tree.roc)

ggplot(data = modelo_logistic, aes(x = x, y = y, color = Cat_PESO)) +
  geom_point() +
  labs(title = "Modelo Logístico Multinomial",
       x = "Variável X",
       y = "Variável Y",
       color = "Classe")


class(modelo_logistic)

# Random Forest -----------------------------------------------------------
## Random Forest --> 0.5 usado de split --> Váriavel Target precisa ser Factor

rf <- randomForest(Cat_PESO ~. ,
                     data = TrainSet, importance = TRUE, 
                     na.action = na.omit
                     )
## Salvando modelo

saveRDS(rf, "modelo_random_forest_classificacao_certo.rds")

## Matrix de confusão RandomForest

ValidSet$Cat_PESO <- as.factor(ValidSet$Cat_PESO)

rf_train <- predict(modelo_random_forest, newdata = ValidSet)
cm_rf <- confusionMatrix(rf_train, reference = ValidSet$Cat_PESO)

cm_rf
rf$importance
varImpPlot(rf)


## ROC Curve

arvore_roc <- predict(modelo_random_forest, newdata = ValidSet, type = "prob")

tree.roc <- multiclass.roc(ValidSet$Cat_PESO, arvore_roc)

auc(tree.roc)



# Xgboost -----------------------------------------------------------------
##Xgboost via caret --> 0.7 split

## Controle
controle_xgboost <- trainControl(method = "cv", 
                                 number = 5,
                                 allowParallel = TRUE,
                                )

modelo_xgb <- caret::train(
  Cat_PESO ~., 
  data = TrainSet, 
  method = "xgbTree",
  na.action = na.exclude) ## Forma de dropar os NAs, outras formas incluem
##  na.action = na.roughfix)

modelo$bestTune

plot(modelo)


plot(varImp(modelo))

saveRDS(modelo, "modelo_xg_boost_tree.rds")


# Treinamento variáveis baixo e alto peso ---------------------------------

df3 <- filter(df2, Cat_PESO != "Peso Normal")


train <- sample(nrow(df3), 0.8*nrow(df3), replace = FALSE)
TrainSet <- df3[train,]
ValidSet <- df3[-train,]

arvore2 <- rpart(Cat_PESO ~. ,
                 data = TrainSet,  
                 method = 'class',    
                 control = rpart.control(cp = 0,00001,
                                         maxdepth = 8, minsplit = 1000, 
                                         minbucket = 1000)
)

rpart.plot::rpart.plot(arvore, type = 2, extra = 0,
                       box.palette = "RdYlGn", digits = 1,
                       tweak = 1.3) # Paleta de cores
prp(arvore)

## Matrix de confusão Rpart

ValidSet$Cat_PESO <- as.factor(ValidSet$Cat_PESO)

arvore_train2 <- predict(arvore2, newdata = ValidSet, type = "class")

cm_arvore2 <- confusionMatrix(arvore_train2, reference = ValidSet$Cat_PESO)

cm_arvore2

arvore2$variable.importance

barplot(rf$importance, 
        main = "Variable Importance",
        xlab = "Value", ylab = "",
        horiz = TRUE, las = 1, cex.names = 0.5)
varImpPlot(rf)


# Rede Neural Artificial --------------------------------------------------
m <- model.matrix(
  ~ ESCMAE + RACACOR + GRAVIDEZ + IDANOMAL +
    RACACORMAE + IDADEMAE + QTDFILVIVO + QTDFILMORT + SEMAGESTAC +
    PARTO + SEXO + QTDPARTCES + QTDGESTANT + QTDPARTNOR + ESTCIVMAE, 
  data = TrainSet )



nn <- neuralnet( Cat_PESO ~ ESCMAE + RACACOR + GRAVIDEZ + IDANOMAL +
                  RACACORMAE + IDADEMAE + QTDFILVIVO + QTDFILMORT + SEMAGESTAC +
                  PARTO + SEXO + QTDPARTCES + QTDGESTANT + QTDPARTNOR + ESTCIVMAE,
                 data = m, hidden = 2)




# Criar o controle para a modelagem
controle <- trainControl(method = "cv", number = 5)

# Criar o modelo de rede neural
modelo <- train(Cat_PESO ~ ESCMAE + RACACOR + GRAVIDEZ + IDANOMAL +
                  RACACORMAE + IDADEMAE + QTDFILVIVO + QTDFILMORT + SEMAGESTAC +
                  PARTO + SEXO + QTDPARTCES + QTDGESTANT + QTDPARTNOR + ESTCIVMAE,
                data = TrainSet, method = "nnet", trControl = controle, tuneLength = 5, trace = FALSE)

# Plotar a rede neural
plot(modelo$finalModel)








