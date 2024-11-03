# Necessary Packages-----------------------------------------------

pacman::p_load("microdatasus", # Load data from DATASUS
               "tidyverse",    # Data manipulation
               "openxlsx",     # Import .xlsx
               'readxl',       # Export .xlsx
               'tidytext',     # Natural language processing
               'rio',          # Import/export data
               'janitor',      # Name cleaning
               'gtsummary',    # Descriptive statistics
               'here',         # Working directory syncronization
               'rstatix',      # Descriptive statistics
               "rpart",        # Classification/Regression trees
               "caret",        # ML
               "tidymodels",   # ML
               "rpart.plot",   # Rpart object tree plot
               "randomForest", # Randomforest Tree
               "pROC",         # ROC curve
               "neuralnet",
               "skimr")    # Neural networks  


# Loading data --------------------------------------------------------
df <- import(setclass = "tibble", here("data_sinasc_2013_2020_pr.csv"))

## Add weight classification column

df <- df %>% 
  mutate(Cat_PESO = case_when(
    PESO < 2500 ~ "Baixo Peso",
    PESO >= 2500 & PESO <= 4000 ~ "Peso Normal",
    PESO > 4000 ~ "Macrossômico")
    )

# Descriptive statistics --------------------------------------------------

skim(df)

df %>% 
  get_summary_stats(type = "common",)  %>% 
  flextable::flextable() %>%    
  flextable::autofit()          


# Quantitative Variable Histograms --------------------------------


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

df <- df %>%
  mutate_if(is.numeric, as.double) %>%
  mutate_if(negate(is.numeric), as.factor)

df$PESO <- as.numeric(df$PESO)
df$QTDFILVIVO <- as.numeric(df$QTDFILVIVO)
df$QTDFILMORT <- as.numeric(df$QTDFILMORT)
df$IDADEMAE <- as.numeric(df$IDADEMAE)
df$SEMAGESTAC <- as.numeric(df$SEMAGESTAC)
df$QTDPARTCES <- as.numeric(df$QTDPARTCES)
df$QTDGESTANT <- as.numeric(df$QTDGESTANT)
df$QTDPARTNOR <- as.numeric(df$QTDPARTNOR)
df$MESPRENAT <- as.numeric(df$MESPRENAT)

df$ESCMAE <- as.factor(df$ESCMAE)
df$SEXO <- as.factor(df$SEXO)
df$RACACOR <- as.factor(df$RACACOR)
df$RACACORMAE <- as.factor(df$RACACORMAE)
df$GRAVIDEZ <- as.factor(df$GRAVIDEZ)
df$PARTO <- as.factor(df$PARTO)
df$IDANOMAL <- as.factor(df$IDANOMAL)
df$GESTACAO <- as.factor(df$GESTACAO)
df$RACACORMAE <- as.factor(df$RACACORMAE)
df$ESTCIVMAE <- as.factor(df$ESTCIVMAE)
df$CODOCUPMAE <- as.factor(df$CODOCUPMAE)

df$Cat_PESO <- as.factor(df$Cat_PESO)


df2 <- select(df, ESCMAE, RACACOR, GRAVIDEZ, IDANOMAL,
              RACACORMAE, Cat_PESO, IDADEMAE,  Cat_PESO, QTDFILVIVO,
              QTDFILMORT,  SEMAGESTAC, PARTO, SEXO, QTDPARTCES,
              QTDGESTANT, QTDPARTNOR, ESTCIVMAE
)

## Splitting training and test dataset
train <- sample(nrow(df2), 0.7*nrow(df2), replace = FALSE)
TrainSet <- df2[train,]
ValidSet <- df2[-train,]



# Classification tree -----------------------------------
                    
arvore <- rpart(Cat_PESO ~. ,
                data = TrainSet,  
                method = 'class',
                model = T,
                control = rpart.control(cp = 0,00001,
                                        maxdepth = 8, minsplit = 1000, 
                                        minbucket = 1000)
)


rpart.plot::rpart.plot(arvore, type = 2, extra = 106,
                       box.palette = "RdYlGn", digits = 2) 

arvore$variable.importance


## Variable importante

barplot(arvore$variable.importance, 
        main = "Variable Importance",
        xlab = "Value", ylab = "",
        horiz = TRUE, las = 1, cex.names = 0.52)

## Confusion Matrix

ValidSet$Cat_PESO <- as.factor(ValidSet$Cat_PESO)

arvore_train <- predict(arvore, newdata = ValidSet, type = "class")

cm_arvore <- confusionMatrix(arvore_train, reference = ValidSet$Cat_PESO)

cm_arvore


## ROC Curve

arvore_roc <- predict(arvore, newdata = ValidSet, type = "prob")

tree.roc <- multiclass.roc(ValidSet$Cat_PESO, arvore_roc)

auc(tree.roc)


## Saving model
saveRDS(arvore, "modelo_rpart_classificacao_pr_08958_last_.rds")


# Multinomial Logistic Regression -----------------------------------------

logistic_model <- multinom(formula = Cat_PESO ~. , 
                            data = TrainSet)


summary(logistic_model)

modelo_logistic$AIC

## Confusion matrix

ValidSet$Cat_PESO <- as.factor(ValidSet$Cat_PESO)

logistic_teste <- predict(modelo_logistic, newdata = ValidSet, type = "class")

cm_lgm <- confusionMatrix(logistic_teste, reference = ValidSet$Cat_PESO)

cm_lgm

## ROC Curve

arvore_roc <- predict(modelo_logistic, newdata = ValidSet, type = "prob")

tree.roc <- multiclass.roc(ValidSet$Cat_PESO, arvore_roc)

auc(tree.roc)


## Saving model
saveRDS(logistic_model, "modelo_logistic_multinomial_classificacao.rds")

# Random Forest -----------------------------------------------------------
## Random Forest --> 0.5 split --> Target variable needs to be Factor

rf <- randomForest(Cat_PESO ~. ,
                     data = TrainSet, importance = TRUE, 
                     na.action = na.omit
                     )

## Confusion Matrix

ValidSet$Cat_PESO <- as.factor(ValidSet$Cat_PESO)

rf_train <- predict(modelo_random_forest, newdata = ValidSet)
cm_rf <- confusionMatrix(rf_train, reference = ValidSet$Cat_PESO)
cm_rf

## Variable Importance
                     
rf$importance

varImpPlot(rf)


## ROC Curve

arvore_roc <- predict(modelo_random_forest, newdata = ValidSet, type = "prob")

tree.roc <- multiclass.roc(ValidSet$Cat_PESO, arvore_roc)

auc(tree.roc)

## Saving model
                     
saveRDS(rf, "modelo_random_forest_classificacao_certo.rds")
