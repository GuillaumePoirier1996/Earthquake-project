library(readr)
data <- read_csv("C:/Users/davyd/Desktop/Eartquakes-1990-2023.csv/data_clean.csv")

#On va filtrer par phénomènes TREMBLEMENT DE TERRE = EARTHQUAKE
table(data$type)
data <-  subset(data,data$type=="earthquake")
# Supprimer la colonne d'origine 'type'
data <- data[, !(names(data) %in% "type")]


#----------Data  Spliting--------------
# Ici on choisi de se concentrer
str(data$date)

data_alsk = subset(data,data$state=="Alaska")

library(lubridate)
# Filter the data for the last month
last_1_week_data <- subset(data_alsk, data_alsk$date >= (max(data_alsk$date) %m-% weeks(1)))
data = subset(data_alsk, data_alsk$date < (max(data_alsk$date) %m-% weeks(1)))




# A faire fonctionner sur RStudio

# Spécifier la largeur et la hauteur du graphique
options(repr.plot.width = 22, repr.plot.height = 10)


#--------------------Série Alaska------------------------

data = read.csv("data_clean.csv")
data <-  subset(data,data$type=="earthquake")
str(data$date)


data = subset(data,(data$mag>3) & (data$state=="Alaska"))

data$date = as.Date(data$date)

data_alaska <- data %>%
  mutate(year_month = format(date, "%Y-%m")) %>%  # Ajouter une colonne 'year_month' avec l'année et le mois
  group_by(year_month) %>%
  summarise(Count = n())

# Ici on transforme en exponentielle pour être à l'échelle 'réelle' car mag est exprimé en log
# Supposons que votre variable year_month est une chaîne de caractères
data_alaska$year_month <- as.Date(paste0(data_alaska$year_month, "-01"))

# Ensuite, vous pouvez créer le graphique
ggplot(data_alaska, aes(x = year_month , y = Count)) +
  geom_line() +
  labs(title = "Nombre de séisme par mois en Alaska",
       x = "Date (année-mois)",
       y = "Nombre") +
  theme_economist() +
  theme


# Convertir la variable 'year_month' en classe Date
data_alaska$year_month <- as.Date(data_alaska$year_month)

# Filtre pour les données d'entraînement (années jusqu'à 2000 inclus)
data_train <- data_alaska[data_alaska$year_month <= as.Date("2022-07-01"), ]

# Filtre pour les données de test (années après 2000)
data_test <- data_alaska[data_alaska$year_month > as.Date("2022-07-01"), ]


write.csv(data_train,"data_TRAIN_clean_feature_engineering.csv")
write.csv(data_test,"data_TEST_clean_feature_engineering.csv")



#---------------------FEATURE ENGINEERING---------------------------
#On créé une fonction pour appliquer les modifications aux 2 jeux de données

feature_engineering <- function(data,nom_fichier_export){
  #On applique l'exponentielle sur la magnitude
  data$mag = exp(data$mag)
  
  #------Variable temporelle------
  data$year <- as.numeric(format(data$date, "%Y"))
  data$month <- as.numeric(format(data$date, "%m"))
  data$day <- as.numeric(format(data$date, "%d"))
  data$hour <- as.numeric(format(data$date, "%H"))
  
  #Variable time en numérique
  epoch <- as.POSIXct("1970-01-01T00:00:00.000Z", format="%Y-%m-%dT%H:%M:%S.%OSZ", tz="UTC")
  
  # Calculer la différence en millisecondes depuis l'epoch
  data$time <- as.numeric(difftime(data$date, epoch, units = "secs")) * 1000
  # Supprimer la colonne d'origine 'date' qui ne peut pas être exploité dans un modèle
  #data <- data[, !(names(data) %in% "date")]
  
  
  #------ONE HOT ENCODING------
  freq_table <- table(data$state)
  sorted_freq_table <- freq_table[order(freq_table, decreasing = F)]
  
  #+10K earthquake  en binaire sinon other
  
  # Calculer la fréquence des États
  state_frequencies <- table(data$state)
  
  # Sélectionner les États avec plus de 10 000 tremblements de terre
  selected_states <- names(state_frequencies[state_frequencies > 500000])
  
  # Créer des colonnes one-hot pour les États sélectionnés
  for (state in selected_states) {
    data[paste("state_", state, sep = "")] <- as.integer(data$state == state)
  }
  
  # Créer une colonne 'Other' pour tous les autres États
  data$state_Other <- as.integer(!(data$state %in% selected_states))
  
  
  #------variable cardinal------
  head(data$place)
  library(stringr)
  data$place <- gsub("\\s+(?=km)", "", data$place, perl = TRUE)
  
  # Extraire les points cardinaux
  data$cardinal <- str_extract(data$place, '(?<=km\\s)\\w+')
  sum(is.na(data$cardinal)) #148203
  data[is.na(data$cardinal)==T,"cardinal"] <- "Other"
  
  # Convertir la colonne en one-hot encoding
  one_hot_encoding <- model.matrix(~0 + factor(data$cardinal))
  
  # Ajouter des noms de colonnes significatifs
  colnames(one_hot_encoding) <- levels(factor(data$cardinal))
  
  # Ajouter les nouvelles colonnes one-hot encoding au DataFrame
  data <- cbind(data, one_hot_encoding)
  
  
  data <- data[, !(names(data) %in% c("place","status","tsunami","cardinal","state"))]
  
  write.csv(data,nom_fichier_export)
  
}

feature_engineering(data,"data_TRAIN_clean_feature_engineering.csv")
feature_engineering(last_1_week_data,"data_TEST_clean_feature_engineering.csv")







#--------------------Série Alaska------------------------
data_alsk = subset(data,data$state=="Alaska")

data_alsk$date
data_alsk$mag


library(lubridate)
# Filter the data for the last month
last_1_week_data <- subset(data_alsk, data_alsk$date >= (max(data_alsk$date) %m-% weeks(1)))

library(ggplot2)
library(ggthemes)
theme <-theme(axis.line = element_blank(),
              axis.text.x = element_text(face="bold", color="#797979"),
              axis.text.y = element_text(face="bold", color="#797979"),
              plot.title = element_text(color="#5A5A5A", face="bold",hjust = 0.5,size=20),
              axis.title.x = element_text(color="#767B7B", face="bold",size=14,
                                          margin = margin(t = 20, r = 0, b = 0, l = 0)),
              axis.title.y = element_text(color="#767B7B",  face="bold",size=14,
                                          margin = margin(t = 0, r = 20, b = 0, l = 0)),
              legend.text = element_text(size = 13),
              legend.title = element_text(size = 14, face = "bold"),
              strip.text.x = element_text(
                size = 11, color = "#5A5A5A", face = "italic"
              ))


# Plot the time series using ggplot2
ggplot(data_alsk, aes(x = date, y = (mag))) +
  geom_line() +
  labs(title = "Série temporelle de la dernière semaine de juillet 2023",
       x = "Date",
       y = "Magnitude")+theme_economist()+theme


data = read.csv("data_clean_feature_engineering.csv")


data = read.csv("data_TRAIN_clean_feature_engineering.csv")
library(dplyr)

data_ts <- ts(data$Count,frequency=1)


acf(diff(data$Count),lag=50)
pacf(diff(data$Count),lag=50)

#Test de stationnarité
library(tseries)
a = adf.test(data_ts)
a
a$p.value

lag.length = 10
Box.test(data_ts, lag=lag.length, type="Ljung-Box")




data$year_month <- as.Date(paste0(data$year_month, "-01"))

# Ensuite, vous pouvez créer le graphique
ggplot(data, aes(x = year_month , y = Count)) +
  geom_line() +
  labs(title = "Nombre de séisme par mois en Alaska",
       x = "Date (année-mois)",
       y = "Nombre") +
  theme_economist() +
  theme



y = data$Count
ts.plot(diff(y))

# Appliquer un ordre de différenciation
data$diff_count <- c(NA, diff(data$Count))

data$year_month <- as.Date(data$year_month)

# Tracer le graphique avec ggplot2
ggplot(data, aes(x = year_month, y = diff_count)) +
  geom_line() +
  labs(title = "Série temporelle avec un ordre de différenciation",
       x = "Date (year-month)",
       y = "")+theme_economist()+theme


y = na.omit(data$diff_count)

acf(y)
pacf(y,lag=40)

#Test de stationnarité
library(tseries)
a = adf.test(y)
a
a$p.value

# 
# Statistique du test (Dickey-Fuller) : La valeur de la statistique du test est -38.621.
# Ordre de retard (Lag order) : L'ordre de retard est de 22.
# P-value : La valeur p est de 0.01.
# La statistique du test de Dickey-Fuller est significativement négative.
# La valeur p est inférieure à un seuil de signification standard (0.01), indiquant que nous pouvons rejeter l'hypothèse nulle.
# L'hypothèse alternative suggère que la série temporelle différenciée est stationnaire.

#Test
library(forecast)
auto.arima(data$Count)
# Series: data$Count 
# ARIMA(0,1,3) 
# 
# Coefficients:
#   ma1      ma2      ma3
# -0.6603  -0.1869  -0.0864
# s.e.   0.0507   0.0609   0.0530
# 
# sigma^2 = 6561:  log likelihood = -2237.57
# AIC=4483.14   AICc=4483.24   BIC=4498.95



#le graph pacf montre que les autocorrélations semble décroître vers 0
#Et on a constaté un modèle ARIMA(0,1,1) candidat
#On utilisera une grille pour tester plusieurs modèle à la fois et se baser sur le critère aic et bic

y = data$Count
# Initialiser le dataframe pour stocker les résultats
result_df <- data.frame(p = integer(),
                        q = integer(),
                        aic = numeric(),
                        bic = numeric())

# Boucle pour ajuster les modèles ARIMA avec différentes combinaisons d'ordres p et q
for (p in 0:10) {
  for (q in 0:10) {
    # Ignorer les combinaisons où p et q sont tous les deux 0
    if (p == 0 & q == 0) next
    
    # Ajuster le modèle ARIMA
    arima_model <- arima(y, order = c(p, 1, q))  # 1 pour l'ordre d'intégration
    
    # Stocker les résultats dans le dataframe
    result_df <- rbind(result_df, data.frame(p = p, q = q, aic = AIC(arima_model), bic = BIC(arima_model)))
    
    print(paste0("-------------"))
    print(paste0("p:", p))
    print(paste0("q:", q))
    }
}

# Trouver l'index du modèle avec le plus petit AIC
best_aic_index <- which.min(result_df$aic)

# Extraire les ordres p et q associés au meilleur AIC
best_aic_p <- result_df$p[best_aic_index]
best_aic_q <- result_df$q[best_aic_index]

# Trouver l'index du modèle avec le plus petit BIC
best_bic_index <- which.min(result_df$bic)

# Extraire les ordres p et q associés au meilleur BIC
best_bic_p <- result_df$p[best_bic_index]
best_bic_q <- result_df$q[best_bic_index]

# Afficher les résultats
{
cat("Meilleur modèle selon AIC (p, q):", best_aic_p, ",", best_aic_q, "\n")
cat("Meilleur modèle selon BIC (p, q):", best_bic_p, ",", best_bic_q, "\n")
}

# Meilleur modèle selon AIC (p, q): 6 , 10 
# Meilleur modèle selon BIC (p, q): 1 , 1 


# Modèles
arima_model <- arima(y, order = c(6, 1, 10)) #AIC
arima_model_2 <- arima(y, order = c(1, 1, 1)) #BIC
arima_model_3 <- arima(y, order = c(0, 1, 3)) #Auto.arima
arima_model_4 <- arima(y, order = c(8, 1, 1)) #Notre hypothèse


test = read.csv("data_TEST_clean_feature_engineering.csv")
test$date = as.Date(test$year_month)
# Utiliser dplyr pour filtrer les observations
test <- test %>%
  group_by(date) %>%
  filter(mag == max(mag)) %>%
  distinct(date, .keep_all = TRUE)

test = test[-1,]

y_test = test$mag
x_test = test$time

library(forecast)
# Générer des prévisions avec les modèles ARIMA
forecast_arima <- forecast(arima_model, h = length(y_test))
forecast_arima_hyp <- forecast(arima_model_hyp, h = length(y_test))

# Extraire les prévisions des modèles
y_pred_arima <- forecast_arima$mean
y_pred_arima_hyp <- forecast_arima_hyp$mean

# Calculer le RMSE pour chaque modèle
rmse_arima <- sqrt(mean((y_pred_arima - y_test)^2))
rmse_arima_hyp <- sqrt(mean((y_pred_arima_hyp - y_test)^2))

# Afficher les résultats
cat("RMSE pour le modèle ARIMA :", rmse_arima, "\n")
cat("RMSE pour le modèle ARIMA avec ordre hypothétique :", rmse_arima_hyp, "\n")



# Ajuster le modèle ARIMA
arima_model <- arima(y, order = c(5, 1, 5))
arima_model_2 <- arima(y, order = c(1, 1, 1))
arima_model_3 <- arima(y, order = c(0, 1, 3))

library(forecast)
# Créer le graphique avec les lignes
plot(y, col = "blue", type = "l", ylim = range(c(y, fitted(arima_model), fitted(arima_model_2), fitted(arima_model_3))), xlab = "Index", ylab = "Magnitude")
lines(fitted(arima_model), col = "red")
lines(fitted(arima_model_2), col = "green")
lines(fitted(arima_model_3), col = "violet")

# Ajouter la légende
legend("topright", legend = c("Observed", "ARIMA(5,1,5)", "ARIMA(1,1,1)", "ARIMA(0,1,3)"), col = c("blue", "red", "green", "violet"), lty = 1)

library(ggplot2)
library(reshape2)
library(dplyr)
library(plotly)

# Créer un dataframe avec les données observées et ajustées
df_plot <- data.frame(
  Index = data$year_month,
  Observed = y,
  ARIMA_1 = fitted(arima_model),
  ARIMA_2 = fitted(arima_model_2),
  ARIMA_3 = fitted(arima_model_3)
)

# Melt du dataframe
melted_df <- melt(df_plot, id.vars = "Index", variable.name = "Model", value.name = "Value")

# Modification des noms des modèles dans le dataframe fondu
melted_df <- melted_df %>%
  mutate(Model = case_when(
    Model == "Observed" ~ "Observées",
    Model == "ARIMA_1" ~ "ARIMA(5,1,5)",
    Model == "ARIMA_2" ~ "ARIMA(1,1,1)",
    Model == "ARIMA_3" ~ "ARIMA(0,1,3)",
    TRUE ~ as.character(Model)
  ))

# Créer le graphique interactif avec plotly
plot_ly(data = melted_df, x = ~Index, y = ~Value, color = ~Model, type = 'scatter', mode = 'lines', line = list(width = 1)) %>%
  layout(title = "Ajustements ARIMA aux données", xaxis = list(title = "Temps(mois)"), yaxis = list(title = "Nombre de séisme")) %>%
  colorbar(title = "Modèle")






# Charger le package stats
library(stats)

# Récupérer les résidus pour chaque modèle
residuals_arima_fit <- residuals(arima_model)
residuals_arima_model_2 <- residuals(arima_model_2)
residuals_arima_model_3 <- residuals(arima_model_3)

# Effectuer le test de Ljung-Box pour chaque modèle
ljung_box_test_arima_fit <- Box.test(residuals_arima_fit, lag = 20, type = "Ljung-Box")
ljung_box_test_arima_model_2 <- Box.test(residuals_arima_model_2, lag = 20, type = "Ljung-Box")
ljung_box_test_arima_model_3 <- Box.test(residuals_arima_model_3, lag = 20, type = "Ljung-Box")


model_final_arima = arima_model

data_test = read.csv("data_TEST_clean_feature_engineering.csv")
y_test = data_test$Count

# Effectuer les prédictions à l'horizon 31 avec le modèle ARIMA
predictions <- round(predict(model_final_arima, n.ahead = length(y_test))$pred,0)

# Calculer les métriques de performance (par exemple, RMSE)
rmse <- sqrt(mean((y_test - predictions)^2))

# Afficher les prédictions et les métriques de performance
print("Prédictions :")
print(predictions)

print("Métriques de performance :")
print(paste("RMSE :", rmse))

library(forecast)
future2 = forecast(model_final_arima, h = 12)


# Fusionner les données de test avec les prédictions
df_plot <- data.frame(
  Index = c(as.Date(date_train),as.Date(data_test$year_month)),
  Observed = c(y_train,y_test),
  Predicted = c(rep(NA,length(y_train)),future2$mean),
  Lower_CI = c(rep(NA,length(y_train)),future2$lower[,1]),
  Upper_CI = c(rep(NA,length(y_train)),future2$upper[,1])
)

# Tracé des courbes avec ggplot2
ggplot(df_plot, aes(x = Index)) +
  geom_line(aes(y = Observed), color = "blue", size = 1, linetype = "solid", alpha = 0.7, label = "Observées") +
  geom_line(aes(y = Predicted), color = "red", size = 1, linetype = "solid", alpha = 0.7, label = "Prédictions") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey", alpha = 0.2, linetype = "solid", label = "Intervalle de confiance") +
  labs(title = "Prédictions ARIMA(5,1,5) avec Intervalle de Confiance",
       x = "Date (year_month)",
       y = "Nombre de séisme") +
  scale_color_manual(values = c("blue", "red")) +
  theme_economist() + theme+
  scale_fill_manual(values = "grey")



data_train = read.csv("data_TRAIN_clean_feature_engineering.csv")
data_train = tail(data_train,10)
y_train = data_train$Count
date_train = data_train$year_month


# Créer un dataframe avec les données observées, les prédictions et les intervalles de confiance
df_plot <- data.frame(
  Index = c(as.Date(date_train),as.Date(data_test$year_month)),
  Observed = c(y_train,y_test),
  Predicted = c(rep(NA,length(y_train)),future2$mean),
  Lower_CI = c(rep(NA,length(y_train)),future2$lower[,1]),
  Upper_CI = c(rep(NA,length(y_train)),future2$upper[,1])
)

# Tracé des courbes avec ggplot2
ggplot(df_plot, aes(x = Index)) +
  geom_line(aes(y = Observed), color = "blue", size = 1, linetype = "solid", alpha = 0.7, label = "Observées") +
  geom_line(aes(y = Predicted), color = "red", size = 1, linetype = "solid", alpha = 0.7, label = "Prédictions") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), fill = "grey", alpha = 0.2, linetype = "solid", label = "Intervalle de confiance") +
  labs(title = "Prédictions ARIMA(5,1,5) avec Intervalle de Confiance",
       x = "Date (année-mois)",
       y = "NB de séisme") +
  scale_color_manual(values = c("blue", "red")) +
  theme_economist() + theme+
  theme(legend.position = "bottom") +
  scale_fill_manual(values = "grey")


data = read.csv("data_TRAIN_clean_feature_engineering.csv")
library(dplyr)
# Convertir la variable 'date' en type Date
data$date <- as.Date(data$date)

# Utiliser dplyr pour filtrer les observations
filtered_data <- data %>%
  group_by(date) %>%
  filter(mag == max(mag)) %>%
  distinct(date, .keep_all = TRUE)


acf(diff(filtered_data$mag))
pacf(diff(filtered_data$mag))


# Appliquer un ordre de différenciation
filtered_data$diff_mag <- c(NA, diff(filtered_data$mag))

filtered_data$date = as.Date(filtered_data$date)

# Modèles
arima_model <- arima(filtered_data$mag, order = c(4, 1, 5))
arima_model_2 <- arima(filtered_data$mag, order = c(2, 1, 1))
arima_model_3 <- arima(filtered_data$mag, order = c(5, 1, 0))


df_plot <- data.frame(
  Index = seq_along(filtered_data$mag[1:40]),
  Observed = filtered_data$mag[1:40],
  ARIMA_MODEL_1 = fitted(arima_model)[1:40],
  ARIMA_Model_2 = fitted(arima_model_2)[1:40],
  ARIMA_Model_3 = fitted(arima_model_3)[1:40]
)

library(reshape2)
# Melt du dataframe
melted_df <- melt(df_plot, id.vars = "Index", variable.name = "Nom_Model", value.name = "Value")

# Modification des noms des modèles dans le dataframe fondu
melted_df <- melted_df %>%
  mutate(Nom_Model = case_when(
    Nom_Model == "Observed" ~ "Observées",
    Nom_Model == "ARIMA_MODEL_1" ~ "ARIMA(4,1,5)",
    Nom_Model == "ARIMA_Model_2" ~ "ARIMA(2,1,1)",
    Nom_Model == "ARIMA_Model_3" ~ "ARIMA(5,1,0)",
    TRUE ~ as.character(Nom_Model)
  ))

# Tracé des courbes
ggplot(melted_df, aes(x = Index, y = Value, color = Nom_Model)) +
  geom_line(size = 1) +
  labs(title = "Ajustements ARIMA aux données", color = "Modèle", x = "Index", y = "Magnitude") +
  scale_color_manual(values = c("Observées" = "blue", "ARIMA(4,1,5)" = "brown", "ARIMA(2,1,1)" = "green", "ARIMA(5,1,0)" = "violet")) +
  theme_economist() + theme




#-------------------------------------------------------------------------------------------
#------------GARCH-----------------------

data = read.csv("data_TRAIN_clean_feature_engineering.csv")
library(dplyr)


# Convertir la variable 'date' en type Date
data$year_month  <- as.Date(data$year_month )

y = data$Count

# Créer un objet de la série temporelle
ts_data <- ts(y, frequency = 1)

# Supprimer les valeurs manquantes
ts_data <- na.omit(ts_data)

library(rugarch)


s1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0)),distribution.model="norm")
m1 <- ugarchfit(data = ts_data, spec = s1)
m1

plot(m1, which = 'all')


s2 <-  ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(1,2)),distribution.model="norm")
m2 <- ugarchfit(data = ts_data, spec = s2)
m2

# Supprimer les valeurs manquantes
ts_data <- na.omit(ts_data)


order_arma = c(2,2)

# Spécifier les ordres potentiels (p, q)
orders <- expand.grid(p = 1:5, q = 1:5)

# Stocker les résultats
results <- matrix(NA, nrow = nrow(orders), ncol = 6,
                  dimnames = list(NULL, c("p", "q", "AIC", "BIC", "Shibata", "Hannan-Quinn")))

# Boucle sur les combinaisons d'ordres
for (i in 1:nrow(orders)) {
  p <- orders[i, "p"]
  q <- orders[i, "q"]
  
  # Spécifier le modèle GARCH
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
                     mean.model = list(armaOrder = order_arma),
                     distribution.model = "norm")
  
  # Ajuster le modèle GARCH
  fit <- try(ugarchfit(data = ts_data, spec = spec, solver.control = list(tol = 1e-6, maxeval = 10000)), silent = TRUE)
  
  # Stocker les résultats
  if (!inherits(fit, "try-error")) {
    results[i, ] <- c(p, q, infocriteria(fit)[1], infocriteria(fit)[2], infocriteria(fit)[3], infocriteria(fit)[4])
  } else {
    results[i, ] <- c(p, q, NA, NA, NA, NA)
  }
}

results <-  as.data.frame(results)

# Sélectionner le modèle avec le plus petit AIC
best_model_aic <- subset(results,results$AIC==min(results$AIC))

# Sélectionner le modèle avec le plus petit BIC
best_model_bic <- subset(results,results$BIC==min(results$BIC))

# Sélectionner le modèle avec le plus petit score Shibata
best_model_shibata <- subset(results,results$Shibata==min(results$Shibata))

# Sélectionner le modèle avec le plus petit score Hannan-Quinn
best_model_hq <- subset(results,results$`Hannan-Quinn`==min(results$`Hannan-Quinn`))

{
  # Afficher les résultats
  print("Meilleur modèle basé sur le critère AIC :")
  print(best_model_aic)
  
  print("Meilleur modèle basé sur le critère BIC :")
  print(best_model_bic)
  
  print("Meilleur modèle basé sur le critère Shibata :")
  print(best_model_shibata)
  
  print("Meilleur modèle basé sur le critère Hannan-Quinn :")
  print(best_model_hq)
}

#---------------
#arma : (0,0)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 23 3 5 11.59672 11.6992 11.59542     11.63736
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 23 3 5 11.59672 11.6992 11.59542     11.63736
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 23 3 5 11.59672 11.6992 11.59542     11.63736
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 23 3 5 11.59672 11.6992 11.59542     11.63736


#----------
#ARMA : (1,0)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.45474 11.56747 11.45317     11.49944
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.45474 11.56747 11.45317     11.49944
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.45474 11.56747 11.45317     11.49944
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.45474 11.56747 11.45317     11.49944

#------------
#ARMA : (0,1)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.51723 11.62996 11.51567     11.56194
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 12 2 3 11.54751 11.6295 11.54667     11.58002
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.51723 11.62996 11.51567     11.56194
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.51723 11.62996 11.51567     11.56194

#------------
#ARMA : (1,1)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q     AIC      BIC  Shibata Hannan-Quinn
# 15 5 3 11.3597 11.48268 11.35785     11.40847
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q     AIC      BIC  Shibata Hannan-Quinn
# 15 5 3 11.3597 11.48268 11.35785     11.40847
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q     AIC      BIC  Shibata Hannan-Quinn
# 15 5 3 11.3597 11.48268 11.35785     11.40847
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q     AIC      BIC  Shibata Hannan-Quinn
# 15 5 3 11.3597 11.48268 11.35785     11.40847

#------------
#ARMA : (2,1)
#nan

#------------
#ARMA : (1,2)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 22 2 5 11.21829 11.34127 11.21643     11.26706
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 22 2 5 11.21829 11.34127 11.21643     11.26706
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 22 2 5 11.21829 11.34127 11.21643     11.26706
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 22 2 5 11.21829 11.34127 11.21643     11.26706

#------------
#ARMA : (2,2)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q     AIC      BIC  Shibata Hannan-Quinn
# 24 4 5 11.3214 11.47512 11.31853     11.38236
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.32425 11.46772 11.32174     11.38115
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q     AIC      BIC  Shibata Hannan-Quinn
# 24 4 5 11.3214 11.47512 11.31853     11.38236
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 23 3 5 11.32425 11.46772 11.32174     11.38115


#-------------
#Conclusion : modèle retenu
# ARMA(1,2) - GARCH(2,5)
# AIC : 11.21829
# BIC : 11.34127

# ARMA(1,1) - GARCH(5,3)
# AIC : 11.3597
# BIC : 11.48268

s1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,5)),
                 mean.model=list(armaOrder=c(1,2)),distribution.model="norm")
m1 <- ugarchfit(data = ts_data, spec = s1)
m1

s2 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(5,3)),
                 mean.model=list(armaOrder=c(1,1)),distribution.model="norm")
m2 <- ugarchfit(data = ts_data, spec = s2)
m2


model_final_arima = m1
spec = getspec(m2)
forecast1 = ugarchforecast(m1, n.ahead = 12)
forecast2 = ugarchforecast(m2, n.ahead = 12)
forecast1



data_test = read.csv("data_TEST_clean_feature_engineering.csv")
y_test = data_test$Count

# Effectuer les prédictions à l'horizon 31 avec le modèle ARIMA
predictions <- as.vector(sigma(forecast1)[,1])

# Calculer les métriques de performance (par exemple, RMSE)
rmse <- sqrt(mean((y_test - predictions)^2))

# Afficher les prédictions et les métriques de performance
print("Prédictions :")
print(predictions)

print("Métriques de performance :")
print(paste("RMSE :", rmse))



data_train = read.csv("data_TRAIN_clean_feature_engineering.csv")
data_train = tail(data_train,10)
y_train = data_train$Count
date_train = data_train$year_month


# Fusionner les données de test avec les prédictions
df_plot <- data.frame(
  Index = c(as.Date(date_train),as.Date(data_test$year_month)),
  Observed = c(y_train,y_test),
  GARCH_1 = c(rep(NA,length(y_train)),as.vector(sigma(forecast1)[,1])),
  GARCH_2 = c(rep(NA,length(y_train)),as.vector(sigma(forecast2)[,1]))
)


library(reshape2)
# Melt du dataframe
melted_df <- melt(df_plot, id.vars = "Index", variable.name = "Nom_Model", value.name = "Value")

# Modification des noms des modèles dans le dataframe fondu
melted_df <- melted_df %>%
  mutate(Nom_Model = case_when(
    Nom_Model == "Observed" ~ "Observées",
    Nom_Model == "GARCH_1" ~ "GARCH_1(2,5)-ARMA(1,2)",
    Nom_Model == "GARCH_2" ~ "GARCH_1(5,3)-ARMA(1,1)",
    TRUE ~ as.character(Nom_Model)
  ))

# Tracé des courbes
ggplot(melted_df, aes(x = Index, y = Value, color = Nom_Model)) +
  geom_line(size = 1) +
  labs(title = "Prévision GARCH", color = "Modèle", x = "Date (année-mois)", y = "Nombre de séisme") +
  scale_color_manual(values = c("Observées" = "blue", "GARCH_1(2,5)-ARMA(1,2)" = "brown", "GARCH_1(5,3)-ARMA(1,1)" = "green")) +
  theme_economist() + theme



library(ggplot2)
library(reshape2)
library(dplyr)
library(plotly)


# Créer un dataframe avec les données observées et ajustées
df_plot <- data.frame(
  Index = data$year_month,
  Observed = y,
  GARCH_1 = as.vector(fitted(m1)[,1]),
  GARCH_2 = as.vector(fitted(m2)[,1])
)

# Melt du dataframe
melted_df <- melt(df_plot, id.vars = "Index", variable.name = "Model", value.name = "Value")

# Modification des noms des modèles dans le dataframe fondu
melted_df <- melted_df %>%
  mutate(Model = case_when(
    Model == "Observed" ~ "Observées",
    Model == "GARCH_1" ~ "GARCH_1(2,5)-ARMA(1,2)",
    Model == "GARCH_2" ~ "GARCH_1(5,3)-ARMA(1,1)",
    TRUE ~ as.character(Model)
  ))

# Créer le graphique interactif avec plotly
plot_ly(data = melted_df, x = ~Index, y = ~Value, color = ~Model, type = 'scatter', mode = 'lines', line = list(width = 1)) %>%
  layout(title = "Ajustements GARCH aux données", xaxis = list(title = "Temps(mois)"), yaxis = list(title = "Nombre de séisme")) %>%
  colorbar(title = "Modèle")

# Récupérer les résidus pour chaque modèle GARCH
residuals_garch_model_1 <- residuals(m1)
residuals_garch_model_2 <- residuals(m2)

# Effectuer le test de Ljung-Box pour chaque modèle GARCH
ljung_box_test_garch_model_1 <- Box.test(residuals_garch_model_1, lag = 20, type = "Ljung-Box")
ljung_box_test_garch_model_2 <- Box.test(residuals_garch_model_2, lag = 20, type = "Ljung-Box")

# Créer un dataframe pour les résultats du test de Ljung-Box
ljung_box_results <- data.frame(
  Model = c("GARCH_Model_1", "GARCH_Model_2"),
  p_value = c(ljung_box_test_garch_model_1$p.value, ljung_box_test_garch_model_2$p.value)
)

# Afficher les résultats du test de Ljung-Box
print(ljung_box_results)


#----------------------------ETAS-------------------------
data = read.csv("data_TRAIN_clean_feature_engineering.csv")
library(dplyr)


# Convertir la variable 'date' en type Date
data$year_month  <- as.Date(data$year_month )

y = data$Count

# Créer un objet de la série temporelle
ts_data <- ts(y, frequency = 1)

# Supprimer les valeurs manquantes
ts_data <- na.omit(ts_data)


library(keras)

# Préparation des données
window_size <- 10  # Taille de la fenêtre temporelle pour la prédiction
ts_data <- as.matrix(ts_data)

# Créer les données d'entrée et de sortie
X <- matrix(0, nrow = length(ts_data) - window_size, ncol = window_size)
Y <- numeric(length(ts_data) - window_size)

for (i in 1:(length(ts_data) - window_size)) {
  X[i, ] <- ts_data[i:(i + window_size - 1)]
  Y[i] <- ts_data[i + window_size]
}

# Réorganiser les données pour l'entrée du modèle LSTM
X <- array_reshape(X, c(dim(X), 1))

# Définir le modèle LSTM
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(window_size, 1)) %>%
  layer_dense(units = 1)

# Compiler le modèle
model %>% compile(
  optimizer = optimizer_adam(),
  loss = "mean_squared_error"
)

# Entraîner le modèle
model %>% fit(
  x = X,
  y = Y,
  epochs = 50,
  batch_size = 32
)

# Prédire avec le modèle
predictions <- model %>% predict(X)

# Tracer les prédictions
plot(ts_data, type = "l", col = "blue", main = "Prédictions avec LSTM")
lines((length(ts_data) - window_size + 1):length(ts_data), predictions, col = "red", lty = 2)
legend("topright", legend = c("Observations", "Prédictions"), col = c("blue", "red"), lty = 1:2)


# Utilisation de SHINY
# Le déploiement sur Shiny.io n'a pas pu être possible car notre jeu de données data_clean.csv est trop volumineux
data = read.csv("data_clean.csv")
test = subset(data,data$mag < -4)

str(data)

data = subset(data,data$state=="Alaska")
data$year <- as.numeric(format(as.Date(data$date), "%Y"))

#data  = subset(data,data$year==2023)
# Load libraries
library(leaflet)
library(leaflet.extras)

# Load libraries
library(leaflet)
library(shiny)
library(dplyr)

# Supposons que 'data' soit votre dataframe
# Assurez-vous que 'latitude', 'longitude', 'mag' et 'date' sont des colonnes dans votre dataframe

# Création de l'application Shiny
# Convertir la colonne 'date' en format de date
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Création de l'application Shiny
shinyApp(
  ui = fluidPage(
    leafletOutput("map"),
    sliderInput("date_range", "Sélectionnez la plage de dates",
                min = min(data$date), max = max(data$date),
                value = c(min(data$date), max(data$date)),
                timeFormat = "%Y-%m-%d"),
    br(),
    tags$style("#map {height: calc(100vh - 150px) !important;}")
  ),
  
  server = function(input, output) {
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
        addTiles() %>%
        addHeatmap(lng = data$longitude, lat = data$latitude,
                   intensity = data$mag, blur = 5, radius = 2) %>%
        addLegend(position = "bottomright",
                  pal = colorNumeric(palette = viridisLite::viridis(5), domain = (data$mag)),
                  values = (data$mag),
                  opacity = 1,
                  title = "Magnitude",
                  layerId = "unique_legend_id")
    })
    
    observe({
      filtered_data <- data %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
      
      # Supprimer la légende existante
      leafletProxy("map") %>%
        removeControl("unique_legend_id")
      
      # Ajouter la nouvelle légende
      leafletProxy("map") %>%
        addLegend(position = "bottomright",
                  pal = colorNumeric(palette = viridisLite::viridis(5), domain = (filtered_data$mag)),
                  values = (filtered_data$mag),
                  opacity = 1,
                  title = "Magnitude",
                  layerId = "unique_legend_id")
      
      # Mettre à jour la couche de heatmap
      leafletProxy("map") %>%
        clearHeatmap() %>%
        addHeatmap(lng = filtered_data$longitude, lat = filtered_data$latitude,
                   intensity = (filtered_data$mag), blur = 5, radius =2)
    })
  }
)





data = read.csv("data_TRAIN_clean_feature_engineering.csv")
library(dplyr)


# Convertir la variable 'date' en type Date
data$year_month  <- as.Date(data$year_month )

y = data$Count

# Créer un objet de la série temporelle
ts_data <- ts(y, frequency = 1)

# Supprimer les valeurs manquantes
ts_data <- na.omit(ts_data)

library(rugarch)


s1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0)),distribution.model="norm")
m1 <- ugarchfit(data = ts_data, spec = s1)
m1

plot(m1, which = 'all')


#--------------------------------------------------------------------



#Order arma à faire varier à chaque fois
order_arma = c(2,3)

# Spécifier les ordres potentiels (p, q)
orders <- expand.grid(p = 1:10, q = 1:10)

# Stocker les résultats
results <- matrix(NA, nrow = nrow(orders), ncol = 6,
                  dimnames = list(NULL, c("p", "q", "AIC", "BIC", "Shibata", "Hannan-Quinn")))

# Boucle sur les combinaisons d'ordres
for (i in 1:nrow(orders)) {
  p <- orders[i, "p"]
  q <- orders[i, "q"]
  
  # Spécifier le modèle GARCH
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)),
                     mean.model = list(armaOrder = order_arma),
                     distribution.model = "norm")
  
  # Ajuster le modèle GARCH
  fit <- try(ugarchfit(data = ts_data, spec = spec, solver.control = list(tol = 1e-6, maxeval = 10000)), silent = TRUE)
  
  # Stocker les résultats
  if (!inherits(fit, "try-error")) {
    results[i, ] <- c(p, q, infocriteria(fit)[1], infocriteria(fit)[2], infocriteria(fit)[3], infocriteria(fit)[4])
  } else {
    results[i, ] <- c(p, q, NA, NA, NA, NA)
  }
}

results <-  as.data.frame(results)

# Sélectionner le modèle avec le plus petit AIC
best_model_aic <- subset(results,results$AIC==min(results$AIC))

# Sélectionner le modèle avec le plus petit BIC
best_model_bic <- subset(results,results$BIC==min(results$BIC))

# Sélectionner le modèle avec le plus petit score Shibata
best_model_shibata <- subset(results,results$Shibata==min(results$Shibata))

# Sélectionner le modèle avec le plus petit score Hannan-Quinn
best_model_hq <- subset(results,results$`Hannan-Quinn`==min(results$`Hannan-Quinn`))

{
  # Afficher les résultats
  print("Meilleur modèle basé sur le critère AIC :")
  print(best_model_aic)
  
  print("Meilleur modèle basé sur le critère BIC :")
  print(best_model_bic)
  
  print("Meilleur modèle basé sur le critère Shibata :")
  print(best_model_shibata)
  
  print("Meilleur modèle basé sur le critère Hannan-Quinn :")
  print(best_model_hq)
}

#---------------
#arma : (0,0)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.50254 11.62552 11.50068     11.55131
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC     BIC Shibata Hannan-Quinn
# 8 8 1 11.50597 11.6187 11.5044     11.55067
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.50254 11.62552 11.50068     11.55131
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC     BIC Shibata Hannan-Quinn
# 8 8 1 11.50597 11.6187 11.5044     11.55067


#----------
#ARMA : (1,0)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.23542 11.36865 11.23325     11.28825
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.23542 11.36865 11.23325     11.28825
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.23542 11.36865 11.23325     11.28825
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.23542 11.36865 11.23325     11.28825

#------------
#ARMA : (0,1)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.39015 11.52337 11.38797     11.44298
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.39015 11.52337 11.38797     11.44298
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.39015 11.52337 11.38797     11.44298
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 18 8 2 11.39015 11.52337 11.38797     11.44298

#------------
#ARMA : (1,1)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 28 8 3 11.13666 11.29039 11.13379     11.19763
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 18 8 2 11.14011 11.28358 11.1376       11.197
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 28 8 3 11.13666 11.29039 11.13379     11.19763
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 18 8 2 11.14011 11.28358 11.1376       11.197

#------------
#ARMA : (2,1)
#nan

#------------
#ARMA : (1,2)
#nan

#------------
#ARMA : (2,2)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 29 9 3 11.04339 11.22786 11.0393     11.11655
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 29 9 3 11.04339 11.22786 11.0393     11.11655
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 29 9 3 11.04339 11.22786 11.0393     11.11655
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 29 9 3 11.04339 11.22786 11.0393     11.11655

#------------
#ARMA : (0,2)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 18 8 2 11.31361 11.45709 11.3111     11.37051
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 18 8 2 11.31361 11.45709 11.3111     11.37051
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 18 8 2 11.31361 11.45709 11.3111     11.37051
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC Shibata Hannan-Quinn
# 18 8 2 11.31361 11.45709 11.3111     11.37051

#------------
#ARMA : (3,0)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 18 8 2 11.20618 11.3599 11.20331     11.26714
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 18 8 2 11.20618 11.3599 11.20331     11.26714
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 18 8 2 11.20618 11.3599 11.20331     11.26714
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 18 8 2 11.20618 11.3599 11.20331     11.26714


#------------
#ARMA : (3,1)
#NA

#------------
#ARMA : (3,2)
#NA

#------------
#ARMA : (3,3)
#nan

#------------
#ARMA : (0,3)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 8 8 1 11.27765 11.42112 11.27514     11.33454
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 8 8 1 11.27765 11.42112 11.27514     11.33454
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 8 8 1 11.27765 11.42112 11.27514     11.33454
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 8 8 1 11.27765 11.42112 11.27514     11.33454

#------------
#ARMA : (1,3)
#NAN

#------------
#ARMA : (2,3)
# [1] "Meilleur modèle basé sur le critère AIC :"
# p  q      AIC      BIC  Shibata Hannan-Quinn
# 98 8 10 11.01312 11.26933 11.00539     11.11472
# [1] "Meilleur modèle basé sur le critère BIC :"
# p q      AIC      BIC  Shibata Hannan-Quinn
# 28 8 3 11.03532 11.21979 11.03122     11.10847
# [1] "Meilleur modèle basé sur le critère Shibata :"
# p  q      AIC      BIC  Shibata Hannan-Quinn
# 98 8 10 11.01312 11.26933 11.00539     11.11472
# [1] "Meilleur modèle basé sur le critère Hannan-Quinn :"
# p q      AIC     BIC  Shibata Hannan-Quinn
# 29 9 3 11.02818 11.2229 11.02363      11.1054

#-------------
#-------------
#Conclusion : modèle retenu
# Minimisation de l'AIC
# ARMA(2,3) - GARCH(8,10)
# AIC : 11.01312
# BIC : 11.26933

# ARMA(2,3) - GARCH(8,3)
# AIC : 11.03532
# BIC : 11.21979



# Prévision
s1 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(8,10)),
                 mean.model=list(armaOrder=c(2,3)),distribution.model="norm")
m1 <- ugarchfit(data = ts_data, spec = s1)
m1

#Expérimental
#Les ordres garchs donnez par grid search ont donné sont douteux
#Par conséquent, on tente de créer un modèle GARCH par nous même
#arma : 9 pacf ; 2 arbitraire pour ne pas que ça soit trop élevé
# garch : petit ordre 3 et 2 arbitraire

s2 <- ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(3,2)),
                 mean.model=list(armaOrder=c(9,2)),distribution.model="norm")
m2 <- ugarchfit(data = ts_data, spec = s2)
m2

forecast1 = ugarchforecast(m1, n.ahead = 12)
forecast2 = ugarchforecast(m2, n.ahead = 12)

library(reshape2)
#Ajustement sur la série
# Créer un dataframe avec les données observées et ajustées
df_plot <- data.frame(
  Index = data$year_month,
  Observed = y,
  GARCH_1 = as.vector(fitted(m1)[,1]),
  GARCH_2 = as.vector(fitted(m2)[,1])
)

# Melt du dataframe
melted_df <- melt(df_plot, id.vars = "Index", variable.name = "Model", value.name = "Value")

# Modification des noms des modèles dans le dataframe fondu
melted_df <- melted_df %>%
  mutate(Model = case_when(
    Model == "Observed" ~ "Observées",
    Model == "GARCH_1" ~ "GARCH_1(8,10)-ARMA(2,3)",
    Model == "GARCH_2" ~ "GARCH_1(3,2)-ARMA(9,2)",
    TRUE ~ as.character(Model)
  ))

# Créer le graphique interactif avec plotly
library(plotly)
plot_ly(data = melted_df, x = ~Index, y = ~Value, color = ~Model, type = 'scatter', mode = 'lines', line = list(width = 1)) %>%
  layout(title = "Ajustements GARCH aux données", xaxis = list(title = "Temps(mois)"), yaxis = list(title = "Nombre de séisme")) %>%
  colorbar(title = "Modèle")

# Récupérer les résidus pour chaque modèle GARCH
residuals_garch_model_1 <- residuals(m1)
residuals_garch_model_2 <- residuals(m2)

# Effectuer le test de Ljung-Box pour chaque modèle GARCH
ljung_box_test_garch_model_1 <- Box.test(residuals_garch_model_1, lag = 20, type = "Ljung-Box")
ljung_box_test_garch_model_2 <- Box.test(residuals_garch_model_2, lag = 20, type = "Ljung-Box")

# Créer un dataframe pour les résultats du test de Ljung-Box
ljung_box_results <- data.frame(
  Model = c("GARCH_Model_1", "GARCH_Model_2"),
  p_value = c(ljung_box_test_garch_model_1$p.value, ljung_box_test_garch_model_2$p.value)
)

# Afficher les résultats du test de Ljung-Box
print(ljung_box_results)


#Prévision sur les 12 mois prochain
data_test = read.csv("data_TEST_clean_feature_engineering.csv")
y_test = data_test$Count

data_train = read.csv("data_TRAIN_clean_feature_engineering.csv")
data_train = tail(data_train,10)
y_train = data_train$Count
date_train = data_train$year_month


# Fusionner les données de test avec les prédictions
df_plot <- data.frame(
  Index = c(as.Date(date_train),as.Date(data_test$year_month)),
  Observed = c(y_train,y_test),
  GARCH_1 = c(rep(NA,length(y_train)),as.vector(fitted(forecast1)[,1])),
  GARCH_2 = c(rep(NA,length(y_train)),as.vector(fitted(forecast2)[,1]))
)


library(reshape2)
# Melt du dataframe
melted_df <- melt(df_plot, id.vars = "Index", variable.name = "Nom_Model", value.name = "Value")

# Modification des noms des modèles dans le dataframe fondu
melted_df <- melted_df %>%
  mutate(Nom_Model = case_when(
    Nom_Model == "Observed" ~ "Observées",
    Nom_Model == "GARCH_1" ~ "GARCH_1(8,10)-ARMA(2,3)",
    Nom_Model == "GARCH_2" ~ "GARCH_1(3,2)-ARMA(9,2)",
    TRUE ~ as.character(Nom_Model)
  ))

# Tracé des courbes
ggplot(melted_df, aes(x = Index, y = Value, color = Nom_Model)) +
  geom_line(size = 1) +
  labs(title = "Ajustements ARIMA aux données", color = "Modèle", x = "Index", y = "Magnitude") +
  scale_color_manual(values = c("Observées" = "blue", "GARCH_1(8,10)-ARMA(2,3)" = "brown", "GARCH_1(3,2)-ARMA(9,2)" = "green")) +
  theme_economist() + theme

# Modèle grid search
data_test = read.csv("data_TEST_clean_feature_engineering.csv")
y_test = data_test$Count

# Effectuer les prédictions à l'horizon 12 avec le modèle ARIMA
predictions <- as.vector(sigma(forecast1)[,1])

# Calculer les métriques de performance (par exemple, RMSE)
rmse <- sqrt(mean((y_test - predictions)^2))

# Afficher les prédictions et les métriques de performance
print("Prédictions :")
print(predictions)

print("Métriques de performance :")
print(paste("RMSE :", rmse))

# Modèle expérimental
data_test = read.csv("data_TEST_clean_feature_engineering.csv")
y_test = data_test$Count

# Effectuer les prédictions à l'horizon 12 avec le modèle ARIMA
predictions <- as.vector(sigma(forecast2)[,1])

# Calculer les métriques de performance (par exemple, RMSE)
rmse <- sqrt(mean((y_test - predictions)^2))

# Afficher les prédictions et les métriques de performance
print("Prédictions :")
print(predictions)

print("Métriques de performance :")
print(paste("RMSE :", rmse))