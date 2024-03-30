---
title: "M565 Exam2_Part2"
author: "Neeraj Namani"
date: '2024-03-15'
output: pdf_document
---


```{r}
Data <- read.csv("~/Downloads/PlantData.txt", sep = "",
                 stringsAsFactors = TRUE)
head(Data)
```
Exploratory Data Analysis:

```{r}
# Data Frame Description

str(Data)
```
```{r}
# Displaying Summary Statistics for all variables

summary(Data)
```
```{r}
sapply(Data, is.factor)
```

Here, we can see that all columns are "FALSE", it means that there are no categorical variables.

```{r}
# Check for any missing values

missing_values <- colSums(is.na(Data))
missing_values
```

Here, there are no missing values in the data.

```{r}
# Find the unique value count for all the columns in the data to know whether there are any categorical variables

unique_counts <- sapply(Data, function(x) length(unique(x)))
unique_counts
```


```{r}
library(GGally)

library(ggplot2)
```

```{r}
# Graphical display of data

ggpairs(Data)
```

```{r}

# Distribution of Numeric Variables
# Histogram for all numeric variables


hist(Data$Elev, main = "Distribution of Elevations", xlab = "Elevation")
hist(Data$NR, main = "Distribution of NR", xlab = "NR")
hist(Data$Area, main = "Distribution of Area", xlab = "Area")
hist(Data$Soil, main = "Distribution of Soil", xlab = "Soil")
hist(Data$Years, main = "Distribution of Years", xlab = "Years")
hist(Data$Deglac, main = "Distribution of Deglac", xlab = "Deglac")
hist(Data$Human.pop, main = "Distribution of Human Population", xlab = "Human pop")

# Boxplot for all numeric variables

boxplot(Data$Area, main = "Boxplot of Areas", ylab = "Area")
boxplot(Data$NR, main = "Boxplot of NR", ylab = "NR")
boxplot(Data$Elev, main = "Boxplot of Elev", ylab = "Elev")
boxplot(Data$Soil, main = "Boxplot of Soil", ylab = "Soil")
boxplot(Data$Years, main = "Boxplot of Years", ylab = "Years")
boxplot(Data$Deglac, main = "Boxplot of Deglac", ylab = "Deglac")
boxplot(Data$Human.pop, main = "Boxplot of Human pop", ylab = "Human pop")

# Boxplot for Distance in Data
boxplot(Data$Dist, main = "Boxplot of Distance", ylab = "Dist")

```
Let us construct the Correlation matrix for all numeric variables present in the dataset.

```{r}
# Correlation matrix for numeric variables
cor_matrix <- cor(Data[,sapply(Data, is.numeric)])
print(cor_matrix)

# Plot the correlation matrix
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust")
```
```{r}
w <- 1 # constant weight
Data$Human.pop <- Data$Human.pop + w 
head(Data)
```


Diagnostics:

Human.pop is skewed to the right. Taking log may make it more symmetric. 
Let us look at the distribution of log(Human.pop)

```{r}
hist(log(Data$Human.pop))
```

This does not make it symmetric. So, let us try to transform Human.pop with log(1+x).

```{r}
Data$Human.pop <- log1p(Data$Human.pop)
hist(Data$Human.pop)
```

```{r}
summary(Data$Human.pop)
```

This seems like it worked better. So let us transform the Human.pop using log(1+x).

```{r}
# Compare relationships of NR with Area because it has more correlation with NR



plot(NR~Area,Data, main = "NR vs Area")
plot(log(NR)~Area,Data, main = "log(NR) vs Area")
plot(log(NR)~log(Area),Data, main = "log(NR) vs log(Area)")
```
Modelling:

Full Model:

```{r}
model <- lm(NR~Area + Latitude + Elev + Dist + Soil + Years + Deglac + Human.pop, data = Data)
summary(model)
```

From this, we can say that Latitude, Dist, Years, Deglac and Human.pop are not significant due to their high p-value.

```{r}
model1 <- lm(NR ~ Area + Elev + Soil, data = Data)
summary(model1)
```
Let us include Human.pop


```{r}
model2 <- lm(NR ~ Area + Elev + Soil + Human.pop, data = Data)
summary(model2)
```
Let us try transforming Area with log.

```{r}
model3 <- lm(NR ~ log(Area) + Elev + Soil + Human.pop, data = Data)
summary(model3)
```
Let us try the log transformation on the full model with every predictor variables
```{r}
model4 <- lm(log(NR) ~ log(Area) + log(Latitude) + log(Elev) + log(Dist) + log(Soil) +log(Years) + log(Deglac) + log(Human.pop), data = Data)
summary(model4)
```

Let us try some interactions

```{r}
model5 <- lm(NR ~ Area * Elev + Soil + Human.pop, data = Data)
summary(model5)
```
```{r}
r_squared_model1 <- summary(model1)$r.squared
r_squared_model2 <- summary(model2)$r.squared
r_squared_model3 <- summary(model3)$r.squared
r_squared_model4 <- summary(model4)$r.squared
r_squared_model5 <- summary(model5)$r.squared

cat("R-Squared values\nModel1: ",r_squared_model1,"\nModel2: ",r_squared_model2,
    "\nModel3: ",r_squared_model3,"\nModel4: ",r_squared_model4,"\nModel5: ",r_squared_model5)
```
from all these models, our best model based on R2 value is model4 which is model4 <- lm(log(NR) ~ log(Area) + log(Latitude) + log(Elev) + log(Dist) + log(Soil) +log(Years) + log(Deglac) + log(Human.pop), data = Data)

So we can say that this is our best model.

```{r}
plot(model4)
```
Let us try some other transformations

```{r}
# polynomial transformation

model6 <- lm(NR ~ Area + Latitude + Elev + poly(Dist,2) + Soil + Years + Deglac + Human.pop, data = Data)
summary(model6)

#Square Root Transformation

model7 <- lm(NR ~ Area + Latitude + Elev + Dist + sqrt(Soil) + Years + Deglac + Human.pop, data = Data)
summary(model7)

# Cube Root transformation

model8 <- lm(NR ~ Area + Latitude + I(Elev^(1/3)) + Dist + Soil + Years + Deglac + Human.pop, data = Data)
summary(model8)

# sine transformation

model9 <- lm(NR ~ Area + Latitude + Elev + Dist + Soil + sin(Years) + Deglac + Human.pop, data = Data)
summary(model9)
```


Model Selection:

Train Test Split:

```{r}
# Create an index for the training. and testing sets

split_index <- sample(1:nrow(Data), nrow(Data)*0.7) # Adjust the split ratio as needed

# Create training and testing datasets

train_data <- Data[split_index,]
test_data <- Data[-split_index,]

# Backwards Selection

direction = "backward"

bsel <- step(model, trace = 0) # By AIC
formula(bsel)

# NR ~ Area + Elev + Soil

Criteria <- function(model){
  out <- data.frame(`p+1`=length(model$coef),
                    R2adj = summary(model)$adj,
                    AIC = AIC(model),
                    BIC = BIC(model))
  return(out)
}

rbind(
  bsel = Criteria(bsel)
)
```

All Subsets Regression model

```{r}
library(leaps)
model_sub <- regsubsets(NR ~ Area + Latitude + Elev + Dist + Soil + Years + Deglac + Human.pop, nbest = 3, really.big = TRUE, nvmax = 10, data = train_data)
summary(model_sub)
```

Check the results

```{r}
sbest <- summary(model_sub)

names(sbest)
```

```{r}
cbind(sbest$which, sbest$cp)
```

```{r}
cbind(sbest$which, sbest$adjr2)
```

```{r}
cbind(sbest$which, sbest$bic)
```

```{r}
cbind(sbest$which, sbest$aic)
```

Best model aic, bic,Cp

```{r}
mybestmodel <- function(Xnames, Yname, dataset, p, crit = "bic"){
  if(crit == "Cp"){
    n <- dim(dataset)[1]
    
    fullMSE = summary(lm(as.formula(paste(Yname,"~.")), data = dataset))$sigma^2
  }
  varsel <- lapply(0:p, function(x) combn(p,x))
  
  modcrit <- numeric(p); 
  form <- character(p)
  
  for(k in 1:p){
    s <- dim(varsel[[k+1]])[2]
    tempform <- character(s); tempcrit <- numeric(s)
    
    for(j in 1:s){
      temp <- Xnames[varsel[[k+1]][,j]]
      tempform[j] <- ifelse(length(temp)>1,
                            paste(temp, collapse = "+"), temp)
      tempform[j] <- paste(Yname, tempform[j], sep = '~')
      tempmod <- lm(as.formula(tempform[j]), data = dataset)
      if (crit == "aic"){
        tempcrit[j] <- AIC(tempmod)
      }
      if (crit == "bic"){
        tempcrit[j] <- BIC(tempmod)
      }
      if (crit == "r2"){
        tempcrit[j] <- summary(tempmod)$adj
      }
      if(crit=="Cp"){
        tempcrit[j] <- sum(tempmod$res^2)/fullMSE+2*(k+1)-n
      }
    }
    # best model of size k
    if(crit %in% c("aic", "bic")){
      best <- which.min(tempcrit)
    }
    if(crit == "r2"){
      best <- which.max(tempcrit)
    }
    if(crit == "Cp"){
      best <- which.min(abs(tempcrit[j]-(k+1)))
    }
    form[k] <- tempform[best]
    modcrit[k] <- tempcrit[best]
  }
  if(crit %in% c("aic","bic")){
    out <- form[which.min(modcrit)]
  }
  if(crit == "r2"){
    out <- form[which.max(modcrit)]
  }
  if(crit == "Cp"){
    out <- form[which.min(abs(modcrit[-p]-(2:p)))]
  }
  return(out)
}

p <- length(names(train_data))-1
Xnames <- names(train_data)[-1]
Yname <- "NR"
dataset <- train_data
form <- mybestmodel(Xnames, Yname, train_data, p, crit = "bic")
form
```

```{r}
bicform <- mybestmodel(Xnames, Yname, train_data, p, crit = "bic")
modbic <- lm(as.formula(bicform), data = train_data)
aicform <- mybestmodel(Xnames, Yname, train_data, p, crit = "aic")
modaic <- lm(as.formula(aicform), data = train_data)
cpform <- mybestmodel(Xnames, Yname, train_data, p, crit = "Cp")
modCp <- lm(as.formula(cpform), data = train_data)
r2form <- mybestmodel(Xnames, Yname, train_data, p, crit = "r2")
modr2 <- lm(as.formula(r2form), data = train_data)
```

Now finding the best model using backward stepwise selection:

```{r}
modback <- step(lm(model, data = train_data), trace = 0, direction = "backward")
```

Display the adjR2, BIC, Cp, AIC and Bsel for all the models

```{r}
rbind(bicm = Criteria(modbic),
      aicm = Criteria(modaic),
      adjr2m = Criteria(modr2),
      cpm = Criteria(modCp),
      bsel = Criteria(modback)
      )
```

Now use cross validation to select the final model.

```{r}
cv.lm <- function(data, formulae, nfolds = 5) {
  data <- na.omit(data) # remove missing values
  formulae <- sapply(formulae, as.formula)
  n <- nrow(data)
  fold.labels <- sample(rep(1:nfolds, length.out = n))
  mses <- matrix(NA, nrow = nfolds, ncol = length(formulae))
  colnames <- as.character(formulae)
  for(fold in 1:nfolds) {
    test.rows <- which(fold.labels == fold)
    train <- data[-test.rows,]
    test <- data[test.rows,]
    for(form in 1:length(formulae)) {
      current.model <- lm(formula = formulae[[form]], data = train)
      predictions <- predict(current.model, newdata = test)
      test.responses <- eval(formulae[[form]][[2]], envir = test)
      test.errors <- test.responses - predictions
      mses[fold, form] <- mean(test.errors^2)
    }
  }
  return(colMeans(mses))
}

#set.seed(10)
formulae <- c(formula(modbic),
              formula(modaic),
              formula(modr2),
              formula(modCp),
              formula(modback))

mse <- cv.lm(data = train_data, formulae, nfolds = 5)
print(mse)
```

In this seed, models modaic, modbic and modback metrics has the same least error value. Therefore, we select the model modbic is the final model.

Error values in order: modCp > modr2 > modbic = modaic = modback


```{r}
summary(modbic)
```

Weighted Regression

Now, lets perform weighted regression on modbic.

```{r}
par(mfrow = c(2,2))
plot(modbic,1)
plot(modbic,2)
plot(modbic,3)
plot(modbic,5)
```

```{r}
errs <- modbic$residuals
fit <- modbic$fitted.values
plot(abs(errs)~fit)
lomod<-loess(abs(errs)~fit)
inx<-order(fit)
lines(fit[inx],lomod$fitted[inx], col = "red")
```

```{r}
ermod <- lm(abs(errs)~fit)
fit.as.sd <- abs(ermod$fitted.values)
w <- 1/fit.as.sd
wls_model <- lm(modbic, weights = w, data = train_data)
summary(wls_model)
```

```{r}
par(mfrow = c(2,2))
plot(wls_model,1)
plot(wls_model,2)
plot(wls_model,3)
plot(wls_model,5)
```

Prediction Error (Generalization Error)

```{r}
predictions_test <- predict(modback, newdata = test_data)

generalization_error <- mean((test_data$NR - predictions_test)^2)

cat("Generalization Error:", generalization_error, "\n")

predictions_test <- predict(modCp, newdata = test_data)

generalization_error <- mean((test_data$NR - predictions_test)^2)

cat("Generalization Error:", generalization_error, "\n")

predictions_test <- predict(modaic, newdata = test_data)

generalization_error <- mean((test_data$NR - predictions_test)^2)

cat("Generalization Error:", generalization_error, "\n")

predictions_test <- predict(modbic, newdata = test_data)

generalization_error <- mean((test_data$NR - predictions_test)^2)

cat("Generalization Error:", generalization_error, "\n")

predictions_test <- predict(modr2, newdata = test_data)

generalization_error <- mean((test_data$NR - predictions_test)^2)

cat("Generalization Error:", generalization_error, "\n")

predictions_test <- predict(wls_model, newdata = test_data)

generalization_error <- mean((test_data$NR - predictions_test)^2)

cat("Generalization Error:", generalization_error, "\n")
```
Final Model

from the above we can see that wls_model, modback, modaic and modback have the same generalization error.

Let us further investigate the coefficients of these four models.

```{r}
modbic$coefficients
```

```{r}
modaic$coefficients
```

```{r}
modback$coefficients
```

```{r}
wls_model$coefficients
```

We have noticed that the coefficients of all these models are same which indicated that these are the same models. So we can consider any of these as our final model. Let us say modbic is our final model due to less generalization error.

```{r}
# Let us print the model

bicform
```

```{r}
# Let us print the summary of the model

summary(modbic)
```

```{r}
# Let us look at the assessment plots of our final model

plot(modbic)
```
```{r}
# Finding the BIC score of the final model modbic

BIC(modbic)

```

