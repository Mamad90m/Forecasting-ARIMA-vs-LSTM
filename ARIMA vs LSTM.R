
## Libraries
library(readxl)
library(forecast)
library(tseries)
library(imputeTS)
library(funtimes)
library(keras)
library(tensorflow)
library(TSLSTM)
library(tsutils)
library(dplyr)
use_python("C:\\Users\\Mamad\\NEWFOL~2\\python.exe")

## Data
WILL5000PRFC <- read_excel("D:/My Documents/pdf/R/data.R/WILL5000PRFC.xls")
str(WILL5000PRFC)
WIL <- WILL5000PRFC$WILL5000PRFC 


## plot WILSHIRE and logarithm of WILSHIRE
df1 <- data.frame(time = 1:401, values = c(WIL, log(WIL)), type = rep(
  c("Wilshire index", "logarithm of Wilshire index"), each = 401))

ggplot(data = df1, aes(x = time, y = values)) +
  geom_line(size = 0.8, col = "dodgerblue") +
  facet_grid(type ~ ., scales = "free") +
  ggtitle("") + theme(plot.title = 
                        element_text(color="black", face="bold", size=16), 
                      axis.text=element_text(size=5)) + 
  theme_gray(base_size = 15)

## Log transformation
x_t <- log(WIL)

## Splitting data
ts_train <- as.vector(window(x_t, start = 1, end = 348))
sub1 <- as.vector(window(x_t, start = 1, end = 300))
val1 <- as.vector(window(x_t, start = 301, end = 312))
sub2 <- as.vector(window(x_t, start = 1, end = 312))
val2 <- as.vector(window(x_t, start = 313, end = 324))
sub3 <- as.vector(window(x_t, start = 1, end = 324))
val3 <- as.vector(window(x_t, start = 325, end = 336))
sub4 <- as.vector(window(x_t, start = 1, end = 336))
val4 <- as.vector(window(x_t, start = 337, end = 348))
sub5 <- as.vector(window(x_t, start = 1, end = 348))
ts_test <- as.vector(window(x_t, start = 349, end = 401))
n_train <- length(ts_train)
n_test <- length(ts_test)


## Choose ARIMA

grid <- expand.grid(p = 0:4, q = 0:4)
grid <- cbind(grid$p, rep(1, 25), grid$q)

#fold 1

val1_rmse_arima <- NULL

start_time <- Sys.time()

for(i in 1:dim(grid)[1]){
  fit <- arima(sub1, order = c(grid[i, ]), method = "ML")
  p <- forecast(fit, h = 12)
  val1_rmse_arima[i] <- sqrt(mean((exp(val1) - exp(p$mean)) ^ 2))
}
end_time <- Sys.time()
end_time - start_time

data.frame(p = grid[, 1], d = grid[, 2], q = grid[, 3], 
           ValidationRmseARIMA = val1_rmse_arima)

plot(val1_rmse_arima, type = "b", lwd = 2)
points(which(val1_rmse_arima == min(val1_rmse_arima)), 
       val1_rmse_arima[which(val1_rmse_arima == 
                               min(val1_rmse_arima))], col = "red", pch = 20, cex = 2)

#fold 2

val2_rmse_arima <- NULL
start_time <- Sys.time()
for(i in 1:dim(grid)[1]){
  fit <- arima(sub2, order = c(grid[i, ]), method = "ML")
  p <- forecast(fit, h = 12)
  val2_rmse_arima[i] <- sqrt(mean((exp(val2) - exp(p$mean)) ^ 2))
}
end_time <- Sys.time()
end_time - start_time

data.frame(p = grid[, 1], d = grid[, 2],q = grid[, 3], 
           ValidationRmseARIMA = val2_rmse_arima)

plot(val2_rmse_arima, type = "b", lwd = 2)
points(which(val2_rmse_arima == min(val2_rmse_arima)), 
       val2_rmse_arima[which(val2_rmse_arima == 
                               min(val2_rmse_arima))], col = "red", pch = 20, cex = 2)

#fold 3

val3_rmse_arima <- NULL
start_time <- Sys.time()
for(i in 1:dim(grid)[1]){
  fit <- arima(sub3, order = c(grid[i, ]), method = "ML")
  p <- forecast(fit, h = 12)
  val3_rmse_arima[i] <- sqrt(mean((exp(val3) - exp(p$mean)) ^ 2))
}
end_time <- Sys.time()
end_time - start_time

data.frame(p = grid[, 1], d = grid[, 2],q = grid[, 3], 
           ValidationRmseARIMA = val3_rmse_arima)

plot(val3_rmse_arima, type = "b", lwd = 2)
points(which(val3_rmse_arima == min(val3_rmse_arima)), 
       val3_rmse_arima[which(val3_rmse_arima == 
                               min(val3_rmse_arima))], col = "red", pch = 20, cex = 2)


#fold 4

val4_rmse_arima <- NULL
start_time <- Sys.time()
for(i in 1:dim(grid)[1]){
  fit <- arima(sub4, order = c(grid[i, ]), method = "ML")
  p <- forecast(fit, h = 12)
  val4_rmse_arima[i] <- sqrt(mean((exp(val4) - exp(p$mean)) ^ 2))
}
end_time <- Sys.time()
end_time - start_time

data.frame(p = grid[, 1], d = grid[, 2],q = grid[, 3], 
           ValidationRmseARIMA = val4_rmse_arima)

plot(val4_rmse_arima, type = "b", lwd = 2)
points(which(val4_rmse_arima == min(val4_rmse_arima)), 
       val4_rmse_arima[which(val4_rmse_arima == 
                               min(val4_rmse_arima))], col = "red", pch = 20, cex = 2)


validation_rmse_arima2 <- rbind(val1_rmse_arima, val2_rmse_arima, val3_rmse_arima, val4_rmse_arima)

opt_order <- grid[which(colMeans(validation_rmse_arima2) == 
                          min(colMeans(validation_rmse_arima2))), ]

## Checking the adequacy of selected ARIMA
fit <- arima(x_t, order = opt_order)
Box.test(residuals(fit))

p <- ggtsdisplay(residuals(fit), plot.type = "histogram")
p <- p + labs(y = "value", x = "residuals")
print(p, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))

## walk-forward ARIMA

pred_arima <- NULL
start_time <- Sys.time()
for(i in 1:n_test){
  history <- as.vector(window(x_t, start = 1, end = n_train + i - 1))
  fit <- arima(history, order = opt_order)
  p <- forecast(fit, h = 1)
  pred_arima[i] <- p$mean
}
end_time <- Sys.time()
end_time - start_time


cbind(exp(ts_test), exp(pred_arima), difference = abs(exp(ts_test) - exp(pred_arima)))
plot.ts(exp(ts_test), type = "o", lwd = 2, cex = 1, ylim = c(25000, 55000))
lines(exp(pred_arima), col = "red", type = "o", lwd = 2, cex = 1, lty = 2)

rmse_arima <- sqrt(mean((exp(pred_arima) - exp(ts_test)) ^ 2))
mae_arima <- mean(abs(exp(ts_test) - exp(pred_arima)))
mape_arima <- sum(abs(exp(ts_test) - exp(pred_arima)) / exp(ts_test)) * 100 / n_test
out <- cbind(actual_test = exp(ts_test), pred_test = exp(pred_arima), rmse_arima, mape_arima, mae_arima)


## LSTM function

LSTM <- function(ts, xreg = NULL, tsLag, xregLag = 0, LSTMUnits, DropoutRate = 0.00, Epochs = 10, CompLoss = "mse", CompMetrics = "mae",
                 ActivationFn = 'tanh', SplitRatio = 0.8, ValidationSplit = 0.1)
{
  
  ### Lag selection################################
  
  ## data matrix preparation
  
  feature_mat <- NULL
  if (is.null(xreg)){
    lag_y <- lagmatrix(as.ts(ts), lag = c(0:(tsLag)))
    all_feature <- cbind(lag_y, feature_mat)
  } else {
    exo <- xreg
    exo_v <- dim((exo))[2]
    for (var in 1:exo_v) {
      lag_x <- lagmatrix(as.ts(exo[, var]), lag = c(0:xregLag))
      feature_mat <- cbind(feature_mat, lag_x)
    }
    lag_y <- lagmatrix(as.ts(ts), lag = c(0:(tsLag)))
    all_feature <- cbind(lag_y, feature_mat)
  }
  if(xregLag >= tsLag){
    data_all <- all_feature[-c(1:xregLag), ]
  } else {
    data_all <- all_feature[-c(1:tsLag), ]
  }
  data <- data_all[ ,-1]
  feature <- ncol(data)
  
  a <- 1/(max(data[, 1]) - min(data[, 1]))
  b <- min(data[, 1]) / (max(data[ ,1]) - min(data[, 1]))
  
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  denormalize <- function(x) {
    return ((x + b) / a)
  }
  data_normalise <- apply(data, 2, normalize)
  
  ##################### Data Split #################################
  
  train_normalise <- data_normalise[c(1:(nrow(data_normalise) * SplitRatio)), ]
  test_normalise<- data_normalise[-c(1:(nrow(data_normalise) * SplitRatio)), ]
  
  n_train <- nrow(train_normalise)
  n_test <- nrow(test_normalise)
  
  #Data Array Preparation
  train_data <- timeseries_generator(data = train_normalise,targets = train_normalise[,1],length = 1,sampling_rate = 1, batch_size = 1)
  test_data <- timeseries_generator(data = test_normalise,targets = test_normalise[,1],length = 1,sampling_rate = 1, batch_size = 1)
  
  # LSTM model
  lstm_model <- keras_model_sequential() %>%
    layer_lstm(units =LSTMUnits, input_shape = c(1, feature), activation = ActivationFn, dropout = DropoutRate, return_sequences = TRUE) %>%
    layer_dense(units = 1)
  
  lstm_model %>% compile(optimizer = "sgd", loss=CompLoss, metrics=CompMetrics)
  
  summary(lstm_model)
  
  lstm_history <- lstm_model %>% fit(
    train_data,
    batch_size = 1,
    epochs = Epochs, validation.split=ValidationSplit
  ) #
  
  # LSTM fitted value
  lstm_model %>% evaluate(train_data)
  lstm_fiited_norm <- lstm_model %>%
    predict(train_data)
  train_lstm_fiited<-denormalize(lstm_fiited_norm)
  
  
  # LSTM  Prediction
  lstm_model %>% evaluate(test_data)
  lstm_predicted_norm <- lstm_model %>%
    predict(test_data)
  test_lstm_predicted <- denormalize(lstm_predicted_norm)
  
  ### accuracy measurements ##################
  
  actual_data <- data_all[, 2]
  train_actual <- actual_data[c((1+1):n_train)]
  test_actual <- actual_data[c((1+n_train+1):(n_train+n_test))]
  
  AccuracyTable <- matrix(nrow=2, ncol=2)
  AccuracyTable[1, 1] <- round(sqrt(mean((exp(train_actual) - exp(train_lstm_fiited)) ^ 2)), digits = 4)
  AccuracyTable[1, 2] <- round(mean(abs((exp(train_actual) - exp(train_lstm_fiited)) / exp(train_actual))), digits = 4)
  AccuracyTable[2, 1] <- round(sqrt(mean((exp(test_actual) - exp(test_lstm_predicted)) ^ 2)), digits = 4)
  AccuracyTable[2, 2] <- round(mean(abs((exp(test_actual) - exp(test_lstm_predicted)) / exp(test_actual))), digits = 4)
  row.names(AccuracyTable) <- c("Train", "Test")
  colnames(AccuracyTable) <- c("RMSE", "MAPE")
  return(list(TrainFittedValue = train_lstm_fiited, TestPredictedValue = test_lstm_predicted, AccuracyTable = AccuracyTable))
}

## Choose LSTM

#fold 1

val1_rmse_lstm <- NULL
tslag <- c(2, 3, 4, 5, 10, 15)
LSTMUnits <- c(4, 8, 16, 32, 64)
start_time <- Sys.time()
for(i in tslag){
  for(j in LSTMUnits){
    
    TSLSTM <- LSTM(ts = sub2, tsLag = i, xregLag = 0, 
                   LSTMUnits = j, Epochs = 100, 
                   SplitRatio = 0.962963) 
    val1_rmse_lstm <- append(val1_rmse_lstm, TSLSTM$AccuracyTable[2, 1])
  }
}
end_time <- Sys.time()
end_time - start_time

plot(val1_rmse_lstm, type = "b", col = "dodgerblue", lwd = 2)
points(which(val1_rmse_lstm == min(val1_rmse_lstm)), 
       min(val1_rmse_lstm), col = "red", pch = 19)

#fold 2 

val2_rmse_lstm <- NULL
tslag <- c(2, 3, 4, 5, 10, 15)
LSTMUnits <- c(4, 8, 16, 32, 64)
start_time <- Sys.time()
for(i in tslag){
  for(j in LSTMUnits){
    
    TSLSTM <- LSTM(ts = sub3, tsLag = i, xregLag = 0, 
                   LSTMUnits = j, Epochs = 100, 
                   SplitRatio = 0.9652857) 
    
    val2_rmse_lstm <- append(val2_rmse_lstm, TSLSTM$AccuracyTable[2, 1])
  }
}
end_time <- Sys.time()
end_time - start_time

plot(val2_rmse_lstm, type = "b", col = "dodgerblue", lwd = 2)
points(which(val2_rmse_lstm == min(val2_rmse_lstm)), 
       min(val2_rmse_lstm), col = "red", pch = 19)


#fold 3

val3_rmse_lstm <- NULL
tslag <- c(2, 3, 4, 5, 10, 15)
LSTMUnits <- c(4, 8, 16, 32, 64)
start_time <- Sys.time()
for(i in tslag){
  for(j in LSTMUnits){
    
    TSLSTM <- LSTM(ts = sub4, tsLag = i, xregLag = 0, 
                   LSTMUnits = j, Epochs = 100, 
                   SplitRatio = 0.9665172) 
    
    val3_rmse_lstm <- append(val3_rmse_lstm, TSLSTM$AccuracyTable[2, 1])
  }
}
end_time <- Sys.time()
end_time - start_time

plot(val3_rmse_lstm, type = "b", col = "dodgerblue", lwd = 2)
points(which(val3_rmse_lstm == min(val3_rmse_lstm)), 
       min(val3_rmse_lstm), col = "red", pch = 19)


#fold4

val4_rmse_lstm <- NULL
tslag <- c(2, 3, 4, 5, 10, 15)
LSTMUnits <- c(4, 8, 16, 32, 64)
start_time <- Sys.time()
for(i in tslag){
  for(j in LSTMUnits){
    
    TSLSTM <- LSTM(ts = sub5, tsLag = i, xregLag = 0, 
                   LSTMUnits = j, Epochs = 100, 
                   SplitRatio = 0.966667) 
    
    val4_rmse_lstm <- append(val4_rmse_lstm, TSLSTM$AccuracyTable[2, 1])
  }
}
end_time <- Sys.time()
end_time - start_time

plot(val4_rmse_lstm, type = "b", col = "dodgerblue", lwd = 2)
points(which(val4_rmse_lstm == min(val4_rmse_lstm)), 
       min(val4_rmse_lstm), col = "red", pch = 19)

validation_rmse_lstm2 <- rbind(val1_rmse_lstm, val2_rmse_lstm, val3_rmse_lstm, val4_rmse_lstm)
opt_hyp <- which(colMeans(validation_rmse_lstm2) == min(colMeans(validation_rmse_lstm2)))

grid_lstm <- expand.grid(tslag = c(2, 3, 4, 5, 10, 15), LSTMUnits = c(4, 8, 16, 32, 64))
grid_lstm <- cbind(grid_lstm$tslag, grid_lstm$LSTMUnits)

## Checking the overfitting for selected LSTM
tsLag <- grid_lstm[opt_hyp, 1]
feature_mat <- NULL

lag_y <- lagmatrix(as.ts(x_t), lag = c(0:(tsLag)))
all_feature <- cbind(lag_y, feature_mat)


data_all <- all_feature[-c(1:tsLag), ]

data <- data_all[, -1]
feature <- ncol(data)

a <- 1 / (max(data[, 1]) - min(data[, 1]))
b <- min(data[, 1]) / (max(data[,1]) - min(data[, 1]))


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

denormalize <- function(x) {
  return ((x + b) / a)
}

data_normalise <- apply(data, 2, normalize)

SplitRatio <- 0.85
train_normalise <- data_normalise[c(1:(nrow(data_normalise) * SplitRatio)), ]
test_normalise <- data_normalise[-c(1:(nrow(data_normalise) * SplitRatio)), ]

n_train <- nrow(train_normalise)
n_test <- nrow(test_normalise)

train_data <- timeseries_generator(data = train_normalise, targets = train_normalise[, 1], length = 1, sampling_rate = 1, batch_size = 1)
test_data <- timeseries_generator(data = test_normalise, targets = test_normalise[, 1], length = 1, sampling_rate = 1, batch_size = 1)


lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = grid_lstm[opt_hyp, 2], input_shape = c(1, feature), 
             activation = "tanh", dropout = 0.0, return_sequences = TRUE) %>%
  layer_dense(units = 1)

#optimizer_rmsprop()
CompLoss <- "mse"
CompMetrics <- "mae"

lstm_model %>% compile(optimizer = "sgd", 
                       loss = CompLoss , metrics = CompMetrics )

summary(lstm_model)

lstm_history <- lstm_model %>% fit(
  train_data,
  batch_size = 1,
  epochs = 300, validation_data = list(test_data)
)

## Loss-Validation curve
df2 <- data.frame(c(rep("train", 300), rep("validation", 300)),
                  rep(seq(1:300), 2), c(lstm_history$metrics$los, lstm_history$metrics$val_los))
colnames(df) <- c("Data", "Epoch", "Loss")

df2%>%
  ggplot(aes(x = Epoch, y = Loss, group = Data, colour = Data)) +
  geom_line(size = 1.1) +
  ggtitle("") + theme(plot.title = 
                        element_text(color = "black", face = "bold", size = 16), 
                      axis.text = element_text(size = 12)) + theme_gray(base_size = 13) +
  theme(legend.position = "top")

## Walk-Forward LSTM

pred_lstm <- NULL
start_time <- Sys.time()
for(i in 1:n_test){
  history <- as.vector(window(x_t, start = 1, end = n_train + i))
  
  TSLSTM <- LSTM(ts = history, tsLag = 15, xregLag = 0, 
                 LSTMUnits = 64, Epochs = 300, 
                 SplitRatio = 1 - (2 / (length(history))), 
                 ValidationSplit = 0.2) 
  
  pred_lstm[i] <- TSLSTM$TestPredictedValue
}
end_time <- Sys.time()
end_time - start_time

cbind(ts_test[1:5], pred_lstm, difference = abs(ts_test[1:5] - pred_lstm))
plot.ts(exp(ts_test[-c(52, 53)]), type = "o", lwd = 2, ylim = c(25000, 50000))
lines(exp(pred_lstm[-c(1,2)]), type = "o", col = "blue", lwd = 2)

rmse_lstm <- sqrt(mean((exp(pred_lstm - exp(ts_test)) ^ 2))
mae_lstm <- mean(abs(exp(ts_test) - exp(pred_lstm)))
mape_lstm <- sum(abs(exp(ts_test) - exp(pred_lstm)) / exp(ts_test)) * 100 / n_test 
out <- cbind(exp(ts_test), exp(pred_lstm), rmse_lstm, mape_lstm, mae_lstm)
                  
## Final result (Accuracy Metrics)
result <- data.frame(Model = c("ARIMA", "LSTM"), TestRMSE = 
    c(rmse_arima, rmse_lstm), TestMAPE = c(mape_arima, mape_lstm), 
                           TestMAE = c(mae_arima, mae_lstm))
                  
## Distribution of RMSE on validation sets
arima.val.mean <- colMeans(validation_rmse_arima2)
lstm.val.mean <- colMeans(validation_rmse_lstm2)

df3 <- data.frame(Model = c(rep("ARIMA", 24), rep("LSTM", 30)), 
                  Average.RAMSE = c(arima.val.mean[-1], lstm.val.mean))

ggplot(df3, aes(x = Model, y = Average.RAMSE, fill = Model)) + 
  geom_boxplot(width = 0.5) +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 7)) +
  ggtitle("") + theme(plot.title = 
                        element_text(color = "black", face = "bold", size = 16), 
                      axis.text = element_text(size = 10)) + theme_gray(base_size = 13) +
  theme(legend.position = "top")

## Forecast performance plot
df4 <- data.frame(c(rep("actual", 53), rep("ARIMA", 53), rep("LSTM", 53)),
                  rep(seq(1:53), 3), c(ts_test, pred_arima, pred_lstm))
colnames(df2) <- c("Data", "time", "index")

df4%>%
  ggplot(aes(x = time, y = index, group = Data, colour = Data)) +
  geom_line(size = 0.75, aes(linetype = Data)) +
  geom_point(size = 1, aes(color = Data)) +
  ylim(15000, 60000) +
  ggtitle("") + theme(plot.title = 
                        element_text(color = "black", face = "bold", size = 16)) +
  theme_gray(base_size = 12) +
  theme(legend.position = "top")



