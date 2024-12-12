library(fpp3)
library(stats)
library(forecast)

######################################################################
# Don't change anything here

Grading <- FALSE

if (Grading==TRUE){
  load("data2015.RData")
  load("data2016.RData")
  load("data2017.RData")
  load("data2018.RData")
  load("data2019.RData")
  Train1 <- data2015
  Train2 <- data2016
  Train3 <- data2017
  Train4 <- data2018
  Test1 <- data2019
} else {
  load("data2014.RData")
  load("data2015.RData")
  load("data2016.RData")
  load("data2017.RData")
  load("data2018.RData")
  Train1 <- data2014
  Train2 <- data2015
  Train3 <- data2016
  Train4 <- data2017
  Test1 <- data2018
}
####################################################################
# Use Train1, Train2, Train3, Train4, Test1 instead of
# data2014,..., data2018 in your codes.


df = rbind(Train1,Train2,Train3,Train4)

############### Prepare the training time series ################
Delta = df$WIN_TAKE.x
pool_at_bet = df$WIN_POOL.x
pool_at_final = df$WIN_POOL.y
di_at_bet = df[,4:17]
di_at_final = df[34:47]
Ci=(1-Delta)*(pool_at_final/di_at_final - pool_at_bet/di_at_bet)
Csum = rowSums(Ci,na.rm = TRUE)
num_rows = nrow(df)
newdf = data.frame(matrix(nrow = num_rows, ncol = 0))
newdf$time = 1:num_rows
newdf <- cbind(newdf,csum=Csum)
newdf |>
  mutate(time =  as.numeric(time ) )|>
  as_tsibble(index = time)->
  race1
race1|>
  autoplot(csum)+
  labs(title = "Time Series Raw")
####################################################################


###################### Test for Stationary ########################
race1 |>
  mutate(diffcsum = difference(csum))|>
  features(diffcsum,unitroot_kpss)

race1 |>
  mutate(diffcsum = difference(csum))|>
  features(diffcsum,unitroot_ndiffs)

race1|>
  autoplot(difference(csum))+
  labs(title = "Differenced time series")

###################### d = 1 ########################################


##################### Determine p and q ############################
race1|>ACF(difference(csum))|>autoplot()+labs(title = "ACF of Differenced time series")
race1|>PACF(difference(csum))|>autoplot()+labs(title = "PACF of Differenced time series")

#AIC 
find_aic_matrix <- function(ts_data, max_p, max_q) {
  aic_matrix <- matrix(NA, nrow = max_p + 1, ncol = max_q + 1)
  
  for (p in 0:max_p) {
    for (q in 0:max_q) {
      print(p,q)
      model <- arima(ts_data, order = c(p, 1, q), method = "ML")
      aic <- AIC(model)
      
      # Store AIC value in the matrix
      aic_matrix[p + 1, q + 1] <- aic
    }
  }
  
  # Convert the matrix to a data frame
  aic_df <- as.data.frame(aic_matrix)
  rownames(aic_df) <- 0:max_p  # Set row names to represent p values
  colnames(aic_df) <- 0:max_q  # Set column names to represent q values
  
  return(aic_df)
}

#BIC
find_best_arima <- function(ts_data) {
  best_aic <- Inf
  best_order <- c(0, 0)
  
  for (p in 0:10) {
    for (q in 0:10) {
      print(c(p,q))
      model <- arima(ts_data, order = c(p, 1, q), method = "ML")
      aic <- BIC(model)
      
      if (aic < best_aic) {
        best_aic <- aic
        best_order <- c(p, q)
      }
    }
  }
  
  return(best_order)
}

####################################### ############################

##################### Prepare Assistant Series #########################
pibet = df[18:31]
dibet = df[4:17]
diff = sign(pibet*dibet-1)
x = matrix(rowSums(diff),ncol=1)
################### Setting up Arima Model #############################

p=5
d=1
q=7
P=5
D=1
Q=7
arima_model <- Arima(race1$csum, order = c(p, d, q) , xreg = x)

refit <- Arima(race1$csum, model=arima_model ,xreg = x)
# Plot the first time series
plot(race1$csum, col = "blue", type = "l", lty = 1, ylab = "Values", xlab = "Time")

# Add the second time series to the same plot
lines(fitted.values(refit), col = "red", lty = 2)

#######################################################################

################# Prepare Test Set ####################################
Delta = Test1$WIN_TAKE.x
pool_at_bet = Test1$WIN_POOL.x
pool_at_final = Test1$WIN_POOL.y
di_at_bet = Test1[,4:17]
di_at_final = Test1[34:47]

Ci=(1-Delta)*(pool_at_final/di_at_final - pool_at_bet/di_at_bet)

Csum = rowSums(Ci,na.rm = TRUE)

k = nrow(newdf)

newdft = data.frame(matrix(nrow = nrow(Test1), ncol = 0))

newdft$time = k+1:nrow(newdft)

newdft <- cbind(newdft,csum=Csum)

newdft |>
  mutate(time =  as.numeric(time ) )|>
  as_tsibble(index = time)->
  at

pibet_test = Test1[18:31]
dibet_test = Test1[4:17]
diff_test = sign(pibet_test-1/dibet_test)
x_test = rowSums(diff_test)
#####################################################################

########################## Testing #####################################

df = newdf
mape_values <- numeric(nrow(newdft))
pred_values <- numeric(nrow(newdft))
p95_values <- numeric(nrow(newdft))
mapel <- numeric(nrow(newdft))
xt = x
for (i in 1:nrow(newdft)){
  k = forecast(arima_model,h=1,level=90,xreg = xt[length(xt)])
  pred_values[i]=k$mean
  p95_values[i]=k$upper
  
  mapel[i] = abs((newdft[i,2]-k$mean)/k$mean)
  xt = matrix(append(xt,x_test[i]),ncol=1)
  df = rbind(df,newdft[i,1:2])

  
  arima_model = Arima(df$csum, model = arima_model,  xreg = xt)
  
}
plot(at$csum, col = "blue", type = "l", lty = 1, ylab = "Values", xlab = "Time", main = "Two Time Series")

# Add the second time series to the same plot
lines(pred_values, col = "red", lty = 2)
###################################################################

###################### MAPE #######################################
MAPE = mean(mapel)
print(accuracy(arima_model))
print("MAPE: ")
print(MAPE)


##################### Quantile ####################################
Q<- function(f,y){
  if (y<f){
    return(2*(1-0.95)*(f-y))
  }
  else{
    return(2*0.95*(y-f))
    }
}
result = numeric(nrow(newdft))
for (i in 1:length(p95_values)){
  f = p95_values[i]
  y = at$csum[i]
  
  result[i] = Q(f,y)
}
QS = mean(result)
print("QS: ")
print(QS)












