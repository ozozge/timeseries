x<-c(5,3,7,6)
y<-c(2,4,3,1)

#c gitti
x[4:2]*y[1:3]

data(package = 'datasets')

air<-as.data.frame(AirPassengers)

AirPassengers

# Extract the time and passengers data
years <- floor(time(AirPassengers))  # Extract the year part
months <- cycle(AirPassengers)       # Extract the month part
passengers <- as.numeric(AirPassengers)  # Convert the time series to a numeric vector

# Combine them into a data frame
air_df <- data.frame(Year = years, Month = months, Passengers = passengers)

# View the resulting data frame
print(air_df)

nottem

y=diff(AirPassengers,lag=4)

y


plot(nhtemp)
plot(USAccDeaths)
plot(austres)

linear_model <- lm(USAccDeaths ~ time(USAccDeaths))

# Add the trend line to the plot
abline(linear_model, col = "red", lwd = 2)

linear_model2 <- lm(nhtemp ~ time(nhtemp))

# Add the trend line to the plot
abline(linear_model2, col = "red", lwd = 2)

set.seed(1)
x<-rnorm(100)
Y<-rnorm(100)
set.seed(1)
Z<-rnorm(200)
F<-c(x,Y)
F==Z
hist(Z)
var(Z)
hist(x)
hist(Y)
var(x)

plot(AirPassengers)

plot(diff(AirPassengers, lag = 12))

plot(log(AirPassengers))

plot(diff(log(AirPassengers), lag = 1))

plot(diff(diff(log(AirPassengers), lag = 12)), lag=1)

UKgas[9]

plot(mdeaths)





