# ---------------------------------------
# Understanding US Financial Crisis Indicators
# An examination of LIBOR vs. TED and the S&P 500
# Jumpstart code by Thomas Miller
# ---------------------------------------

library(quantmod) # use for gathering and charting economic data
library(lubridate) # date functions
library(latticeExtra) # package used for horizon plot
library(zoo)  # utilities for working with time series

# S&P 500
getSymbols("SP500",src="FRED",return.class = "xts")
print(str(SP500)) # show the structure of this xtx time series object
# plot the series
chartSeries(SP500,theme="white")

# Total Consumer Credit Owned and Securitized, Outstanding
getSymbols("TOTALSL",src="FRED",return.class = "xts")
print(str(TOTALSL)) # show the structure of this xtx time series object
# plot the series
chartSeries(TOTALSL,theme="white")

# 1-Month London Interbank Offered Rate (LIBOR), based on U.S. Dollar
getSymbols("USD1MTD156N",src="FRED",return.class = "xts")
print(str(USD1MTD156N)) # show the structure of this xtx time series object
# plot the series
chartSeries(USD1MTD156N,theme="white")

# TED Spread
getSymbols("TEDRATE",src="FRED",return.class = "xts")
print(str(TEDRATE)) # show the structure of this xtx time series object
# plot the series
chartSeries(TEDRATE,theme="white")

# blip in 2008, but credit owned still growing

# ---------------------------------------
# Multiple time series plots 
# ---------------------------------------

# TED spread
getSymbols("TEDRATE", src="FRED", return.class = "xts")
TED <- TEDRATE # use simple name for xts object
dimnames(TED)[2] <- "TED" # use simple name for index
chartSeries(TED, theme="white")
TED.data.frame <- as.data.frame(TED)
TED.data.frame$date <- ymd(rownames(TED.data.frame))
TED.time.series <- ts(TED.data.frame$TED, 
                      start = c(year(min(TED.data.frame$date)), month(min(TED.data.frame$date))),
                      end = c(year(max(TED.data.frame$date)),month(max(TED.data.frame$date))),
                      frequency=12)

# LIBOR
getSymbols("USD1MTD156N", src="FRED", return.class = "xts")
LIBOR <- USD1MTD156N # use simple name for xts object
dimnames(LIBOR)[2] <- "LIBOR" # use simple name for index
chartSeries(LIBOR, theme="white")
LIBOR.data.frame <- as.data.frame(LIBOR)
LIBOR.data.frame$date <- ymd(rownames(LIBOR.data.frame))
LIBOR.time.series <- ts(LIBOR.data.frame$LIBOR, 
                      start = c(year(min(LIBOR.data.frame$date)), month(min(LIBOR.data.frame$date))),
                      end = c(year(max(LIBOR.data.frame$date)),month(max(LIBOR.data.frame$date))),
                      frequency=12)

# S&P500
getSymbols("SP500",src="FRED",return.class = "xts")
SP <- SP500
dimnames(SP)[2] <- "SP" # use simple name for index
chartSeries(SP, theme="white")
SP.data.frame <- as.data.frame(SP)
SP.data.frame$date <- ymd(rownames(SP.data.frame))
SP.time.series <- ts(SP.data.frame$SP, 
                      start = c(year(min(SP.data.frame$date)),month(min(SP.data.frame$date))),
                      end = c(year(max(SP.data.frame$date)),month(max(SP.data.frame$date))),
                      frequency=12)

# define multiple time series object
deliquency.mts <- cbind(TED.time.series,
                      SP.time.series) 
dimnames(deliquency.mts)[[2]] <- c("TED","SP") # keep simple names 
modelingdelq.mts <- (deliquency.mts) # keep overlapping time intervals only 

# examine the structure of the multiple time series object
# note that this is not a data frame object
print(str(modelingdelq.mts))

# plot multiple time series using standard R graphics 
plot(modelingdelq.mts,main="")
