# Check Spectral analysis Temperature record
# Chr
# Bun
# Mck
# Stei

cur_dir <- getwd()
#t_dir <- "/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/databases/external_data/"
t_dir <- "/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/CultureClimate/data/ref_data/"
setwd(t_dir)

download_data <- function(data_file_name, url)
{ 
  if (!(data_file_name %in% dir())) 
    download.file(url, data_file_name)
}

process_data <- function(data_file_name, col_num, sep=" ") 
{ 
  # read the data file 
  f <- file(data_file_name, 'r') 
  # read all the lines from the file 
  data <- readLines(f) 
  # get the line for the column names 
  data_cols <- data[col_num] 
  start_num <- col_num + 1
  # cut out the data portion 
  data_cols <- strsplit(data_cols, sep)[[1]]
  data_cols <- data_cols[data_cols != ""] 
  data <- data[start_num:length(data)]
  data_frame <- data.frame()
  # generate the data frame after string processing, basically collecting the column data  
  for(l in data) 
  {
    if (sep != "\t") {
      chars <- strsplit(l, " ")[[1]] 
      chars <- chars[chars != ""] 
    } else if (sep == "\t") {
      chars <- strsplit(l, sep)[[1]]
    }
    chars <- as.double(chars) 
    data_frame <- rbind(data_frame, chars) 
  }
  
  # set the column name 
  colnames(data_frame) <- data_cols
  return(data_frame)
}

dp_fname <- 'christiansen2012.xls'

sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname, sheet=sheets[3], range = "A13:F1987")
c <- colnames(dfxl) # column header
c
# The list below gives the two-millennia long reconstruction and confidence intervals shown in Fig. 5.
# Local temperatures from HadCRUT3v, calibration period 1880-1960
# 
# The first column is time
# The second column is the un-smoothed reconstruction (°C anomalies vs. 1880-1960AD). Thin black curve in Fig. 5.
# The third column is the 50y-smoothed reconstruction. Thick black curve in Fig. 5.
# The fourth column is the 50y-smoothed reconstruction plus bias. Thick red curve in Fig. 5.
# The fifth column is the 50y-smoothed reconstruction plus upper 2.5 % quantile. Dashed red curve in Fig. 5.
# The sixth column is the 50y-smoothed reconstruction plus lower 2.5 % quantile. Dashed red curve in Fig. 5.

yr <- dfxl[[c[1]]]
temp_recon <- dfxl[[c[2]]]
temp_50y <- dfxl[[c[3]]]
temp_50y_bias <- dfxl[[c[4]]]
temp_50y_upper_quant <- dfxl[[c[5]]]
temp_50y_lower_quant <- dfxl[[c[6]]]

chr_df <- data.frame(year=yr, temp=temp_recon)

plot(yr, temp_recon, t='l', xlab='Period (yr)', ylab='Temperature(°C)',
     main='Stack of different temperature of Northern Hemisphere')
lines(yr, temp_50y, t='l', col='red')
lines(yr, temp_50y_bias, t='l', col='green')
lines(yr, temp_50y_upper_quant, t='l', col='blue', lty=2, lwd=2)
lines(yr, temp_50y_lower_quant, t='l', col='blue', lty=2, lwd=2)

# using periodogram
Pspec<- spec.pgram(temp_recon, demean = TRUE, detrend = TRUE, 
                   taper = 0.1, log='no', plot=F)
plot(Pspec$freq, Pspec$spec, t='l', 
     xlim=c(0, 0.015), lwd=2,
     main="Spectral Power vs frequency for Northern Hemisphere temperature data",
     xlab='Frequency(1/yr)',
     ylab='Spectral Power'
)
dfPspec <- data.frame(freq = Pspec$freq, spec=Pspec$spec)
dfPspec <- dfPspec[order(dfPspec$spec, decreasing = T),]
time_window <- max(yr) - min(yr)
dfPspec$period <- 1 / dfPspec$freq * (time_window / length(temp_recon))
head(dfPspec, n=10)
#     freq      spec     period
# 2  0.0010 34.016485  999.49341
# 4  0.0020 13.493142  499.74671
# 5  0.0025 11.257701  399.79737
# 1  0.0005  9.646063 1998.98683
# 10 0.0050  8.898903  199.89868
# 11 0.0055  7.659273  181.72608
# 12 0.0060  6.761297  166.58224
# 17 0.0085  4.912861  117.58746
# 18 0.0090  4.600561  111.05482
# 22 0.0110  4.546332   90.86304

freql <- head(dfPspec$freq, n=10)
period <- head(dfPspec$period, n=10)
legnd <- head(dfPspec$period, n=10)
# remove 1998 yr
freql <- freql[-4] 
period <- period[-4]
legnd <- legnd[-4]

abline(v=freql, lty=2, col=1:length(legnd))
periodl <- round(head(1/freql), 0)
legnd <- paste(round(legnd, 0), ' yr', sep="")
legend('topright', legend=legnd, 
       lty=2, col=1:length(freql), cex=0.75)

dfPspecP <- dfPspec[order(dfPspec$period, decreasing = F),]
head(dfPspecP)
plot(dfPspecP$period, dfPspecP$spec, 
     type='l', lwd=2, xlim=c(50, 500), ylim=c(0,15),
     main="Spectral Power vs period for Northern Hemisphere temperature data",
     xlab='Period (yr)',
     ylab='Spectral Power')

period <- round(period[-1], 0)
abline(v=period, lty=2, col=1:length(legnd))
periodl <- paste(period, 'yr')
legend('bottomright', legend=periodl, 
       lty=2, col=1:length(periodl), cex=0.75)

head(dfPspec, n=20)

# Using blackman-tuckey
BTspec <- spec.BT(temp_recon,demean = T,
                  dt = 1, lag=1/3, unit=" ", x_lim = c(0,0.1))
dfBTspec <- data.frame(BTspec) 
dfBTspec <- dfBTspec[order(dfBTspec$spec, decreasing = T), ]
dfBTspec$period <- (1 / dfBTspec$freq) * (time_window / length(temp_recon))
head(dfBTspec, n=10)

#       freq       spec      period
# 2  0.001519757 14.1334514 657.66667
# 1  0.000000000 10.5253087       Inf
# 3  0.003039514  5.8683450 328.83333
# 5  0.006079027  5.3001291 164.41667
# 6  0.007598784  2.6396223 131.53333
# 7  0.009118541  2.0554349 109.61111
# 8  0.010638298  1.7537283  93.95238
# 9  0.012158055  0.8712505  82.20833
# 10 0.013677812  0.7333986  73.07407
# 4  0.004559271  0.6602227 219.22222

# Using multitaper
library(astrochron)
chr_model <- linterp(chr_df, dt=1)
Mspec <- mtm(chr_model, demean = T, 
             ntap = 5, tbw = 3, #ar1 = T,
             output = 1, pl=2)
Mspecdf <- data.frame(Mspec)
Mspecdf <- Mspecdf[order(Mspecdf$AR1_CL, decreasing = T),]
time_window <- max(chr_df$year) - min(chr_df$year)
N <- length(chr_df$temp)
Mspecdf$period <- (1/Mspecdf$Frequency) #* (time_window / N)
head(Mspecdf, n = 20)

# bun temp data
#(Bün) Büntgen et al. [53]: tree-rings, extracted from over thousand ring width series of central Europe, temperature anomaly [oC], -499 to 2003 BC/AD, 1 year time resolution.

process_bun_data <- function(data_file_name, col_num, end_line, sep=" ") 
{ 
  # read the data file 
  f <- file(data_file_name, 'r') 
  # read all the lines from the file 
  data <- readLines(f) 
  # get the line for the column names 
  data_cols <- data[col_num] 
  start_num <- col_num + 1
  # cut out the data portion 
  data_cols <- strsplit(data_cols, sep)[[1]]
  data_cols <- data_cols[data_cols != ""] 
  data <- data[start_num+1:end_line]
  data_frame <- data.frame()
  # generate the data frame after string processing, basically collecting the column data  
  
  ranges <- list()
  excols <- list()
  ranges[[1]] <- c(-499,-399) # exclude 2, 3, 4, 5, 9-th col
  excols[[1]] <- c(-1,  2,  2,  4, 5, -1, -1, -1, 9)
  
  ranges[[2]] <- c(-398, 754) # exclude 5, 9-th col
  excols[[2]] <- c(-1, -1, -1, -1, 5, -1, -1, -1, 9) 
  
  ranges[[3]] <- c(755, 995)  # exclude 5-th col
  excols[[5]] <- c(-1, -1, -1, -1, 5, -1, -1, -1, -1) 
  
  ranges[[4]] <- c(996, 2003) # no exclusion
  excols[[6]] <- c(-1, -1, -1, -1, -1, -1, -1, -1, -1) 
  
  l <- data[1]
  for(l in data) 
  {
    if (sep != "\t") {
      chars <- strsplit(l, " ")[[1]]
      chars <- chars[chars != ""]
    } else if (sep == "\t") {
      chars <- strsplit(l, sep)[[1]]
    }
    
    yr <- as.double(chars[1])
    if (is.na(yr) || yr > 2003)
      break
    i <- 1
    for(r in ranges) 
    {
      if (yr >= r[1] && yr <= r[2]) 
      {
        j <- 0
        ecols <- excols[[i]]
        for (ec in ecols)
        {
          if (ec != -1)
            chars <- append(chars, "-Inf", j) 
          j <- j + 1
        }  
        if (length(chars) == 8)
          chars <- append(chars, "-Inf", 9) 
        break
      } else {
        break
      }
      i <- i + 1
    }
    
    chars <- as.double(chars) 
    data_frame <- rbind(data_frame, chars) 
  } 
  
  colnames(data_frame) <- data_cols
  # set the column name 
  return(data_frame)
}

# Bun data download
bun_data_file_name <- "buentgen2011europe.txt"
bun_ftp_data_url <-"ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/reconstructions/europe/buentgen2011europe.txt"
download_data(bun_data_file_name, bun_ftp_data_url)
col_num <- 106
end_line <- 2600
bun_df <- process_bun_data(bun_data_file_name, col_num, end_line, sep=" ")
bun_df <- bun_df[bun_df$Year >= 0, ]
plot(bun_df$Year, bun_df$TempJJA - mean(bun_df$TempJJA), type='l', col='blue', xlab="Year", ylab="T ANN(°C)", main="Büntgen et al.2017: temperature anomaly")

Pspec<- spec.pgram(bun_df$TempJJA, demean = TRUE, detrend = TRUE, 
                   taper = 0.1, log='no', plot=F)
plot(Pspec$freq, Pspec$spec, t='l', 
     xlim=c(0, 0.04), lwd=2,
     main="Spectral Power vs frequency for tree ring based temperature anomaly",
     xlab='Frequency(1/yr)',
     ylab='Spectral Power'
)
dfPspec <- data.frame(freq = Pspec$freq, spec=Pspec$spec)
dfPspec <- dfPspec[order(dfPspec$spec, decreasing = T),]
time_window <- max(bun_df$Year) - min(bun_df$Year)
dfPspec$period <- 1 / dfPspec$freq * (time_window / length(temp_recon))
head(dfPspec, n=10)
dfPspec <- dfPspec[dfPspec$period <=700, ]
head(dfPspec, n=15)
#         freq      spec    period
# 11 0.005432099 25.016489 186.79539
# 16 0.007901235 20.261888 128.42183
# 5  0.002469136 18.514951 410.94985
# 8  0.003950617 16.895049 256.84366
# 27 0.013333333 13.704289  76.10182
# 38 0.018765432 13.486532  54.07235
# 35 0.017283951 11.485751  58.70712
# 22 0.010864198  9.516972  93.39769
# 71 0.035061728  9.284124  28.94013
# 68 0.033580247  8.406972  30.21690
# 18 0.008888889  7.202809 114.15274
# 25 0.012345679  6.947213  82.18997
# 12 0.005925926  6.870159 171.22910
# 17 0.008395062  6.814563 120.86760
# 43 0.021234568  6.544153  47.78487

freql <- head(dfPspec$freq, n=10)
period <- head(dfPspec$period, n=10)
legnd <- head(dfPspec$period, n=10)
abline(v=freql, lty=2, col=1:length(legnd))
periodl <- paste(periodl, 'yr')
legnd <- paste(round(legnd, 0), ' yr', sep="")
legend('topright', legend=legnd, 
       lty=2, col=1:length(freql), cex=0.5)


dfPspecP <- dfPspec[order(dfPspec$period, decreasing = F),]
head(dfPspecP)
plot(dfPspecP$period, dfPspecP$spec, 
     type='l', lwd=2, xlim=c(50, 500), ylim=c(0,30),
     main="Spectral Power vs period for tree ring based temperature anomaly",
     xlab='Period (yr)',
     ylab='Spectral Power')

abline(v=period, lty=2, col=1:length(legnd))
periodl <- paste(periodl, 'yr')
legnd <- paste(round(legnd, 0), ' yr', sep="")
legend('topright', legend=legnd, 
       lty=2, col=1:length(freql), cex=0.5)

head(dfPspec, n=20)

# McKay NP, Kaufmann DS. An extended Arctic proxy temperature database 
# for the past 2,000 years. Sci Data 2014; 1(140026). 
#Data available at http://ncdc.noaa.gov/paleo/study/16973 2017
arctic_proxy_temp_data_file_name <- 'Reconstruction_Arc2kv1.1.1.xlsx'
dfxl <- read_excel(arctic_proxy_temp_data_file_name)
excel_sheets(arctic_proxy_temp_data_file_name)
c <- colnames(dfxl)
c
yr <- dfxl[[c[1]]]
temp_recon <- dfxl[[c[2]]]
temp_2sigma_low <- dfxl[[c[3]]]
temp_2sigma_high <- dfxl[[c[4]]]
yr_30y <- dfxl[[c[6]]]
yr_30y <- yr_30y[!is.na(yr_30y)]
temp_30y_recon <- dfxl[[c[7]]]
temp_30y_recon <- temp_30y_recon[!is.na(temp_30y_recon)]
yr_100y <- dfxl[[c[9]]]
yr_100y <- yr_100y[!is.na(yr_100y)]
temp_100y_recon <- dfxl[[c[10]]]
temp_100y_recon <- temp_100y_recon[!is.na(temp_100y_recon)]

arc_df <- data.frame(year=yr, temp=temp_recon)

plot(yr, temp_recon, t='l', xlab='Period (yr)', ylab='Temperature(°C)',
     main='Stack of different Arctic temperature proxies')
lines(yr, temp_2sigma_high, t='l', col='blue', lty=2, lwd=2)
lines(yr, temp_2sigma_low, t='l', col='darkblue', lty=2, lwd=2)
lines(yr_30y, temp_30y_recon, t='l', col='red', lwd=2)
lines(yr_100y, temp_100y_recon, t='l', col='green', lwd=2)
legend <- c('temperature reconstruction', '30 year moving average', 
            '100 year moving average', '2 sigma higher', '2 sigma lower')
legend(1400, 1.3, lty=1, legend=legend, 
       col=c('black', 'red', 'green', 'blue', 'darkblue'), cex=0.5)

Pspec<- spec.pgram(temp_recon, demean = TRUE, detrend = TRUE, 
                   taper = 0.1, log='no', plot=F)
plot(Pspec$freq, Pspec$spec, t='l', 
     xlim=c(0, 0.04), lwd=2,
     main="Spectral Power vs frequency for arctic temperatures",
     xlab='Frequency(1/yr)',
     ylab='Spectral Power'
)
dfPspec <- data.frame(freq = Pspec$freq, spec=Pspec$spec)
dfPspec <- dfPspec[order(dfPspec$spec, decreasing = T),]
time_window <- max(yr) - min(yr)
dfPspec$period <- 1 / dfPspec$freq * (time_window / length(temp_recon))
head(dfPspec, n=10)
dfPspec <- dfPspec[dfPspec$period <=700, ]
head(dfPspec, n=15)
# freq      spec    period
# 4  0.0020 11.380630 499.75000
# 12 0.0060  6.213752 166.58333
# 28 0.0140  3.877466  71.39286
# 10 0.0050  3.698009 199.90000
# 34 0.0170  2.485110  58.79412
# 9  0.0045  2.443499 222.11111
# 3  0.0015  2.114470 666.33333
# 29 0.0145  2.077940  68.93103
# 11 0.0055  2.035319 181.72727
# 58 0.0290  1.970610  34.46552
# 54 0.0270  1.936190  37.01852
# 83 0.0415  1.926896  24.08434
# 14 0.0070  1.862902 142.78571
# 60 0.0300  1.754072  33.31667
# 17 0.0085  1.673170 117.58824

freql <- head(dfPspec$freq, n=10)
period <- head(dfPspec$period, n=10)
legnd <- head(dfPspec$period, n=10)
abline(v=freql, lty=2, col=1:length(legnd))
periodl <- paste(periodl, 'yr')
legnd <- paste(round(legnd, 0), ' yr', sep="")
legend('topright', legend=legnd, 
       lty=2, col=1:length(freql), cex=0.5)


dfPspecP <- dfPspec[order(dfPspec$period, decreasing = F),]
head(dfPspecP)
plot(dfPspecP$period, dfPspecP$spec, 
     type='l', lwd=2, xlim=c(50, 700), ylim=c(0,15),
     main="Spectral Power vs period for arctic temperatures",
     xlab='Period (yr)',
     ylab='Spectral Power')

abline(v=period, lty=2, col=1:length(legnd))
periodl <- paste(periodl, 'yr')
legnd <- paste(round(legnd, 0), ' yr', sep="")
legend('topright', legend=legnd, 
       lty=2, col=1:length(freql), cex=0.5)

head(dfPspec, n=20)


# Pet data download
pet_data_file_name <- "deutnat.txt"
pet_ftp_data_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/vostok/deutnat.txt"
download_data(pet_data_file_name, pet_ftp_data_url)
col_num <- 111
pet_df <- process_data(pet_data_file_name, col_num, sep="\t")

#plot(pet_df$`Ice age (GT4)`, pet_df$deltaTS, type='l')

pet_df_holocene <- subset(pet_df, pet_df$`Ice age (GT4)` <= 12000)
plot(-pet_df_holocene$`Ice age (GT4)`, pet_df_holocene$deltaTS, type='l', col='blue', xlab='Year BC/AD', ylab='delta-T(°C)', main="Petit et al. 2017: temperature anomaly [°C] from δD of ice-cores")
