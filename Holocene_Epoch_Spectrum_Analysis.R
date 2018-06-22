## Spectrum Analysis
#Demean and Detrend data
#```{r}
library(readxl)
library(plotly)
library(dplyr)

# project directory
proj_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/CultureClimate/'
# datapack directory
dp_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/datapacks/'
setwd(dp_dir)
dp_fname <- 'HumanCulture MidEastIntervals wGreenland-Ice cleaned 19Jan2016.xls'

dfxl <- read_excel(dp_fname)
c <- colnames(dfxl) # column header
df2c <- dfxl[[c[2]]] # second column

STARTING_AGE <- 0 + 0.000001
ENDING_AGE <- 2005/1000 # 2Ka
AGE_SLIDE <- 50/1000 # 50 yr

# second column
c <- list()
c <- colnames(dfxl)
df2c <- dfxl[[c[2]]]

# Check column formats on : https://engineering.purdue.edu/Stratigraphy/tscreator/download/TSC_ColumnFormats_Examples_Sept2017.txt
column_type <- c('block', 'event', 'point')

columns <- list()
for (ct in column_type) {
  msg <- paste("Processing ", ct, " column.\n",
             "----------------------------------------------------------------------",
               sep="")
  print(msg)
  columns[[ct]] <- list() 
  
  # get excel row num for each column
  rows <- which(df2c == ct) + 1
  
  
  cols <- list()
  if (ct == 'block') {
    cnum <- 1
    for (r in rows) {
      cols[[cnum]] <- list(name = c(), content=list())
      colname <- dfxl[[c[1]]][r-1]
      cols[[cnum]]$name <- colname

      r_i <- r
      column <- list()
      while(TRUE) {
        age <- dfxl[[c[3]]][r_i]
        if (is.na(age)) {
          break
        }
        name <- dfxl[[c[2]]][r_i]
        if (is.na(name) && !is.na(age))
          name <- ""
        line_type <- dfxl[[c[4]]][r_i]
        popup <- dfxl[[c[5]]][r_i]

        column$name <- c(column$name, name)
        column$age <- c(column$age, as.numeric(age))
        column$line_type <- c(column$line_type, line_type)
        column$popup <- c(column$popup, popup)
        r_i <- r_i + 1
      }
      cols[[cnum]]$content <- column
      cnum <- cnum + 1
    }
  }
  else if (ct == 'event') {
    cnum <- 1
    for (r in rows) {
      cols[[cnum]] <- list(name = c(), content=list())
      colname <- dfxl[[c[1]]][r-1]
      cols[[cnum]]$name <- colname
      
      r_i <- r
      column <- list()
      type <- NA
      while(TRUE) {
        age <- dfxl[[c[3]]][r_i]
        if (is.na(age)) {
          if (is.na(dfxl[[c[1]]][r_i]))
            break
          else {
            type <- dfxl[[c[1]]][r_i]
            r_i <- r_i + 1
            next
          }
        }
        name <- dfxl[[c[2]]][r_i]
        if (is.na(name) && !is.na(age))
          name <- ""
        line_type <- dfxl[[c[4]]][r_i]
        popup <- dfxl[[c[5]]][r_i]
        
        column$name <- c(column$name, name)
        column$age <- c(column$age, as.numeric(age))
        if (!is.na(type))
          column$type <- c(column$type, type)
        column$line_type <- c(column$line_type, line_type)
        column$popup <- c(column$popup, popup)
        r_i <- r_i + 1
      }
      cols[[cnum]]$content <- column
      cnum <- cnum + 1
    }
  } 
  else if (ct == 'point') {
    cnum <- 1
    for (r in rows) {
      cols[[cnum]] <- list(name = c(), content=list())
      colname <- dfxl[[c[1]]][r-1]
      cols[[cnum]]$name <- colname

      r_i <- r + 1
      column <- list()
      while(TRUE) {
        age <- dfxl[[c[2]]][r_i]
        if (is.na(age)) {
          break
        }
        name <- dfxl[[c[3]]][r_i]
        popup <- dfxl[[c[5]]][r_i]

        column$age <- c(column$age, as.numeric(age))
        column$name <- c(column$name, name) # actually value
        column$popup <- c(column$popup, popup)
        r_i <- r_i + 1
      }
      cols[[cnum]]$content <- column
      cnum <- cnum + 1
    }
  }
  columns[[ct]] <- cols
}

regional_column_list <- list()

region_info_rows <- which(dfxl[[c[2]]] == ':')

for (r in region_info_rows) {
  region <- dfxl[[c[1]]][r]
  if (region %in% regional_columns) {
    regional_column_list[[region]] <- list()
  } else {
    next
  }
  sub_regions <- list()
  for (rc in c[3:12]) {
    sub_region_n <- dfxl[[rc]][r]
    if(!is.null(sub_region_n) && !is.na(sub_region_n) && 
       !not_column(sub_region_n)) {
      sub_regions[[sub_region_n]] <- list()
      
      # Get sub sub region row number
      sr <- which(dfxl[[c[1]]] == sub_region_n)
      if (length(sr) == 0 && dfxl[[c[2]]][sr] != ":") {
        sub_regions[[sub_region_n]] <- list(sub_region_n)
        next
      } else {
        sub_sub_regions <- list()
      }
      
      for (src in c[3:12]) {
        sub_sub_region_n <- dfxl[[src]][sr]
        if(!is.null(sub_sub_region_n) && !is.na(sub_sub_region_n) &&
           !not_column(sub_sub_region_n)) {
          sub_sub_regions[[sub_sub_region_n]] <- list()
          
          # Get sub sub sub region row number
          ssr <- which(dfxl[[c[1]]] == sub_sub_region_n)
          if (length(ssr) == 0 || dfxl[[c[2]]][ssr] != ":") {
            sub_sub_regions[[sub_sub_region_n]] <- list(sub_sub_region_n)
            next
          } else {
            sub_sub_sub_regions <- list()
          }
          
          for(ssrc in c[3:12]) {
            sub_sub_sub_region_n <- dfxl[[ssrc]][ssr]
            if (!is.null(sub_sub_sub_region_n) && !is.na(sub_sub_sub_region_n) &&
                !not_column((sub_sub_sub_region_n))) {
              # Assuming there's no more level
              sub_sub_sub_regions[[sub_sub_sub_region_n]] <- sub_sub_sub_region_n
            }
          }
          
          sub_sub_regions[[sub_sub_region_n]] <- sub_sub_sub_regions
        }
      }
      sub_regions[[sub_region_n]] <- sub_sub_regions
    }
  }
  regional_column_list[[region]] <- sub_regions
}

get_main_region_name <- function(col_name) {
  if (col_name %in% regional_columns) {
    return(col_name)
  } else {
    for(region in regional_columns) {
      # number of sub-regions
      sub_region_num <- length(regional_column_list[[region]])
      if (sub_region_num == 0)
        next
      sub_regions <- names(regional_column_list[[region]][1:sub_region_num])
      if (col_name %in% sub_regions)
        return(region)
      else {
        for(sub_region_n in sub_regions) {
          sub_region <- regional_column_list[[region]][[sub_region_n]]
          sub_sub_regions <- names(sub_region)
          sub_sub_region_num <- length(sub_sub_regions)
          if (sub_sub_region_num == 0)
            next
          if (col_name %in% sub_sub_regions)
            return(region)
          
          for(sub_sub_region_n in sub_sub_regions) {
            sub_sub_sub_regions <- regional_column_list[[region]][[sub_region_n]][[sub_sub_region_n]]
            sub_sub_sub_region_num <- length(sub_sub_sub_regions)
            if (sub_sub_sub_region_num == 0)
              next
            if (col_name %in% sub_sub_sub_regions)
              return(region)
          }
        }
      }
    }
  }
  
  return(NA)
}

# Regional Columns
regional_columns <- c('Africa', 'Eastern Mediterranean', 'Middle East to India',
                      'East Asia and Oceania', 'Europe', 'Arctic and Subarctic',
                      'Northwest and Canada', 'North America', 'Middle America',
                      'South America')

not_column <- function(col_name) {
  if (col_name == '_METACOLUMN_OFF')
    return(TRUE)
  else if (col_name == 'off' || col_name == 'on') {
    return(TRUE)
  } else {
    # # column width
    m <- regexec("^[0-9]+$",col_name)
    ml <- regmatches(col_name, m)
    if (length(ml[[1]]) == 1) {
      return(TRUE)
    }
    
    # color code
    m <- regexec("([0-9])+/([0-9])+/([0-9])+",col_name)
    ml <- regmatches(col_name, m)
    
    if (length(ml[[1]]) != 0) {
      return(TRUE)  
    }
    
    return(FALSE)
  }
}

# Get column wise event numbers
dir <- proj_dir 

events_by_col <- list()
event_names_by_col <- list()
ev_df_by_col <- list()
events_by_col_by_regions <- list()
event_names_by_col_by_regions <- list()
ev_df_by_col_by_regions <- list()
for(col_type in column_type) {
  msg <- paste("Processing ", col_type, " column events.\n",
               "----------------------------------------------------------------------",
               sep="")
  print(msg)
  ncol <- length(columns[[col_type]])
  
  msg <-paste("Total number of " , col_type , " columns = ", ncol, sep ="") 
  print(msg)
  
  if (ncol == 0) {
    msg <- paste("No column data or not extracted yet.")
    next
  }
  
  start_age <- STARTING_AGE
  end_age <- ENDING_AGE
  age_diff <- AGE_SLIDE
  
  events <- list()
  event_names <- list()
  event_types <- list()
  events_by_regions <- list()
  event_names_by_regions <- list()
  
  while(start_age <= end_age) {
    next_age <- start_age + age_diff
    mid_age <- (start_age + next_age) / 2
    
    key <- as.character(mid_age)
    evs <- 0
    ev_names <- c()
    ev_types <- c()
    
    # region wise event numbers
    evs_by_regions <- list()
    ev_names_by_regions <- list()
    for(region in regional_columns) {
      evs_by_regions[[region]] <- 0
      ev_names_by_regions[[region]] <- c()
    }
    
    for(col in columns[[col_type]]){
      cn <- length(col$content$age)
      for (k in 1:cn) {
        if (length(col$content) == 0)
          next
        a <- col$content$age[k]
        n <- col$content$name[k]
        t <- col$content$type[k]
        if (a >= start_age && a < next_age) {
          evs <- as.numeric(evs) + 1
          ev_names <- c(ev_names, n)
          ev_types <- c(ev_types, t)
        
          # adding to the region 
          col_region <- get_main_region_name(col$name)
          if(is.na(col_region)) {
            print(paste("Something is wrong with column ", col, " for region ", col_region, sep=""))
          } else {
            evs_by_regions[[col_region]] <- as.numeric(evs_by_regions[[col_region]]) + 1
            ev_names_by_regions[[col_region]] <- c(ev_names_by_regions[[col_region]], n)
          }
        }
        k <- k+1
      }
    }
    events[[key]] <- evs
    event_names[[key]] <- ev_names
    if(col_type == 'event')
      event_types[[key]] <- ev_types
    
    events_by_regions[[key]] <- list()
    event_names_by_regions[[key]] <- list()
    for (region in regional_columns) {
      events_by_regions[[key]][[region]] <- evs_by_regions[[region]]
      event_names_by_regions[[key]][[region]] <- ev_names_by_regions[[region]]
    }
    start_age <- next_age
  }
  
  age <- as.numeric(names(events))
  freq <- as.numeric(unlist(events))
  ev_df <- data.frame(age = age, freq = freq)
  
  ev_df_by_regions[[col_type]] <- list()
  for (region in regional_columns) {
    age <- as.numeric(names(events_by_regions[[col_type]][[region]]))
    freq <- as.numeric(unlist(events_by_regions[[col_type]][[region]]))
    ev_df_by_regions[[col_type]][[region]] <- data.frame(age = age, freq = freq)
  }
 
  msg <- paste("Saving age vs frequency plot.")
  print(msg)
  im_str <- paste(col_type, '_column_', AGE_SLIDE, '_mil_event_frequency_through_phanerozoic', sep="")
  image_fn <- paste(dir, 'images/', im_str, ".png", sep="")
  png(image_fn)
  plot(age, freq, main=im_str, t='l')
  dev.off()
  
  p <- plot_ly(data=ev_df, 
               x=~age, y=~freq, 
               type='scatter',
               mode='lines',
               line=list(color='black')) %>%
       layout(title=im_str)
  print(p)
  
  data_dir <- paste(dir, 'data/', sep="")
  event_names_fn <- paste(col_type, '_column_', AGE_SLIDE, '_mil_event_names_phanerozoic.txt', sep="")
  fn0 <- paste(data_dir, event_names_fn, sep="")
  msg <- paste("Writing event names in ", fn0, sep="")
  print(msg)
  
  sink(fn0)
  print(event_names)
  sink()
  
  event_freq_datapack_file_name <- paste('Phanerozoic_', col_type, '_column_every_', AGE_SLIDE,'_mil_event_frequencies_datapack.txt', sep="")
  fn <- paste(data_dir, event_freq_datapack_file_name, sep="")
  msg <- paste("Writing datapack ", fn, sep="")
  print(msg)
  
  sink(fn)
  str <- 'format version:	1.3\nage units:	Ma\n'
  writeLines(str)
  str <- paste(col_type, ' Column ', AGE_SLIDE, ' Million Year Event Frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  sink()
  #file.show(fn)
  
  event_names_by_col[[col_type]] <- event_names
  events_by_col[[col_type]] <- events
  ev_df_by_col[[col_type]] <- ev_df
  
  events_by_col_by_regions[[col_type]] <- list()
  event_names_by_col_by_regions[[col_type]] <- list()
  ev_df_by_col_by_regions[[col_type]] <- list()
  
  for(region in regional_columns) {
    events_by_col_by_regions[[col_type]][[region]] <- events_by_regions[[col_type]][[region]] 
    event_names_by_col_by_regions[[col_type]][[region]] <- event_names_by_regions[[col_type]][[region]]
    ev_df_by_col_by_regions[[col_type]][[region]] <- ev_df_by_regions[[col_type]][[region]]
  }
}

events_by_regions_mat <- matrix(unlist(events_by_regions), byrow = TRUE, 
       nrow=length(names(events_by_regions)), 
       ncol=length(regional_columns), 
       dimnames=list(names(events_by_regions), regional_columns))

head(events_by_regions_mat)

m <- data.frame(events_by_regions_mat)
m$DominantRegion <- as.integer(apply(m, 1,which.max))
# principal component analysis
# another example using the iris
ncol <- ncol(m)

kpc <- kpca(~.,data=m[,1:(ncol-1)], #iris[-test, -5],
            kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)

#print the principal component vectors
pcv(kpc)

#plot the data projection on the components
par(mar=c(4,4,4,12))

x <- as.double(rotated(kpc)[,1])
y <- as.double(rotated(kpc)[,2])

par(xpd=FALSE)
plot(x, y, 
     #type='n',
     col=as.integer(m[,ncol]), 
     pch=as.integer(m[,ncol]),
     xlab="1st Principal Component",ylab="2nd Principal Component")
arrows(0, 0, x, y, lty=1, lwd=0.5,
       col=as.integer(m[,ncol]))
abline(h=0, lty=2)
abline(v=0, lty=2)

par(xpd=TRUE)
legend(4.725, 2.5, 
       legend = as.character(regional_columns), 
       col=1:10,
       pch=1:10)
par(xpd=FALSE)

#embed remaining points 
#emb <- predict(kpc,iris[test,-5])
#points(emb,col=as.integer(iris[test,5]))

# Number of events every 50 year
#data_dir <- ('/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/programming/')
#setwd(data_dir)
#fn_50 <- paste(data_dir, 'event_frequency_per_50_yr_dat.txt', sep='')
#ev_f_50 <- read.csv(fn_50, sep=' ')
a <- ev_df_by_col[['block']]$age
f <- ev_df_by_col[['block']]$freq + ev_df_by_col[['event']]$freq
ev_f <- data.frame(ages = a, events = f)

#time_interval <- 2.0
#ev_f <- subset(ev_f, ev_f$ages <= time_interval)

eva <- ev_f$ages
ev <- ev_f$events
# plot
fn <- paste('Global_event_numbers_',
        AGE_SLIDE, '_year_bin_from_', round(STARTING_AGE), 
        '_to_', ENDING_AGE, '_ka', sep="")
im_fn <- paste(proj_dir, 'images/', fn, '.png', sep="")
png(im_fn)
plot(eva, ev, xlab='Age(Ka)', 
     ylab='Number of Events', type='p', cex=0.25, col='black', lwd=2,
     main='Number of global events per 50 years', ylim=c(-10,40)
)
lines(eva, ev, col='black')
dev.off()

# demean event data
ev.dm <- ev - mean(ev)

# plot
plot(eva, ev.dm, xlab='Age(Ka)', 
     ylab='Number of Events', type='p', cex=0.25, col='blue', lwd=2,
     main='Demeaned Event Number Data', ylim=c(-10,40)
)
lines(eva, ev.dm, col='blue')
abline(h=0, lty=2, col='blue')

hanning_ma <- function(df, n) {
  x <- 0.5 * df[n] + 0.25*(df[n-1] + df[n+1])
  return(x)
}

# filter event data
ev.filt <- c()
i <- 1
nr <- length(ev.dm)
for (n in 2:(nr-1)) {
  h <- hanning_ma(ev.dm, n)
  ev.filt <- c(ev.filt, h)
}

nr <- length(ev_f$ages)
plot(eva[2:(nr-1)], ev.filt, t='l', col='black', lwd=2)
abline(h=0, lty=2, col='black')

# detrend
#pev <- prewhiten(ev.filt)
#ev.dtrend <- pev[['prew_lm']]
lmfit <- lm(ev.filt ~ eva[2:(nr-1)])
ev.dtrend <- lmfit$residuals
plot(eva[2:(nr-1)], ev.dtrend, t='l', col='black', lwd=2, main='Demeaned + Detrended')
lines(eva[2:(nr-1)], ev.filt, t='l', col='blue', lty=2)
abline(h=0, lty=2, col='black')

# taper event data
ev.tap <- ev.dtrend
i <- 1
nr <- length(as.numeric(eva))
n <- length(ev.filt)

tap_perc_n <- n * 10/100 # 10% tapering to reduce the effect of the terminations of data at the ends of the window

for (j in 1:tap_perc_n) {
  w = j * pi / tap_perc_n
  ev.tap[j] = ev.tap[j] * 0.5 * (1-cos(w))
}
for (j in (n-tap_perc_n):n) {
  w = (512-j) * pi / tap_perc_n
  ev.tap[j] = ev.filt[j] * 0.5 * (1-cos(w))
}
# plot
plot(as.numeric(eva)[2:(nr-1)], ev.tap, col='black', 
     lty=1, t='l', lwd=2,
     main='Demeaned + Detrended + Tapered')
lines(eva[2:(nr-1)], ev.filt, t='l', col='blue', lty=2)
abline(h=0, lty=2)

# Spectral Analysis
# FFT
ev.fft <- fft(ev.tap)


# Calculate Amplitude
Amp <- Mod(ev.fft)

# Calculate Power Spectrum
PowerSpec <- Amp^2

# Take half of it , coz mirrored
PowerSpec <- PowerSpec[1:(length(Amp)/2)]

PowerSpec.han <- c()
i <- 1
nr <- length(PowerSpec)
for (n in 2:(nr-1)) {
  h <- hanning_ma(PowerSpec, n)
  PowerSpec.han <- c(PowerSpec.han, h)
}

t <- as.numeric(ev_f_50$ages_50)
window_width <- max(t) - min(t)
freq <- 1:(length(PowerSpec)-2)
period <- window_width/freq 

TotPowerSpec <- sum(PowerSpec)
RelPowerSpec <- PowerSpec.han/TotPowerSpec

# Charts of Wavenumber vs. Smoothed Power 
plot(freq, PowerSpec.han, t='l', 
     xlab='Frequency (Cycles / 5Ka)', 
     ylab='Power Spectra [Amplitude]^2',
     lwd=2
)

plot(period*1000, PowerSpec.han, t='l', 
     xlab='Time (Yr)', 
     ylab='Power Spectra [Amplitude]^2',
     xlim=c(0,1000),
     lwd=2
)
abline(v=250,lty=2)
abline(v=490, lty=2)
abline(v=205, lty=3)
abline(v=170, lty=3)

# )Wavelength vs. Relative Power Spectra
plot(RelPowerSpec, t='l', 
     xlab='Year (Ka)', 
     ylab='Relative Power Spectra [Amplitude]^2',
     main='Spectral analysis using single split cosine taper method',
     xaxt='n'
     )
idx <- seq(1,max(freq),by=12)
axis(side=1, at=idx, labels=FALSE)
mtext(side=1, padj = 1, text=round(period[idx],2), at=freq[idx])


evt <- ts(ev, start=0.04, frequency=20, deltat=5)
library(psd)
library(multitaper)
s <- spec.mtm(ts(ev), demean=FALSE, detrend=FALSE, nw = 1, k=1)
  # , 
  #             sineSmoothFact = 0.02,
  #             xlab='Year', 
  #             ylab='Relative Power Spectra [Amplitude]^2',
  #             main='Spectral analysis using multitaper method',
  #             Ftest = TRUE)
idx <- seq(0,4,by=1)
axis(side=1, at=idx, labels=FALSE)
p_idx <- seq(1, length(period), by=5)
mtext(side=1, padj = 1, text=round(period[p_idx],2), at=idx)



### With spec.pgram --- ambiguous


#CC <- ts(ev, start=0, end=12, frequency=10)  # 1000/100 = 10
CC <- ts(ev.tap, start=0, end=1, frequency=12)  # 5000/100 = 50
par(mfrow=c(1,2))
P1 <- spec.pgram(CC,log='no',taper=0,pad=0,fast=FALSE,demean=TRUE,detrend=FALSE, xlab="Frequency(1/Ky)") 

# Periods calculated from frequency
Periods = time_interval/P1$freq
Periods

abline(v=24, lty=2)
abline(v=37, lty=2)
abline(v=48, lty=2)
# logarithmic plot
P2 <- spec.pgram(CC,log='yes',taper=0,pad=0,fast=FALSE,demean=TRUE,detrend=FALSE, xlab="Frequency(1/Ky)") 

abline(v=24, lty=2)
abline(v=37, lty=2)
abline(v=48, lty=2)

par(mfrow=c(2,1))
plot.frequency.spectrum(ev.fft, xlab='Cycles / 5Ka')


amp <- Mod(ev.fft)
N <- length(amp)
amp.h <- amp[0:(N/2)]
x <- 0:(N/2-1)
d <- data.frame(x, amp.h)
d.sorted <- d[order(amp.h, decreasing = TRUE),]
head(d.sorted, 20)

hm <- c(10, 20, 19, 3, 13)
plot.show(ev.tap, start_time = 0, time = 5, harmonics = hm,
          xlab='Age (Ka)', ylab='', scale=0.5)

hm_l <- paste(hm, ' cycles')
hm_l <- c(hm_l, 'combined cycle')
legend('topleft', legend=hm_l, col = c(1, 2, 3, 4, 5, 6,'darkblue'), 
       lty = rep(1,7), cex=0.40)

Xspec<- spec.pgram(ev.tap, log='no', lwd=2)

BTspec <- spec.BT(ev.tap, plot=FALSE)
plot(BTspec$freq, BTspec$spec, t='l', lwd=2)
df <- data.frame(freq=BTspec$freq, spec=BTspec$spec)
df <- df[order(df$spec, decreasing=T),]
df$period <- (1/df$freq)*1000
df

library(multitaper)
Mspec <- spec.mtm(ev.tap, deltat=1/100,
                  nw = 4, k = 7,
                  jackknife = TRUE, 
                  log='no', lwd=2)

a <- unlist(Mspec$mtm$jk$upperCI) #Mspec$spec)
N <- length(a)
a.h <- a[0:(N/2)]
x <- 0L:(N/2-1)
d <- data.frame(x, a.h)
d.sorted <- d[order(a.h, decreasing = TRUE),]
head(Mspec$freq[d.sorted$x], 20)

hm <- c(13, 16, 6, 21, 6, 8)
plot.show(ev.tap, start_time = min(eva), time = max(eva) - min(eva), harmonics = hm,
          xlab='Age (Ka)', ylab='', scale=0.5)
hm_l <- paste(hm, ' cycles')
hm_l <- c(hm_l, 'combined cycle')
legend('topright', legend=hm_l, col = c('blue','green','red','darkblue'),
       lty = rep(1,4), cex=0.40)


# multitaper analysis
dat <- data.frame(t=eva[2:(nr-1)], e=ev.tap)
dat2 <- resample(dat, seq(0.05, 10, by = 0.05))

eha(dat2,win=8,pad=1000, step=1, output=6)

