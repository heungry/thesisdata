# Set Working Directory to the Source File Location
# Session -> Set Working Directory -> To Source File Location

# Load libraries
library(data.table)
library(readxl)

#############################################################################
## I. Import short position disclosures
#############################################################################
rd = read.csv2("Short_disclosure.csv", header = TRUE, sep = ",")
dt_short = data.table(as.character(rd$Positionsinhaber),
                as.character(rd$Emittent),
                as.character(rd$ISIN),
                rd$Position/100,
                as.Date(rd$Datum),
                stringsAsFactors=FALSE)

# Initialize columns' name
cnames = c("position_holder",
           "name_of_share_issuer",
           "ISIN",
           "net_short_position",
           "position_date")
colnames(dt_short) = cnames

#############################################################################
## II. Import stock market data
#############################################################################
# Initialize the data table
df = data.frame(stringsAsFactors=FALSE)

# Import the firm list and ISIN
fm = read_excel("Active.xlsx", range = "A1:B2041", sheet = "sample", col_names = TRUE)
isin = read_excel("Data_ISIN.xlsm", range = "B1:B2041", sheet = "Tabelle1", col_names = TRUE)
fm = cbind(fm, isin)

# Import date
dt = read_excel("Data_P.xlsm", range = "A2:A1762", sheet = "Tabelle1", col_names = "date")
dt = as.Date(dt[[1]])

# number of firms
n = dim(fm)[1]
# length of time
m = length(dt)

# 0.Initialization of the frame rows
# This might take more than 5 minutes!!!
for (i in 1:n) {
  dnf = cbind(dt, fm[i, ])
  df = rbind(df, dnf)
}

# 1.Close price
rd = read_excel("Data_P.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)

# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
close_price = c()
for (i in 1:n) {
  close_price = c(close_price, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, close_price, stringsAsFactors = FALSE)

# 2.Market value
rd = read_excel("Data_MV.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
market_value = c()
for (i in 1:n) {
  market_value = c(market_value, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, market_value, stringsAsFactors = FALSE)

# 3.Close ask price
rd = read_excel("Data_PA.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
ask_price = c()
for (i in 1:n) {
  ask_price = c(ask_price, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, ask_price, stringsAsFactors = FALSE)

# 4.Close bid price
rd = read_excel("Data_PB.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
bid_price = c()
for (i in 1:n) {
  bid_price = c(bid_price, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, bid_price, stringsAsFactors = FALSE)

# 5.PE rate
rd = read_excel("Data_PE.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
PE_rate = c()
for (i in 1:n) {
  PE_rate = c(PE_rate, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, PE_rate, stringsAsFactors = FALSE)

# 6.Turnover with volume
rd = read_excel("Data_VO.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
turnover_vol = c()
for (i in 1:n) {
  turnover_vol = c(turnover_vol, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, turnover_vol, stringsAsFactors = FALSE)

# 7.Turnover with value
rd = read_excel("Data_VA.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
turnover_val = c()
for (i in 1:n) {
  turnover_val = c(turnover_val, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, turnover_val, stringsAsFactors = FALSE)

# 8.Number of shares
rd = read_excel("Data_NOSH.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
number_of_shares = c()
for (i in 1:n) {
  number_of_shares = c(number_of_shares, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, number_of_shares, stringsAsFactors = FALSE)

# 9.Free float persentage
rd = read_excel("Data_NOSHFF.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
free_float = c()
for (i in 1:n) {
  free_float = c(free_float, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, free_float, stringsAsFactors = FALSE)

# 10.Highest price
rd = read_excel("Data_PH.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
high_price = c()
for (i in 1:n) {
  high_price = c(high_price, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, high_price, stringsAsFactors = FALSE)

# 11.Lowest price
rd = read_excel("Data_PL.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
low_price = c()
for (i in 1:n) {
  low_price = c(low_price, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, low_price, stringsAsFactors = FALSE)

# 12.Opening price
rd = read_excel("Data_PO.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
open_price = c()
for (i in 1:n) {
  open_price = c(open_price, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, open_price, stringsAsFactors = FALSE)

# 13.Price to book value
rd = read_excel("Data_PTBV.xlsm", range = "B2:BZM1762", sheet = "Tabelle1", col_names = FALSE)
# Check missing columns and replace error report to NA
# The Datastream names the missing colmuns' first row as the type of error
for (i in 1:n) {
  if (is.na(rd[[i]][2]))
    rd[[i]][1] = NA
}

# Reshape all data into one column
price_to_book = c()
for (i in 1:n) {
  price_to_book = c(price_to_book, rd[[i]])
}

# Add the data to the data frame
df = cbind(df, price_to_book, stringsAsFactors = FALSE)

# Formalize the column names
cnames = c("date",
           "name",
           "dscode",
           "ISIN",
           "close",
           "market_value",
           "ask",
           "bid",
           "PE",
           "turnover_vol",
           "turnover_val",
           "number_of_shares",
           "free_float_persentage",
           "high",
           "low",
           "open",
           "PB")
colnames(df) = cnames

# Convert data.frame to data.table and clean redundent variables
dt_stock = data.table(df)

# Delete useless data
rm(list = setdiff(ls(), c("dt_stock", "dt_short")))

# Import DAX30 data
dt_dax = read_excel("Data_DAX30.xlsm", range = "A2:B1762", sheet = "Tabelle1", col_names = c("date", "dax_close"))
dt_dax$date = as.Date(dax$date)

#############################################################################
## III. Clean short disclosure data
#############################################################################
# 1.Keep data since the start of regulation "2012-11-01" and keep unique records
a_short = unique(dt_short[position_date > as.Date("2012-10-31")])

# 2.Filter out large position (% is missing)
a_short[which(a_short$net_short_position > 0.2), 4] =
  a_short[which(a_short$net_short_position > 0.2), 4] / 100
# 3.Filter out no trading day (replaced by latest trading day)
a_short[which(a_short$position_date == as.Date("2016-12-11")), 5] = as.Date("2016-12-09")
a_short[which(a_short$position_date == as.Date("2016-09-04")), 5] = as.Date("2016-09-02")
a_short[which(a_short$position_date == as.Date("2016-01-09")), 5] = as.Date("2016-01-08")
a_short[which(a_short$position_date == as.Date("2015-09-20")), 5] = as.Date("2015-09-18")
a_short[which(a_short$position_date == as.Date("2014-12-28")), 5] = as.Date("2014-12-26")
a_short[which(a_short$position_date == as.Date("2014-11-15")), 5] = as.Date("2014-11-14")
a_short[which(a_short$position_date == as.Date("2014-02-08")), 5] = as.Date("2014-02-07")
a_short[which(a_short$position_date == as.Date("2013-01-13")), 5] = as.Date("2013-01-11")

# 4.Revise no-trading dates
# According to https://www.xetra.com/xetra-en/newsroom/trading-calendar
# Non-trading date should also be excluded or revised
date_nt = c("2012-12-24", "2012-12-25", "2012-12-26", "2012-12-31",
            "2013-01-01", "2013-03-29", "2013-04-01", "2013-05-01",
            "2013-12-24", "2013-12-25", "2013-12-26", "2013-12-31",
            "2014-01-01", "2014-04-18", "2014-04-21", "2014-05-01",
            "2014-10-03", "2014-12-24", "2014-12-25", "2014-12-26", "2014-12-31",
            "2015-01-01", "2015-04-03", "2015-04-06", "2015-05-01",
            "2015-05-25", "2015-12-24", "2015-12-25", "2015-12-31",
            "2016-01-01", "2016-03-25", "2016-03-28", "2016-05-16", "2016-10-03", "2016-12-26",
            "2017-04-14", "2017-04-17", "2017-05-01", "2017-06-05",
            "2017-10-03", "2017-10-31", "2017-12-25", "2017-12-26",
            "2018-01-01", "2018-03-30", "2018-04-02", "2018-05-01",
            "2018-05-21", "2018-10-03", "2018-12-24", "2018-12-25", "2018-12-26", "2018-12-31",
            "2019-01-01")
date_nt = as.Date(date_nt)

# Exclude no-trading date from dt_stock date
date = unique(dt_stock$date)
for (i in 1:length(date_nt)){
  if(sum(date == date_nt[i])) date = date[-which(date == date_nt[i])]
}
# Replace no-trading date in dt_short
for (i in 1:length(date_nt)){
  if (sum(a_short$position_date == date_nt[i])) {
    # Generate date to replace (the latest trading date)
    d = date[date <= date_nt[i]]
    d = d[length(d)]
    a_short[position_date == date_nt[i]]$position_date = d
  }
}
rm(d, i)

# 5. Take mean position for redundant disclosures
m_short = unique(a_short[, c(1,3,5)])
num_disclosure = NA
mean_position = NA
for (i in 1:dim(m_short)[1]) {
  ss = a_short[position_holder == m_short$position_holder[i] &
                 ISIN == m_short$ISIN[i] &
                 position_date == m_short$position_date[i]]
  num_disclosure[i] = dim(ss)[1]
  mean_position[i] = mean(ss$net_short_position)
}
m_short$num_disclosure = num_disclosure
m_short$mean_position = mean_position
rm(num_disclosure, mean_position, i, ss)

#############################################################################
## IV. Calculate accumulated short position for each stock
#############################################################################
# 1.Convert disclosures to accumulatied short position on all trading date
c_id = unique(m_short[, 1:2])
c_short = data.table()
for (i in 1:dim(c_id)[1]) {
  i_id = c_id[i, ]
  i_short = data.table(i_id$position_holder, i_id$ISIN, date, 0, 0)
  ss = m_short[position_holder == i_id$position_holder & ISIN == i_id$ISIN]
  ss = ss[order(ss$position_date), c(3, 5)]
  # Fill the i_short
  this_d = which(i_short$date == ss$position_date[1])
  # Mark short changes, -1 is short coverage, 1 is short increase
  if (ss$mean_position[1] > 0)
    i_short[this_d, 5] = 1

  if (length(ss$position_date) == 1) {
    next_d = which(i_short$date == last(i_short$date))
    i_short[this_d : next_d, 4] = ss$mean_position[1]
  } else{
    for (j in 2:dim(ss)[1]) {
      next_d = which(i_short$date == ss$position_date[j])
      i_short[this_d : (next_d - 1), 4] = ss$mean_position[j - 1]
      # Mark short changes, -1 is short coverage, 1 is short increase
      i_short[next_d, 5] = sign(ss$mean_position[j] - ss$mean_position[j - 1])
      this_d = next_d
    }
    next_d = which(i_short$date == last(i_short$date))
    i_short[this_d : next_d, 4] = ss$mean_position[j]
  }
  c_short = rbind(c_short, i_short)
}
colnames(c_short) = c("position_holder", "ISIN", "position_date", "mean_position", "marks")
c_short$coverage_amount = 0
c_short$increase_amount = 0
c_short[which(c_short$marks == -1), 6] = c_short[which(c_short$marks == -1) - 1, 4] - c_short[which(c_short$marks == -1), 4]
c_short[which(c_short$marks == 1), 7] = - c_short[which(c_short$marks == 1) - 1, 4] + c_short[which(c_short$marks == 1), 4]
# Add a count for shorter, 1 for positive positions
c_short$shorter_count = sign(c_short$mean_position)

rm(c_id, next_d, this_d, i_short, i_id)

# 2.Sum up all holders' short position
c_short_sum = data.table()
for (i in 1:length(isin)){
  ss = c_short[ISIN == isin[i]]
  i_short = data.table(isin[i], date, 0, 0, 0, 0, 0, 0)
  # Num of shorters for each ISIN
  ns = dim(ss)[1] / length(date)
  for (j in 1:ns) {
    i_short[, 3] = i_short[, 3] + ss[1:length(date) + (j - 1) * length(date), 4]
    i_short[, 6] = i_short[, 6] + ss[1:length(date) + (j - 1) * length(date), 6]
    i_short[, 7] = i_short[, 7] + ss[1:length(date) + (j - 1) * length(date), 7]
    i_short[, 8] = i_short[, 8] + ss[1:length(date) + (j - 1) * length(date), 8]
    marks = ss[1:length(date) + (j - 1) * length(date), 5]
    # Count short marks
    i_short[which(marks == -1), 4] = i_short[which(marks == -1), 4] + 1
    i_short[which(marks == 1), 5] = i_short[which(marks == 1), 5] + 1
  }
  c_short_sum = rbind(c_short_sum, i_short)
}
colnames(c_short_sum) = c("ISIN", "position_date", "sum_mean_position",
                          "num_coverage", "num_increase", "coverage_amount", "increase_amount", "num_shorter")
rm(ss, i, j, marks, ns)

#############################################################################
## V. Clean stock market data
#############################################################################
# 1.Identify stock according to ISIN
isin = unique(m_short$ISIN)
num_name_stock = NA
for(i in 1 : length(isin)) {
  num_name_stock[i] = length(unique(dt_stock[ISIN == isin[i]]$name))
}

isin_ft = c() #fitted ISIN within the dt_position
isin_uf = c() #unfitted ISIN
for (i in 1:length(isin)) {
  if (num_name_stock[i] == 0){
    isin_uf = c(isin_uf, isin[i])
    next
  }
  isin_ft = c(isin_ft, isin[i])
}

a_stock = data.table()
t = length(unique(dt_stock$date))
for (i in 1:length(isin_ft)) {
  ss = dt_stock[ISIN == isin_ft[i]]
  sn = unique(ss$name)
  if (length(sn) == 1){
    i_stock = ss
  } else {
    vol = c()
    for (j in 1:length(sn)) {
      # turnover_vol is used to identify a qualified data set but maybe another is better
      sv = as.numeric(ss[name == sn[j]]$turnover_vol)
      sv[is.na(sv)] = 0
      vol = c(vol, mean(sv))
    }
    i_stock = ss[1:t + (which.max(vol) - 1) * t, ]
  }
  a_stock = rbind(a_stock, i_stock)
}
# 2.Exclude non-trading date items
for (i in 1:length(date_nt)){
  a_stock = a_stock[-which(a_stock$date == date_nt[i]), ]
}
# 3.Switch chr to num
for (i in 5:17){
  a_stock[[i]] = as.numeric(a_stock[[i]])
}
# 4.Exclude turnover_val and rename turnover_vol to volume
a_stock = a_stock[, -11]
colnames(a_stock)[10] = "volume"

rm(i, j, num_name_stock, sn, sv, t, vol, ss, i_stock)

# 5. Calculate DAX30 return
r_dax = data.table(date = date)
r_dax$dax_close = 0
r_dax$dax_return = 0
for (i in 1:length(date)){
  r_dax$dax_close[i] = dt_dax$dax_close[which(dt_dax$date == r_dax$date[i])]
  if (i > 1) r_dax$dax_return[i] = (r_dax$dax_close[i] - r_dax$dax_close[i - 1]) / r_dax$dax_close[i - 1]
}

#############################################################################
## VI. Merge stock market data and short position
#############################################################################
# 1,Merge data
data_s = data.table() #Initial merge table (short and stock)
data_a = data.table() #Initial merge table (short_sum and stock)
# Filtering all the fitted shorts
for (i in 1:length(isin_ft)){
  ss1 = c_short[ISIN == isin_ft[i]]
  ss2 = c_short_sum[ISIN == isin_ft[i]]
  ss = a_stock[ISIN == isin_ft[i]]
  data_s = rbind(data_s, cbind(ss1, ss[, 5:16]))
  data_a = rbind(data_a, cbind(ss2, ss[, 5:16]))
}

# Add turnover and float_shares
data_a$float_shares = data_a$free_float_persentage/100 * data_a$number_of_shares
data_a$turnover = data_a$volume / data_a$number_of_shares

rm(ss, ss1, ss2, i)

# 2.Add coverage and increase structure
data_a$a_coverage = 0
data_a$a_increase = 0
for (i in 1:length(isin_ft)){
  data_a$a_coverage[10:length(date) + (i - 1)*length(date)] =
    data_a$num_coverage[10:length(date) - 2 + (i - 1)*length(date)] +
    data_a$num_coverage[10:length(date) - 3 + (i - 1)*length(date)] +
    data_a$num_coverage[10:length(date) - 4 + (i - 1)*length(date)] +
    data_a$num_coverage[10:length(date) - 5 + (i - 1)*length(date)] +
    data_a$num_coverage[10:length(date) - 6 + (i - 1)*length(date)] +
    data_a$num_coverage[10:length(date) - 7 + (i - 1)*length(date)] +
    data_a$num_coverage[10:length(date) - 8 + (i - 1)*length(date)] +
    data_a$num_coverage[10:length(date) - 9 + (i - 1)*length(date)]
  
  data_a$a_increase[10:length(date) + (i - 1)*length(date)] =
    data_a$num_increase[10:length(date) - 2 + (i - 1)*length(date)] +
    data_a$num_increase[10:length(date) - 3 + (i - 1)*length(date)] +
    data_a$num_increase[10:length(date) - 4 + (i - 1)*length(date)] +
    data_a$num_increase[10:length(date) - 5 + (i - 1)*length(date)] +
    data_a$num_increase[10:length(date) - 6 + (i - 1)*length(date)] +
    data_a$num_increase[10:length(date) - 7 + (i - 1)*length(date)] +
    data_a$num_increase[10:length(date) - 8 + (i - 1)*length(date)] +
    data_a$num_increase[10:length(date) - 9 + (i - 1)*length(date)]
}
data_a$mark_coverage = as.numeric(data_a$a_coverage > 0)
data_a$mark_increase = as.numeric(data_a$a_increase > 0)

# 3.Calculate short coverage and short increase time span
span_coverage = rep(0, dim(data_a)[1])
span_increase = rep(0, dim(data_a)[1])
last_coverage = rep(0, dim(data_a)[1])
last_increase = rep(0, dim(data_a)[1])

for (i in 1:length(isin_ft)){
  marks = which(data_a[1: length(date) + (i - 1) * length(date), 4] > 0)
  if (length(marks) > 1){
    span_coverage[marks[-1] + (i - 1) * length(date)] = marks[-1] - marks[-length(marks)]
    for (j in 2: length(marks)){
      last_coverage[(marks[j - 1] + 1) : marks[j] + (i - 1) * length(date)] = c(1: (marks[j] - marks[j - 1]))
    }
    last_coverage[(marks[j] + 1) : length(date) + (i - 1) * length(date)] = c(1: (length(date) - marks[j]))
  }
  
  marks = which(data_a[1: length(date) + (i - 1) * length(date), 5] > 0)
  if (length(marks) > 1){
    span_increase[marks[-1] + (i - 1) * length(date)] = marks[-1] - marks[-length(marks)]
    for (j in 2: length(marks)){
      last_increase[(marks[j - 1] + 1) : marks[j] + (i - 1) * length(date)] = c(1: (marks[j] - marks[j - 1]))
    }
    last_increase[(marks[j] + 1) : length(date) + (i - 1) * length(date)] = c(1: (length(date) - marks[j]))
  }
}
data_a$last_coverage = last_coverage
data_a$last_increase = last_increase

rm(i, j, marks)

#############################################################################
## VII. Generate sample pool
#############################################################################
# Sample days are last_coverage < 10 at the same time > 1
data_logit = data.table()
sample_marks = which(data_a$last_coverage > 1 & data_a$last_coverage < 10)
# Data on t
data_logit$y = sign(data_a$num_coverage[sample_marks])
data_logit$z = sign(data_a$num_increase[sample_marks])
data_logit$ISIN = data_a$ISIN[sample_marks]
data_logit$date = data_a$position_date[sample_marks]
data_logit$last_coverage = data_a$last_coverage[sample_marks]
data_logit$return_pred_oc = (data_a$close[sample_marks] - data_a$open[sample_marks]) / data_a$open[sample_marks]
data_logit$return_pred_cc = (data_a$close[sample_marks] - data_a$close[sample_marks - 1]) / data_a$close[sample_marks - 1]
# Data on t-1
data_logit$return_cc = (data_a$close[sample_marks - 1] - data_a$close[sample_marks - 2]) / data_a$close[sample_marks - 2]
data_logit$return_oc = (data_a$close[sample_marks - 1] - data_a$open[sample_marks - 1]) / data_a$open[sample_marks - 1]
data_logit$position = data_a$sum_mean_position[sample_marks - 2]
data_logit$shorter_num = data_a$num_shorter[sample_marks - 2]
data_logit$turnover = data_a$turnover[sample_marks - 1]
data_logit$turnover_lag = data_a$turnover[sample_marks - 2]
data_logit$PE = data_a$PE[sample_marks - 1]
data_logit$PB = data_a$PB[sample_marks - 1]
# Data before
data_logit$coverage_num_1 = 0
data_logit$coverage_num_2 = 0
data_logit$coverage_num_3 = 0
data_logit$coverage_amt_1 = 0
data_logit$coverage_amt_2 = 0
data_logit$coverage_amt_3 = 0
# Coverage_num & coverage_scale
data_logit$coverage_num_1 = data_a$num_coverage[sample_marks - 2] + data_a$num_coverage[sample_marks - 3]
data_logit$coverage_num_2 = data_a$num_coverage[sample_marks - 4] + data_a$num_coverage[sample_marks - 5]
data_logit$coverage_num_3 = (data_a$num_coverage[sample_marks - 6] + data_a$num_coverage[sample_marks - 7]
                             + data_a$num_coverage[sample_marks - 8] + data_a$num_coverage[sample_marks - 9])
data_logit$coverage_amt_1 = data_a$coverage_amount[sample_marks - 2] + data_a$coverage_amount[sample_marks - 3]
data_logit$coverage_amt_2 = data_a$coverage_amount[sample_marks - 4] + data_a$coverage_amount[sample_marks - 5]
data_logit$coverage_amt_3 = (data_a$coverage_amount[sample_marks - 6] + data_a$coverage_amount[sample_marks - 7]
                             + data_a$coverage_amount[sample_marks - 8] + data_a$coverage_amount[sample_marks - 9])
# Indicator head and tail
# head: more coverage in t-2:t-3 than t-4:t-5
# tail: more coverage in t-6:t-9 than t-2:t:5
data_logit$I_head = as.numeric(data_logit$coverage_num_1 > data_logit$coverage_num_2)
data_logit$I_tail = as.numeric((data_logit$coverage_num_1 + data_logit$coverage_num_2) < data_logit$coverage_num_3)

# Indicator and number of increase exclosure
data_logit$increase_num = data_a$a_increase[sample_marks]
data_logit$I_increase = as.numeric(data_logit$increase_num > 0)

rm(sample_marks)
#############################################################################
## VIII. Save all the cleaned data
#############################################################################
save(a_short, a_stock, c_short, c_short_sum, data_logit,
     data_a, data_s, date, date_nt, dt_dax, dt_short, dt_stock,
     isin, isin_ft, isin_uf, last_coverage, last_increase, m_short,
     r_dax, span_coverage, span_increase ,file = "Cleaned_data.RData")
