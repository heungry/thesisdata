# Set Working Directory to the Source File Location
# Session -> Set Working Directory -> To Source File Location

# Load libraries
library(data.table)
library(readxl)

# Load cleaned data
load(file = "Cleaned_data.RData")

#############################################################################
## I.Figures and statistics
#############################################################################
# 1.Short position histogram
hist(m_short$mean_position, breaks = c(0, 0.001*1:30, 1), xlim = c(0,0.031), freq = TRUE, right = FALSE,
     xlab = "Short_position", ylab = "Frequency", main = "Histogram of short positions")

# 2.Number of short coverage and short increase
# Change the time to get statistics in different years
ss = c_short[marks != 0 & position_date < as.Date("2013-01-01") & position_date >= as.Date("2012-01-01")]
dim(ss)[1]
sum(ss$marks == 1)
sum(ss$marks == -1)
length(unique(ss$ISIN))

# 3. Statistics of returns
marks = which(data_a$num_coverage > 0 & data_a$num_increase == 0 &!is.na(data_a$close) & !is.na(data_a$open))
marks = which(data_a$num_coverage == 0 & data_a$num_increase > 0 &!is.na(data_a$close) & !is.na(data_a$open))
# Check Nan records
which(is.na(data_a$close[marks - 1]) == TRUE)
marks = marks[-which(is.na(data_a$close[marks - 1]) == TRUE)]
length(marks)
# C-C return
mean((data_a$close[marks] - data_a$close[marks - 1])/data_a$close[marks - 1])
median((data_a$close[marks] - data_a$close[marks - 1])/data_a$close[marks - 1])
sd((data_a$close[marks] - data_a$close[marks - 1])/data_a$close[marks - 1])
# O-C return
mean((data_a$close[marks] - data_a$open[marks])/data_a$open[marks])
median((data_a$close[marks] - data_a$open[marks])/data_a$open[marks])
sd((data_a$close[marks] - data_a$open[marks])/data_a$open[marks])
# DAX30 return
r_dt = data_a$position_date[marks]
r_30 = rep(0, length(r_dt))
for (i in 1:length(r_30)){
  r_30[i] = as.numeric(r_dax[date == r_dt[i]]$dax_return)
}
mean(r_30)
median(r_30)
sd(r_30)

# 4. CDF of short coverage and increase
cdf_span_coverage = rep(0, 10)
cdf_span_increase = rep(0, 10)
cdf_span_coverage[1] = ecdf(span_coverage[span_coverage>0])(1)
cdf_span_increase[1] = ecdf(span_increase[span_increase>0])(1)
for (i in 2:9){
  cdf_span_coverage[i] = ecdf(span_coverage[span_coverage>0])(i) -ecdf(span_coverage[span_coverage>0])(i-1)
  cdf_span_increase[i] = ecdf(span_increase[span_increase>0])(i) -ecdf(span_increase[span_increase>0])(i-1)
}
cdf_span_coverage[10] = 1 - ecdf(span_coverage[span_coverage>0])(9)
cdf_span_increase[10] = 1 - ecdf(span_increase[span_increase>0])(9)
View(cbind(cdf_span_coverage, cdf_span_increase))

# 5.Sample volume
sample_vol = c()
for (i in 1:8){
  sample_vol[i] = sum(year(data_logit$date) == (2011 + i))
}
bar = barplot(sample_vol, names.arg = c(2012 : 2019), ylim = c(0, 10000),
              xlab = "Year", ylab = "Number", main = "Qualified sample points in each year")
text(bar, sample_vol, labels = sample_vol, pos = 3, cex = 0.8)

#############################################################################
## II. Logistic model and speculation strategy
#############################################################################
# 1.Logistic model
logitmodle_pro = function(formula_1, formula_2, data = data_logit, pred_lv = 0.5){
  # Fit model
  # For y
  model_fit_1 = glm(formula_1, data = data, subset = train, family = binomial, na.action = "na.omit")
  model_sum_1 = summary(model_fit_1)
  model_prd_1 = predict(model_fit_1, newdata = data[test, ], type = "response")
  coverage_pred_1 = ifelse(model_prd_1 > pred_lv, "TRUE", "FALSE")
  # For z
  model_fit_2 = glm(formula_2, data = data, subset = train, family = binomial, na.action = "na.omit")
  model_sum_2 = summary(model_fit_2)
  model_prd_2 = predict(model_fit_2, newdata = data[test, ], type = "response")
  coverage_pred_2 = ifelse(model_prd_2 > pred_lv, "TRUE", "FALSE")
  
  coverage_real_y = ifelse(data$y[test] == 1, "TRUE", "FALSE")
  coverage_real_z = ifelse(data$z[test] == 1, "TRUE", "FALSE")
  coverage_pred = ifelse(model_prd_1 > pred_lv & model_prd_2 < pred_lv, "TRUE", "FALSE")
  # Simulated performance
  test_perf = data.table()
  year_start = min(year(data$date[test]))
  year_end = max(year(data$date[test]))
  time_line = c()
  for (i in year_start : year_end){
    time_line = c(time_line, date[which(year(date) == i)])
  }
  time_line = as.Date(time_line, origin = "1970-01-01")
  test_perf$date = time_line
  
  sub_data = data[test, ]
  sub_data_1 = data[test, ]
  sub_data_2 = data[test, ]
  sub_data = sub_data[which(coverage_pred_1 == "TRUE" & coverage_pred_2 == "FALSE"), ]
  sub_data_1 = sub_data_1[which(coverage_pred_1 == "TRUE"), ]
  sub_data_2 = sub_data_2[which(coverage_pred_1 == "TRUE" & coverage_pred_2 == "TRUE"), ]
  test_perf$return_oc = 0
  test_perf$return_aoc = 1
  test_perf$return_cc = 0
  test_perf$return_acc = 1
  
  test_perf$return_oc_1 = 0
  test_perf$return_aoc_1 = 1
  test_perf$return_cc_1 = 0
  test_perf$return_acc_1 = 1
  
  test_perf$return_oc_2 = 0
  test_perf$return_aoc_2 = 1
  test_perf$return_cc_2 = 0
  test_perf$return_acc_2 = 1
  
  test_perf$return_dax = 0
  test_perf$return_dax_acc = 1
  
  for (i in 1:dim(test_perf)[1]){
    if (length(which(sub_data$date == test_perf$date[i])) > 0){
      test_perf$return_oc[i] = mean(sub_data$return_pred_oc[which(sub_data$date == test_perf$date[i])])
      test_perf$return_cc[i] = mean(sub_data$return_pred_cc[which(sub_data$date == test_perf$date[i])])
    }
    
    if (length(which(sub_data_1$date == test_perf$date[i])) > 0){
      test_perf$return_oc_1[i] = mean(sub_data_1$return_pred_oc[which(sub_data_1$date == test_perf$date[i])])
      test_perf$return_cc_1[i] = mean(sub_data_1$return_pred_cc[which(sub_data_1$date == test_perf$date[i])])
    }
    
    if (length(which(sub_data_2$date == test_perf$date[i])) > 0){
      test_perf$return_oc_2[i] = mean(sub_data_2$return_pred_oc[which(sub_data_2$date == test_perf$date[i])])
      test_perf$return_cc_2[i] = mean(sub_data_2$return_pred_cc[which(sub_data_2$date == test_perf$date[i])])
    }
    
    test_perf$return_dax[i] = r_dax$dax_return[which(date == test_perf$date[i])]
    
    if (i == 1) {
      test_perf$return_aoc[i] = test_perf$return_oc[i] + 1
      test_perf$return_acc[i] = test_perf$return_cc[i] + 1
      
      test_perf$return_aoc_1[i] = test_perf$return_oc_1[i] + 1
      test_perf$return_acc_1[i] = test_perf$return_cc_1[i] + 1
      
      test_perf$return_aoc_2[i] = test_perf$return_oc_2[i] + 1
      test_perf$return_acc_2[i] = test_perf$return_cc_2[i] + 1
      
      test_perf$return_dax_acc[i] = test_perf$return_dax[i] + 1
    } else{
      test_perf$return_aoc[i] = (test_perf$return_oc[i] + 1) * test_perf$return_aoc[i - 1]
      test_perf$return_acc[i] = (test_perf$return_cc[i] + 1) * test_perf$return_acc[i - 1]
      
      test_perf$return_aoc_1[i] = (test_perf$return_oc_1[i] + 1) * test_perf$return_aoc_1[i - 1]
      test_perf$return_acc_1[i] = (test_perf$return_cc_1[i] + 1) * test_perf$return_acc_1[i - 1]
      
      test_perf$return_aoc_2[i] = (test_perf$return_oc_2[i] + 1) * test_perf$return_aoc_2[i - 1]
      test_perf$return_acc_2[i] = (test_perf$return_cc_2[i] + 1) * test_perf$return_acc_2[i - 1]
      
      test_perf$return_dax_acc[i] = (test_perf$return_dax[i] + 1) * test_perf$return_dax_acc[i - 1]
    }
  }
  test_perf$return_aoc = test_perf$return_aoc - 1
  test_perf$return_acc = test_perf$return_acc - 1
  
  test_perf$return_aoc_1 = test_perf$return_aoc_1 - 1
  test_perf$return_acc_1 = test_perf$return_acc_1 - 1
  
  test_perf$return_aoc_2 = test_perf$return_aoc_2 - 1
  test_perf$return_acc_2 = test_perf$return_acc_2 - 1
  test_perf$return_dax_acc = test_perf$return_dax_acc - 1
  
  print(model_sum_1)
  print(model_sum_2)
  print("Prediction for y")
  print(table(coverage_real_y, coverage_pred_1))
  print("Prediction for z")
  print(table(coverage_real_z, coverage_pred_2))
  print("Prediction for y pro")
  print(table(coverage_real_y, coverage_pred))
  
  plot(test_perf$return_aoc, type = "l", xlab = "Trading days", ylab = "Return",
       main = "Accumulated return in test period", ylim = c(-1, 1))
  lines(test_perf$return_aoc_1, col = "blue")
  lines(test_perf$return_aoc_2, col = "green")
  lines(test_perf$return_dax_acc, col = "red")
  legend("topleft",cex = 0.8, legend = c("Prediction_coverage & no increase", "Prediction_coverage", "Prediction_coverage & increase","DAX 30"),
         col = c("black", "blue", "green", "red"), pch = c("_", "_", "_", "_"))
  return(test_perf)
}

# 2.Prediction model
prediction_test = function(formula_1, formula_2){
  # Fit model
  model_fit_1 = glm(formula_1, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
  model_sum_1 = summary(model_fit_1)
  model_prd_1 = predict(model_fit_1, newdata = data_logit[test, ], type = "response")
  
  model_fit_2 = glm(formula_2, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
  model_sum_2 = summary(model_fit_2)
  model_prd_2 = predict(model_fit_2, newdata = data_logit[test, ], type = "response")
  
  data_pred = data_logit[test, 1:8]
  data_pred$prob_1 = model_prd_1
  data_pred$prob_2 = model_prd_2
  return(data_pred)
}

# 3.Model simulation
lv = 0.5
train = year(data_logit$date) < 2020
test = year(data_logit$date) < 2020

# Model_1
f_1 = y ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_num_1 + coverage_num_2 + coverage_num_3 + PE + PB
model_fit = glm(f_1, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
summary(model_fit)
# Model_2
f_1 = y ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_num_1 + coverage_num_2 + coverage_num_3
model_fit = glm(f_1, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
summary(model_fit)
# Model_3
f_1 = y ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_amt_1 + coverage_amt_2 + coverage_amt_3 + PE + PB
model_fit = glm(f_1, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
summary(model_fit)
# Model_4
f_1 = y ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_amt_1 + coverage_amt_2 + coverage_amt_3
model_fit = glm(f_1, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
summary(model_fit)

# Table 9
# Short increase
f_1 = z ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_num_1 + coverage_num_2 + coverage_num_3
model_fit = glm(f_1, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
summary(model_fit)
# Short coverage
f_1 = y ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_num_1 + coverage_num_2 + coverage_num_3
model_fit = glm(f_1, data = data_logit, subset = train, family = binomial, na.action = "na.omit")
summary(model_fit)

# 4.Speculation Strategy
f_1 = y ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_num_1 + coverage_num_2 + coverage_num_3
f_2 = z ~ return_cc + turnover + last_coverage + shorter_num + position + I_increase + coverage_num_1 + coverage_num_2 + coverage_num_3

train = (year(data_logit$date) < 2015 & year(data_logit$date) > 2011)
test = year(data_logit$date) == 2015
r = logitmodle_pro(f_1, f_2, pred_lv = lv)
pred = prediction_test(f_1, f_2)

train = (year(data_logit$date) < 2016 & year(data_logit$date) > 2012)
test = year(data_logit$date) == 2016
r = rbind(r, logitmodle_pro(f_1, f_2, pred_lv = lv))
pred = rbind(pred, prediction_test(f_1, f_2))

train = (year(data_logit$date) < 2017 & year(data_logit$date) > 2013)
test = year(data_logit$date) == 2017
r = rbind(r, logitmodle_pro(f_1, f_2, pred_lv = lv))
pred = rbind(pred, prediction_test(f_1, f_2))

train = (year(data_logit$date) < 2018 & year(data_logit$date) > 2014)
test = year(data_logit$date) > 2017
r = rbind(r, logitmodle_pro(f_1, f_2, pred_lv = lv))
pred = rbind(pred, prediction_test(f_1, f_2))

# Sum up all the trading periods
r$return_aoc = 1
r$return_acc = 1
r$return_aoc_1 = 1
r$return_acc_1 = 1
r$return_aoc_2 = 1
r$return_acc_2 = 1

r$return_dax_acc = 1
r$return_mix_oc = 0
r$return_mix_cc = 0
r$return_mix_aoc = 1
r$return_mix_acc = 1

for (i in 1:dim(r)[1]){
  if (r$return_oc[i] == 0){
    r$return_mix_oc[i] = r$return_dax[i]
  } else{
    r$return_mix_oc[i] = r$return_oc[i]
  }
  if (r$return_cc[i] == 0){
    r$return_mix_cc[i] = r$return_dax[i]
  } else{
    r$return_mix_cc[i] = r$return_cc[i]
  }
  
  if (i == 1){
    r$return_aoc[i] = r$return_oc[i] + 1
    r$return_acc[i] = r$return_cc[i] + 1
    
    r$return_aoc_1[i] = r$return_oc_1[i] + 1
    r$return_acc_1[i] = r$return_cc_1[i] + 1
    
    r$return_aoc_2[i] = r$return_oc_2[i] + 1
    r$return_acc_2[i] = r$return_cc_2[i] + 1
    
    r$return_dax_acc[i] = r$return_dax[i] + 1
    r$return_mix_aoc[i] = r$return_mix_oc[i] + 1
    r$return_mix_acc[i] = r$return_mix_cc[i] + 1
  } else{
    r$return_aoc[i] = (r$return_oc[i] + 1) * r$return_aoc[i - 1]
    r$return_acc[i] = (r$return_cc[i] + 1) * r$return_acc[i - 1]
    r$return_aoc_1[i] = (r$return_oc_1[i] + 1) * r$return_aoc_1[i - 1]
    r$return_acc_1[i] = (r$return_cc_1[i] + 1) * r$return_acc_1[i - 1]
    r$return_aoc_2[i] = (r$return_oc_2[i] + 1) * r$return_aoc_2[i - 1]
    r$return_acc_2[i] = (r$return_cc_2[i] + 1) * r$return_acc_2[i - 1]
    r$return_dax_acc[i] = (r$return_dax[i] + 1) * r$return_dax_acc[i - 1]
    r$return_mix_aoc[i] = (r$return_mix_oc[i] + 1) * r$return_mix_aoc[i - 1]
    r$return_mix_acc[i] = (r$return_mix_cc[i] + 1) * r$return_mix_acc[i - 1]
  }
}
r$return_aoc = r$return_aoc - 1
r$return_acc = r$return_acc - 1
r$return_aoc_1 = r$return_aoc_1 - 1
r$return_acc_1 = r$return_acc_1 - 1
r$return_aoc_2 = r$return_aoc_2 - 1
r$return_acc_2 = r$return_acc_2 - 1
r$return_dax_acc = r$return_dax_acc - 1
r$return_mix_aoc = r$return_mix_aoc - 1
r$return_mix_acc = r$return_mix_acc - 1

#############################################################################
## III.Plot and analyze strategy
#############################################################################
# 1.Figure 4
plot(r$date, r$return_aoc_1, type = "l", xlab = "Prediction period", ylab = "Return",
     main = "Accumulated return of strategy in prediction period", ylim = c(-0.5, 1.5))
lines(r$date, r$return_dax_acc, col = "red")
legend("topleft",cex = 0.8, legend = c("Strategy return", "DAX 30 return"),
       col = c("black", "red"), pch = c("_", "_"))

# 2.Figure 5
plot(r$date, r$return_aoc_1, type = "l", xlab = "Prediction period", ylab = "Return",
     main = "Accumulated return of strategy in prediction period", ylim = c(-1, 3))
lines(r$date, r$return_acc_1, col = "blue")
lines(r$date, r$return_dax_acc, col = "red")
legend("topleft",cex = 0.8, legend = c("Strategy return_oc", "Strategy return_cc", "DAX 30 return"),
       col = c("black", "blue", "red"), pch = c("_", "_", "_"))

# 3.Figure 6
plot(r$date, r$return_aoc_1, type = "l", xlab = "Prediction period", ylab = "Return",
     main = "Accumulated return of new strategies in prediction period", ylim = c(-0.5, 1.5))
lines(r$date, r$return_aoc, col = "blue")
lines(r$date, r$return_dax_acc, col = "red")
lines(r$date, r$return_mix_aoc, col = "purple")
legend("topleft",cex = 0.8, legend = c("Original_strategy", "New_strategy", "New_strategy_enhanced", "DAX 30"),
       col = c("black", "blue", "purple", "red"), pch = c("_", "_", "_", "_"))

