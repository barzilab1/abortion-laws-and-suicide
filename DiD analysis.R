library(readr)
library(readxl)
library(fixest)
library(weights)

suicide_rate <- read_csv("data/0_MASTER_suicide_rate_1968_2019_wide_v5.csv")
states = read_csv("data/states.csv")
unemployment <- read_excel("data/unemployment_export.xls")
population_estimates_1960_2018 <- read_excel("data/0_intercensus_population_estimates_1960_2018.xlsx")
motor_accidents = read_csv("data/0_motor_vehicle_accidents_1968_2016.csv")

suicide_rate = merge(suicide_rate,states, by.x = "statefip", by.y = "FIPS")
str(suicide_rate)

suicide_rate = merge(suicide_rate , unemployment[,c("statefip", "year","adj_unemp_rate_avg")], all.x = T)
suicide_rate = merge(suicide_rate , population_estimates_1960_2018)


motor_accidents = motor_accidents[,grep("statefip|year|F_(20|25|35)", colnames(motor_accidents) )]
motor_accidents$F_20_34_car_deaths = motor_accidents$F_20_24_car_deaths + motor_accidents$F_25_34_car_deaths
motor_accidents$F_20_44_car_deaths = motor_accidents$F_20_34_car_deaths + motor_accidents$F_35_44_car_deaths

motor_accidents$F_20_34_car_pop_100k = (motor_accidents$F_20_24_car_pop + motor_accidents$F_25_34_car_pop)/100000
motor_accidents$F_20_44_car_pop_100k = (motor_accidents$F_20_24_car_pop + motor_accidents$F_25_34_car_pop + motor_accidents$F_35_44_car_pop)/100000

motor_accidents$F_20_34_car_deaths_rate = motor_accidents$F_20_34_car_deaths / motor_accidents$F_20_34_car_pop_100k
motor_accidents$F_20_44_car_deaths_rate = motor_accidents$F_20_44_car_deaths / motor_accidents$F_20_44_car_pop_100k

suicide_rate = merge(suicide_rate , motor_accidents[,grep("statefip|year|rate", colnames(motor_accidents), value = T )], all.x = T)






did_analysis <- function(dependent, treatment) {
  
  features_not_scale = c("statefip", "year", "population_tot")
  features = c(dependent, treatment, "black_per", "frac_repub", "gdp_growth", "adj_unemp_rate_avg", features_not_scale)
  
  # subset the data, remove rows with missing data and only then scale
  temp_data = suicide_rate[,features]
  temp_data = temp_data[complete.cases(temp_data),]
  temp_data[,-which(names(temp_data) %in% features_not_scale)] = apply(temp_data[,-which(names(temp_data) %in% features_not_scale)], 2, stdz,temp_data$population_tot)
  
  
  did_formula = formula(paste0(dependent, "~", treatment, 
                           "+ black_per + frac_repub + gdp_growth + adj_unemp_rate_avg |                 
                             statefip + year "))
  cat("\n\n\n")
 mod = feols(did_formula,                            
              weights = ~ population_tot,
              cluster = ~ statefip + year ,                           
              data = temp_data)
  
  print(summary(mod))
  cat("\n")
  print(confint(mod))
  
  mod  

}




sink(file = "results_r.txt", append = F, type = c("output", "message"), split = T)

#######################################
### TRAP_law_aus_har
#######################################

cat("\n\n#######################################
### TRAP_law_aus_har
#######################################")
mod2 = did_analysis("F_suicide_rate_20_34", "TRAP_law_aus_har")
mod1 = did_analysis("F_suicide_rate_20_44", "TRAP_law_aus_har")
mod3 = did_analysis("F_suicide_rate_45_64", "TRAP_law_aus_har")
mod4 = did_analysis("F_suicide_rate_45_74", "TRAP_law_aus_har")
m = did_analysis("F_20_34_car_deaths_rate", "TRAP_law_aus_har")
m = did_analysis("F_20_44_car_deaths_rate", "TRAP_law_aus_har")


#######################################
### naral_sum_index_exc_insurance_t1
#######################################

cat("\n\n#######################################
### naral_sum_index_exc_insurance_t1
#######################################")
suicide_rate$naral_sum_index_exc_insurance_t1_negative = -1 * suicide_rate$naral_sum_index_exc_insurance_t1
mod5 = did_analysis("F_suicide_rate_20_44", "naral_sum_index_exc_insurance_t1_negative")
mod6 = did_analysis("F_suicide_rate_20_34", "naral_sum_index_exc_insurance_t1_negative")
mod7 = did_analysis("F_suicide_rate_45_64", "naral_sum_index_exc_insurance_t1_negative")
mod8 = did_analysis("F_suicide_rate_45_74", "naral_sum_index_exc_insurance_t1_negative")
m = did_analysis("F_20_34_car_deaths_rate", "naral_sum_index_exc_insurance_t1_negative")
m = did_analysis("F_20_44_car_deaths_rate", "naral_sum_index_exc_insurance_t1_negative")

#######################################
### naral_sum_index_wt_exc_ins_abrt1
#######################################

cat("\n\n#######################################
### naral_sum_index_wt_exc_ins_abrt1
#######################################")
suicide_rate$naral_sum_index_wt_exc_ins_abrt1_negative = -1 *suicide_rate$naral_sum_index_wt_exc_ins_abrt1
mod9 = did_analysis("F_suicide_rate_20_44", "naral_sum_index_wt_exc_ins_abrt1_negative")
mod10 = did_analysis("F_suicide_rate_20_34", "naral_sum_index_wt_exc_ins_abrt1_negative")
mod11 = did_analysis("F_suicide_rate_45_64", "naral_sum_index_wt_exc_ins_abrt1_negative")
mod12 = did_analysis("F_suicide_rate_45_74", "naral_sum_index_wt_exc_ins_abrt1_negative")
m = did_analysis("F_20_34_car_deaths_rate", "naral_sum_index_wt_exc_ins_abrt1_negative")
m = did_analysis("F_20_44_car_deaths_rate", "naral_sum_index_wt_exc_ins_abrt1_negative")

sink(file=NULL)





library(data.table)
setDT(suicide_rate)[, trap_state := any(TRAP_law_aus_har, na.rm = T)*1, by = Cstate]
suicide_rate[, binary_trap := (TRAP_law_aus_har>0)*1 ]
suicide_rate[is.na(binary_trap) & year == 1973 , binary_trap := 0 ]

suicide_rate = suicide_rate[order(year),.SD,by = Cstate]



suicide_rate[ trap_state == 1 ,event_time_0 := binary_trap - shift(binary_trap), by = Cstate]

# fix
suicide_rate[ statefip %in% c(2,17,18,44),event_time_0 := 0]
suicide_rate[ statefip %in% c(17,18,44) & year == 1973 ,event_time_0 := 1]
suicide_rate[ statefip == 2 & year == 1970 ,event_time_0 := 1]

suicide_rate[ trap_state == 1 ,event_time_1 := shift(event_time_0, 1,"lead")  , by = Cstate]
suicide_rate[ trap_state == 1 ,event_time_2 := shift(event_time_1, 1,"lead")  , by = Cstate]
suicide_rate[ trap_state == 1 ,event_time_LR3 := {
  rn <- 1:.N
  i2 <- which(event_time_2 == 1)
  fcase(
    rn %inrange% .(i2 + 1, .N), 1,
    default = 0
  )
} , by = Cstate]


suicide_rate[ ,event_time_m1 := 0 ]
suicide_rate[ trap_state == 1 ,event_time_m2 := shift(event_time_0, -2)  , by = Cstate]
suicide_rate[ trap_state == 1 ,event_time_mLR3 := {
  rn <- 1:.N
  i2 <- which(event_time_m2 == 1)
  fcase(
    rn %inrange% .(1, i2 - 1), 1,
    default = 0
  )
} , by = Cstate]
# https://stackoverflow.com/questions/66191616/r-filling-the-leading-and-lagging-values-in-data-table-if-some-condition-occurs

# clean all irrelevant values. including the fixing states from above 
suicide_rate[is.na(TRAP_law_aus_har), c("event_time_0","event_time_1", "event_time_2","event_time_LR3", "event_time_m2", "event_time_mLR3"):=NA]
# add 0 instead of NA
suicide_rate[is.na(event_time_0) & !is.na(TRAP_law_aus_har) , event_time_0:=0]
suicide_rate[is.na(event_time_1) & !is.na(TRAP_law_aus_har) , event_time_1:=0]
suicide_rate[is.na(event_time_2) & !is.na(TRAP_law_aus_har) , event_time_2:=0]
suicide_rate[is.na(event_time_m2) & !is.na(TRAP_law_aus_har) , event_time_m2:=0]


suicide_rate[,View(.SD), .SDcols = c("Cstate", "TRAP_law_aus_har","binary_trap","year" ,"event_time_mLR3","event_time_m2","event_time_m1", "event_time_0","event_time_1" , "event_time_2", "event_time_LR3" )]




features_not_scale = c("statefip", "year", "population_tot",
                       "F_suicide_rate_20_34","F_suicide_rate_45_64",
                       "event_time_0","event_time_1", "event_time_2","event_time_LR3",
                       "event_time_m1","event_time_m2","event_time_mLR3" )
features = c("black_per", "frac_repub", "gdp_growth", "adj_unemp_rate_avg", features_not_scale)

# subset the data, remove rows with missing data and only then scale
temp_data = as.data.frame(suicide_rate)[,features]
temp_data = temp_data[complete.cases(temp_data),]
temp_data[,-which(names(temp_data) %in% features_not_scale)] = apply(temp_data[,-which(names(temp_data) %in% features_not_scale)], 2, stdz,temp_data$population_tot)


did_formula = formula("F_suicide_rate_20_34 ~ event_time_mLR3 + event_time_m2  +  #event_time_m1+
                             event_time_0 + event_time_1 + event_time_2 + event_time_LR3 +
                             black_per + frac_repub + gdp_growth + adj_unemp_rate_avg |                 
                             statefip + year ")

mod = feols(did_formula,                            
            weights = ~ population_tot,
            cluster = ~ statefip + year ,                           
            data =  suicide_rate[trap_state ==1,])

summary(mod)
confint(mod)



did_formula = formula("F_suicide_rate_45_64 ~ event_time_mLR3 + event_time_m2  +event_time_m1+
                             event_time_0 + event_time_1 + event_time_2 + event_time_LR3 +
                             black_per + frac_repub + gdp_growth + adj_unemp_rate_avg |                 
                             statefip + year ")

mod = feols(did_formula,                            
            weights = ~ population_tot,
            cluster = ~ statefip + year ,                           
            data = suicide_rate[trap_state ==1,])

summary(mod)
confint(mod)





##########################
# economic magnitude
##########################



did_formula = formula(paste0("F_suicide_rate_20_34", "~", "TRAP_law_aus_har", 
                             "+ black_per + frac_repub + gdp_growth + adj_unemp_rate_avg |                 
                             statefip + year "))
mod = feols(did_formula,                            
            weights = ~ population_tot,
            cluster = ~ statefip + year ,                           
            data = suicide_rate)

summary(mod)




suicide_rate[ trap_state == 1 ,event_time_m1 := shift(event_time_0, -1)  , by = Cstate]

suicide_rate_before_trap = as.data.frame(suicide_rate[suicide_rate$event_time_m1 == 1 | suicide_rate$event_time_m2 == 1 | suicide_rate$event_time_mLR3 == 1,
                                                     .SD, .SDcols= c("F_suicide_rate_20_34", "TRAP_law_aus_har", "black_per","frac_repub","gdp_growth","adj_unemp_rate_avg","population_tot" )])
suicide_rate_before_trap = suicide_rate_before_trap[complete.cases(suicide_rate_before_trap),]

mean_20_34 = weighted.mean(suicide_rate_before_trap$F_suicide_rate_20_34, suicide_rate_before_trap$population_tot, na.rm = T)


beta_20_34 = coef(mod)["TRAP_law_aus_har"]


beta_20_34/mean_20_34*100









