library(shiny)
library(stringr)
library(readxl)
library(ggplot2)
library(viridis)
library(ggrepel)
library("RColorBrewer")
library(gridExtra)


## Fertilizer data -- add

## Future Notes
## draw population from census data so don't have to upload?
## stat factors -- waste water (maybe by locality)
## electricity could be associated by region and if enter zipcode(s) that could draw
## pat w envi sci r other tool institutional

## hosting -- use Amazon S3 or Dropbox -- both remote, arbitrary data storage

## Look into
## tidy census
## snap is by census track
## us avg with different methods


## Data

#cex_data <- read.csv("charlottesville_footprint.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
#general_data <- read.csv("generalData.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
#general_data <- sapply(general_data, as.numeric)
#block_groups <- cex_data$ID
block_groups <- c()

us_avg_meal_per <- read.csv("avg_meal_percentage.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
# need to fix us_avg_meal_per
#food_to_weight <- read.csv("food_to_weight.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
#food_out_cost <- read.csv("food_out_cost.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

constants <- read_excel("constants.xlsx")
food_out_cost <- read_excel("constants.xlsx", sheet = "foodOutCost")
food_out_cost$Average_Cost_Per_Meal <- as.numeric(food_out_cost$Average_Cost_Per_Meal)
food_to_weight <- read_excel("constants.xlsx", sheet = "FAH $ to Weight")
food_constants <- read_excel("Constants.xlsx", sheet = "foodFactors")
snap_to_weight <- read_excel("Constants.xlsx", sheet = "SNAP $ to Weight")
#food_out_cost <- read_excel(constants, sheet = ) ## NEED TO ADD
meal_percentages <- read_excel("Constants.xlsx", sheet = "mealPercentages")
pet_constants <- read_excel("constants.xlsx", sheet = "pet")
pet_constants$Cats <- as.numeric(pet_constants$Cats)
pet_constants$Dogs <- as.numeric(pet_constants$Dogs)

electricity_natGas_constants <- read_excel("constants.xlsx", sheet = "electricityAndNatGas")
transportation_constants <- read_excel("constants.xlsx", sheet = "transportation")
wastewater_constants <- read_excel("constants.xlsx", sheet = "wastewater")
misc_constants <- read_excel("constants.xlsx", sheet = "misc")

total_treated_wastewater <- 1398475760  # Charlottesville's numbers
total_therms_by_residents <- 5867406.73 # Charlottesville's numbers
total_therms_by_business <- 5902852.02  # Charlottesville's numbers
wastewater_removal_factor <- 0.79       # Charlottesville's numbers


#kg_to_lb <- 2.20462
kg_to_lb <- misc_constants$Kg_to_lb


## FOOD ###############################################################################################################
food_calculations <- function(cex_data, general_data, isForInputCheck = FALSE){
  #cex_data <- cex_data_input
  #if(is.na(cex_data_input)){
  #  cex_data <- read.csv("charlottesville_footprint.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  #}
  ## FAH  ###############################################################################################################
  fah_col_lookup <- c("X1005_X", "X1006_X", "X1007_X", "X1008_X", "X1009_X", "X1011_X", 
                      "X1012_X", "X1014_X", "X1015_X", "X1016_X", "X1018_X", "X1019_X",
                      "X1020_X", "X1021_X", "X1022_X", "X1025_X", "X1026_X", "X1027_X",
                      "X1028_X", "X1029_X", "X1030_X", "X1031_X", "X1032_X", "X1034_X",
                      "X1035_X", "X1036_X", "X1037_X", "X1038_X", "X1039_X", "X1039_X",
                      "X1039_X", "X1040_X", "X1041_X", "X1042_X", "X1042_X", "X1042_X",
                      "X1043_X", "X1045_X", "X1046_X", "X1047_X", "X1049_X", "X1050_X",
                      "X1051_X", "X1052_X", "X1054_X", "X1055_X", "X1056_X", "X1057_X",
                      "X1058_X", "X1059_X", "X1059_X", "X1062_X", "X1063_X", "X1064_X",
                      "X1065_X", "X1066_X", "X1068_X", "X1069_X", "X1070_X", "X1071_X",
                      "X1074_X", "X1075_X", "X1076_X", "X1077_X", "X1078_X", "X1079_X", 
                      "X1080_X", "X1082_X", "X1084_X", "X1085_X", "X1086_X", "X1087_X",
                      "X1087_X", "X1088_X", "X1089_X", "X1092_X", "X1093_X", "X1094_X",
                      "X1095_X", "X1097_X", "X1098_X", "X1099_X", "X1100_X", "X1100_X",
                      "X1100_X", "X1101_X", "X1103_X", "X1103_X", "X1103_X", "X1103_X",
                      "X1103_X", "X1103_X", "X1103_X", "X1103_X", "X1103_X", "X1103_X", 
                      "X1103_X", "X1103_X", "X1103_X", "X1103_X", "X1103_X", "X1103_X",
                      "X1104_X", "X1104_X", "X1104_X", "X1104_X", "X1104_X", "X1104_X",
                      "X1104_X", "X1104_X", "X1104_X", "X1104_X", "X1104_X", "X1104_X",
                      "X1104_X", "X1104_X", "X1104_X", "X1104_X", "X1106_X", "X1107_X",
                      "X1109_X", "X1110_X", "X1111_X", "X1112_X", "X1114_X", "X1115_X",
                      "X1116_X", "X1117_X", "X1118_X", "X1118_X", "X1118_X", "X1118_X",
                      "X1118_X", "X1118_X", "X1118_X", "X1118_X", "X1118_X", "X1118_X",
                      "X1118_X", "X1118_X", "X1118_X", "X1118_X", "X1118_X", "X1118_X",
                      "X1120_X", "X1121_X", "X1123_X", "X1124_X", "X1125_X", "X1126_X",
                      "X1127_X", "X1128_X", "X2003_X", "X2005_X", "X2004_X", "X2006_X")
  fah_cols <- match(fah_col_lookup, names(cex_data))
  fah_to_weight_lookup <- c(1, 2, 141, 3, 4, 5, 6, 8, 142, 144, 146, 147, 143, 148, 145,
                            9, 12, 15, 16, 17, 19, 20, 21, 23, 24, 27, 29, 30, 62, 62, 62,
                            31, 32, 33, 33, 33, 34, 90, 91, 93, 96, 150, 149, 97, 98, 99,
                            100, 102, 104, 186, 186, 105, 106, 107, 108, 110, 115, 116, 118, 
                            119, 127, 153, 128, 154, 155, 156, 157, 158, 159, 160, 161, 129, 
                            129, 130, 162, 163, 131, 133, 164, 101, 165, 166, 167, 167, 167,
                            135, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 184, 
                            184, 184, 184, 184, 185, 185, 185, 185, 185, 185, 185, 185, 185, 
                            185, 185, 185, 185, 185, 185, 185, 136, 168, 169, 171, 172, 173, 
                            151, 123, 174, 152, 183, 183, 183, 183, 183, 183, 183, 183, 183, 
                            183, 183, 183, 183, 183, 183, 183, 175, 176, 137, 138, 182, 178,
                            179, 178, 139, 140, 180, 181)  
  
  fah_weights <- matrix(0, nrow=nrow(cex_data), ncol=length(fah_cols))
  for(i in 1:nrow(cex_data)){
    fah_weights[i,] <- as.numeric(cex_data[i, fah_cols] / food_to_weight$`PRICE USED (per lb)`[fah_to_weight_lookup])
  }
  fah_weights <- fah_weights / misc_constants$Kg_to_lb
  
  
  beef_col <- c(16:23, 29, 32, 34, 37, 98, 114, 140)
  pork_col <- c(24:28, 31, 33, 36, 97, 113, 139)
  chicken_col <- c(30, 35, 38:40, 96, 112, 138)
  cheese_col <- c(48, 51, 100, 116, 142)
  eggs_col <- c(44, 101, 117, 143)
  milk_col <- c(45:47, 49, 50, 99, 115, 141)
  fish_col <- c(41:43, 102, 118, 144)
  liquids_col <- c(66, 67, 75, 83, 95, 103, 104, 111, 125, 128, 137, 145, 146, 150, 156)
  grains_col <- c(1:15, 127)
  fruits_col <- c(52:56, 61:65, 79, 90, 106, 122, 132)
  nuts_col <- c(84, 86, 94, 110, 120, 136)
  oils_col <- c(80:82, 123, 124)
  beans_col <- c(69, 72, 85, 91, 107, 133)
  spices_col <- c(121)
  potatoes_col <- c(57, 92, 108, 119, 134)
  coffee_tea_col <- c(87, 103, 129, 147, 148, 149)
  sugar_col <- c(76:78)
  vegetables_col <- c(58:60, 68, 70, 71, 73, 74, 93, 109, 126, 135)
  ## wheat 88, 104 130
  ## rice 89, 105 131
  
  ## subtract when some columns are split between different categories
  fah_beef_n <- rowSums(fah_weights[, beef_col]) - fah_weights[,29] * (2/3) - fah_weights[,34] * (2/3) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[12]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[12]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[12])
  fah_pork_n <- rowSums(fah_weights[, pork_col]) - fah_weights[,31] * (2/3) - fah_weights[,36] * (2/3) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[11]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[11]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[11])
  fah_chicken_n <- rowSums(fah_weights[, chicken_col]) - fah_weights[,30] * (2/3) - fah_weights[,35] * (2/3) - 
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[10]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[10]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[10])
  fah_cheese_n <- rowSums(fah_weights[, cheese_col]) - fah_weights[,51] * (1/2) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[14]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[14]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[14])
  fah_eggs_n <- rowSums(fah_weights[, eggs_col]) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[15]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[15]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[15])
  fah_milk_n <- rowSums(fah_weights[, milk_col]) - fah_weights[,50] * (1/2) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[13]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[13]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[13])
  fah_fish_n <- rowSums(fah_weights[, fish_col]) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[16]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[16]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[16])
  fah_liquids_n <- rowSums(fah_weights[, liquids_col]) - fah_weights[,83] * (2/3) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[9]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[9]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[9])
  fah_grains_n <- rowSums(fah_weights[, grains_col])
  fah_fruits_n <- rowSums(fah_weights[, fruits_col]) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[4]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[4]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[4])
  fah_nuts_n <- rowSums(fah_weights[, nuts_col]) - fah_weights[,84] * (2/3) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[8]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[8]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[8])
  fah_oils_n <- rowSums(fah_weights[, oils_col])
  fah_beans_n <- rowSums(fah_weights[, beans_col]) - fah_weights[,72] * (1/2) - fah_weights[,85] * (2/3) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[5]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[5]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[5])
  fah_spices_n <- fah_weights[, spices_col]
  fah_potatoes_n <- rowSums(fah_weights[, potatoes_col]) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[6]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[6]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[6])
  fah_coffee_tea_n <- rowSums(fah_weights[, coffee_tea_col]) - 
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[1]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[1]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[1])
  fah_sugar_n <- rowSums(fah_weights[, sugar_col])
  fah_vegetables_n <- rowSums(fah_weights[, vegetables_col]) - fah_weights[,73] * (1/2) -
    fah_weights[,87] * (1 - meal_percentages$`Percent of Total Food Weight`[7]) -
    fah_weights[,103] * (1 - meal_percentages$`Percent of Total Food Weight`[7]) -
    fah_weights[,129] * (1 - meal_percentages$`Percent of Total Food Weight`[7]) 
  fah_category_n <- cbind(fah_beef_n, fah_pork_n, fah_chicken_n, fah_cheese_n, fah_eggs_n, 
                          fah_milk_n, fah_fish_n, fah_liquids_n, fah_grains_n, fah_fruits_n,
                          fah_nuts_n, fah_oils_n, fah_beans_n, fah_spices_n, fah_potatoes_n,
                          fah_coffee_tea_n, fah_sugar_n, fah_vegetables_n)
  
  fah_n <- rowSums(fah_category_n)
  ## liquids is diff, and fruit
  ## FAFH ###############################################################################################################
  fafh_col_lookup <- c("X1133_X",	"X1134_X",	"X1135_X",	"X1136_X",	"X1138_X",
                       "X1139_X",	"X1140_X",	"X1141_X",	"X1143_X",	"X1144_X",
                       "X1145_X",	"X1146_X",	"X1148_X",	"X1149_X",	"X1150_X",
                       "X1151_X",	"X1152_X",	"X1155_X")
  fafh_cols <- match(fafh_col_lookup, names(cex_data))
  fafh_sum_of_meals <- c()
  for (i in 1:nrow(cex_data)){
    fafh_num_of_meals <- cex_data[i, fafh_cols] / food_out_cost$Average_Cost_Per_Meal
    fafh_sum_of_meals <- c(fafh_sum_of_meals, sum(fafh_num_of_meals, na.rm=T))
  }
  
  fafh_weight_of_meals <- fafh_sum_of_meals * misc_constants$Weight_per_meal
  
  fafh_beef_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[12]
  fafh_pork_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[11]
  fafh_chicken_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[10]
  fafh_cheese_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[14]
  fafh_eggs_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[15]
  fafh_milk_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[13]
  fafh_fish_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[16]
  fafh_liquids_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[9]
  fafh_grains_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[2] +
    fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[3]
  fafh_fruits_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[4]
  fafh_nuts_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[8]
  fafh_oils_n <- 0
  fafh_beans_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[5]
  fafh_spices_n <- 0
  fafh_potatoes_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[6]
  fafh_coffee_tea_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[1]
  fafh_sugar_n <- 0
  fafh_vegetables_n <- fafh_weight_of_meals * meal_percentages$`Percent of Total Food Weight`[7]
  
  fafh_meal_percent_rows <- c(12, 11, 10, 14, 15, 13, 16, 9, 2, 3, 4, 8, 5, 6, 1, 7)
  fafh_per_category_n <- matrix(0, nrow=nrow(cex_data), ncol=length(fafh_meal_percent_rows))
  for(i in 1:nrow(cex_data)){
    fafh_per_category_n[i,] <- fafh_weight_of_meals[i] * meal_percentages$`Percent of Total Food Weight`[fafh_meal_percent_rows]
  }
  fafh_n <- rowSums(fafh_per_category_n)
  
  ## SNAP ###############################################################################################################
  # Charlottesville's
  # snap_percentages <- c(.079, .079, .0790, .12, .12,  .12, .108, .108, .198, .198,
  #                       .164, .164, .164, .164, .19, .19, .19, .02, .02, .02, 0.02,
  #                       .02, .042, .042, .028, .028, .028, .028, .107, .107, .107,
  #                       .107, .022, .022, .06, .06, .06)
  # num_of_households <- c(383, 344, 399, 566, 511, 321, 770, 476, 809, 700, 408, 593, 733,
  #                        527, 685, 274, 473, 729, 590, 508, 324, 173, 719, 453, 513, 561,
  #                        396, 244, 418, 238, 265, 655, 597, 403, 606, 504, 540)
  snap_percentages <- as.numeric(general_data[,'Percent.of.Households.on.SNAP'])
  num_of_households <- as.numeric(general_data[,'Households..total.'])
  households_on_snap <- snap_percentages * num_of_households
  avg_snap_money_per_household <- 1978.91666666667   
  avg_money_spent_on_snap <- snap_percentages * num_of_households * avg_snap_money_per_household 
  snap_percentage_of_total <- c(0.048,	0.048,	0.048,	0.048,	0.093,	0.072,	0.000470098,
                                0.00778274,	0.006999243,	0.000417865,	0.003604088,
                                0.00778274,	0.000313399,	0.007051476,	0.004178653,
                                0.002559425,	0.003395155,	0.020527631,	0.001253596,
                                0.001201363,	0.001462528,	0.069,	0.065,	0.054,	0.047,
                                0.035,	0.034,	0.000211204,	0.003496593,	0.003144587,
                                0.000187737,	0.001619228,	0.003496593,	0.000140802,
                                0.003168055,	0.001877366,	0.001149886,	0.00152536,
                                0.009222559,	0.00056321,	0.000539743,	0.000657078,	0.028,
                                0.027,	0.024,	0.021,	0.019,	0.017,	0.013,	0.011,	0.011,
                                0.01,	0.01,	0.009,	0.008,	0.006,	0.005,	0.004,	0.003)
  
  avg_price_entries <- c(1, 1, 1, 1, 30, 33, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
                         38, 38, 38, 39, 40, 43, 47, 57, 58, 59, 59, 59, 59, 59, 59, 59, 59,
                         59, 59, 59, 59, 59, 59, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 73,
                         74, 75, 77, 78, 79, 80, 81)
  avg_price <- snap_to_weight$`PRICE USED (per lb)`[avg_price_entries]
  
  snap_weight_by_category <- matrix(0, nrow=nrow(cex_data), ncol=length(avg_price_entries))
  for(i in 1:length(avg_price_entries)){
    snap_weight_by_category[i,] <- avg_money_spent_on_snap[i] * snap_percentage_of_total / 
      avg_price[i] / misc_constants$Kg_to_lb
  }
  snap_beef_col <- c(1, 17, 38)
  snap_pork_col <- c(2, 16, 37)
  snap_chicken_col <- c(3, 15, 36)
  snap_cheese_col <- c(19, 23, 40)
  snap_eggs_col <- c(20, 41, 50)
  snap_milk_col <- c(18, 26, 39)
  snap_fish_col <- c(4, 21, 42)
  snap_liquids_col <- c(5, 14, 35, 47, 48, 53)
  snap_grains_col <- c(8, 22, 24, 29, 43, 52, 57, 59)
  snap_fruits_col <- c(9, 25, 30, 58)
  snap_nuts_col <- c(13, 34, 55)
  snap_oils_col <- c(44, 45)
  snap_beans_col <- c(10, 31, 56)
  snap_spices_col <- c()
  snap_potatoes_col <- c(11, 27, 32)
  snap_coffee_tea_col <- c(7, 28, 49)
  snap_sugar_col <- c(46, 54)
  snap_vegetables_col <- c(6, 12, 33)
  ## split? 51
  
  snap_beef <- rowSums(snap_weight_by_category[, snap_beef_col])
  snap_pork <- rowSums(snap_weight_by_category[, snap_pork_col])
  snap_chicken <- rowSums(snap_weight_by_category[, snap_chicken_col])
  snap_cheese <- rowSums(snap_weight_by_category[, snap_cheese_col])
  snap_eggs <- rowSums(snap_weight_by_category[, snap_eggs_col])
  snap_milk <- rowSums(snap_weight_by_category[, snap_milk_col])
  snap_fish <- rowSums(snap_weight_by_category[, snap_fish_col])
  snap_liquids <- rowSums(snap_weight_by_category[, snap_liquids_col])
  snap_grains <- rowSums(snap_weight_by_category[, snap_grains_col])
  snap_fruits <- rowSums(snap_weight_by_category[, snap_fruits_col])
  snap_nuts <- rowSums(snap_weight_by_category[, snap_nuts_col])
  snap_oils <- rowSums(snap_weight_by_category[, snap_oils_col])
  snap_beans <- rowSums(snap_weight_by_category[, snap_beans_col])
  snap_spices <- rowSums(snap_weight_by_category[, snap_spices_col])
  snap_potatoes <- rowSums(snap_weight_by_category[, snap_potatoes_col])
  snap_coffee_tea <- rowSums(snap_weight_by_category[, snap_coffee_tea_col])
  snap_sugar <- rowSums(snap_weight_by_category[, snap_sugar_col])
  snap_vegetables <- rowSums(snap_weight_by_category[, snap_vegetables_col])
  snap_by_category <- cbind(snap_beef, snap_pork, snap_chicken, snap_cheese, snap_eggs, snap_milk, snap_fish,
                            snap_liquids, snap_grains, snap_fruits, snap_nuts, snap_oils, snap_beans, snap_spices,
                            snap_potatoes, snap_coffee_tea, snap_sugar, snap_vegetables)
  
  snap_totals <- rowSums(snap_by_category)
  
  ## IF For Input Check #################################################################################################
  if(isForInputCheck){
    beef_kg <- fah_beef_n + fafh_beef_n + snap_beef
    pork_kg <- fah_pork_n + fafh_pork_n + snap_pork
    chicken_kg <- fah_chicken_n + fafh_chicken_n + snap_chicken
    cheese_kg <- fah_cheese_n + fafh_cheese_n + snap_cheese
    eggs_kg <- fah_eggs_n + fafh_eggs_n + snap_eggs
    milk_kg <- fah_milk_n + fafh_milk_n + snap_milk
    fish_kg <- fah_fish_n + fafh_fish_n + snap_fish
    liquids_kg <- fah_liquids_n + fafh_liquids_n + snap_liquids
    grains_kg <- fah_grains_n + fafh_grains_n + snap_grains
    nuts_kg <- fah_nuts_n + fafh_nuts_n + snap_nuts
    fruits_kg <- fah_fruits_n + fafh_fruits_n + snap_fruits
    oils_kg <- fah_oils_n + fafh_oils_n + snap_oils
    beans_kg <- fah_beans_n + fafh_beans_n + snap_beans
    spices_kg <- fah_spices_n + fafh_spices_n + snap_spices
    potatoes_kg <- fah_potatoes_n + fafh_potatoes_n + snap_potatoes
    coffee_kg <- fah_coffee_tea_n + fafh_coffee_tea_n + snap_coffee_tea
    sugar_kg <- fah_sugar_n + fafh_sugar_n + snap_sugar
    vegetable_kg <- fah_vegetables_n + fafh_vegetables_n + snap_vegetables
    fah_kg <- fah_beef_n + fah_pork_n + fah_chicken_n + fah_cheese_n + fah_milk_n +
      fah_fish_n + fah_liquids_n + fah_grains_n + fah_liquids_n + fah_fruits_n + fah_nuts_n +
      fah_oils_n + fah_beans_n + fah_spices_n + fah_potatoes_n + fah_coffee_tea_n +
      fah_sugar_n + fah_vegetables_n
    fafh_kg <- fafh_beef_n + fafh_pork_n + fafh_chicken_n + fafh_cheese_n + fafh_milk_n + 
      fafh_fish_n + fafh_liquids_n + fafh_grains_n + fafh_liquids_n + fafh_fruits_n + fafh_nuts_n +
      fafh_oils_n + fafh_beans_n + fafh_spices_n + fafh_potatoes_n + fafh_coffee_tea_n +
      fafh_sugar_n + fafh_vegetables_n
    snap_kg <- snap_beef + snap_pork + snap_chicken + snap_cheese + snap_milk +
      snap_fish + snap_liquids + snap_grains + snap_liquids + snap_fruits + snap_nuts + 
      snap_sugar + snap_vegetables
    #food_by_sources <- data.frame( group = c("Beef", "Pork", "Chicken", "Cheese", "Eggs",
    #                                         "Milk", "Fish", "Liquids", "Grains", "Nuts",
    #                                         "Fruits", "Oils", "Beans", "Spices", "Potatoes",
    #                                         "Coffee and Tea", "Sugar", "Vegetables",
    #                                         "Food At Home", "Food Away From Home", "SNAP Food Data"),
    #                               value = c(beef_kg, pork_kg, chicken_kg,
    #                                         cheese_kg, eggs_kg, milk_kg,
    #                                         fish_kg, liquids_kg, grains_kg,
    #                                         nuts_kg, fruits_kg, oils_kg,
    #                                         beans_kg, spices_kg, potatoes_kg,
    #                                         coffee_kg, sugar_kg, vegetable_kg,
    #                                         fah_kg, fafh_kg, snap_kg))
    food_by_sources <- data.frame("Block Group" <- cex_data$ID,
                                  "Beef" <- beef_kg,
                                  "Pork" <- pork_kg,
                                  "Chicken" <- chicken_kg,
                                  "Cheese" <- cheese_kg,
                                  "Eggs" <- eggs_kg,
                                  "Milk" <- milk_kg,
                                  "Fish" <- fish_kg,
                                  "Liquids" <- liquids_kg,
                                  "Grains" <- grains_kg,
                                  "Nuts" <- nuts_kg,
                                  "Fruits" <- fruits_kg,
                                  "Oils" <- oils_kg,
                                  "Beans" <- beans_kg,
                                  "Spices" <- spices_kg,
                                  "Potatoes" <- potatoes_kg,
                                  "Coffee and Tea" <- coffee_kg,
                                  "Sugar" <- sugar_kg,
                                  "Vegetables" <- vegetable_kg,
                                  "Food At Home" <- fah_kg,
                                  "Food Away From Home" <- fafh_kg,
                                  "SNAP Food" <- snap_kg
      
    )
    return (food_by_sources)
    }
  
  
  ## FOOD PRODUCTION ####################################################################################################
  
  beef_combined <- fah_beef_n + fafh_beef_n + snap_beef
  beef_production_n <- beef_combined * food_constants$`Updated N content`[1]*food_constants$`Updated VNFs`[1] +
    beef_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[1] * food_constants$Transport_N_EF[1] +
    beef_combined * food_constants$`Updated N content`[1] * food_constants$`average % food waste (from N-print FY14 2.1)`[1]
  
  pork_combined <- fah_pork_n + fafh_pork_n + snap_pork
  pork_production_n <- pork_combined * food_constants$`Updated N content`[2]*food_constants$`Updated VNFs`[2] +
    pork_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[2] * food_constants$Transport_N_EF[1] +
    pork_combined * food_constants$`Updated N content`[2] * food_constants$`average % food waste (from N-print FY14 2.1)`[2]
  
  chicken_combined <- fah_chicken_n + fafh_chicken_n + snap_chicken
  chicken_production_n <- chicken_combined * food_constants$`Updated N content`[3]*food_constants$`Updated VNFs`[3] +
    chicken_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[3] * food_constants$Transport_N_EF[1] +
    chicken_combined * food_constants$`Updated N content`[3] * food_constants$`average % food waste (from N-print FY14 2.1)`[3]
  
  cheese_combined <- fah_cheese_n + fafh_cheese_n + snap_cheese
  cheese_production_n <- cheese_combined * food_constants$`Updated N content`[4]*food_constants$`Updated VNFs`[4] +
    cheese_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[4] * food_constants$Transport_N_EF[1] +
    cheese_combined * food_constants$`Updated N content`[4] * food_constants$`average % food waste (from N-print FY14 2.1)`[4]
  
  eggs_combined <- fah_eggs_n + fafh_eggs_n + snap_eggs
  eggs_production_n <- eggs_combined * food_constants$`Updated N content`[5]*food_constants$`Updated VNFs`[5] +
    eggs_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[5] * food_constants$Transport_N_EF[1] +
    eggs_combined * food_constants$`Updated N content`[5] * food_constants$`average % food waste (from N-print FY14 2.1)`[5]
  
  milk_combined <- fah_milk_n + fafh_milk_n + snap_milk
  milk_production_n <- milk_combined * food_constants$`Updated N content`[6]*food_constants$`Updated VNFs`[6] +
    milk_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[6] * food_constants$Transport_N_EF[1] +
    milk_combined * food_constants$`Updated N content`[6] * food_constants$`average % food waste (from N-print FY14 2.1)`[6]
  
  fish_combined <- fah_fish_n + fafh_fish_n + snap_fish
  fish_production_n <- fish_combined * food_constants$`Updated N content`[7]*food_constants$`Updated VNFs`[7] +
    fish_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[7] * food_constants$Transport_N_EF[1] +
    fish_combined * food_constants$`Updated N content`[7] * food_constants$`average % food waste (from N-print FY14 2.1)`[7]
  
  liquids_combined <- fah_liquids_n + fafh_liquids_n + snap_liquids
  liquids_production_n <- liquids_combined * food_constants$`Updated N content`[8]*food_constants$`Updated VNFs`[8] +
    liquids_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[8] * food_constants$Transport_N_EF[1] +
    liquids_combined * food_constants$`Updated N content`[8] * food_constants$`average % food waste (from N-print FY14 2.1)`[8]
  
  grains_combined <- fah_grains_n + fafh_grains_n + snap_grains
  grains_production_n <- grains_combined * food_constants$`Updated N content`[9]*food_constants$`Updated VNFs`[9] +
    grains_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[9] * food_constants$Transport_N_EF[1] +
    grains_combined * food_constants$`Updated N content`[9] * food_constants$`average % food waste (from N-print FY14 2.1)`[9]
  
  nuts_combined <- fah_nuts_n + fafh_nuts_n + snap_nuts
  nuts_production_n <- nuts_combined * food_constants$`Updated N content`[10]*food_constants$`Updated VNFs`[10] +
    nuts_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[10] * food_constants$Transport_N_EF[1] +
    nuts_combined * food_constants$`Updated N content`[10] * food_constants$`average % food waste (from N-print FY14 2.1)`[10]
  
  fruits_combined <- fah_fruits_n + fafh_fruits_n + snap_fruits
  fruits_production_n <- fruits_combined * food_constants$`Updated N content`[11]*food_constants$`Updated VNFs`[11] +
    fruits_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[11] * food_constants$Transport_N_EF[1] +
    fruits_combined * food_constants$`Updated N content`[11] * food_constants$`average % food waste (from N-print FY14 2.1)`[11]
  
  oils_combined <- fah_oils_n + fafh_oils_n + snap_oils
  oils_production_n <- oils_combined * food_constants$`Updated N content`[12]*food_constants$`Updated VNFs`[12] +
    oils_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[12] * food_constants$Transport_N_EF[1] +
    oils_combined * food_constants$`Updated N content`[12] * food_constants$`average % food waste (from N-print FY14 2.1)`[12]
  
  beans_combined <- fah_beans_n + fafh_beans_n + snap_beans
  beans_production_n <- beans_combined * food_constants$`Updated N content`[13]*food_constants$`Updated VNFs`[13] +
    beans_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[13] * food_constants$Transport_N_EF[1] +
    beans_combined * food_constants$`Updated N content`[13] * food_constants$`average % food waste (from N-print FY14 2.1)`[13]
  
  spices_combined <- fah_spices_n + fafh_spices_n + snap_spices
  spices_production_n <- spices_combined * food_constants$`Updated N content`[14]*food_constants$`Updated VNFs`[14] +
    spices_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[14] * food_constants$Transport_N_EF[1] +
    spices_combined * food_constants$`Updated N content`[14] * food_constants$`average % food waste (from N-print FY14 2.1)`[14]
  
  potatoes_combined <- fah_potatoes_n + fafh_potatoes_n + snap_potatoes
  potatoes_production_n <- potatoes_combined * food_constants$`Updated N content`[15]*food_constants$`Updated VNFs`[15] +
    potatoes_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[15] * food_constants$Transport_N_EF[1] +
    potatoes_combined * food_constants$`Updated N content`[15] * food_constants$`average % food waste (from N-print FY14 2.1)`[15]
  
  coffee_tea_combined <- fah_coffee_tea_n + fafh_coffee_tea_n + snap_coffee_tea
  coffee_tea_production_n <- coffee_tea_combined * food_constants$`Updated N content`[16]*food_constants$`Updated VNFs`[16] +
    coffee_tea_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[16] * food_constants$Transport_N_EF[1] +
    coffee_tea_combined * food_constants$`Updated N content`[16] * food_constants$`average % food waste (from N-print FY14 2.1)`[16]
  
  sugar_combined <- fah_sugar_n + fafh_sugar_n + snap_sugar
  sugar_production_n <- sugar_combined * food_constants$`Updated N content`[17]*food_constants$`Updated VNFs`[17] +
    sugar_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[17] * food_constants$Transport_N_EF[1] +
    sugar_combined * food_constants$`Updated N content`[17] * food_constants$`average % food waste (from N-print FY14 2.1)`[17]
  
  vegetables_combined <- fah_vegetables_n + fafh_vegetables_n + snap_vegetables
  vegetables_production_n <- vegetables_combined * food_constants$`Updated N content`[18]*food_constants$`Updated VNFs`[18] +
    vegetables_combined / food_constants$Food_Truck_Cargo_Capacity[1] * food_constants$`Food Miles`[18] * food_constants$Transport_N_EF[1] +
    vegetables_combined * food_constants$`Updated N content`[18] * food_constants$`average % food waste (from N-print FY14 2.1)`[18]
  
  total_food_production_totals <- cbind(beef_production_n, pork_production_n, chicken_production_n,
                                        cheese_production_n, eggs_production_n, milk_production_n,
                                        fish_production_n, liquids_production_n, grains_production_n,
                                        nuts_production_n, fruits_production_n, oils_production_n,
                                        beans_production_n, spices_production_n, potatoes_production_n,
                                        coffee_tea_production_n, sugar_production_n, vegetables_production_n)
 
  total_food_production_n <- rowSums(total_food_production_totals)
  
  fah_food <- cbind(fah_beef_n, fah_pork_n, fah_chicken_n, fah_cheese_n, fah_eggs_n, fah_milk_n,
                    fah_fish_n, fah_liquids_n, fah_grains_n, fah_nuts_n, fah_fruits_n, 
                    fah_oils_n, fah_beans_n, fah_spices_n, fah_potatoes_n, fah_coffee_tea_n,
                    fah_sugar_n, fah_vegetables_n)
  fafh_food <- cbind(fafh_beef_n, fafh_pork_n, fafh_chicken_n, fafh_cheese_n, fafh_eggs_n,
                     fafh_milk_n, fafh_fish_n, fafh_liquids_n, fafh_grains_n, fafh_nuts_n,
                     fafh_fruits_n, fafh_oils_n, fafh_beans_n, fafh_spices_n, fafh_potatoes_n,
                     fafh_coffee_tea_n, fafh_sugar_n, fafh_vegetables_n)                                                                                                                        
  snap_food <- cbind(snap_beef, snap_pork, snap_chicken, snap_cheese, snap_eggs, snap_milk,
                     snap_fish, snap_liquids, snap_grains, snap_nuts, snap_fruits, snap_oils,
                     snap_beans, snap_spices, snap_potatoes, snap_coffee_tea, snap_sugar,
                     snap_vegetables)
  food_by_sources <- data.frame( group = c("Beef", "Pork", "Chicken", "Cheese", "Eggs",
                                           "Milk", "Fish", "Liquids", "Grains", "Nuts",
                                           "Fruits", "Oils", "Beans", "Spices", "Potatoes",
                                           "Coffee and Tea", "Sugar", "Vegetables"),
                                 value = c(sum(beef_production_n, na.rm=T), sum(pork_production_n, na.rm=T), sum(chicken_production_n, na.rm=T),
                                           sum(cheese_production_n, na.rm=T), sum(eggs_production_n, na.rm=T), sum(milk_production_n, na.rm=T),
                                           sum(fish_production_n, na.rm=T), sum(liquids_production_n, na.rm=T), sum(grains_production_n, na.rm=T),
                                           sum(nuts_production_n, na.rm=T), sum(fruits_production_n, na.rm=T), sum(oils_production_n, na.rm=T),
                                           sum(beans_production_n, na.rm=T), sum(spices_production_n, na.rm=T), sum(potatoes_production_n, na.rm=T),
                                           sum(coffee_tea_production_n, na.rm=T), sum(sugar_production_n, na.rm=T), sum(vegetables_production_n, na.rm=T)))
  return(total_food_production_totals)
}
# total_food_production_totals <- food_calculations(cex_data)
# beef_production_n <- total_food_production_totals[,1]
# pork_production_n <- total_food_production_totals[,2]
# chicken_production_n <- total_food_production_totals[,3]
# cheese_production_n <- total_food_production_totals[,4]
# eggs_production_n <- total_food_production_totals[,5]
# milk_production_n <- total_food_production_totals[,6]
# fish_production_n <- total_food_production_totals[,7]
# liquids_production_n <- total_food_production_totals[,8]
# grains_production_n <- total_food_production_totals[,9]
# nuts_production_n <- total_food_production_totals[,10]
# fruits_production_n <- total_food_production_totals[,11]
# oils_production_n <- total_food_production_totals[,12]
# beans_production_n <- total_food_production_totals[,13]
# spices_production_n <- total_food_production_totals[,14]
# potatoes_production_n<- total_food_production_totals[,15]
# coffee_tea_production_n <- total_food_production_totals[,16]
# sugar_production_n <- total_food_production_totals[,17]
# vegetables_production_n <- total_food_production_totals[,18]

## PETS ###############################################################################################################
pet_calculations <- function(general_data, avg_cats_per_person, avg_dogs_per_person, isForInputCheck = FALSE){
  avg_cats_person <- avg_cats_per_person
  avg_dogs_person <- avg_dogs_per_person
  
  if(isForInputCheck){
    pet_df <- data.frame(
      "Block Group" <- general_data[,1],
      "Population" <- general_data$Total.Population.of.BG,
     # "Number of Businesses" <- general_data$Number.of.Businesses.in.BG,
      "Number of Cats" <- avg_cats_person * general_data$Total.Population.of.BG,
      "Number of Dogs" <- avg_dogs_person * general_data$Total.Population.of.BG
    )
      return (pet_df)
  }
  
  avg_cat_food_year <- as.numeric(pet_constants[2, "Cats"] * 365) ## average food per cat per year (kg)
  avg_dog_food_year <- as.numeric(pet_constants[2, "Dogs"] * 365) ## average food per dog per year (kg)
  
  ## Find which row each food type is stored in
  pet_food_types <- c("Chicken", "Grains", "Beef", "Fish", "Beans")
  pet_food_types_index <- c()
  for (t in pet_food_types){
    index <- which(grepl(t, food_constants$`Protein contents`))
    pet_food_types_index <- c(pet_food_types_index, index)
  }
  cat_food_per_block <- avg_cats_person * avg_cat_food_year * general_data[,'Total.Population.of.BG'] 
  dog_food_per_block <- avg_dogs_person * avg_dog_food_year * general_data[,'Total.Population.of.BG']
  
  ## Find average nitrogen factor per block per ingredient in pet food type
  cat_waste <- c()
  dog_waste <- c()
  amount_cat_food <- c()
  amount_dog_food <- c()
  for (i in 3:7){
    avg_cat_food_type <- as.numeric(pet_constants[i, "Cats"]) ##average percent of that food type in cat food
    avg_dog_food_type <- as.numeric(pet_constants[i, "Dogs"]) ##average percent of that food type in dog food
    food_type_factor <- as.numeric(food_constants$`Updated N content`[pet_food_types_index[i-2]])
    cat_waste_block <- avg_cat_food_type * food_type_factor * cat_food_per_block 
    dog_waste_block <- avg_dog_food_type * food_type_factor * dog_food_per_block 
    cat_waste <- c(cat_waste, cat_waste_block)
    dog_waste <- c(dog_waste, dog_waste_block)
    amount_cat_food <- c(amount_cat_food, avg_cat_food_type * cat_food_per_block)
    amount_dog_food <- c(amount_dog_food, avg_dog_food_type * dog_food_per_block)
  }
  cat_waste <- data.frame(matrix(cat_waste, length(cat_food_per_block), 5))
  dog_waste <- data.frame(matrix(dog_waste, length(dog_food_per_block), 5))
  amount_pet_food <- data.frame(matrix(amount_cat_food + amount_dog_food, length(cat_food_per_block), 5)) 
  
  pet_waste <- cat_waste + dog_waste
  colnames(cat_waste) <- pet_food_types
  colnames(dog_waste) <- pet_food_types
  ## Sum for each block and multiply by nitrogen up take in ground
  pet_waste_n <- rowSums(pet_waste * misc_constants$Pet_Waste_N_Uptake_Factor_of_Ground)
  
  
  ## Pet Food 
  pet_food <- c()
  ## for each ingredient in pet food, find the virtual Nitrogen, Nitrogen from transport, and food waste
  for (i in 1:5){
    virtual_n <- pet_waste[,i] * food_constants$`Updated VNFs`[pet_food_types_index[i]]
    num_of_trips <- amount_pet_food[,i] / food_constants$Food_Truck_Cargo_Capacity[1] 
    food_miles <- food_constants$`Food Miles`[1]
    transport_n <- num_of_trips * food_miles * food_constants$Transport_N_EF[1]
    food_waste_n <- pet_waste[,i] * food_constants$`average % food waste (from N-print FY14 2.1)`[pet_food_types_index[i]]
    pet_food <- c(pet_food , virtual_n + transport_n + food_waste_n)
  }
  pet_food <- data.frame(matrix(pet_food, length(cat_food_per_block), 5))
  colnames(pet_food) <- pet_food_types
  pet_food_n <- rowSums(pet_food)
  return(cbind(pet_food_n, pet_waste_n))
}
#pet_data <- pet_calculations(general_data)
#pet_food_n <- pet_data[,1]
#pet_waste_n <- pet_data[,2]

## FERTILIZER #########################################################################################################
## TRANSPORTATION #####################################################################################################
transportation_calculations <- function(cex_data, motorcycles_miles_year_input, passenger_cars_miles_year_input, light_trucks_miles_year_input, buses_miles_year_input, heavy_trucks_miles_year_input){
  # Charlottesville's numbers
  # motorcycles_miles_year <-378052
  # passenger_cars_miles_year <- 199774486
  # light_trucks_miles_year <- 26507935
  # buses_miles_year <- 611792
  # heavy_trucks_miles_year <- 4301174
  spent_on_gas <- cex_data$X6011_X
  spent_on_diesel <- cex_data$X6012_X
  spent_on_fares <- cex_data$X6061_X

  total_spent_on_gas <- sum(spent_on_gas, na.rm=T)
  total_spent_on_diesel <- sum(spent_on_diesel, na.rm=T)
  total_spent_on_fares <- sum(spent_on_fares, na.rm=T)

  motorcycles_spent_on_gas <- total_spent_on_gas / motorcycles_miles_year_input
  motorcycles_spent_on_diesel <- total_spent_on_diesel / motorcycles_miles_year_input
  passenger_cars_spent_on_gas <- total_spent_on_gas / passenger_cars_miles_year_input
  passenger_cars_spent_on_diesel <- total_spent_on_diesel / passenger_cars_miles_year_input
  light_trucks_spent_on_gas <- total_spent_on_gas / light_trucks_miles_year_input
  light_trucks_spent_on_diesel <- total_spent_on_diesel / light_trucks_miles_year_input
  buses_spent_on_fares <- total_spent_on_fares / buses_miles_year_input
  heavy_trucks_spent_on_gas <- total_spent_on_gas / heavy_trucks_miles_year_input
  heavy_trucks_spent_on_diesel <- total_spent_on_diesel / heavy_trucks_miles_year_input

  motorcycle_transit <- spent_on_gas / motorcycles_spent_on_gas * transportation_constants$'Gas%_Motorcycles'[1] +
    spent_on_diesel / motorcycles_spent_on_diesel * transportation_constants$`Diesel%_Motorcycles`[1]
  passenger_car_transit <- spent_on_gas / passenger_cars_spent_on_gas * transportation_constants$`Gas%_Passenger_Cars`[1] +
    spent_on_diesel / passenger_cars_spent_on_diesel * transportation_constants$`Diesel%_Passenger_Cars`[1]
  buses_transit <- spent_on_fares / buses_spent_on_fares
  light_truck_transit <- spent_on_gas / light_trucks_spent_on_gas * transportation_constants$`Gas%_Light_Trucks`[1] +
    spent_on_diesel / light_trucks_spent_on_diesel * transportation_constants$`Diesel%_Light_Trucks`[1]
  heavy_truck_transit <- spent_on_gas / heavy_trucks_spent_on_gas * transportation_constants$`Gas%_Heavy_Trucks`[1] +
    spent_on_diesel / heavy_trucks_spent_on_diesel * transportation_constants$`Diesel%_Heavy_Trucks`[1]
  
  motorcycle_n <- motorcycle_transit * transportation_constants$EF_Motorcycles[1] *
    transportation_constants$convert_to_N[1]+ 
    motorcycle_transit * transportation_constants$EF_Motorcycles[2] * 
    transportation_constants$convert_to_N[2]
  passenger_car_n <- passenger_car_transit * transportation_constants$EF_Passenger_Cars[1] * 
    transportation_constants$convert_to_N[1]  + 
    passenger_car_transit * transportation_constants$EF_Passenger_Cars[2] * 
    transportation_constants$convert_to_N[2] 
  buses_n <- buses_transit * transportation_constants$EF_Buses[1] * 
    transportation_constants$convert_to_N[1]  +
    buses_transit * transportation_constants$EF_Buses[2] * 
    transportation_constants$convert_to_N[2] 
  light_truck_n <- light_truck_transit * transportation_constants$EF_Light_Duty_Trucks[1] * 
    transportation_constants$convert_to_N[1]   + 
    light_truck_transit * transportation_constants$EF_Light_Duty_Trucks[2] * 
    transportation_constants$convert_to_N[2]
  heavy_truck_n <- heavy_truck_transit * transportation_constants$EF_Heavy_Duty_Trucks[1] *
    transportation_constants$convert_to_N[1] + 
    heavy_truck_transit * transportation_constants$EF_Heavy_Duty_Trucks[2] * 
    transportation_constants$convert_to_N[2] 
  
  transportation_n <- motorcycle_n + passenger_car_n + buses_n + light_truck_n + heavy_truck_n
  return(transportation_n)
}
## using Charlottesville's numbers for Motorcycle Miles per year, etc...
#transport_n <- transportation_calculations(cex_data, 378052, 199774486, 26507935, 611792, 4301174)
## ELECTRICITY ########################################################################################################
electricity_calculations <- function(cex_data, general_data, electricity_by_residents_input, electricity_by_businesses_input, egridRegion){
  dollars_spent_electricity <- cex_data[,"X3063_X"]
  num_of_businesses <- general_data[,"Number.of.Businesses.in.BG"]
  # electricity_by_residents <- 188422670 # Charlottesville's number
  # electricity_by_businesses <- 248985273.4 #Charlottesville's number
  
  regions <- c("AKGD", "AKMS", "AZNM", "CAMX", "ERCT", "FRCC", "HIMS", "HIOA", "MROE", "MROW", "NEWE", "NWPP", "NYCW", "NYLI", "NYUP", 
               "RFCE", "RFCM", "RFCW", "RMPA", "SPNO", "SPSO", "SRMV", "SRMW", "SRSO", "SRTV", "SRVC")
  N20_EF_all <- c(0.011, 0.004, 0.011, 0.004, 0.009, 0.009, 0.018, 0.027, 0.025, 0.020, 0.011, 0.009, 0.003, 0.018, 0.002, 0.008, 0.018, 0.017, 0.018, 0.018, 0.013, 0.008, 0.027, 0.012, 0.014, 0.009) # in original units
  NOx_EF_all <- c(5.5, 7.7, 0.7, 0.5, 0.5, 0.4, 7.6, 3.5, 0.9, 1.0, 0.4, 0.6, 0.3, 0.9, 0.1, 0.3, 0.8, 0.8, 0.7, 0.6, 0.8, 0.6, 1.1, 0.5, 0.6, 0.4)
  ## convert to units (lb/MWh to kg/kWh)
  N20_EF_all <- N20_EF_all / 2205
  NOx_EF_all <- NOx_EF_all / 2205
  
  ## Find correct index
  region_col <- regions == egridRegion
  
  ## Efficiency in kg/kwh
  NOx_EF <- (NOx_EF_all[region_col])
  N2O_EF <- (N20_EF_all[region_col])
  NOx_To_N <- electricity_natGas_constants$Electricty[3]
  N2O_To_N <- electricity_natGas_constants$Electricty[4]
  
  avg_electricity_rate <- sum(dollars_spent_electricity, na.rm=T)/electricity_by_residents_input
  kwh_by_residents <- dollars_spent_electricity/avg_electricity_rate
  kwh_by_businesses <- electricity_by_businesses_input * (num_of_businesses / sum(num_of_businesses, na.rm=T))
 
  total_kwh_used <- (kwh_by_residents + kwh_by_businesses)
  electricity_n <- total_kwh_used * NOx_EF * NOx_To_N + total_kwh_used * N2O_EF * N2O_To_N
  return(electricity_n)
}
#electricity_n <- electricity_calculations(cex_data, general_data, 188422670, 248985273.4) #Charlottsville's numbers
## NATURAL GAS ########################################################################################################
## Find therms for residents in the census block
nat_gas_calculations <- function(cex_data_input, general_data, total_therms_by_residents, total_therms_by_business){
  #if(is.na(cex_data_input)){
  #  cex_data_input = cex_data
  #}
  
  spend_on_nat_gas <- cex_data_input$X3059_X
  avg_rate_for_residents <- sum(spend_on_nat_gas, na.rm=T) / total_therms_by_residents
  therms_by_residents <- spend_on_nat_gas / avg_rate_for_residents 

  ## Find the therms for businesses in the census block
#  num_of_business <- general_data[,"Number.of.Businesses.in.BG"]
  therms_by_business <- total_therms_by_business /(sum(general_data[,"Number.of.Businesses.in.BG"], na.rm=T)) * 
    general_data[,"Number.of.Businesses.in.BG"]/(sum(general_data[,"Number.of.Businesses.in.BG"], na.rm=T))

  ## Combine residential and business natural gas use and multiply by constants regarding NO and N2O in   natural gas
  therms_per_block <- therms_by_residents + therms_by_business
  nat_gas_n <- therms_per_block * 
      (as.numeric(electricity_natGas_constants[1, "Natural_Gas"]) *
       as.numeric(electricity_natGas_constants[3, "Natural_Gas"]) + 
       as.numeric(electricity_natGas_constants[2, "Natural_Gas"]) * 
       as.numeric(electricity_natGas_constants[4, "Natural_Gas"]))
  return(nat_gas_n)
}
#nat_gas_n <- nat_gas_calculations(cex_data, therms_by_residents, therms_by_business)  
## WASTEWATER #########################################################################################################
wastewater_calculations <- function(wastewater_removal_factor, total_treated_wastewater, population_per_BG){
  # if 0, use Charlottesville numbers
   if(wastewater_removal_factor == 0){
    total_treated_wastewater <- 1398475760  # Charlottesville's numbers
  }
  if(wastewater_removal_factor == 0){
      wastewater_removal_factor <- 0.79       # Charlottesville's numbers
  }
  wastewater_n <- c()
  for (census_block in population_per_BG){ #as.numeric(general_data$Total.Population.of.BG)){
    wastewater_for_block <- total_treated_wastewater / census_block
    wastewater_n_for_block <- wastewater_for_block*wastewater_constants$N_Content_of_Wastewater *
      (1-wastewater_removal_factor) + (wastewater_constants$Wastewater_Leakage_Estimate * wastewater_constants$Wastewater_Leakage_N_Content)
    wastewater_n <- c(wastewater_n, wastewater_n_for_block)
  }
  return(wastewater_n)
}
#wastewater_n <- wastewater_calculations(wastewater_removal_factor, total_treated_wastewater, as.numeric(general_data$Total.Population.of.BG))
## COMBINED TOTAL ######################################################################################################
## No fertilizer yet
# combined_by_block_group_n <- total_food_production_n  + pet_food_n + pet_waste_n + wastewater_n +
#   transportation_n + electricity_n + nat_gas_n
# all_n <- sum(total_food_production_n) + sum(pet_food_n) + sum(pet_waste_n) + sum(wastewater_n) +
#   sum(transportation_n) + sum(electricity_n) + sum(nat_gas_n)
# combined_by_category_n <- data.frame("Food" <- sum(total_food_production_n), 
#                                         "Pets" <- sum(pet_food_n) + sum(pet_waste_n),
#                                         "Wastewater" <- sum(wastewater_n), 
#                                         "Transportation" <- sum(transportation_n), 
#                                         "Electricity" <- sum(electricity_n), 
#                                         "Natural Gas" <- sum(nat_gas_n))
# combined_by_category_n_table <- data.frame("Block Group" <- block_groups,
#                                            "Food" <- total_food_production_n,
#                                            "Pets" <- pet_food_n + pet_waste_n,
#                                            "Wastewater" <- wastewater_n,
#                                            "Transportation" <- transportation_n,
#                                            "Electricity" <- electricity_n,
#                                            "Natural Gas" <- nat_gas_n,
#                                            "Total" <- combined_by_block_group_n)
# combined_by_category_n_table["Total" ,] <- colSums(combined_by_category_n_table)
# combined_by_category_n_table <- round(combined_by_category_n_table, 3)
# 
# combined_by_category_n_values <- data.frame(
#   group = c("Food", "Pets", "Wastewater", "Transportation", "Electricity", "Natural Gas"),
#   value = c(sum(total_food_production_n), sum(pet_food_n) + sum(pet_waste_n), sum(wastewater_n), 
#           sum(transportation_n), sum(electricity_n), sum(nat_gas_n)))
# combined_by_category_n_percentages <- data.frame(
#   group = c("Electricity","Food", "Natural Gas", "Pets", "Transportation", "Wastewater"),
#   value = c(sum(electricity_n) / all_n,
#             sum(total_food_production_n) / all_n, 
#             sum(nat_gas_n) / all_n),
#             (sum(pet_food_n) + sum(pet_waste_n)) / all_n, 
#             sum(transportation_n) / all_n,
#             sum(wastewater_n) / all_n)

combined_by_category_filtered <- function(block_groups, blockgroups, total_food_production_n, pet_food_n, pet_waste_n, wastewater_n, transportation_n, electricity_n, nat_gas_n){
  block_groups <- as.numeric(block_groups)
  blockgroups <- as.numeric(block_groups)
  all_n_filtered <- sum(total_food_production_n[block_groups %in% blockgroups], na.rm=T) + 
    sum(pet_food_n[block_groups %in% blockgroups], na.rm=T) + 
    sum(pet_waste_n[block_groups %in% blockgroups], na.rm=T) + 
    sum(wastewater_n[block_groups %in% blockgroups], na.rm=T) +
    sum(transportation_n[block_groups %in% blockgroups], na.rm=T) + 
    sum(electricity_n[block_groups %in% blockgroups], na.rm=T) + 
    sum(nat_gas_n[block_groups %in% blockgroups], na.rm=T)
  return(data.frame(
  group = c("Electricity", "Food",  "Natural Gas", "Pets", "Transportation", "Wastewater"),
  value = c(sum(electricity_n[block_groups %in% blockgroups], na.rm=T) / all_n_filtered,
            sum(total_food_production_n[block_groups %in% blockgroups], na.rm=T) / all_n_filtered, 
            sum(nat_gas_n[block_groups %in% blockgroups], na.rm=T) / all_n_filtered,
            (sum(pet_food_n[block_groups %in% blockgroups], na.rm=T) + 
               sum(pet_waste_n[block_groups %in% blockgroups], na.rm=T)) / all_n_filtered, 
            sum(transportation_n[block_groups %in% blockgroups], na.rm=T) / all_n_filtered,
            sum(wastewater_n[block_groups %in% blockgroups], na.rm=T) / all_n_filtered)))
}

food_by_sources_filtered <- function(block_groups, blockgroups, beef_production_n, pork_production_n, chicken_production_n, cheese_production_n, eggs_production_n, milk_production_n, fish_production_n, liquids_production_n, grains_production_n, nuts_production_n, fruits_production_n, oils_production_n, beans_production_n, spices_production_n, potatoes_production_n, coffee_tea_production_n, sugar_production_n, vegetables_production_n){
  return (data.frame( group = c("Beef", "Pork", "Chicken", "Cheese", "Eggs",
                                         "Milk", "Fish", "Liquids", "Grains", "Nuts",
                                         "Fruits", "Oils", "Beans", "Spices", "Potatoes",
                                         "Coffee and Tea", "Sugar", "Vegetables"),
              value = c(sum(beef_production_n[block_groups %in% blockgroups], na.rm=T), 
                        sum(pork_production_n[block_groups %in% blockgroups], na.rm=T), 
                        sum(chicken_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(cheese_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(eggs_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(milk_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(fish_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(liquids_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(grains_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(nuts_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(fruits_production_n[block_groups %in% blockgroups], na.rm=T), 
                        sum(oils_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(beans_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(spices_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(potatoes_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(coffee_tea_production_n[block_groups %in% blockgroups], na.rm=T),
                        sum(sugar_production_n[block_groups %in% blockgroups], na.rm=T), 
                        sum(vegetables_production_n[block_groups %in% blockgroups], na.rm=T))))
}

######################################################################### Graphs  ######################################
# category_percentages_pie_chart <- ggplot(combined_by_category_n_percentages, aes(x="", y=value, fill=group)) + 
#   geom_bar(stat="identity")+coord_polar("y", start=0)
# 
# 
# 
# a <- ggplot(combined_by_category_n_percentages, aes(x="", y=value, fill=group)) + 
#   geom_bar(stat="identity", width = 1) + coord_polar("y", start = 0) + theme_void()
# b <- ggplot(food_by_sources, aes(x=group, y = value, fill=group)) +
#   geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
# a
# 
# food_by_sources2 <- food_by_sources
# food_by_sources2$x <- 1
# c <- ggplot(food_by_sources2, aes(x=x, y=value, fill=group))+geom_col()
