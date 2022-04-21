install.packages("gdata")
install.packages("tidyverse")
install.packages("stringr")
install.packages("lubridate")
install.packages("scales")
install.packages("graphics")
install.packages("caret")
install.packages("janitor")
install.packages("plyr")
library(gdata)
library(tidyverse)
library(stringr)
library(lubridate)
library(scales)
library(graphics)
library(caret)
library(janitor)
library(plyr)

Ames<-read.csv("AmesHousing.csv",stringsAsFactors = FALSE)
attach(Ames)
glimpse(Ames)
Ames<-clean_names(Ames)
any(is.na(Ames))
sum(is.na(Ames))
na.cols = which(colSums(is.na(Ames)) > 0)
sort(colSums(sapply(Ames[na.cols], is.na)), decreasing = TRUE)

dplyr::summarise(Ames,
                 n_obs = n(),
                 n_pid = n_distinct(pid))

Ames <- Ames %>% 
  dplyr::select(everything(), -pid)

summary(Ames$lot_frontage)

index <- which(is.na(Ames$lot_frontage))
head(Ames[index,])

table(Ames[index, 'neighborhood'])
any(is.na(Ames$neighborhood))

frontage_by_hood <- df %>% 
  dplyr::select(neighborhood, lot_frontage) %>% 
  group_by(neighborhood) %>% 
  summarise(median_frontage = median(lot_frontage, na.rm = TRUE))
# Any missing data in new df?
any(is.na(frontage_by_hood$median_frontage))
index2 <- which(is.na(frontage_by_hood$median_frontage))
frontage_by_hood[index2, ]

Ames %>% 
  filter(neighborhood == "GrnHill" | neighborhood == "Landmrk") %>% 
  count()

Ames <- Ames %>% 
  filter(neighborhood != "GrnHill" & neighborhood != "Landmrk")
# drop from frontage df as well
frontage_by_hood <- frontage_by_hood %>% 
  filter(neighborhood != "GrnHill" & neighborhood != "Landmrk")
# redefine index for missing frontage data
index <- which(is.na(Ames$lot_frontage))

for (i in index) {
  med_frontage = frontage_by_hood[frontage_by_hood == Ames$neighborhood[i], 'median_frontage']
  # then replace the missing value with the median
  Ames[i, 'lot_frontage'] = med_frontage[[1]]
}
# check to see that it worked
any(is.na(Ames$lot_frontage))


index <- which(is.na(Ames$alley))
Ames[index, 'alley'] <- 'No Alley'

attach(Ames)

#Impute 'None'  for GarageType that is not having cars
Ames$garage_type <- fct_explicit_na(Ames$garage_type, na_level = "None")

# Impute 'None' for Grage Finishing for No grage values 
Ames$garage_finish <- fct_explicit_na(Ames$garage_finish, na_level = "None")

# Impute 'None' for GrageQuality for No grage values 
Ames$garage_qual <- fct_explicit_na(Ames$garage_qual, na_level = "None")

# Impute 'None' for GarageCond for No grage values 
Ames$garage_cond <- fct_explicit_na(Ames$garage_cond, na_level = "None")

# Impute MasVnr Type to None and its area to 0
Ames$mas_vnr_type[is.na(Ames$mas_vnr_type)] <- "None"
Ames$mas_vnr_area[is.na(Ames$mas_vnr_area)] <- 0

# Based on the average price range, SBrkr is selected to impute Electical categrory
Ames$electrical[is.na(Ames$electrical)] <- 'SBrkr'
# Impute Utilities in testdf na
Ames$utilities[is.na(Ames$utilities)] <- 'AllPub'

### Impute None category for BsmtQual, BsmtCond, BsmtExposure,BsmtFinType1, BsmtFinType2 in traindf
Ames$bsmt_qual <- fct_explicit_na(Ames$bsmt_qual, na_level = "None")
Ames$bsmt_exposure <- fct_explicit_na(Ames$bsmt_exposure, na_level = "No")
Ames$bsmt_cond <- fct_explicit_na(Ames$bsmt_cond, na_level = "None")
Ames$bsmt_fin_type_1 <- fct_explicit_na(Ames$bsmt_fin_type_1, na_level = "None")
Ames$bsmt_fin_type_2 <- fct_explicit_na(Ames$bsmt_fin_type_2, na_level = "None")
Ames$garage_yr_blt[is.na(Ames$garage_yr_blt)] <- Ames$year_built[is.na(Ames$garage_yr_blt)]

#GarageArea, GarageCars
# Impute testdf GarageCars Na to 1, and GarageArea to 280
Ames$garage_cars[is.na(Ames$garage_cars)] <- 1
Ames$garage_area[is.na(Ames$garage_area)] <- 280


#MsZoning 
Ames$ms_zoning <- as.factor(Ames$ms_zoning)

Ames$bsmt_fin_sf_1[is.na(Ames$bsmt_fin_sf_1)] <- 0
Ames$bsmt_fin_sf_2[is.na(Ames$bsmt_fin_sf_2)] <- 0
Ames$total_bsmt_sf[is.na(Ames$total_bsmt_sf)] <- 0
Ames$bsmt_unf_sf[is.na(Ames$bsmt_unf_sf)] <- 0
Ames$bsmt_full_bath[is.na(Ames$bsmt_full_bath)] <- 0
Ames$bsmt_half_bath[is.na(Ames$bsmt_half_bath)]<-0


#Functional

Ames$functional[is.na(Ames$functional)] <- names(sort(-table(Ames$functional)))[1]
Ames$functional <- as.integer(revalue(Ames$functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))

#Sale type
Ames$sale_type <- as.factor(Ames$sale_type)

# Kitchen Quality
Ames$kitchen_qual <- as.factor(Ames$kitchen_qual)

Ames$exterior_1st <- as.factor(Ames$exterior_1st)

Ames$exterior_2nd <- as.factor(Ames$exterior_2nd)


na.cols = which(colSums(is.na(Ames)) > 0)
sort(colSums(sapply(Ames[na.cols], is.na)), decreasing = TRUE)

Ames$pool_qc <- fct_explicit_na(Ames$pool_qc, na_level = "None")

Ames$misc_feature <- fct_explicit_na(Ames$misc_feature, na_level = "None")

Ames$fence <- fct_explicit_na(Ames$fence, na_level = "None")

Ames$fireplace_qu <- fct_explicit_na(Ames$fireplace_qu, na_level = "None")

Ames$ms_sub_class <- as.factor(Ames$ms_sub_class)
Ames$overall_qual<- as.factor(Ames$overall_qual)
Ames$overall_cond<- as.factor(Ames$overall_cond)
Ames$year_built<- as.factor(Ames$year_built)
Ames$year_remod_add <- as.factor(Ames$year_remod_add)
Ames$yr_sold <- as.factor(Ames$yr_sold)
Ames$garage_yr_blt <- as.factor(Ames$garage_yr_blt)

any(is.na(Ames))

str(Ames)

Ames<-read.csv("Clean_Ames.csv")
any(is.na(Ames))

library(glmnet)
library(dplyr)
library(MASS)
index <- sample(nrow(Ames),nrow(Ames)*0.90)
Ames.train <- Ames[index,]
Ames.test <- Ames[-index,]
X<- as.matrix(dplyr::select(Ames.train, -sale_price))
Y<- as.matrix(dplyr::select(Ames.train, sale_price))
lasso.fit<- glmnet(x=X, y=Y)
plot(lasso.fit, xvar = "lambda")


