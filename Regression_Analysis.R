# IN THIS MODEL WE PREDICT THE LUNG CAPACITY GIVEN DIFFERENT VARIABLES. THE DATA 
# AND SOME PARTS OF THE CODE WERE ADAPTED ON YOUTUBE https://www.youtube.com/watch?v=66z_MRwtFJM
# FROM MarinStatsLectures

attach(LungCapData)
names(LungCapData)

# MODEL BUILDING WITH AGE (INDEPENDENT VARIABLE)

## exploring the correlation between Lung Capacity and age first
cor(LungCap,Age)
## the age and lung capacity of a person have got a strong correlation of 0.8196

## model building
Model1<-lm(LungCap ~ Age)
summary(Model1)
## the model explains 67.19% of the data. It can  be written as
## LungCap=1.14686 + 0.54485 Age.
## since the p value is <0.05, age is significant in explaining the data.


# MODEL BUILDING WITH AGE AND HEIGHT (INDEPENDENT VARIABLES)
Model2<-lm(LungCap ~ Age + Height)
# summary of the model
summary(Model2)
## the model explains 84.25% of the data. It can be written as 
## LungCap = -11.747065 + 0.126368 Age + 0.278432 Height
## Since the p value for both age and height are <0.05, the factors are 
## important in explaining the data


# Calculating the pearson correlation between Age and Height 
cor(Age,Height,method = "pearson")
## there is a strong correlation between age and height of 0.8357368


# calculating the confidence interval of the model
CONFIDFENCE_INTERVAL<-confint(Model2, conf.level=0.95)
CONFIDFENCE_INTERVAL

#FITTING THE MODEL WITH ALL VARIABLES
Model3<-lm(LungCap ~ Age + Height + Smoke + Gender + Caesarean)

# summary for model2
summary(Model3)
## the model explains approximately 85% of the data. It can be written as
## LungCap = -11.32249 + 0.16053Age + 0.26411Height -0.60956Smokeyes 
## +  0.38701Gendermale  -0.21422Caesareanyes
## since all p values for the independent variables are < 0.05, all independent 
## variables are significant in explaining the data.

#CHECKING MODEL ASSUMPTIONS
plot(Model3)
## the Q-q plot shows that the errors are normally distributed and symmetric about
##the mean 0. Therefore the assumptions are satisfied.