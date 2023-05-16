
require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)
library(nnet)
library(FSA)
library(caret)
require(mapproj)
require(rgdal)
require(car)



setwd('E:\\BMI')

bmidata <- read.csv("Final.csv")

bmidata$Sex

round(table(bmidata$Sex,bmidata$BMI_cat)) #Age
round(prop.table(table(bmidata$Sex,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Sex))
round(prop.table(table(bmidata$Sex))*100,2)
chisq.test(bmidata$Sex,bmidata$BMI_cat) #sig

round(table(bmidata$Age_cat,bmidata$BMI_cat)) #Age
round(prop.table(table(bmidata$Age_cat,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Age_cat))
round(prop.table(table(bmidata$Age_cat))*100,2)
chisq.test(bmidata$Age_cat,bmidata$BMI_cat) #sig


round(table(bmidata$bloodgroup,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$bloodgroup,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$bloodgroup))
round(prop.table(table(bmidata$bloodgroup))*100,2)
chisq.test(bmidata$bloodgroup,bmidata$BMI_cat) #sig


round(table(bmidata$education,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$education,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$education))
round(prop.table(table(bmidata$education))*100,2)
chisq.test(bmidata$education,bmidata$BMI_cat) #sig


round(table(bmidata$coronavirustreatment,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$coronavirustreatment,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$coronavirustreatment))
round(prop.table(table(bmidata$coronavirustreatment))*100,2)
chisq.test(bmidata$coronavirustreatment,bmidata$BMI_cat) #sig


round(table(bmidata$occupation,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$occupation,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$occupation))
round(prop.table(table(bmidata$occupation))*100,2)
chisq.test(bmidata$occupation,bmidata$BMI_cat) #sig


round(table(bmidata$visitedearlier,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$visitedearlier,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$visitedearlier))
round(prop.table(table(bmidata$visitedearlier))*100,2)
chisq.test(bmidata$visitedearlier,bmidata$BMI_cat) #sig


round(table(bmidata$otherhealthcarefacilitybeforetestingforCOVID.19,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$otherhealthcarefacilitybeforetestingforCOVID.19,bmidata$BMI_cat),2)*100,2)
round(prop.table(table(bmidata$otherhealthcarefacilitybeforetestingforCOVID.19))*100,2)
chisq.test(bmidata$otherhealthcarefacilitybeforetestingforCOVID.19,bmidata$BMI_cat) #sig


round(table(bmidata$Soap,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Soap,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Soap))
round(prop.table(table(bmidata$Soap))*100,2)
chisq.test(bmidata$Soap,bmidata$BMI_cat) #sig


round(table(bmidata$Handwashingsolution,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Handwashingsolution,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Handwashingsolution))
round(prop.table(table(bmidata$Handwashingsolution))*100,2)
chisq.test(bmidata$Handwashingsolution,bmidata$BMI_cat) #sig


round(table(bmidata$Hand.sanitizers,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Hand.sanitizers,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Hand.sanitizers))
round(prop.table(table(bmidata$Hand.sanitizers))*100,2)
chisq.test(bmidata$Hand.sanitizers,bmidata$BMI_cat) #sig


# round(table(bmidata$Detergentwater,bmidata$BMI_cat)) #child age
# round(prop.table(table(bmidata$Detergentwater,bmidata$BMI_cat),2)*100,2)
# round(table(bmidata$visitedearlier))
# round(prop.table(table(bmidata$Detergentwater))*100,2)
# chisq.test(bmidata$Detergentwater,bmidata$BMI_cat) #sig


round(table(bmidata$Justwater,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Justwater,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Justwater))
round(prop.table(table(bmidata$Justwater))*100,2)
chisq.test(bmidata$Justwater,bmidata$BMI_cat) #sig


round(table(bmidata$Fruits,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Fruits,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Fruits))
round(prop.table(table(bmidata$Fruits))*100,2)
chisq.test(bmidata$Fruits,bmidata$BMI_cat) #sig


round(table(bmidata$proteinaceous,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$proteinaceous,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$proteinaceous))
round(prop.table(table(bmidata$proteinaceous))*100,2)
chisq.test(bmidata$proteinaceous,bmidata$BMI_cat) #sig


round(table(bmidata$Vitaminsupplement,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Vitaminsupplement,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Vitaminsupplement))
round(prop.table(table(bmidata$Vitaminsupplement))*100,2)
chisq.test(bmidata$Vitaminsupplement,bmidata$BMI_cat) #sig


round(table(bmidata$Water,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Water,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Water))
round(prop.table(table(bmidata$Water))*100,2)
chisq.test(bmidata$Water,bmidata$BMI_cat) #sig


round(table(bmidata$exercise,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$exercise,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$exercise))
round(prop.table(table(bmidata$exercise))*100,2)
chisq.test(bmidata$exercise,bmidata$BMI_cat) #sig


round(table(bmidata$Happiness,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Happiness,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$Happiness))
round(prop.table(table(bmidata$Happiness))*100,2)
chisq.test(bmidata$Happiness,bmidata$BMI_cat) #sig


round(table(bmidata$Boredom,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Boredom,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$visitedearlier))
round(prop.table(table(bmidata$Boredom))*100,2)
chisq.test(bmidata$Boredom,bmidata$BMI_cat) #sig


round(table(bmidata$Depression,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Depression,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$visitedearlier))
round(prop.table(table(bmidata$Depression))*100,2)
chisq.test(bmidata$Depression,bmidata$BMI_cat) #sig


round(table(bmidata$Mentalactivity,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Mentalactivity,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$visitedearlier))
round(prop.table(table(bmidata$Mentalactivity))*100,2)
chisq.test(bmidata$Mentalactivity,bmidata$BMI_cat) #sig


round(table(bmidata$Anxiety,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$Anxiety,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$visitedearlier))
round(prop.table(table(bmidata$Anxiety))*100,2)
chisq.test(bmidata$Anxiety,bmidata$BMI_cat) #sig


round(table(bmidata$symptoms_cat,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$symptoms_cat,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$symptoms_cat))
round(prop.table(table(bmidata$symptoms_cat))*100,2)
chisq.test(bmidata$symptoms_cat,bmidata$BMI_cat) #sig


round(table(bmidata$contacted,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$contacted,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$contacted))
round(prop.table(table(bmidata$contacted))*100,2)
chisq.test(bmidata$contacted,bmidata$BMI_cat) #sig


round(table(bmidata$RiskFactors,bmidata$BMI_cat)) #child age
round(prop.table(table(bmidata$RiskFactors,bmidata$BMI_cat),2)*100,2)
round(table(bmidata$RiskFactors))
round(prop.table(table(bmidata$RiskFactors))*100,2)
chisq.test(bmidata$RiskFactors,bmidata$BMI_cat) #sig

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
bmidata$exercise

logit1 <- (glm(BMI_cat ~ factor(Age_cat) + bloodgroup + education + relevel(factor(visitedearlier), ref = "Workplace") +
                 relevel(factor(Fruits), ref = "Very frequently") + relevel(factor(exercise), ref = "Always")  + symptoms_cat + contacted, data=bmidata, na.action = na.omit))
summary(logit1) #sig

model.or <- cbind(exp(coef(logit1)),exp(confint(logit1)))
colnames(model.or) <- c('odds ratio','ci(2.5%)','ci(97.5%)')
model.or #for odds ratio and ci

vif(logit1)


#multivariable logistic


hoslem.test(logit1$y, fitted(logit1), g=10) #hosmer and lemeshow goodness of fit  test

#auc value

prob <- predict(logit1,type="response")
pred <- prediction(as.numeric(prob),as.numeric(logit1$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

#roc curve

plot(perf, main="ROC Curve ", xlab="1 - specificity",  ylab="sensitivity")  
grid()
abline(0,1, col="blue", lty=2)


library(ggplot2)
library(ggpmisc)



FAM <- c(19.62411, 22.59592, 23.87989, 27.84400, 23.85037, 18.68919, 23.48866, 23.44695, 22.13240, 25.01092, 19.10288, 26.04067, 25.57982, 27.42988, 24.90263, 20.05888, 26.83621, 20.09217, 29.79719,
          22.05361, 26.65163, 23.22915, 17.81302, 24.95113, 17.42973, 24.37949, 25.77414, 29.78408, 23.81682, 24.03144, 22.31971, 25.10197, 24.07837, 23.85600, 23.64262, 23.80582, 28.06183, 27.16079,
          20.10604, 24.28835, 18.15525, 23.92286, 21.42632, 30.75074, 27.54754, 25.61225, 23.96199, 25.59868, 22.01888, 25.37204, 30.98469, 24.13922, 23.37370, 24.62713, 21.99646, 25.88873, 30.10264,
          20.55458, 24.86129, 24.42129, 22.89686, 26.87291, 29.71065, 25.53609, 25.01784, 21.49784, 20.94509, 19.52935, 22.87648, 23.92535, 27.60039, 21.60010, 27.29929, 28.54077, 23.83051, 23.80103,
          21.83941, 23.10505, 21.26882, 28.62851, 24.45000, 25.16644, 28.83747, 22.52697, 26.24052, 28.03790, 27.73418, 23.81609, 24.46443, 27.23207, 23.37862, 31.15210, 29.16001, 21.35642, 21.18303,
          22.06274, 28.14408, 24.47972, 22.89752, 29.74639, 25.60921, 20.75621, 31.30820, 20.74693, 31.22639, 25.64587, 28.34990, 22.63603, 21.40129, 20.26493, 29.17715, 23.53518, 22.64847, 27.86465,
          26.29323, 32.05027, 22.67689, 28.04378, 25.42916, 31.42799, 23.44331, 25.76469, 21.59340, 32.04976, 32.10148, 24.00164, 23.39492, 31.67624, 23.69268, 26.74145, 30.15286, 32.26234, 28.97911,
          26.94796, 29.11573, 27.66067, 25.09674, 29.41419, 29.14700, 31.64592, 29.40946, 28.26212, 21.36186, 24.55337, 28.27199, 28.18878, 25.32154, 27.51006, 23.42263, 24.45727, 26.04217, 20.96090,
          23.37847, 28.08237, 24.52500, 25.23179, 28.56673, 28.72863, 22.52822, 22.92592, 21.17807, 24.08028, 25.24931, 23.94363, 23.38354, 33.68436, 28.13294, 26.69428, 22.58396, 30.15008, 26.21730,
          20.50301, 29.29484, 31.16161, 27.56283, 26.06968, 32.40676, 26.19229, 27.29045, 28.50862, 26.19379, 30.08452, 26.80193, 23.41147, 26.93048, 23.80744, 29.06452, 30.61877, 31.60865, 22.15452,
          26.24428, 25.41121, 25.55621, 24.72900, 24.84023, 29.46612, 26.87968, 26.36196, 27.87136, 24.16393, 25.47398, 31.92118, 32.48976, 33.72625, 30.13262, 30.99604, 27.75490, 21.17085, 28.47878,
          35.82360, 28.40384, 26.65011, 27.00807, 32.12790, 27.75014, 19.66042, 26.11089, 29.03915, 30.74926, 33.08907, 27.93808, 28.30986, 24.15684, 29.20165, 22.71260, 30.25629, 28.99797, 28.17430,
          30.00101, 28.69216, 24.21543, 33.93351, 29.11455, 30.61196, 26.03273, 26.95783, 25.42311, 29.23500, 26.86542, 25.46290, 35.01331, 25.75685, 23.97605, 35.98087, 26.04215, 22.36214, 25.64021,
          34.36976, 27.42001, 31.54223, 31.49885, 27.62882, 30.29494, 22.57522, 28.52599, 32.02116, 33.56940, 23.79191, 28.36363, 29.01983, 33.32860, 28.66165, 28.61858, 30.88499, 28.90363, 33.34936,
          27.59275, 29.76634, 31.01798, 29.65972, 29.53873, 26.97843, 31.03688, 28.29427, 25.17042, 26.67743, 34.59080, 25.96666, 31.46521, 29.62762, 27.59683, 25.80957, 29.99671, 26.56202, 22.82406,
          28.86729, 27.86402, 26.93266, 32.88896, 22.30959, 28.31437, 28.50070, 33.38487, 33.47955, 29.33054, 28.03580, 30.77142, 33.09739, 33.20394, 28.11630, 31.26457)


ROX <- c(20.97182, 17.97422, 18.80500, 17.76007, 21.81151, 26.74189, 21.50640, 20.02917, 18.91102, 20.76461, 25.52991, 17.79877, 25.95793, 21.71470, 23.85437, 24.29289, 24.27890, 28.12132, 24.34652,
         19.16169, 21.87164, 24.37452, 18.72495, 26.96465, 25.31768, 20.97143, 21.73156, 26.10244, 23.32495, 20.20185, 20.41808, 19.74658, 19.53654, 18.07220, 22.41916, 23.21622, 25.99188, 23.95191,
         17.66894, 19.45859, 21.86327, 23.31087, 22.84990, 19.42786, 24.00526, 24.01230, 21.74307, 29.52232, 20.90839, 21.07817, 18.71035, 21.67707, 23.10468, 21.50705, 25.46623, 16.75264, 28.56138,
         29.50826, 22.71414, 21.83365, 21.02613, 25.80109, 20.77953, 22.92352, 24.95478, 16.06577, 27.24850, 20.61241, 26.83131, 16.08397, 20.63936, 23.89150, 24.42553, 27.16613, 23.07135, 21.81038,
         22.90215, 25.02686, 23.19296, 20.41045, 26.09879, 26.76961, 22.13547, 21.12983, 21.76619, 27.96831, 23.70633, 28.13341, 25.78443, 25.40585, 27.92257, 25.06428, 18.44185, 29.24532, 27.24940,
         27.48035, 19.49095, 19.13804, 21.00039, 27.09222, 25.66714, 21.60578, 26.06029, 20.99268, 22.06557, 23.65616, 28.20854, 19.32719, 22.79845, 24.31658, 24.97622, 26.99594, 27.72590, 25.93073,
         23.25347, 22.26458, 27.76811, 23.81312, 24.82464, 20.68364, 18.73855, 21.38219, 26.03377, 24.74136, 25.01729, 21.29207, 31.39040, 27.65462, 27.12777, 22.92859, 23.89016, 24.65615, 22.83047,
         30.30477, 25.65554, 23.75843, 19.55219, 25.93890, 25.35470, 27.37654, 24.46751, 29.34085, 25.34166, 24.07901, 24.73379, 24.47468, 28.57838, 25.97767, 22.32950, 26.81981, 28.42485, 22.90582,
         23.91922, 25.80659, 20.08239, 28.85787, 22.14035, 24.21002, 31.68105, 22.13355, 26.30927, 32.90734, 24.40392, 29.27926, 23.78317, 23.36261, 20.06503, 22.84136, 27.40048, 27.57563, 23.08021,
         23.35677, 25.20848, 27.23211, 25.47385, 24.41629, 20.54747, 27.46117, 25.70207, 24.33806, 23.89142, 27.06491, 27.93897, 28.79946, 22.80793, 29.27269, 28.70552, 21.64996, 27.35529, 26.14102,
         25.34728, 20.93217, 32.39739, 27.91730, 26.26251, 28.59416, 23.84459, 32.39531, 26.31155, 26.43872, 27.04871, 31.42782, 27.33904, 22.95968, 25.06471, 27.43508, 25.60283, 22.47391, 21.87254,
         24.62510, 27.94183, 30.56372, 26.35019, 25.75061, 31.61689, 26.57732, 30.06801, 20.34684, 30.61404, 28.62180, 32.21308, 22.88016, 32.99924, 22.53438, 23.48284, 24.04293, 23.88354, 25.45173,
         23.68090, 26.56170, 26.83993, 24.61969, 26.75618, 27.67281, 23.23433, 27.77124, 27.77196, 19.90452, 22.70989, 26.51383, 29.29072, 25.80863, 22.35770, 29.42186, 29.60125, 31.24827, 25.64029,
         22.57039, 23.92723, 27.39788, 27.40977, 30.39270, 29.28356, 24.04706, 29.80480, 26.35485, 30.23969, 26.86360, 25.32483, 28.99724, 29.64559, 30.70353, 29.04643, 29.24142, 22.06806, 26.37304,
         29.01157, 27.11936, 25.39738, 27.46079, 29.75270, 26.37045, 27.10604, 27.79641, 31.26252, 26.63566, 27.46382, 28.28017, 26.90750, 29.69996, 25.28366, 29.81987, 30.66082, 26.45253, 29.11530,
         25.63898, 28.37586, 30.19345, 31.65964, 30.13847, 26.10715, 27.21258, 26.86992, 27.65652, 26.27182, 26.50744, 32.38424, 32.82744, 30.41059, 29.23404, 31.30829)


Cy5 <- c(21.43211, 17.49298, 19.37600, 23.89817, 19.03255, 26.79208, 21.30522, 30.54706, 27.71346, 24.17710, 23.77278, 21.86592, 21.64878, 21.89750, 20.58730, 24.97603, 23.04673, 24.17483, 22.75775,
         24.60991, 22.49913, 24.99317, 19.69999, 28.57506, 20.97063, 25.95022, 20.33005, 18.21450, 24.75088, 23.95389, 23.03979, 23.86892, 22.39637, 26.57269, 25.24871, 19.76687, 18.76258, 23.93217,
         21.04427, 20.48197, 27.05272, 22.53549, 23.65013, 24.73912, 21.90874, 24.66755, 23.84527, 29.43505, 20.54144, 27.39003, 25.20843, 21.78722, 25.68554, 25.62685, 17.94894, 22.93475, 24.09606,
         28.28707, 27.04750, 28.62011, 20.41613, 22.56885, 19.39578, 25.57110, 24.56986, 23.44325, 23.46253, 25.53735, 25.38947, 24.96734, 20.47090, 27.44364, 26.04632, 28.22476, 25.64956, 20.25631,
         23.22888, 23.44245, 20.17398, 28.24889, 21.00408, 25.06479, 24.43170, 32.18492, 29.10866, 22.54014, 19.97298, 28.33411, 23.24585, 17.41954, 25.12182, 23.93176, 22.00992, 28.45409, 23.33272,
         21.77721, 22.88524, 25.99095, 26.92700, 28.72849, 23.79324, 22.31660, 21.67132, 25.44021, 23.32911, 25.38705, 25.84889, 25.36031, 30.64845, 27.92418, 26.71563, 27.22861, 25.43777, 20.43239,
         21.35109, 19.89797, 21.80146, 27.09352, 20.55592, 21.86861, 29.97525, 23.84940, 25.28366, 26.53926, 29.69554, 23.35999, 20.28917, 25.57085, 24.70859, 27.35519, 25.83785, 24.62831, 28.09186,
         30.39277, 28.71715, 24.25388, 23.42480, 23.28438, 24.09932, 24.30175, 26.15510, 25.54095, 23.41941, 27.42159, 28.20386, 20.58297, 26.05277, 26.05133, 26.80155, 25.75536, 26.48853, 26.70111,
         23.98535, 30.06066, 27.87904, 27.97231, 23.62781, 23.40671, 18.74740, 28.87729, 29.80576, 23.59589, 20.74694, 29.39487, 22.70326, 28.43177, 25.12057, 18.30951, 28.46076, 24.39285, 24.65619,
         26.99248, 28.48773, 27.45715, 26.51830, 29.31210, 25.81843, 20.93293, 28.47949, 26.76024, 31.98295, 21.89802, 30.13382, 28.59886, 23.56827, 21.41745, 26.99541, 26.16729, 34.00750, 27.23599,
         20.72477, 29.02236, 27.37278, 28.09072, 23.59362, 28.41329, 30.89974, 26.49270, 24.06658, 28.61492, 27.55924, 32.72544, 30.25439, 22.04272, 29.78626, 27.31244, 25.17803, 23.13902, 27.46635,
         29.88730, 29.25577, 28.12681, 28.28277, 30.47917, 23.36055, 29.83479, 24.47151, 26.89388, 23.13017, 28.50772, 25.99969, 23.28565, 29.10375, 25.82228, 30.37538, 24.34029, 30.43226, 25.84079,
         29.81711, 31.85572, 27.95088, 23.26849, 26.12112, 28.33954, 27.21749, 22.18457, 30.18987, 30.81145, 26.88989, 24.88604, 24.69627, 25.81928, 26.63127, 32.49155, 28.57655, 30.10204, 25.73755,
         29.03065, 26.49026, 24.57463, 31.30636, 34.23450, 19.88037, 27.04577, 29.07814, 26.29564, 29.88771, 26.16518, 22.41397, 27.85979, 24.96812, 35.13363, 31.28385, 30.87131, 31.59378, 32.50538,
         28.44440, 30.31002, 31.18485, 25.69996, 30.19430, 26.15642, 26.15460, 25.64562, 32.52449, 35.68519, 25.04458, 24.93174, 27.82826, 33.53834, 31.98501, 33.20342, 26.27127, 26.82582, 28.42855,
         33.51954, 27.76731, 27.07394, 27.08328, 27.86177, 32.23799, 26.66551, 31.11332, 30.61157, 23.39192, 36.54997, 24.75520, 35.98182, 25.52877, 28.99637, 29.54600)


BMI <- sort(bmidata$BMI,decreasing = TRUE)

dat <- data.frame(FAM, ROX, Cy5, BMI)
# assembling a single label with equation and R2
ggplot(data = dat, aes(x = BMI, y = FAM)) +
  stat_poly_line() +  geom_point() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))






# assembling a single label with equation and R2
ggplot(data = dat, aes(x = BMI, y = ROX)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()




# assembling a single label with equation and R2
ggplot(data = dat, aes(x = BMI, y = Cy5)) +
  stat_poly_line() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*"))) +
  geom_point()




ggplot(data,                                      # Grouped barplot using ggplot2
      aes(x = bmidata$Age_cat,
          y = bmidata$Sex,
          fill = bmidata$Age_cat)) +
  geom_bar(stat = "identity",
           position = "dodge")





round(table(bmidata$Sex,bmidata$Age_cat)) #Age
round(prop.table(table(bmidata$Sex,bmidata$Age_cat),2)*100,2)
round(table(bmidata$Sex))
round(prop.table(table(bmidata$Sex))*100,2)
chisq.test(bmidata$Sex,bmidata$Age_cat) #sig

round(table(bmidata$visitedearlier,bmidata$Sex)) #Age
round(prop.table(table(bmidata$visitedearlier,bmidata$Sex),2)*100,2)
round(table(bmidata$visitedearlier))
round(prop.table(table(bmidata$visitedearlier))*100,2)
chisq.test(bmidata$visitedearlier,bmidata$Sex) #sig




