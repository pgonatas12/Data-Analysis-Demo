#Data was generated with ChatGPT. Coding and analysis is my own work

patient<-read.csv("C:/Users/enrag/Documents/Mock Data Analytics/Generated Data/Mock_Data.csv")

class(patient)  #Type check

#Check for differences in Height
M_pat<-patient[patient$Gender=='Male',]
F_pat<-patient[patient$Gender=='Female',]
t.test(M_pat$Height_cm,F_pat$Height_cm,paired = T)

#Paired t-test

#data:  M_pat$Height_cm and F_pat$Height_cm
#t = 4.4803, df = 4, p-value = 0.01099
#alternative hypothesis: true mean difference is not equal to 0
#95 percent confidence interval:
# 4.715715 20.084285
#sample estimates:
#  mean difference 
#12.4 
#P-value less than 0.05 reject null, heights are different between groups

t.test(M_pat$Height_cm,F_pat$Height_cm,paired = T,alternative = 'greater')
#data:  M_pat$Height_cm and F_pat$Height_cm
#t = 4.4803, df = 4, p-value = 0.005494
#alternative hypothesis: true mean difference is greater than 0
#95 percent confidence interval:
#  6.499751      Inf
#sample estimates:
# mean difference 
#12.4 
#one-sided test confirms male avg is greater

shapiro.test(M_pat$Height_cm-F_pat$Height_cm)
#data:  M_pat$Height_cm - F_pat$Height_cm
#W = 0.83279, p-value = 0.146
#Fail to reject null, data appears normally distributed


#Check if Smoking affects blood pressure

No_smok<-patient[patient$SmokingStatus=='Non-smoker',]
smok<-patient[patient$SmokingStatus %in% c('Smoker','Former-smoker'),]

#Need to separate Blood Pressure column as it cannot be assessed as is
No_bp<-strsplit(as.character(unlist(No_smok$BloodPressure_mmHg)),'/')
Nosm_Systolic<-as.numeric(sapply(No_bp,'[',1))
Nosm_Diastolic<-as.numeric(sapply(No_bp,'[',2))

Smo_bp<-strsplit(as.character(unlist(smok$BloodPressure_mmHg)),'/')
Smo_Systolic<-as.numeric(sapply(Smo_bp,'[',1))
Smo_Diastolic<-as.numeric(sapply(Smo_bp,'[',2))

#Check Systolic
t.test(Nosm_Systolic,Smo_Systolic)
#data:  Nosm_Systolic and Smo_Systolic
#t = -4.787, df = 6.75, p-value = 0.002206
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -38.94026 -13.05974
#sample estimates:
# mean of x mean of y 
#116.5     142.5 

t.test(Nosm_Diastolic,Smo_Diastolic)
#data:  Nosm_Diastolic and Smo_Diastolic
#t = -5.116, df = 7.5176, p-value = 0.001105
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -22.808031  -8.525303
#sample estimates:
#  mean of x mean of y 
#76.00000  91.66667 

#Both Systolic and Diastolic Blood pressures are different between smokers and non-smokers


#Smoking on Weight
t.test(smok$Weight_kg, No_smok$Weight_kg)
#t = 1.2012, df = 7.775, p-value = 0.265
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -7.04869 22.21536
#sample estimates:
#  mean of x mean of y 
#73.83333  66.25000 
#Fail to reject null, smoking has no affect on weight

#Cholesterol
t.test(smok$Cholesterol_mg_dl, No_smok$Cholesterol_mg_dl)
#t = 3.9445, df = 6.1588, p-value = 0.007199
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  14.70143 61.96524
#sample estimates:
#  mean of x mean of y 
#220.8333  182.5000 
#Reject null, Smoking affects cholesterol levels


lm(patient$Cholesterol_mg_dl~patient$Gender+patient$SmokingStatus)
summary(lm(patient$Cholesterol_mg_dl~patient$Gender+patient$SmokingStatus))
plot(lm(patient$Cholesterol_mg_dl~patient$Gender+patient$SmokingStatus))
