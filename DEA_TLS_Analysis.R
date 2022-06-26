deadata<- read.csv("C:\\Users\\pree1\\OneDrive - Johns Hopkins\\Documents\\DEA_Opioid_Suspension\\DEA_Cases.csv")
numtotalcases<- dim (deadata)[1] #total cases in this dataset
tlpp_overall<- mean(deadata$TLPP)
sd(deadata$TLPP)
summary(deadata$TLPP)

#Table 2 Analysis
dea_cases<- deadata[deadata$DEA.Involvement == "1", ] #cases in which dea involvement was reported
num_deacases<-dim (dea_cases)[1] #total cases with dea involvement
nondea_cases<- deadata[deadata$DEA.Involvement == "0", ] #cases in which dea involvement was not reported
num_nondeacases<-dim (dea_cases)[1] #total cases without dea involvement
t.test(dea_cases$TLPP, nondea_cases$TLPP, paired = FALSE)#t test comparing TLPP in cases with DEA involvement vs without
mean(dea_cases$TLPP)#mean of TLPP in patients with DEA involvement
sd(dea_cases$TLPP)#standard deviation of TLPP in patients with DEA involvement
summary(dea_cases$TLPP)
mean(nondea_cases$TLPP)#mean of TLPP in patients without DEA involvement
sd(nondea_cases$TLPP)#standard deviation of TLPP in patients without DEA involvement
summary(nondea_cases$TLPP)

#mean and sd TLPP with or without license suspension
mean(deadata[deadata$MD.DO.license.suspended == "Y", ]$TLPP)
sd(deadata[deadata$MD.DO.license.suspended == "Y", ]$TLPP)
summary(deadata[deadata$MD.DO.license.suspended == "Y", ]$TLPP)
mean(deadata[deadata$MD.DO.license.suspended == "N", ]$TLPP)
sd(deadata[deadata$MD.DO.license.suspended == "N", ]$TLPP)
summary(deadata[deadata$MD.DO.license.suspended == "N", ]$TLPP)

#license suspension with DEA involvement
mean(dea_cases[dea_cases$MD.DO.license.suspended == "Y", ]$TLPP)#mean TLPP in patients with license suspension and DEA involvement
sd(dea_cases[dea_cases$MD.DO.license.suspended == "Y", ]$TLPP)#standard deviation TLPP in patients with license suspension and DEA involvement
summary(dea_cases[dea_cases$MD.DO.license.suspended == "Y", ]$TLPP)
mean(dea_cases[dea_cases$MD.DO.license.suspended == "N", ]$TLPP)#mean TLPP in patients without license suspension and DEA involvement
sd(dea_cases[dea_cases$MD.DO.license.suspended == "N", ]$TLPP)#standard deviation TLPP in patients without license suspension and DEA involvement
summary(dea_cases[dea_cases$MD.DO.license.suspended == "N", ]$TLPP)

#license suspension without DEA involvement
mean(nondea_cases[nondea_cases$MD.DO.license.suspended == "Y", ]$TLPP)#mean TLPP in patients with license suspension and without  DEA involvement
sd(nondea_cases[nondea_cases$MD.DO.license.suspended == "Y", ]$TLPP)#standard deviation TLPP in patients with license suspension and without DEA involvement
summary(nondea_cases[nondea_cases$MD.DO.license.suspended == "Y", ]$TLPP)
mean(nondea_cases[nondea_cases$MD.DO.license.suspended == "N", ]$TLPP)#mean TLPP in patients without license suspension and without DEA involvement
sd(nondea_cases[nondea_cases$MD.DO.license.suspended == "N", ]$TLPP)#standard deviation TLPP in patients without license suspension and without DEA involvement
summary(nondea_cases[nondea_cases$MD.DO.license.suspended == "N", ]$TLPP)

#mean and sd TLPP with or without charges
chargedcases<- deadata[deadata$Criminal.Charges > "0", ]
mean(deadata[deadata$Criminal.Charges > "0", ]$TLPP)
sd(deadata[deadata$Criminal.Charges > "0", ]$TLPP)
summary(deadata[deadata$Criminal.Charges > "0", ]$TLPP)
mean(deadata[deadata$Criminal.Charges == "0", ]$TLPP)
sd(deadata[deadata$Criminal.Charges == "0", ]$TLPP)
summary(deadata[deadata$Criminal.Charges == "0", ]$TLPP)

#charges with DEA involvement
mean(dea_cases[dea_cases$Criminal.Charges > "0", ]$TLPP)#mean TLPP in patients with charges and DEA involvement
sd(dea_cases[dea_cases$Criminal.Charges > "0", ]$TLPP)#standard deviation TLPP in patients with charges and DEA involvement
summary(dea_cases[dea_cases$Criminal.Charges > "0", ]$TLPP)
mean(dea_cases[dea_cases$Criminal.Charges == "0", ]$TLPP)#mean TLPP in patients without charges and DEA involvement
sd(dea_cases[dea_cases$Criminal.Charges == "0", ]$TLPP)#standard deviation TLPP in patients without charges and DEA involvement
summary(dea_cases[dea_cases$Criminal.Charges == "0", ]$TLPP)

#charges without DEA involvement
mean(nondea_cases[nondea_cases$Criminal.Charges > "0", ]$TLPP)#mean TLPP in patients with charges and without DEA involvement
sd(nondea_cases[nondea_cases$Criminal.Charges > "0", ]$TLPP)#standard deviation TLPP in patients with charges and without DEA involvement
summary(nondea_cases[nondea_cases$Criminal.Charges > "0", ]$TLPP)
mean(nondea_cases[nondea_cases$Criminal.Charges == "0", ]$TLPP)#mean TLPP in patients without charges and without DEA involvement
sd(nondea_cases[nondea_cases$Criminal.Charges == "0", ]$TLPP)#standard deviation TLPP in patients without charges and without DEA involvement
summary(nondea_cases[nondea_cases$Criminal.Charges == "0", ]$TLPP)

#note that convictions are only examined in individuals with charges filed against them
#mean and sd TLPP with charges and no convictions
mean(chargedcases[chargedcases$Convicted == "0", ]$TLPP)
sd(chargedcases[chargedcases$Convicted == "0", ]$TLPP)
summary(chargedcases[chargedcases$Convicted == "0", ]$TLPP)
#mean and sd TLPP with charges and civil convictions
mean(chargedcases[chargedcases$Convicted == "1", ]$TLPP)
sd(chargedcases[chargedcases$Convicted == "1", ]$TLPP)
summary(chargedcases[chargedcases$Convicted == "1", ]$TLPP)
#mean and sd TLPP with charges and criminal convictions
mean(chargedcases[chargedcases$Convicted == "2", ]$TLPP)
sd(chargedcases[chargedcases$Convicted == "2", ]$TLPP)
summary(chargedcases[chargedcases$Convicted == "2", ]$TLPP)

#charges and convictions with DEA involvement
mean(chargedcases[chargedcases$Convicted == "0" & chargedcases$DEA.Involvement == "1", ]$TLPP)#mean TLPP in patients with no conviction and DEA involvement
sd(chargedcases[chargedcases$Convicted == "0" & chargedcases$DEA.Involvement == "1", ]$TLPP)#standard deviation TLPP in patients with no conviction and DEA involvement
summary(chargedcases[chargedcases$Convicted == "0" & chargedcases$DEA.Involvement == "1", ]$TLPP)
mean(chargedcases[chargedcases$Convicted == "1" & chargedcases$DEA.Involvement == "1", ]$TLPP)#mean TLPP in patients with civil conviction and DEA involvement
sd(chargedcases[chargedcases$Convicted == "1" & chargedcases$DEA.Involvement == "1", ]$TLPP)#standard deviation TLPP in patients with civil conviction and DEA involvement
summary(chargedcases[chargedcases$Convicted == "1" & chargedcases$DEA.Involvement == "1", ]$TLPP)
mean(chargedcases[chargedcases$Convicted == "2" & chargedcases$DEA.Involvement == "1", ]$TLPP)#mean TLPP in patients with criminal conviction and DEA involvement
sd(chargedcases[chargedcases$Convicted == "2" & chargedcases$DEA.Involvement == "1", ]$TLPP)#standard deviation TLPP in patients with criminal conviction and DEA involvement
summary(chargedcases[chargedcases$Convicted == "2" & chargedcases$DEA.Involvement == "1", ]$TLPP)

#charges and convictions without DEA involvement
mean(chargedcases[chargedcases$Convicted == "0" & chargedcases$DEA.Involvement == "0", ]$TLPP)#mean TLPP in patients with no conviction and no DEA involvement
sd(chargedcases[chargedcases$Convicted == "0" & chargedcases$DEA.Involvement == "0", ]$TLPP)#standard deviation TLPP in patients with no conviction and no DEA involvement
summary(chargedcases[chargedcases$Convicted == "0" & chargedcases$DEA.Involvement == "0", ]$TLPP)
mean(chargedcases[chargedcases$Convicted == "1" & chargedcases$DEA.Involvement == "0", ]$TLPP)#mean TLPP in patients with civil conviction and no DEA involvement
sd(chargedcases[chargedcases$Convicted == "1" & chargedcases$DEA.Involvement == "0", ]$TLPP)#standard deviation TLPP in patients with civil conviction and no DEA involvement
summary(chargedcases[chargedcases$Convicted == "1" & chargedcases$DEA.Involvement == "0", ]$TLPP)
mean(chargedcases[chargedcases$Convicted == "2" & chargedcases$DEA.Involvement == "0", ]$TLPP)#mean TLPP in patients with criminal conviction and no DEA involvement
sd(chargedcases[chargedcases$Convicted == "2" & chargedcases$DEA.Involvement == "0", ]$TLPP)#standard deviation TLPP in patients with criminal conviction and no DEA involvement
summary(chargedcases[chargedcases$Convicted == "2" & chargedcases$DEA.Involvement == "0", ]$TLPP)

#mean and sd TLPP with or without patient death
mean(deadata[deadata$Patient.Death != "0", ]$TLPP)
sd(deadata[deadata$Patient.Death != "0", ]$TLPP)
summary(deadata[deadata$Patient.Death != "0", ]$TLPP)
mean(deadata[deadata$Patient.Death == "0", ]$TLPP)
sd(deadata[deadata$Patient.Death == "0", ]$TLPP)
summary(deadata[deadata$Patient.Death == "0", ]$TLPP)

#patient death with DEA involvement
mean(dea_cases[dea_cases$Patient.Death != "0", ]$TLPP)#mean TLPP in patients with patient death and DEA involvement
sd(dea_cases[dea_cases$Patient.Death != "0", ]$TLPP)#standard deviation TLPP in patients with patient death and DEA involvement
summary(dea_cases[dea_cases$Patient.Death != "0", ]$TLPP)
mean(dea_cases[dea_cases$Patient.Death == "0", ]$TLPP)#mean TLPP in patients without patient death and DEA involvement
sd(dea_cases[dea_cases$Patient.Death == "0", ]$TLPP)#standard deviation TLPP in patients without license suspensiopatient deathn and DEA involvement
summary(dea_cases[dea_cases$Patient.Death == "0", ]$TLPP)

#patient death without DEA involvement
mean(nondea_cases[nondea_cases$Patient.Death != "0", ]$TLPP)#mean TLPP in patients with patient death and without  DEA involvement
sd(nondea_cases[nondea_cases$Patient.Death != "0", ]$TLPP)#standard deviation TLPP in patients with patient death and without DEA involvement
summary(nondea_cases[nondea_cases$Patient.Death != "0", ]$TLPP)
mean(nondea_cases[nondea_cases$Patient.Death == "0", ]$TLPP)#mean TLPP in patients without patient death and without DEA involvement
sd(nondea_cases[nondea_cases$Patient.Death == "0", ]$TLPP)#standard deviation TLPP in patients without patient death and without DEA involvement
summary(nondea_cases[nondea_cases$Patient.Death == "0", ]$TLPP)
