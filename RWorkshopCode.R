#Welcome to the R Workshop

### (Basic numeric operations) #############################
1
1+1
2
x=1
x
x=x+1
x
x+1

x==3
x>3
x<3

#### Install R packages ####
install.packages(nhanesA)
install.packages(tableone)


#### Load R packages ####
library(nhanesA)
library(tableone)

#### Using a function from an R package ####
nhSearch=nhanesSearch(search_terms = "bladder")
View(nhSearch)


#### Read data in ####
data<-read.csv("C:/file.csv",header = FALSE) #know the location of the file
data<-read.csv(file.choose(),header = FALSE) #need to find file



#### Get subset of the data ####
auto=subset(mtcars,am==0,select=mpg)
manual=subset(mtcars,am==1,select=mpg)

ex<-subset(mtcars,am==1,select=c(mpg,am,vs))
View(ex)


#### The T-test  ####
tt<-t.test(x = auto, y=manual, alternative = "two.sided",mu = 0,paired = FALSE,var.equal = FALSE,conf.level = 0.95)
tt

tt_v2<-t.test(x = auto, y=manual, alternative = "two.sided",mu = 0,paired = FALSE,var.equal = TRUE,conf.level = 0.95)
tt_v2


#### The help command in R ####
help(mtcars)

#### Access data from dataset ####
mtcars$am
mtcars$vs

#### Tables and Chi-Square Test in R ####
tab<-table(Engine=mtcars$vs,Transmission=mtcars$am)
tab
chisq.test(tab)


#### An NHANES Example and Linear Regression ####
flu_nhSearch=nhanesSearch(search_terms = "flu") #search for the the term flu
View(flu_nhSearch)
vit_nhSearch=nhanesSearchTableNames("VIT",details = TRUE)
View(vit_nhSearch)

flu_data=nhanes("HSQ_D")
vit_data=nhanes("VITAEC_D")

full_data=merge(flu_data,vit_data,by = "SEQN")

table(full_data$HSQ520) #1 is yes, and 2 is no. Everything else is missing
table(full_data$HSQ500) #1 is yes, and 2 is no. Everything else is missing

#LBXVIA is the micronutrient level of Vitamin A (ug/dL) in the blood higher is more of that micronutrient in the blood
summary(full_data$LBXVIA)
hist(full_data$LBXVIA)

#LBXVIE is the micronutrient level of Vitamin E (ug/dL) in the blood higher is more of that micronutrient in the blood
summary(full_data$LBXVIE)
hist(full_data$LBXVIE)

#creating new variables to use with t.test for vitamin A
cold_vita<-subset(full_data,HSQ500==1,LBXVIA)
nocold_vita<-subset(full_data,HSQ500==2,LBXVIA)
tt_nhanes_vita=t.test(cold_vita,nocold_vita) #t test happens here.

tt_nhanes_vita #this prints out the result. 

#what about VitE?
cold_vite<-subset(full_data,HSQ500==1,LBXVIE) 
nocold_vite<-subset(full_data,HSQ500==2,LBXVIE)
tt_nhanes_vite=t.test(cold_vite,nocold_vite) #t-test
tt_nhanes_vite


full_data$newvar=NA
full_data$newvar[full_data$HSQ500==1]="Cold"
full_data$newvar[full_data$HSQ500==2]="No Cold"
full_data$newvar=factor(full_data$newvar,levels = c("No Cold","Cold"))
table(full_data$newvar)

#vitamin E
lm_fit_vite<-lm(formula = "LBXVIE~newvar",data=full_data)
summary(lm_fit_vite)
plot(lm_fit_vita)

#vitamin A
lm_fit_vita<-lm(formula = "LBXVIA~newvar",data=full_data)
summary(lm_fit_vita)
plot(lm_fit_vita)

#vitamin E
plot(x = full_data$newvar,y = full_data$LBXVIE, main="Boxplot Vitamin E by Cold Status",xlab="", ylab="Vitamin E")

#vitamin E and A scatterplot
plot(x = full_data$LBXVIA,y = full_data$LBXVIE, main="Scatterplot for Vitamin E and A",xlab="Vitamin A", ylab="Vitamin E")


#### Create a table for a paper using the tableone package ####

#assure variables are numerical variables in R
full_data[,grep(x = colnames(full_data),pattern = "LBX")]<-lapply(full_data[,grep(x = colnames(full_data),pattern = "LBX")],as.numeric)

#create table of VitE and VitA by Cold Status.
tabone=CreateTableOne(vars = c("LBXVIE","LBXVIA"),data = full_data,strata = "newvar")
tabone

#Get all variables that include LBX in their name
vars<-colnames(full_data)[grep(x = colnames(full_data),pattern = "LBX")]
tabone_a=CreateTableOne(vars = vars,data = full_data,strata = "newvar")
tabone_a

#write file to a csv file in your working directory.
write.csv(print(tabone_a),file = "tableone.csv")