require(dplyr)
require(plyr)
require(reshape2)
require(lubridate)
require(data.table)
require(plotrix)
require(ggplot2)
require(survminer)
require(survival)

### Set Time Zone

ols.sys.timezone <- Sys.timezone()
Sys.setenv(TZ = 'GMT')

########Load data that contains all species########

setwd("~/Jéssica/UNESP/LEMA/MESTRADO/Analises/Analises pos quali/AAR")

df <- read.csv("ArquivoAAR.csv")

########Format dates and times of sequences########

# Remove data with do dates
df[df$begin_date_time=="",] <- NA
df <- df[!is.na(df$begin_date_time),] # Remove data with do dates

# format with date/time
df$Begin.Time <- as.POSIXct(df$begin_date_time, format = "%d/%m/%Y %H:%M")
df$End.Time <- as.POSIXct(df$end_date_time, format = "%d/%m/%Y %H:%M")
str(df$Begin.Time)
str(df$End.Time)

#Subset by species (all preds and 1 prey)
Prey<-"Mtridac"
Pred1<-"Cfamiliaris"
species <- list(Prey, Pred1) # The species the user wants to focus on, in character and scientific name.

#Change the "sn" to whatever your species column is called
df.sp <- subset(df, sn %in% species)
unique(df.sp$sn) #check to make sure data were correctly subset

#Sort by deployment and begin_date_time
df.sp<-df.sp[with(df.sp, order(title, Begin.Time)), ]

#######################################################################
#              Mark the elements of the T ratios                      #
# These "Marks" denote the elements of the predator/prey "sandwich"    #
#                                                                     #
#         See Parsons et al. 2016 for more details                    # 
#                                                                     #
#              T1=Prey followed by a predator                         #
#                                                                     #
#       T2=predator followed by a prey, following a T1 and without    #
#                 another predator species in between                 #
#                                                                     #
#               T3=successive prey detections                         #
#                                                                     #              
#       T4=The total time between successive prey detections with a   #
#                 predator species detection in between               #
#       (may be many predator detection but ofonly one species)       #
#######################################################################

#First split by deployment
df.sp$title<-droplevels(df.sp$title)
out <- split(df.sp, f = df.sp$title)

#Remove any elements with just one row
for(i in names(out)){
  if (nrow(out[[i]]) < 2) {out[[i]]<-NULL}
}

#Now add the marks
sn<-list()
title<-list()
for(i in names(out)){
  out[[i]]$Tmark<-rep(NA, nrow(out[[i]]))
  sn[[i]]<-as.data.frame(out[[i]]$sn)
  names(sn[[i]])<-c("sn")
  for(j in 2:nrow(out[[i]])){
    if(as.character(sn[[i]][j,])==as.character(Prey) & as.character(sn[[i]][j-1,])==as.character(Prey)){out[[i]][j, 'Tmark']<-("T3")} 
    else if (as.character(sn[[i]][j,])==as.character(Pred1) & as.character(sn[[i]][j-1,])==as.character(Prey)) {out[[i]][j, 'Tmark']<-("Pred1T1")}
    else if (as.character(sn[[i]][j,])==as.character(Pred1) & as.character(sn[[i]][j-1,])==as.character(Pred1)) {out[[i]][j, 'Tmark']<-(NA)}
    else if (as.character(sn[[i]][j,])==as.character(Prey) & as.character(sn[[i]][j-1,])==as.character(Pred1)) {out[[i]][j, 'Tmark']<-("Pred1T2")}
  }
}

########################## Clean the Marks ###########################
#We need to make sure all Pred1T1 are 
#followed by Pred1T2 with nothing between and same for Pred2
#the last T1 needs a T2 or to be removed

#First remove all the NAs and add 3 final rows, calling them 
#"Nothing".  These are place holders to allow the loops to work.
non_na<-list()
newc<-list()
newc2<-list()
newc3<-list()
df2<-list()
bad1<-list()
bad2<-list()
bad3<-list()
bad4<-list()
bad5<-list()
r<-list()
for(i in names(out)){
non_na[[i]]<-out[[i]][complete.cases(out[[i]][,'Tmark']),]
c<-ncol(non_na[[1]])
newc[[i]]<-rep(NA, c)
newc2[[i]]<-rep(NA, c)
newc3[[i]]<-rep(NA, c)
newc[[i]][length(newc[[i]])]<-"Nothing"
newc2[[i]][length(newc2[[i]])]<-"Nothing"
newc3[[i]][length(newc3[[i]])]<-"Nothing"
df2[[i]]<-as.data.frame(rbind(non_na[[i]], newc[[i]],
                              newc2[[i]], newc3[[i]]))
r[[i]]<-nrow(df2[[i]])
bad1[[i]]<-rep(NA, nrow(df2[[i]]))
bad3[[i]]<-rep(NA, nrow(df2[[i]]))
bad5[[i]]<-rep(NA, nrow(df2[[i]]))
}

#Loop through all Tmark records at each site, starting with the
#second one.  If a mark is a T2 then see if the one coming before 
#it is a T1 for the appropriate predator and mark it "bad" if the
#sandwich is not intact to remove in the next step
r<-lapply(df2, nrow)

for(i in names(df2)){
  if(length(which(is.na(df2[[i]][,1])))==nrow(df2[[i]])){bad1[[i]][1:r[[i]]]<-rep("Bad", r[[i]])}
  else{
  for(j in 1:r[[i]]){
  if(as.character(df2[[i]][j, 'Tmark'])==as.character("Pred1T1") & as.character(df2[[i]][j+1, 'Tmark'])!=as.character("Pred1T2")){bad1[[i]][j]<-("Bad")}
  }
}
}

for(i in names(df2)){
  if(length(which(is.na(df2[[i]][,1])))==nrow(df2[[i]])){bad3[[i]][j]<-("Bad")}
  else{
    if(as.character(df2[[i]][1, 'Tmark'])==as.character("Pred1T2")){bad3[[i]][1]<-("Bad")}
  else{
for(j in 2:r[[i]]){
  if(as.character(df2[[i]][j, 'Tmark'])==as.character("Pred1T2") & as.character(df2[[i]][j-1, 'Tmark'])!=as.character("Pred1T1")){bad3[[i]][j]<-("Bad")}
      }
    }
  }
}


for(i in names(df2)){
  df2[[i]]$bad<-NA
for(j in 1:r[[i]]){
  if(!is.na(bad1[[i]][j])|!is.na(bad3[[i]][j])){df2[[i]][j, 'bad']<-("Bad")}
}
}

#Remove the bad rows 
#Add back in the first row
df3<-list()
one<-list()
for(i in names(df2)){
df3[[i]]<-df2[[i]]%>%filter(is.na(df2[[i]]$bad))
one[[i]]<-out[[i]][1,]
one[[i]]$bad<-"Nothing"
one[[i]]$Tmark<-"Nothing"
names(df3[[i]])<-names(one[[i]])
df3[[i]]<-as.data.frame(rbind(one[[i]], df3[[i]]))
}

################## Add times for each Tmark ##########################
#Remove any elements with only the first row and no others
df4<-df3
for(i in names(df4)){
  if (nrow(df4[[i]]) < 2) {df4[[i]]<-NULL}
}

#Sort by deployment and begin_date_time
#Then Add column: 
#Time_from = Begin.Time (col 40)-End.Time (col 41) 
#(of row above)
#Adjust the 40 and 41 for your dataset, they should be begin and end
#times for the sequences
for(i in names(df4)){
  df4[[i]]<-df4[[i]][with(df4[[i]], order(title, Begin.Time)), ]
  df4[[i]]$Time_from_min <- c(NA, difftime((df4[[i]][2:nrow(df4[[i]]), 'Begin.Time']), (df4[[i]][1:(nrow(df4[[i]])-1), 'End.Time']), unit="min"))
  #df4[[i]]$Time_from_min <- c(NA,df4[[i]][2:nrow(df4[[i]]), 40] - df4[[i]][1:(nrow(df4[[i]])-1), 41])
  df4[[i]]$Time_from_day <- df4[[i]]$Time_from_min/1440 
  df4[[i]]$title<-droplevels(df4[[i]]$title)
}

################### Calculate T4s ##################################
#Make a list to hold the output and set contents to numeric
#You will need to make a separate list for any additional predators
#and run an additional loop for each below
Pred1T4<-list()
Pred2T4<-list()
for(i in names(Pred1T4)){
  Pred1T4[[i]]<-as.numeric(Pred1T4[[i]])
  Pred2T4[[i]]<-as.numeric(Pred2T4[[i]])
}

#Define the number of rows that should be in each list element
n<-lapply(df4, nrow)

#For each site, sum the times associated with each T1 and T2 in a 
#sandwich and save in the appropriate T4 list
for(i in names(df4)){
  for(j in 2:n[[i]]){
    if(as.character(df4[[i]][j, 'Tmark'])==as.character("Pred1T2") & 
       as.character(df4[[i]][j-1, 'Tmark'])==as.character("Pred1T1")){
      Pred1T4[[i]][j]<-sum(df4[[i]][j, 'Time_from_day'], 
                           df4[[i]][j-1, 'Time_from_day'])}
    else {Pred1T4[[i]][j]<-NA}
  }
}


#################### Calculate T3s, T1s and T2s #################### 
#Make lists to hold the output and set the values to numeric
T3<-list()
Pred1T1T2<-list()
for(i in names(T3)){
  T3[[i]]<-as.numeric(T3[[i]])
  Pred1T1T2[[i]]<-as.numeric(Pred1T1T2[[i]])
}

#Set the length of the output for each site (number of rows)
n<-lapply(df4, nrow)

#Calculate the T3s and save to be compared to the T4s in a later step
for(i in names(df4)){
  for(j in 2:n[[i]]){
    if(as.character(df4[[i]][j, 'Tmark'])==as.character("T3")){T3[[i]][j]<-
      df4[[i]][j, 'Time_from_day']}
    else {T3[[i]][j]<-NA}
  }
}

########## Calculate the T2/T1 ratios for each site/predator ##########
#Note, if dealing with more than two predators you will need to add
#a line for each one in the loop below and add a new output list
for(i in names(df4)){
  for(j in 2:n[[i]]){
    if(as.character(df4[[i]][j, 'Tmark'])==as.character("Pred1T2") & 
       as.character(df4[[i]][j-1, 'Tmark'])==as.character("Pred1T1")){
      Pred1T1T2[[i]][j]<-(df4[[i]][j, 'Time_from_day']/
                            df4[[i]][j-1, 'Time_from_day'])}
    else if(as.character(df4[[i]][j, 'Tmark'])==as.character("Pred2T2") & 
            as.character(df4[[i]][j-1, 'Tmark'])==as.character("Pred2T1")){
      Pred2T1T2[[i]][j]<-(df4[[i]][j, 'Time_from_day']/
                            df4[[i]][j-1, 'Time_from_day'])}
  }
}

#Convert the lists to data frames, adding a column for each
#Predator and then combine together into one data frame.
Pred1_z <- data.frame(Title = rep(names(Pred1T1T2), sapply(Pred1T1T2, length)),
                 T2T1Ratio = unlist(Pred1T1T2, use.names=FALSE))
Pred1_z<-as.data.frame(Pred1_z[complete.cases(Pred1_z),])
Pred1_z$Pred<-rep("Pred1", nrow(Pred1_z))

  
################# Compute T4/T3 ratio for each site/predator #######################
#Calculate means
Pred1T4_mean<-lapply(Pred1T4, mean, na.rm=TRUE)
Pred2T4_mean<-lapply(Pred2T4, mean, na.rm=TRUE)
T3_mean<-lapply(T3, mean, na.rm=TRUE)

#Make new lists to store the T4/T3 ratios for each site
#Then calculate the ratios
T4_T3_Pred1<-NA
for (i in names(T3_mean)){
T4_T3_Pred1[i]<-Pred1T4_mean[[i]]/T3_mean[[i]]
}

#Clean up the list, add a column to label the predators and combine
#together
T4_T3_Pred1<-as.data.frame(T4_T3_Pred1)
T4_T3_Pred1<-as.data.frame(T4_T3_Pred1[complete.cases(T4_T3_Pred1),])
T4_T3_Pred1$Pred<-rep("Pred1", nrow(T4_T3_Pred1))
names(T4_T3_Pred1)<-c("T4T3Ratio", "Pred")

############ Test Significance with Exponential Model ################
#This is an absolute test of significant avoidance of a predator
#by prey
#It is different from the test above which compares the relative 
#strength of avoidance/attraction of different predators on a prey 
#species

#T4 and T3 are most appropriate for this
#First unlist the T4s and T3 into data frames and clean them up
#Make separate data frames for each predator and combine with
#T3.  Then mark a 0 for T3 and 1 for T4s in a new Preds column
T4_Pred1 <- data.frame(Title = rep(names(Pred1T4), sapply(Pred1T4, length)),
                      T4 = unlist(Pred1T4, use.names=FALSE))
T4_Pred1<-as.data.frame(T4_Pred1[complete.cases(T4_Pred1),])
T4_Pred1$Pred<-rep(1, nrow(T4_Pred1))

T3_z <- data.frame(Title = rep(names(T3), sapply(T3, length)),
                       T4 = unlist(T3, use.names=FALSE))
T3_z<-as.data.frame(T3_z[complete.cases(T3_z),])
T3_z$Pred<-rep(0, nrow(T3_z))

#Then make sure we have at least 1 T3 and T4 for each site, otherwise
#Remove the sites
`%notin%` <- function(x,y) !(x %in% y) 
d<-setdiff(as.character(T4_Pred1$Title), as.character(T3_z$Title))
d2<-setdiff(T3_z$Title, T4_Pred1$Title)
dat_P1_z<-T3_z[T3_z$Title %notin% d2,]
dat_P1_q<-T4_Pred1[T4_Pred1$Title %notin% d,]
dat_P1<-as.data.frame(rbind(dat_P1_z, dat_P1_q))


#Fit a Cox proportional hazard model to see if the time between
#successive prey detections is significantly different if a predator
#passes in the middle
cox_P1 <- coxph(Surv(T4) ~ Pred, data = dat_P1)

#A significant p.value indicates predators have an influence on the
#time to the next prey detection (i.e. provoke avoidance)
summary(cox_P1)
#The hazard ratios exp(coef) are interpretable as multiplicative 
#effects on the hazard

##################### Plot the results ################################
#The code below makes a plot for each predator and shows the 
#Time-to-detection of the next prey species at sites where the 
#predator is present (pred=1) and absent (pred=0)

fit_P1 <- survfit(Surv(T4) ~ Pred, data = dat_P1)

ggsurvplot(fit_P1, data = dat_P1, conf.int = TRUE,
           ggtheme = theme_minimal(), 
           xlab=c("Days to next prey passage"), ylab=c("Probability of second prey passage"),
           xlim=c(0, max(dat_P1$T4)), fun="event", title=Pred1)
