library(ggplot2) #importing Libraries
library(lpSolve)

setwd("C:/Users/idris/Desktop/Asmnt") #setting path
raw_data = as.matrix(read.table("Forest_2022.txt")) #reading the data into R
data = raw_data[sample(1:517,330), c(1:13)] #selecting a subset of data
df = as.data.frame(data) #converting to data frame



#hist.data.frame(df) # histogram of all the variables
#qplot(V1,data=df)
#qplot(V2,data=df)
#qplot(V3,data=df)
#qplot(V4,data=df)
qplot(V5,data=df, main="Histogram of FFMC index from the FWI system ")
qplot(V6,data=df, main="Histogram DMC index from the FWI system ")
qplot(V7,data=df, main="Histogram DC index from the FWI system")
qplot(V8,data=df, main="Histogram ISI index from the FWI system")
qplot(V9,data=df, main="Histogram temperature in Celsius degrees")
qplot(V10,data=df, main="Histogram relative humidity in % ")
qplot(V11,data=df, main="Histogram wind speed in km/h ")
qplot(V12,data=df, main="Histogram outside rain in mm/m2")
qplot(V13,data=df, main="Histogram the burned area of the forest (in ha)")

plot(df) # scatter plot of all independent variables vs target variable
#ggplot(df, aes(x = V1, y = V13)) + geom_point()
#ggplot(df, aes(x = V2, y = V13)) + geom_point()
#ggplot(df, aes(x = V3, y = V13)) + geom_point()
#ggplot(df, aes(x = V4, y = V13)) + geom_point()
ggplot(df, aes(x = V5, y = V13)) + geom_point() + ggtitle("FFMC index from the FWI system vs burned area of the forest (in ha)")
ggplot(df, aes(x = V6, y = V13)) + geom_point() + ggtitle("Histogram DMC index from the FWI system vs burned area of the forest (in ha)")
ggplot(df, aes(x = V7, y = V13)) + geom_point() + ggtitle("Histogram DC index from the FWI system vs burned area of the forest (in ha")
ggplot(df, aes(x = V8, y = V13)) + geom_point() + ggtitle("Histogram ISI index from the FWI system vs burned area of the forest (in ha")
ggplot(df, aes(x = V9, y = V13)) + geom_point() + ggtitle("Histogram temperature in Celsius degrees vs burned area of the forest (in ha")
ggplot(df, aes(x = V10, y = V13)) + geom_point() + ggtitle("Histogram relative humidity in % vs burned area of the forest (in ha")
ggplot(df, aes(x = V11, y = V13)) + geom_point() + ggtitle("Histogram wind speed in km/h vs burned area of the forest (in ha)")
ggplot(df, aes(x = V12, y = V13)) + geom_point() + ggtitle("Histogram outside rain in mm/m2 vs burned area of the forest (in ha)")

keep = c("V5","V6","V8","V9","V13") #selecting the 4 variables we will be using to train our model
dff=df[keep]

dff$V8[dff$V8 >= 30] = (mean(dff[,3])) # replacing outliers with mean value
dff$V5[dff$V5 <= 80] = (mean(dff[,1])) # replacing outliers with mean value

#dff[,4] = (dff[,4]-mean(dff[,4]))/sd(dff[,4])
#dff[,6] = (dff[,6]-min(dff[,6]))/(max(dff[,6])-min(dff[,6])) #V13
dff[,5] = (dff[,5]-min(dff[,5]))/(max(dff[,5])-min(dff[,5])) #V13 # normalizing/feature scaling all our variables (including target)
dff[,4] = (dff[,4]-min(dff[,4]))/(max(dff[,4])-min(dff[,4])) #V9
dff[,3] = (dff[,3]-min(dff[,3]))/(max(dff[,3])-min(dff[,3])) #V8
dff[,2] = (dff[,2]-min(dff[,2]))/(max(dff[,2])-min(dff[,2])) #V6
dff[,1] = (dff[,1]-min(dff[,1]))/(max(dff[,1])-min(dff[,1])) #V5

#dff$V5 = log(dff$V5)
dff$V5 = dff$V5^(2) # polynomial transformation of V5 to normalize distribution
dff$V8 = dff$V8^(0.66) # polynomial transformation of v8 to normalize distribution

qplot(V5,data=dff,main="After Scaling & Transformation") #sample plots
ggplot(dff, aes(x = V5, y = V13)) + geom_point() + ggtitle("After Scaling & Transformation") #sample plots
write.table(dff,"Idris-transformed.txt")

source("AggWaFit718.R") #initializing aggregation functions code file
fit.QAM(dff[,c(1:4,5)],output.1="AMoutput1.txt",stats.1="AMstats1.txt", g=AM,g.inv = invAM) #weighted arithmetic mean
fit.QAM(dff[,c(1:4,5)],output.1="05output1.txt",stats.1="05stats1.txt", g=PM05,g.inv = invPM05) #weighted power mean (p=0.5)
fit.QAM(dff[,c(1:4,5)],output.1="2output1.txt",stats.1="2stats1.txt", g=QM,g.inv = invQM) #weighted power mean (p=2)
fit.OWA(dff[,c(1:4,5)],output.1="owaoutput1.txt",stats.1="owastats1.txt") #ordered weighted average

keep = c("V5","V6","V8","V9","V13") #max/min required for scaling test data
dff=df[keep]
wam = c(0.206,0.317,0.106,0.369) #weights from our trained arithmetic mean model
xi = c(96.1,181.1,14.3,20.7) #test values for variables (v5,v6,v8,v9) as given in pdf
xs = c(0,0,0,0) #to store after scaling
pos=1
for (x in xi)
{xs[pos]=(x-min(dff[,pos]))/(max(dff[,pos])-min(dff[,pos])) #scaling the test values (same as for training data)
pos =pos+1}
xs[1] = xs[1]^(2) #polynomial transformation, similar to training data
xs[3] = xs[3]^(0.66)
print(xs) #scaled test values

ya = 0.0147 #expected y value as given in pdf
ys = (wam[1]*xs[1]+(wam[2]*xs[2])+(wam[3]*xs[3])+(wam[4]*xs[4])) # calculating y(scaled) as per model weights & scaled test input
yp = ((ys*max(dff[,5]))-(ys*min(dff[,5])))+min(dff[,5]) # inverse scaling on predicted target variable

print(ya)#expected value
print(yp)#scaled value
error = yp-ya
print(error)# error


#alternatively we can predict the y value using the inbuilt function from AggWaFit718.R script. We will get the same result. Lets Check
ys = QAM(xs,wam) # since our best performance is by using weighted arithmetic mean
yp = ((ys*max(dff[,5]))-(ys*min(dff[,5])))+min(dff[,5]) # inverse scaling on predicted target variable

print(ya)#expected value
print(yp)#scaled value
error = yp-ya
print(error)# error

# we observe that we get the same prediction
