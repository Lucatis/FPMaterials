### R code for data management of the scenarios

# set input parameters 
string <- 'B10_04'    # scenario and subject 
scenario <- 'B10'     # scenario

# read tables
Dat <- read.table(paste('./subjects/', string, '.txt', sep=''), sep=';', dec='.')
Points <- read.csv(paste('./scenarios/points_', scenario,'.csv', sep=''), row.names=1)

# set useful variables
threshold <- 50      # threshold for the points 
radious <- 40       # dimensions of squares for the plot
index <- as.numeric(row.names(Dat[Dat$V1 == 'search',])) # index of 'search' occurrences

## data cleaning and build a list splitting Data
# generate the list and add each part according to index 
theList <- list(Dat[1:(index[1]-1),])
for (i in 2:(length(index))) {
    theList[[i]] <- Dat[((index[i-1])+1):((index[i])-1),] 
}
theList[[length(index)+1]] <- Dat[((index[length(index)])+1):nrow(Dat),]
# coerce to numeric
theList <- lapply(theList, function(x) {data.frame(X=as.numeric(as.character(x$V1)), Y=as.numeric(as.character(x$V2)))})
# smooth and build a new list 
theList2 <- list()
# for each element in the list
for (j in 1:length(theList)) {
    aux <- theList[[j]][1,] # first row
    # for each coordinate     
    for (i in 2:nrow(theList[[j]])) {
        # if distance satisface the threshold and a maximum, add the point
        if(dist(rbind(aux[nrow(aux),], theList[[j]][i,])) > threshold & dist(rbind(aux[nrow(aux),], theList[[j]][i,])) < 100) {
            aux <- rbind(aux, theList[[j]][i,])
        }
    }
    aux <- rbind(aux, theList[[j]][nrow(theList[[j]]),]) # last row
    # add the new data.frame to the new list 
    theList2[[j]] <- aux
}

## plot 
auxPlot <- theList2[[2]] # set the part of the list to plot
# points and squares
plot(Points, xlim=c(0, 640), ylim=c(0, 520))
abline(h=50, col='red'); abline(h=470, col='red'); abline(v=50, col='red'); abline(v=590, col='red')
rect(Points[1,1] - radious, Points[1,2]+radious, Points[1,1]+radious, Points[1,2]-radious, col='dodgerblue1')
for (i in 2:nrow(Points)) {
    rect(Points[i,1]-radious, Points[i,2]+radious, Points[i,1]+radious, Points[i,2]-radious)
}
points(Points, xlim=c(0, 640), ylim=c(0, 520))
points(auxPlot, type='l', lwd = 1.5, lty=2)
# straight lines for the optimal path and participant's path 
points(rbind(auxPlot[1,], auxPlot[nrow(auxPlot),]), type='l', col='red')
points(rbind(auxPlot[1,], Points[1,]), type='l', col='blue')
# add initial and final position points 
points(auxPlot[1,], pch=16, col='red')
points(auxPlot[nrow(auxPlot),], pch=16, col='blue')

## analysis 
auxPlot <- theList2[[2]] # set the part of the list to analyze 
# initial and final coordinates of trajectories  
a <- as.matrix(rbind(auxPlot[1,], Points[1,]))              # optimal
b <- as.matrix(rbind(auxPlot[1,], auxPlot[nrow(auxPlot),])) # trajectory
# coordinates and centred for angle 
A <- as.numeric(Points[1,])                                 # goal
B <- as.numeric(auxPlot[nrow(auxPlot),])                    # final point
O <- as.numeric(auxPlot[1,])                                # origin
Ac <- A - O                                                 # centred goal
Bc <- B - O                                                 # centred final point
# compute variables 
OPT <- trunc(as.numeric(dist(a)))                           # optimal distance 
SUB <- trunc(as.numeric(dist(b) - dist(a)))                 # vacttor substraction 
DIF <- trunc(as.numeric(dist(rbind(a[2,], b[2,]))))         # final points distance 
REL <- abs(SUB / OPT)                                       # relative distance
# angle between vectors in degrees 
AN <- round(acos((Ac%*%Bc) / (sqrt(Ac[1]^2 + Ac[2]^2) * sqrt(Bc[1]^2 + Bc[2]^2))) * (180/pi), 2)
# write the results in a csv fle 
write.csv(data.frame(OPT, SUB, DIF, AN, REL), file='temp.csv')

