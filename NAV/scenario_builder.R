### R code for randomly build navigation scenarios 

# set parameters 
n <- 10             # number of cues
distance <- 100     # minimum distance
radious <- 40       # dimensions of the square

# generate points with uniform spatial distribution 
Points <- data.frame(X = trunc(runif(1, min=50+(radious), max=590-(radious))), Y = trunc(runif(1, min=50+(radious), max=470-(radious))))
while(nrow(Points) < n) {
    aux <- rbind(Points, data.frame(X = trunc(runif(1, min=50+(radious), max=590-(radious))), Y = trunc(runif(1, min=50+(radious), max=470-(radious)))))
    # if minimum distance is satisfied, add the new point 
    if (min(dist(aux)) > distance) {Points <- aux}
}

# plot the scenario
plot(Points, xlim=c(0, 640), ylim=c(0, 520))
# add border lines
abline(h=50, col='red'); abline(h=470, col='red')
abline(v=50, col='red'); abline(v=590, col='red')
# draw the squares
rect(Points[1,1] - radious, Points[1,2] + radious, Points[1,1] + radious, Points[1,2] - radious, col='blue')
for (i in 2:nrow(Points)) {
    rect(Points[i,1] - radious, Points[i,2] + radious, Points[i,1] + radious, Points[i,2] - radious)
}

# write points output in a csv file 
write.csv(Points, file='points.csv')

# write processing code for the scenario in a txt file 
write('\n//in playSound()\n\nif(ActiveSound) {', file = 'data.txt',append = T)
for (i in 1:n) {
    var <- paste('if (v1.x > ', as.character(trunc(Points[i,1]-radious)), 
            ' && v1.x < ', as.character(trunc(Points[i,1]+radious)), 
            ' && v1.y > ', as.character(trunc(Points[i,2]-radious)), ' && v1.y < ', 
            as.character(trunc(Points[i,2]+radious)), 
            ') {\n  if(!song', as.character(i), '.isPlaying()) {\n    song', 
            as.character(i), '.play();\n    song', 
            as.character(i), '.rewind();\n  }\n}', sep='')
   write(var, file = "data.txt",append = T)
}
write('}', file = 'data.txt',append = T)
