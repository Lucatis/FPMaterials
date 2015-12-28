require(ggplot2)        # ggplot2
require(ggbiplot)       # pca biplot
require(reshape)        # melt function
library(pgirmess)       # Post-Hoc-Test after Kruskal-Wallis

# ======================================================================================
#        CUSTOM FUNCTIONS

# stopping-rule for cuttind a dendogram ------------------------------------------------
.cutDendro <- function(hc) {
    # establish the relevant height for cutting by mean(h) + 1.25*sd(h)
    h <- min(hc$height[hc$height > mean(hc$height) + 1.25*sd(hc$height)]) # Mojena (1977)
    k <- 1 + length(hc$height[hc$height > mean(hc$height) + 1.25*sd(hc$height)]) 
    return(list(k = k, h = h, group = cutree(hc, k = k)))
}

# centroids extraction -----------------------------------------------------------------
.centroids <- function (dataset, group) {
    df <- data.frame(group = group, dataset)
    centroids <- data.frame()
    # for each category in group, compute the column means and store in centroids 
    for (i in 1:length(levels(factor(group)))) {
        centroids <- rbind(centroids, colMeans(df[df$group == i,]))
    }
    names(centroids) <- names(df)
    return(centroids)
}

# ======================================================================================
#        DATASET AND CLUSTERING

## Datasets ----------------------------------------------------------------------------
# load Data
Data <- read.csv('dataset.csv')
# add column names 
row.names(Data) <- paste(Data$scenario, Data$subject, Data$trial, sep='_')
# build a dataset with the three variables and scale them 
dataset <- Data[,c(7, 8, 9)]
dataset <- scale(dataset)

## clustering and PCA --------------------------------------------------------------------
# build a hierarchical dendrogram and cut it. See cutDendro function
hc <- hclust(dist(dataset), method = 'ward.D') 
groups <- .cutDendro(hc) 
# run k-means algorithm by passing the centroids of the dendrogram clustering
K <- kmeans(dataset, centers = as.matrix(.centroids(dataset, groups$group)[,-1]))
grp <- K$cluster
# build PCA and compute accumulated variance
pca <- prcomp(dataset, scale = F, center = T)
Variance <- (pca$sdev)^2 / sum(pca$sdev^2)


# ======================================================================================
#        PLOTS

# biplot -------------------------------------------------------------------------------
pcaplot <- ggbiplot(pca, obs.scal=1, var.scale=1, groups=as.factor(grp), circle=F, ellipse=T) + 
    scale_colour_brewer(palette = 'Set1')

# boxplots for groups ------------------------------------------------------------------
gg.aux <- melt(data.frame(grp=as.factor(grp), dataset), id.vars=1, variable_name='variable')
Boxplots <- ggplot(gg.aux, aes(x=grp, y=value, group=interaction(grp, variable))) +
    geom_boxplot(aes(fill=factor(variable)), width=0.7, outlier.size=0) +
    labs(x='cluster', y='z-score') + 
    scale_fill_discrete(guide = guide_legend(title=NULL))

# scatterplot for pca ------------------------------------------------------------------
gg.aux <- data.frame(pca$x, grp=grp, cues=Data$cues, label=row.names(dataset), trial=factor(Data$trial), cues=Data$cues)
pcaplot <- ggplot(gg.aux) +
    geom_point(aes(x=PC1, y=PC2, colour=cues, shape=trial), size=3) + 
    stat_ellipse(aes(x=PC1,y=PC2, fill=factor(grp)), geom='polygon', level=0.75, alpha=0.2) +
    geom_text(aes(label=label, x=PC1, y=PC2), size=1.8, angle=0, vjust=3) + 
    scale_fill_discrete(guide=guide_legend(title='cluster'))

#  boxplots for trials -----------------------------------------------------------------
gg.aux <- melt(data.frame(grp=as.factor(Data$trial), dataset), id.vars=1, variable_name='variable')
Boxplots <- ggplot(gg.aux, aes(x=grp, y=value, group=interaction(grp, variable))) +
    geom_boxplot(aes(fill=factor(variable)), width=0.7, outlier.size=0) +
    labs(x='trial', y='z-score') + 
    scale_fill_discrete(guide=guide_legend(title=NULL))

# boxplots for cues --------------------------------------------------------------------
gg.aux <- melt(data.frame(grp=as.factor(Data$cues), dataset), id.vars=1, variable_name='variable')
Boxplots <- ggplot(gg.aux, aes(x=grp, y=value, group=interaction(grp, variable))) +
    geom_boxplot(aes(fill=factor(variable)), width=0.7, outlier.size=0) +
    labs(x='number of cues', y='z-score') + 
    scale_fill_discrete(guide=guide_legend(title=NULL))

# smooth line for variables as a function of number of cues ----------------------------
# DIF variable
p1 <- ggplot(Data, aes(x=cues, y=DIF, colour=factor(trial))) + 
    stat_smooth(se=F) + stat_smooth(aes(x=cues, y=DIF), colour='black', se=T) + 
    scale_color_discrete(guide = guide_legend(title = 'trial'))
# ANG variable
p2 <- ggplot(Data, aes(x=cues, y=ANG, colour=factor(trial))) + 
    stat_smooth(se=F) + stat_smooth(aes(x=cues, y=ANG), colour='black', se=T) + 
    scale_color_discrete(guide = guide_legend(title = 'trial'))
# REL variable
p3 <- ggplot(Data, aes(x=cues, y=REL, colour=factor(trial))) + 
    stat_smooth(se=F) + stat_smooth(aes(x=cues, y=REL), colour='black', se=T) + 
    scale_color_discrete(guide = guide_legend(title = 'trial'))

# ======================================================================================
#        TESTS

# Chi-squared Test for grp~cues and grp~trial ------------------------------------------
chisq.test(table(grp, Data$cues))
chisq.test(table(grp, Data$trial))

# Kruskal-Wallis rank sum test for the three variables and cues ------------------------
kruskal.test(DIF ~ factor(cues), data = Data)
kruskal.test(ANG ~ factor(cues), data = Data)
kruskal.test(REL ~ factor(cues), data = Data)

# Post-Hoc-Test after Kruskal-Wallis for DIF~cues ans ANG~cues -------------------------
kruskalmc(Data$DIF, Data$cues)
kruskalmc(Data$ANG, Data$cues)

