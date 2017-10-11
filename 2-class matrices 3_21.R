install.packages("ggplot2")

library(ggplot2)

LCA <- read.csv("C:/Users/icarlso1/Desktop/LCA 2-Class se outlier 2.17.1.csv")

LCA.2.Class.abridged.3.14 <- read.csv("C:/Users/icarlso1/Simulation/Programs/LCA 2-Class/Data/LCA 2-Class abridged 3.14.csv")

mydat <- LCA.2.Class.abridged.3.14


mydat$indicators <- factor(mydat$indicators,levels = c(1,2,3,4,5,6,7,8,9),labels = c("4", "5", "6" , "7", "8", "9", "10", "11", "12"))
mydat$cov2 <- factor(mydat$cov2,levels = c(1,2,3,4),labels = c("no covariate", "small covariate", "moderate covariate" , "large covariate"))
mydat$qual2 <- factor(mydat$qual2,levels = c(1,2,3),labels = c("high quality", "moderate quality", "low quality" ))
mydat$observations <- factor(mydat$observations,levels = c(1,2,3,4,5,6,7),labels = c("N = 70", "N = 100", "N = 200" , "N = 300", "N = 500", "N = 1000", "N = 2000"))
mydat$qualse <- factor(mydat$qualse,levels = c(1,2,3,4,5,6),labels = c("high quality, ML", "mod quality, ML", "low quality, ML","high quality, MLR", "mod quality, MLR", "low quality, MLR"))

dat1 <- subset(mydat, se==1)
dat2 <- subset(mydat, cov != 1)
dat3 <- subset(mydat, cov != 1 & se ==1)

plot1 <- ggplot(mydat , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=seoutlier_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix seout.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)



plot1 <- ggplot(mydat , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=unconverged_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix unconverged.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)



plot1 <- ggplot(mydat , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=improper_conv_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix improper.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)


plot1 <- ggplot(mydat , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=labelswitch_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix labelswitch.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)



plot1 <- ggplot(dat1, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=l1ok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix L1 bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat1, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=l2ok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix L2 bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat1, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=crphiok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix crp high bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat1, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=crplook_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix crp low bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat3, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=xok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix covariate bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)



plot1 <- ggplot(mydat, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=l1seok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix L1 se bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(mydat, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=l2seok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix L2 se bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(mydat, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=crphiseok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix crp high se bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(mydat, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=crploseok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix crp low se bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat2, aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=xseok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix covariate se bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)


