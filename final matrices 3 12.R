install.packages("ggplot2")

library(ggplot2)

LCA <- read.csv("C:/Users/icarlso1/Desktop/LCA 2-Class se outlier 2.17.1.csv")

mydat <- LCA.2.Class.abridged.3.14

mydat$summary <- 0 + 1

mydat$summary <- ifelse(mydat$unconverged < 333, c(mydat$summary + 1), c(mydat$summary)) 

mydat$summary <- ifelse(mydat$incorrswitch_tot < 333, c(mydat$summary + 2), c(mydat$summary))

mydat$summary <- ifelse(mydat$improper_tot < 333, c(mydat$summary + 3), c(mydat$summary))

mydat$summary <- ifelse(mydat$seoutlier_tot < 333, c(mydat$summary + 4), c(mydat$summary))



mydat$indicators <- factor(mydat$indicators,levels = c(1,2,3,4,5,6,7,8,9),labels = c("4", "5", "6" , "7", "8", "9", "10", "11", "12"))
mydat$cov2 <- factor(mydat$cov2,levels = c(1,2,3,4),labels = c("no covariate", "small covariate", "moderate covariate" , "large covariate"))
mydat$observations <- factor(mydat$observations,levels = c(1,2,3,4,5,6,7),labels = c("N = 70", "N = 100", "N = 200" , "N = 300", "N = 500", "N = 1000", "N = 2000"))
mydat$qualse <- factor(mydat$qualse,levels = c(1,2,3,4,5,6),labels = c("high quality, ML", "mod quality, ML", "low quality, ML","high quality, MLR", "mod quality, MLR", "low quality, MLR"))

memory.limit(size = 4095)

dat1 <- subset(mydat, se==1)
dat2 <- subset(mydat, se==2)



plot1 <- ggplot(dat1 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=seoutlier_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix se1 seout.png", width = 1000, height = 1000, units = 'px')
print(plot2)
dev.off()
remove(plot3)

plot1 <- ggplot(dat2 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=seoutlier_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2)
png(filename = "E://Master's//Data Meeting Revisions//matrix se2 seout.png", width = 1000, height = 1000, units = 'px')
print(plot2)
dev.off()
remove(plot3)


plot1 <- ggplot(dat1 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=unconverged_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix se1 unconverged.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)


plot1 <- ggplot(dat1 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=seoutlier_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix se1 seout.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat2 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=seoutlier_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2)
png(filename = "E://Master's//Data Meeting Revisions//matrix se2 seout.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)


plot1 <- ggplot(dat1 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=unconverged_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix se1 unconverged.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat2 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=unconverged_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2)
png(filename = "E://Master's//Data Meeting Revisions//matrix se2 unconverged.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)


plot1 <- ggplot(dat1 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=improper_conv_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix se1 improper.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat2 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=improper_conv_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2)
png(filename = "E://Master's//Data Meeting Revisions//matrix se2 improper.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)



plot1 <- ggplot(dat1 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=labelswitch_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix se1 labelswitch.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat2 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=labelswitch_tot)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2)
png(filename = "E://Master's//Data Meeting Revisions//matrix se2 labelswitch.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)



plot1 <- ggplot(mydat , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=l1seok_tot)) + scale_fill_gradient(low="red", high="green")
plot3 <- plot2 + facet_grid(qualse ~cov2) 
png(filename = "E://Master's//Data Meeting Revisions//matrix se1 L1bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

plot1 <- ggplot(dat2 , aes(x=indicators,y=observations))
plot2 <- plot1 + geom_tile(aes(fill=L1_bias)) + scale_fill_gradient(low="green", high="red")
plot3 <- plot2 + facet_grid(qual2 ~cov2)
png(filename = "E://Master's//Data Meeting Revisions//matrix se2 L1bias.png", width = 1000, height = 1000, units = 'px')
print(plot3)
dev.off()
remove(plot3)

