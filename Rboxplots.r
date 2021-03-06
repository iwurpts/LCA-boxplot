library(ggplot2)
library(foreign)
se.outlier.2.class <- read.spss("C:\\Users\\icarlso1\\Simulation\\Programs\\LCA 2-Class\\Data\\LCA 2-Class se outlier 1.16.2.sav")
mydataf = data.frame(se.outlier.2.class)

data11 <- subset(mydataf, se=="ML"& quality=="(high quality)")

g.11L1prop <- ggplot(data11, aes(factor(indicators), L1prop))
g.11L1prop <- g.11L1prop + stat_boxplot()
g.11L1prop <- g.11L1prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L1prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.11L1prop.png", width = 1000, height = 1000, units = 'px')
print(g.11L1prop)
dev.off()

g.11L2prop <- ggplot(data11, aes(factor(indicators), L2prop))
g.11L2prop <- g.11L2prop + stat_boxplot()
g.11L2prop <- g.11L2prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.11L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.11L2prop)
dev.off()

g.11CRPhi <- ggplot(data11, aes(factor(indicators), CRPhi))
g.11CRPhi <- g.11CRPhi + stat_boxplot()
g.11CRPhi <- g.11CRPhi + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPhi ")
png(filename = "E://Master's//Data Meeting Revisions//g.11CRPhi.png", width = 1000, height = 1000, units = 'px')
print(g.11CRPhi)
dev.off()

g.11CRPlo <- ggplot(data11, aes(factor(indicators), CRPlo))
g.11CRPlo <- g.11CRPlo + stat_boxplot()
g.11CRPlo <- g.11CRPlo + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPlo ")
png(filename = "E://Master's//Data Meeting Revisions//g.11CRPlo.png", width = 1000, height = 1000, units = 'px')
print(g.11CRPlo)
dev.off()

g.11gamma <- ggplot(data11, aes(factor(indicators), gamma))
g.11gamma <- g.11gamma + stat_boxplot()
g.11gamma <- g.11gamma + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 gamma ")
png(filename = "E://Master's//Data Meeting Revisions//g.11gamma.png", width = 1000, height = 1000, units = 'px')
print(g.11gamma)
dev.off()

g.11L1propse <- ggplot(data11, aes(factor(indicators), L1propse))
g.11L1propse <- g.11L1propse + stat_boxplot()
g.11L1propse <- g.11L1propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L1propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.11L1propse.png", width = 1000, height = 1000, units = 'px')
print(g.11L1propse)
dev.off()

g.11CRPhise <- ggplot(data11, aes(factor(indicators), CRPhise))
g.11CRPhise <- g.11CRPhise + stat_boxplot()
g.11CRPhise <- g.11CRPhise + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPhise ")
png(filename = "E://Master's//Data Meeting Revisions//g.11CRPhise.png", width = 1000, height = 1000, units = 'px')
print(g.11CRPhise)
dev.off()

g.11CRPlose <- ggplot(data11, aes(factor(indicators), CRPlose))
g.11CRPlose <- g.11CRPlose + stat_boxplot()
g.11CRPlose <- g.11CRPlose + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPlose ")
png(filename = "E://Master's//Data Meeting Revisions//g.11CRPlose.png", width = 1000, height = 1000, units = 'px')
print(g.11CRPlose)
dev.off()

g.11gammase <- ggplot(data11, aes(factor(indicators), gammase))
g.11gammase <- g.11gammase + stat_boxplot()
g.11gammase <- g.11gammase + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 gammase ")
png(filename = "E://Master's//Data Meeting Revisions//g.11gammase.png", width = 1000, height = 1000, units = 'px')
print(g.11gammase)
dev.off()

g.11L2propse <- ggplot(data11, aes(factor(indicators), L2propse))
g.11L2propse <- g.11L2propse + stat_boxplot()
g.11L2propse <- g.11L2propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.11L2propse.png", width = 1000, height = 1000, units = 'px')
print(g.11L2propse)
dev.off()

g.11L2prop <- ggplot(data11, aes(factor(indicators), L2prop))
g.11L2prop <- g.11L2prop + stat_boxplot()
g.11L2prop <- g.11L2prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.11L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.11L2prop)
dev.off()

g.11L3propse <- ggplot(data11, aes(factor(indicators), L3propse))
g.11L3propse <- g.11L3propse + stat_boxplot()
g.11L3propse <- g.11L3propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L3propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.11L3propse.png", width = 1000, height = 1000, units = 'px')
print(g.11L3propse)
dev.off()

data21 <- subset(mydataf, se=="MLR"& quality=="(high quality)")

g.21L1prop <- ggplot(data21, aes(factor(indicators), L1prop))
g.21L1prop <- g.21L1prop + stat_boxplot()
g.21L1prop <- g.21L1prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L1prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.21L1prop.png", width = 1000, height = 1000, units = 'px')
print(g.21L1prop)
dev.off()

g.21L2prop <- ggplot(data21, aes(factor(indicators), L2prop))
g.21L2prop <- g.21L2prop + stat_boxplot()
g.21L2prop <- g.21L2prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.21L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.21L2prop)
dev.off()

g.21CRPhi <- ggplot(data21, aes(factor(indicators), CRPhi))
g.21CRPhi <- g.21CRPhi + stat_boxplot()
g.21CRPhi <- g.21CRPhi + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPhi ")
png(filename = "E://Master's//Data Meeting Revisions//g.21CRPhi.png", width = 1000, height = 1000, units = 'px')
print(g.21CRPhi)
dev.off()

g.21CRPlo <- ggplot(data21, aes(factor(indicators), CRPlo))
g.21CRPlo <- g.21CRPlo + stat_boxplot()
g.21CRPlo <- g.21CRPlo + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPlo ")
png(filename = "E://Master's//Data Meeting Revisions//g.21CRPlo.png", width = 1000, height = 1000, units = 'px')
print(g.21CRPlo)
dev.off()

g.21gamma <- ggplot(data21, aes(factor(indicators), gamma))
g.21gamma <- g.21gamma + stat_boxplot()
g.21gamma <- g.21gamma + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 gamma ")
png(filename = "E://Master's//Data Meeting Revisions//g.21gamma.png", width = 1000, height = 1000, units = 'px')
print(g.21gamma)
dev.off()

g.21L1propse <- ggplot(data21, aes(factor(indicators), L1propse))
g.21L1propse <- g.21L1propse + stat_boxplot()
g.21L1propse <- g.21L1propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L1propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.21L1propse.png", width = 1000, height = 1000, units = 'px')
print(g.21L1propse)
dev.off()

g.21CRPhise <- ggplot(data21, aes(factor(indicators), CRPhise))
g.21CRPhise <- g.21CRPhise + stat_boxplot()
g.21CRPhise <- g.21CRPhise + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPhise ")
png(filename = "E://Master's//Data Meeting Revisions//g.21CRPhise.png", width = 1000, height = 1000, units = 'px')
print(g.21CRPhise)
dev.off()

g.21CRPlose <- ggplot(data21, aes(factor(indicators), CRPlose))
g.21CRPlose <- g.21CRPlose + stat_boxplot()
g.21CRPlose <- g.21CRPlose + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPlose ")
png(filename = "E://Master's//Data Meeting Revisions//g.21CRPlose.png", width = 1000, height = 1000, units = 'px')
print(g.21CRPlose)
dev.off()

g.21gammase <- ggplot(data21, aes(factor(indicators), gammase))
g.21gammase <- g.21gammase + stat_boxplot()
g.21gammase <- g.21gammase + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 gammase ")
png(filename = "E://Master's//Data Meeting Revisions//g.21gammase.png", width = 1000, height = 1000, units = 'px')
print(g.21gammase)
dev.off()

g.21L2propse <- ggplot(data21, aes(factor(indicators), L2propse))
g.21L2propse <- g.21L2propse + stat_boxplot()
g.21L2propse <- g.21L2propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.21L2propse.png", width = 1000, height = 1000, units = 'px')
print(g.21L2propse)
dev.off()

g.21L2prop <- ggplot(data21, aes(factor(indicators), L2prop))
g.21L2prop <- g.21L2prop + stat_boxplot()
g.21L2prop <- g.21L2prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.21L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.21L2prop)
dev.off()

g.21L3propse <- ggplot(data21, aes(factor(indicators), L3propse))
g.21L3propse <- g.21L3propse + stat_boxplot()
g.21L3propse <- g.21L3propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L3propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.21L3propse.png", width = 1000, height = 1000, units = 'px')
print(g.21L3propse)
dev.off()

data12 <- subset(mydataf, se=="ML"& quality=="(low quality)")

g.12L1prop <- ggplot(data12, aes(factor(indicators), L1prop))
g.12L1prop <- g.12L1prop + stat_boxplot()
g.12L1prop <- g.12L1prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L1prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.12L1prop.png", width = 1000, height = 1000, units = 'px')
print(g.12L1prop)
dev.off()

g.12L2prop <- ggplot(data12, aes(factor(indicators), L2prop))
g.12L2prop <- g.12L2prop + stat_boxplot()
g.12L2prop <- g.12L2prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.12L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.12L2prop)
dev.off()

g.12CRPhi <- ggplot(data12, aes(factor(indicators), CRPhi))
g.12CRPhi <- g.12CRPhi + stat_boxplot()
g.12CRPhi <- g.12CRPhi + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPhi ")
png(filename = "E://Master's//Data Meeting Revisions//g.12CRPhi.png", width = 1000, height = 1000, units = 'px')
print(g.12CRPhi)
dev.off()

g.12CRPlo <- ggplot(data12, aes(factor(indicators), CRPlo))
g.12CRPlo <- g.12CRPlo + stat_boxplot()
g.12CRPlo <- g.12CRPlo + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPlo ")
png(filename = "E://Master's//Data Meeting Revisions//g.12CRPlo.png", width = 1000, height = 1000, units = 'px')
print(g.12CRPlo)
dev.off()

g.12gamma <- ggplot(data12, aes(factor(indicators), gamma))
g.12gamma <- g.12gamma + stat_boxplot()
g.12gamma <- g.12gamma + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 gamma ")
png(filename = "E://Master's//Data Meeting Revisions//g.12gamma.png", width = 1000, height = 1000, units = 'px')
print(g.12gamma)
dev.off()

g.12L1propse <- ggplot(data12, aes(factor(indicators), L1propse))
g.12L1propse <- g.12L1propse + stat_boxplot()
g.12L1propse <- g.12L1propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L1propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.12L1propse.png", width = 1000, height = 1000, units = 'px')
print(g.12L1propse)
dev.off()

g.12CRPhise <- ggplot(data12, aes(factor(indicators), CRPhise))
g.12CRPhise <- g.12CRPhise + stat_boxplot()
g.12CRPhise <- g.12CRPhise + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPhise ")
png(filename = "E://Master's//Data Meeting Revisions//g.12CRPhise.png", width = 1000, height = 1000, units = 'px')
print(g.12CRPhise)
dev.off()

g.12CRPlose <- ggplot(data12, aes(factor(indicators), CRPlose))
g.12CRPlose <- g.12CRPlose + stat_boxplot()
g.12CRPlose <- g.12CRPlose + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPlose ")
png(filename = "E://Master's//Data Meeting Revisions//g.12CRPlose.png", width = 1000, height = 1000, units = 'px')
print(g.12CRPlose)
dev.off()

g.12gammase <- ggplot(data12, aes(factor(indicators), gammase))
g.12gammase <- g.12gammase + stat_boxplot()
g.12gammase <- g.12gammase + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 gammase ")
png(filename = "E://Master's//Data Meeting Revisions//g.12gammase.png", width = 1000, height = 1000, units = 'px')
print(g.12gammase)
dev.off()

g.12L2propse <- ggplot(data12, aes(factor(indicators), L2propse))
g.12L2propse <- g.12L2propse + stat_boxplot()
g.12L2propse <- g.12L2propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.12L2propse.png", width = 1000, height = 1000, units = 'px')
print(g.12L2propse)
dev.off()

g.12L2prop <- ggplot(data12, aes(factor(indicators), L2prop))
g.12L2prop <- g.12L2prop + stat_boxplot()
g.12L2prop <- g.12L2prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.12L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.12L2prop)
dev.off()

g.12L3propse <- ggplot(data12, aes(factor(indicators), L3propse))
g.12L3propse <- g.12L3propse + stat_boxplot()
g.12L3propse <- g.12L3propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L3propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.12L3propse.png", width = 1000, height = 1000, units = 'px')
print(g.12L3propse)
dev.off()

data22 <- subset(mydataf, se=="MLR"& quality=="(low quality)")

g.22L1prop <- ggplot(data22, aes(factor(indicators), L1prop))
g.22L1prop <- g.22L1prop + stat_boxplot()
g.22L1prop <- g.22L1prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L1prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.22L1prop.png", width = 1000, height = 1000, units = 'px')
print(g.22L1prop)
dev.off()

g.22L2prop <- ggplot(data22, aes(factor(indicators), L2prop))
g.22L2prop <- g.22L2prop + stat_boxplot()
g.22L2prop <- g.22L2prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.22L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.22L2prop)
dev.off()

g.22CRPhi <- ggplot(data22, aes(factor(indicators), CRPhi))
g.22CRPhi <- g.22CRPhi + stat_boxplot()
g.22CRPhi <- g.22CRPhi + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPhi ")
png(filename = "E://Master's//Data Meeting Revisions//g.22CRPhi.png", width = 1000, height = 1000, units = 'px')
print(g.22CRPhi)
dev.off()

g.22CRPlo <- ggplot(data22, aes(factor(indicators), CRPlo))
g.22CRPlo <- g.22CRPlo + stat_boxplot()
g.22CRPlo <- g.22CRPlo + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPlo ")
png(filename = "E://Master's//Data Meeting Revisions//g.22CRPlo.png", width = 1000, height = 1000, units = 'px')
print(g.22CRPlo)
dev.off()

g.22gamma <- ggplot(data22, aes(factor(indicators), gamma))
g.22gamma <- g.22gamma + stat_boxplot()
g.22gamma <- g.22gamma + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 gamma ")
png(filename = "E://Master's//Data Meeting Revisions//g.22gamma.png", width = 1000, height = 1000, units = 'px')
print(g.22gamma)
dev.off()

g.22L1propse <- ggplot(data22, aes(factor(indicators), L1propse))
g.22L1propse <- g.22L1propse + stat_boxplot()
g.22L1propse <- g.22L1propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L1propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.22L1propse.png", width = 1000, height = 1000, units = 'px')
print(g.22L1propse)
dev.off()

g.22CRPhise <- ggplot(data22, aes(factor(indicators), CRPhise))
g.22CRPhise <- g.22CRPhise + stat_boxplot()
g.22CRPhise <- g.22CRPhise + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPhise ")
png(filename = "E://Master's//Data Meeting Revisions//g.22CRPhise.png", width = 1000, height = 1000, units = 'px')
print(g.22CRPhise)
dev.off()

g.22CRPlose <- ggplot(data22, aes(factor(indicators), CRPlose))
g.22CRPlose <- g.22CRPlose + stat_boxplot()
g.22CRPlose <- g.22CRPlose + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPlose ")
png(filename = "E://Master's//Data Meeting Revisions//g.22CRPlose.png", width = 1000, height = 1000, units = 'px')
print(g.22CRPlose)
dev.off()

g.22gammase <- ggplot(data22, aes(factor(indicators), gammase))
g.22gammase <- g.22gammase + stat_boxplot()
g.22gammase <- g.22gammase + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 gammase ")
png(filename = "E://Master's//Data Meeting Revisions//g.22gammase.png", width = 1000, height = 1000, units = 'px')
print(g.22gammase)
dev.off()

g.22L2propse <- ggplot(data22, aes(factor(indicators), L2propse))
g.22L2propse <- g.22L2propse + stat_boxplot()
g.22L2propse <- g.22L2propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.22L2propse.png", width = 1000, height = 1000, units = 'px')
print(g.22L2propse)
dev.off()

g.22L2prop <- ggplot(data22, aes(factor(indicators), L2prop))
g.22L2prop <- g.22L2prop + stat_boxplot()
g.22L2prop <- g.22L2prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.22L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.22L2prop)
dev.off()

g.22L3propse <- ggplot(data22, aes(factor(indicators), L3propse))
g.22L3propse <- g.22L3propse + stat_boxplot()
g.22L3propse <- g.22L3propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L3propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.22L3propse.png", width = 1000, height = 1000, units = 'px')
print(g.22L3propse)
dev.off()

data13 <- subset(mydataf, se=="ML"& quality=="(mod quality)")

g.13L1prop <- ggplot(data13, aes(factor(indicators), L1prop))
g.13L1prop <- g.13L1prop + stat_boxplot()
g.13L1prop <- g.13L1prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L1prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.13L1prop.png", width = 1000, height = 1000, units = 'px')
print(g.13L1prop)
dev.off()

g.13L2prop <- ggplot(data13, aes(factor(indicators), L2prop))
g.13L2prop <- g.13L2prop + stat_boxplot()
g.13L2prop <- g.13L2prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.13L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.13L2prop)
dev.off()

g.13CRPhi <- ggplot(data13, aes(factor(indicators), CRPhi))
g.13CRPhi <- g.13CRPhi + stat_boxplot()
g.13CRPhi <- g.13CRPhi + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPhi ")
png(filename = "E://Master's//Data Meeting Revisions//g.13CRPhi.png", width = 1000, height = 1000, units = 'px')
print(g.13CRPhi)
dev.off()

g.13CRPlo <- ggplot(data13, aes(factor(indicators), CRPlo))
g.13CRPlo <- g.13CRPlo + stat_boxplot()
g.13CRPlo <- g.13CRPlo + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPlo ")
png(filename = "E://Master's//Data Meeting Revisions//g.13CRPlo.png", width = 1000, height = 1000, units = 'px')
print(g.13CRPlo)
dev.off()

g.13gamma <- ggplot(data13, aes(factor(indicators), gamma))
g.13gamma <- g.13gamma + stat_boxplot()
g.13gamma <- g.13gamma + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 gamma ")
png(filename = "E://Master's//Data Meeting Revisions//g.13gamma.png", width = 1000, height = 1000, units = 'px')
print(g.13gamma)
dev.off()

g.13L1propse <- ggplot(data13, aes(factor(indicators), L1propse))
g.13L1propse <- g.13L1propse + stat_boxplot()
g.13L1propse <- g.13L1propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L1propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.13L1propse.png", width = 1000, height = 1000, units = 'px')
print(g.13L1propse)
dev.off()

g.13CRPhise <- ggplot(data13, aes(factor(indicators), CRPhise))
g.13CRPhise <- g.13CRPhise + stat_boxplot()
g.13CRPhise <- g.13CRPhise + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPhise ")
png(filename = "E://Master's//Data Meeting Revisions//g.13CRPhise.png", width = 1000, height = 1000, units = 'px')
print(g.13CRPhise)
dev.off()

g.13CRPlose <- ggplot(data13, aes(factor(indicators), CRPlose))
g.13CRPlose <- g.13CRPlose + stat_boxplot()
g.13CRPlose <- g.13CRPlose + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 CRPlose ")
png(filename = "E://Master's//Data Meeting Revisions//g.13CRPlose.png", width = 1000, height = 1000, units = 'px')
print(g.13CRPlose)
dev.off()

g.13gammase <- ggplot(data13, aes(factor(indicators), gammase))
g.13gammase <- g.13gammase + stat_boxplot()
g.13gammase <- g.13gammase + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 gammase ")
png(filename = "E://Master's//Data Meeting Revisions//g.13gammase.png", width = 1000, height = 1000, units = 'px')
print(g.13gammase)
dev.off()

g.13L2propse <- ggplot(data13, aes(factor(indicators), L2propse))
g.13L2propse <- g.13L2propse + stat_boxplot()
g.13L2propse <- g.13L2propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.13L2propse.png", width = 1000, height = 1000, units = 'px')
print(g.13L2propse)
dev.off()

g.13L2prop <- ggplot(data13, aes(factor(indicators), L2prop))
g.13L2prop <- g.13L2prop + stat_boxplot()
g.13L2prop <- g.13L2prop + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.13L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.13L2prop)
dev.off()

g.13L3propse <- ggplot(data13, aes(factor(indicators), L3propse))
g.13L3propse <- g.13L3propse + stat_boxplot()
g.13L3propse <- g.13L3propse + facet_grid(cov2~observations) + opts(title =  " se = 1 quality = 1 L3propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.13L3propse.png", width = 1000, height = 1000, units = 'px')
print(g.13L3propse)
dev.off()

data23 <- subset(mydataf, se=="MLR"& quality=="(mod quality)")

g.23L1prop <- ggplot(data23, aes(factor(indicators), L1prop))
g.23L1prop <- g.23L1prop + stat_boxplot()
g.23L1prop <- g.23L1prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L1prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.23L1prop.png", width = 1000, height = 1000, units = 'px')
print(g.23L1prop)
dev.off()

g.23L2prop <- ggplot(data23, aes(factor(indicators), L2prop))
g.23L2prop <- g.23L2prop + stat_boxplot()
g.23L2prop <- g.23L2prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.23L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.23L2prop)
dev.off()

g.23CRPhi <- ggplot(data23, aes(factor(indicators), CRPhi))
g.23CRPhi <- g.23CRPhi + stat_boxplot()
g.23CRPhi <- g.23CRPhi + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPhi ")
png(filename = "E://Master's//Data Meeting Revisions//g.23CRPhi.png", width = 1000, height = 1000, units = 'px')
print(g.23CRPhi)
dev.off()

g.23CRPlo <- ggplot(data23, aes(factor(indicators), CRPlo))
g.23CRPlo <- g.23CRPlo + stat_boxplot()
g.23CRPlo <- g.23CRPlo + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPlo ")
png(filename = "E://Master's//Data Meeting Revisions//g.23CRPlo.png", width = 1000, height = 1000, units = 'px')
print(g.23CRPlo)
dev.off()

g.23gamma <- ggplot(data23, aes(factor(indicators), gamma))
g.23gamma <- g.23gamma + stat_boxplot()
g.23gamma <- g.23gamma + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 gamma ")
png(filename = "E://Master's//Data Meeting Revisions//g.23gamma.png", width = 1000, height = 1000, units = 'px')
print(g.23gamma)
dev.off()

g.23L1propse <- ggplot(data23, aes(factor(indicators), L1propse))
g.23L1propse <- g.23L1propse + stat_boxplot()
g.23L1propse <- g.23L1propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L1propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.23L1propse.png", width = 1000, height = 1000, units = 'px')
print(g.23L1propse)
dev.off()

g.23CRPhise <- ggplot(data23, aes(factor(indicators), CRPhise))
g.23CRPhise <- g.23CRPhise + stat_boxplot()
g.23CRPhise <- g.23CRPhise + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPhise ")
png(filename = "E://Master's//Data Meeting Revisions//g.23CRPhise.png", width = 1000, height = 1000, units = 'px')
print(g.23CRPhise)
dev.off()

g.23CRPlose <- ggplot(data23, aes(factor(indicators), CRPlose))
g.23CRPlose <- g.23CRPlose + stat_boxplot()
g.23CRPlose <- g.23CRPlose + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 CRPlose ")
png(filename = "E://Master's//Data Meeting Revisions//g.23CRPlose.png", width = 1000, height = 1000, units = 'px')
print(g.23CRPlose)
dev.off()

g.23gammase <- ggplot(data23, aes(factor(indicators), gammase))
g.23gammase <- g.23gammase + stat_boxplot()
g.23gammase <- g.23gammase + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 gammase ")
png(filename = "E://Master's//Data Meeting Revisions//g.23gammase.png", width = 1000, height = 1000, units = 'px')
print(g.23gammase)
dev.off()

g.23L2propse <- ggplot(data23, aes(factor(indicators), L2propse))
g.23L2propse <- g.23L2propse + stat_boxplot()
g.23L2propse <- g.23L2propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.23L2propse.png", width = 1000, height = 1000, units = 'px')
print(g.23L2propse)
dev.off()

g.23L2prop <- ggplot(data23, aes(factor(indicators), L2prop))
g.23L2prop <- g.23L2prop + stat_boxplot()
g.23L2prop <- g.23L2prop + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L2prop ")
png(filename = "E://Master's//Data Meeting Revisions//g.23L2prop.png", width = 1000, height = 1000, units = 'px')
print(g.23L2prop)
dev.off()

g.23L3propse <- ggplot(data23, aes(factor(indicators), L3propse))
g.23L3propse <- g.23L3propse + stat_boxplot()
g.23L3propse <- g.23L3propse + facet_grid(cov2~observations) + opts(title =  " se = 2 quality = 2 L3propse ")
png(filename = "E://Master's//Data Meeting Revisions//g.23L3propse.png", width = 1000, height = 1000, units = 'px')
print(g.23L3propse)
dev.off()

