
library(ggplot2)
# Additional trait stuff -- run analysis script first.

# Plot traits by sp x site

aggcol <- c("lday", "bday","wd","sla","X.N","Pore.anatomy")

dxt$site <- factor(dxt$site, labels = c("HF","SH"))

dxt.agg <- aggregate(dxt[aggcol], by = list(dxt$sp,dxt$site,dxt$fg), FUN = mean, na.rm=T)

dxt.agg2 <- aggregate(dxt[aggcol], by = list(dxt$site,dxt$fg), FUN = mean, na.rm=T)

names(dxt.agg)[1:3] = c("sp","site","fg")
names(dxt.agg2)[1:2] = c("site","fg")

pdf("Trait Plots.pdf", width = 20, height = 3)


ggplot(dxt.agg, aes(reorder(sp, wd), wd)) + geom_point() + theme_bw() 
ggplot(dxt.agg, aes(reorder(sp, sla), sla)) + geom_point() + theme_bw() 
ggplot(dxt.agg, aes(reorder(sp, X.N), X.N)) + geom_point() + theme_bw() 
ggplot(dxt.agg, aes(reorder(sp, Pore.anatomy), Pore.anatomy)) + geom_point() + theme_bw() 

# ggplot(dxt.agg, aes(site, wd)) + geom_point() + facet_grid(.~sp) + theme_bw() 
# ggplot(dxt.agg, aes(site, sla)) + geom_point() + facet_grid(.~sp)+ theme_bw() + xlim(c(0, 3)) 
# ggplot(dxt.agg, aes(site, X.N)) + geom_point() + facet_grid(.~sp)+ theme_bw()  + xlim(c(0, 3))
# ggplot(dxt.agg, aes(site, Pore.anatomy)) + geom_point() + facet_grid(.~sp)+ theme_bw()  + xlim(c(0, 3))

ggplot(dxt.agg2, aes(site, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()
ggplot(dxt.agg2, aes(site, sla)) + geom_point() + facet_grid(.~fg)+ theme_bw()
ggplot(dxt.agg2, aes(site, X.N)) + geom_point() + facet_grid(.~fg)+ theme_bw()
ggplot(dxt.agg2, aes(site, Pore.anatomy)) + geom_point() + facet_grid(.~fg)+ theme_bw()

# ggplot(dxt.agg2, aes(site, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()  + xlim(c(0, 3))
# ggplot(dxt.agg2, aes(site, sla)) + geom_point() + facet_grid(.~fg)+ theme_bw()  + xlim(c(0, 3))
# ggplot(dxt.agg2, aes(site, X.N)) + geom_point() + facet_grid(.~fg)+ theme_bw()  + xlim(c(0, 3))
# ggplot(dxt.agg2, aes(site, Pore.anatomy)) + geom_point() + facet_grid(.~fg)+ theme_bw()  + xlim(c(0, 3))

dev.off();system("open 'Trait Plots.pdf' -a/Applications/Preview.app")


# Analyze effect sizes vs traits
dlo <- summary(doym.l)$summary

dxt.agg <- dxt.agg[order(dxt.agg$site, dxt.agg$sp),]

warmeff <- dlo[grep("b_warm\\[",rownames(dlo)),"mean"]
photeff <- dlo[grep("b_photo\\[",rownames(dlo)),"mean"]
chileff <- dlo[grep("b_chill\\[",rownames(dlo)),"mean"]

effs <- data.frame(sp = levels(dxt$sp), warmeff, photeff, chileff)

dxt2 <- merge(dxt.agg, effs, by = "sp", keep.x = T)


pdf("Trait effect plots.pdf", width = 8, height = 5)

ggplot(dxt2, aes(warmeff, sla)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")

ggplot(dxt2, aes(warmeff, X.N)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")

ggplot(dxt2, aes(warmeff, Pore.anatomy)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")



ggplot(dxt2, aes(photeff, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")

ggplot(dxt2, aes(photeff, sla)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")

ggplot(dxt2, aes(photeff, X.N)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")

ggplot(dxt2, aes(photeff, Pore.anatomy)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")


ggplot(dxt2, aes(lday, wd)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")


ggplot(dxt2, aes(lday, sla)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")

ggplot(dxt2, aes(lday, X.N)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")

ggplot(dxt2, aes(lday, Pore.anatomy)) + geom_point() + facet_grid(.~fg) + theme_bw()   + geom_smooth(method="lm")


dev.off(); system("open 'Trait effect plots.pdf' -a/Applications/Preview.app")


