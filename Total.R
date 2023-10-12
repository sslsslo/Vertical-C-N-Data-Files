#Data importing of figure 1-5

data <- read.csv("~/Maindata.csv")
data.sur <- data[data$Layer=="Surface",]
data.mid <- data[data$Layer=="Middle",]
data.dee <- data[data$Layer=="Deep",]
data.md <- rbind(data.mid,data.dee)
data.cor1 <- data.sur[,-1:-5]
data.cor1 <- data.cor1[,-3]
data.cor2 <- data.md[,-1:-5]
data.cor2 <- data.cor2[,-3]

#Packages loading

library(ggplot2)
library(ggpubr)
library(ggpmisc)

#Plotting of figure 2

theme_set(ggpubr::theme_pubr()+
            theme(legend.position = "top"))

a<- data.sur %>% 
  ggplot(aes(x=Longitude, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(NULL) +
  ylab(label = expression("SOCD/kg m"^"-2")) +
  ggtitle('(a)')
a
b<- data.sur %>% 
  ggplot(aes(x=Latitude, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) + xlab(NULL) + ylab(NULL) +
  ggtitle('(b)')
b
c <- data.sur %>% 
  ggplot(aes(x=Altitude, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.93,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.95,  
               label.y = 0.90) + xlab(NULL) + ylab(NULL) +
  ggtitle('(c)')
c

#Plot patching

library(patchwork)
x <- a+b+c

#Plotting

d<- data.md %>% 
  ggplot(aes(x=Longitude, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(label = expression("Longitude/°")) +
  ylab(label = expression("SOCD/kg m"^"-2")) +
  ggtitle('(d)')
d
e<- data.md %>% 
  ggplot(aes(x=Latitude, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) + 
  xlab(label = expression("Latitude/°")) + ylab(NULL) +
  ggtitle('(e)')
e
f <- data.md %>% 
  ggplot(aes(x=Altitude, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.93,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.95, 
               label.y = 0.90) + 
  xlab(label = expression("Altitude/m")) + ylab(NULL) +
  ggtitle('(f)')
f

#Plot patching

y <- d+e+f

x/y

ggsave(
  filename = "SOCvsGeo.png", 
  width = 3750,             
  height = 1876,            
  units = "px",          
  dpi = 300              
)


#Plotting of figure 3
a<- data.sur %>% 
  ggplot(aes(x=Longitude, y=STND)) +
  geom_point(color='#803E33', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  ylab(label = expression("STND/kg m"^"-2")) +
  xlab(NULL)+
  ggtitle('(a)')
a
b<- data.sur %>% 
  ggplot(aes(x=Latitude, y=STND)) +
  geom_point(color='#803E33', size=1) +
  stat_poly_eq(use_label(c("p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  ylab(NULL) +
  xlab(NULL)+
  ggtitle('(b)')
b
c <- data.sur %>% 
  ggplot(aes(x=Altitude, y=STND)) +
  geom_point(color='#803E33', size=1) +
  stat_poly_eq(use_label(c("p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  ylab(NULL) +
  xlab(NULL)+
  ggtitle('(c)')
c

#Plot patching

library(patchwork)
x <- a+b+c

#Plotting

d<- data.md %>% 
  ggplot(aes(x=Longitude, y=STND)) +
  geom_point(color='#803E33', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(label = expression("Longitude/°")) +
  ylab(label = expression("STND/kg m"^"-2")) +
  ggtitle('(d)')
d
e<- data.md %>% 
  ggplot(aes(x=Latitude, y=STND)) +
  geom_point(color='#803E33', size=1) +
  stat_poly_eq(use_label(c("p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.90,  
               label.y = 0.90) + 
  xlab(label = expression("Latitude/°")) + ylab(NULL) +
  ggtitle('(e)')
e
f <- data.md %>% 
  ggplot(aes(x=Altitude, y=STND)) +
  geom_point(color='#803E33', size=1) +
  stat_poly_eq(use_label(c("p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05, 
               label.y = 0.90) + 
  xlab(label = expression("Altitude/m")) + ylab(NULL) +
  ggtitle('(f)')
f

#Plot patching

y <- d+e+f

x/y

ggsave(
  filename = "STNvsGeo.png", 
  width = 3750,             
  height = 1876,            
  units = "px",          
  dpi = 300              
)




#Plotting of figure 4

a<- data.sur %>% 
  ggplot(aes(x=MAP, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(NULL) +
  ylab(label = expression("SOCD/kg m"^"-2")) +
  ggtitle('(a)')
a
b<- data.sur %>% 
  ggplot(aes(x=MAT, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) + xlab(NULL) + ylab(NULL) +
  ggtitle('(b)')
b

x <- a+b

c<- data.md %>% 
  ggplot(aes(x=MAP, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(label = expression("MAP/mm")) +
  ylab(label = expression("SOCD/kg m"^"-2")) +
  ggtitle('(c)')
c
d<- data.md %>% 
  ggplot(aes(x=MAT, y=SOCD)) +
  geom_point(color='#341F16', size=1) +
  stat_poly_eq(use_label(c("p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(label = expression("MAT/°C")) +
  ylab(NULL) +
  ggtitle('(d)')
d

#Plot patching

y <- c+d

x/y

ggsave(
  filename = "SOCvsCli.png", 
  width = 2550,
  height = 1876,
  units = "px",
  dpi = 300
)

#Plotting figure 5

a<- data.sur %>% 
  ggplot(aes(x=MAP, y=STND)) +
  geom_point(color='#803E33', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(NULL) +
  ylab(label = expression("STND/kg m"^"-2")) +
  ggtitle('(a)')
a
b<- data.sur %>% 
  ggplot(aes(x=MAT, y=STND)) +
  geom_point(color='#803E33', size=1) +
  stat_poly_eq(use_label(c("p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(label = expression("MAT/°C")) +
  ylab(NULL) +
  ggtitle('(b)')
b

x <- a+b

c <- data.md %>% 
  ggplot(aes(x=MAP, y=STND)) +
  geom_point(color='#803E33', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(label = expression("MAP")) +
  ylab(label = expression("STND/kg m"^"-2")) +
  ggtitle('(c)')
c
d<- data.md %>% 
  ggplot(aes(x=MAT, y=STND)) +
  geom_point(color='#803E33', size=1) +
  geom_smooth(method = 'lm') +
  stat_poly_eq(use_label(c("eq")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.95) +
  stat_poly_eq(use_label(c("R2", "p.value.label")),
               formula = y ~ x,  parse = TRUE,
               size = 4, 
               label.x = 0.05,  
               label.y = 0.90) +
  xlab(label = expression("MAT/°C")) +
  ylab(NULL) +
  ggtitle('(d)')
d
#Plot patching

y <- c+d
x/y
ggsave(
  filename = "STNvsCli.png",
  width = 2550,
  height = 1876,
  units = "px",
  dpi = 300
)

#Plotting of figure 6

data.sur.scale <- scale(data.cor1)
data.md.scale <- scale(data.cor2)


#Correlation analysis of surface layer

library(ggcorrplot)
library(corrplot)
library(figpatch)
p1.mat <- cor_pmat(data.sur.scale)
cor_data1 <- cor(data.sur.scale , method="spearman", use = "pairwise")
png( 
  filename = "SurfaceCor.png",
  width = 2400,
  height = 1800,
  units = "px",
  bg = "white",
  res = 300)

col1=colorRampPalette(colors =c("#6D9EC1", "white", "#E46726"),space="Lab")
corrplot(cor_data1, type= "lower",col = col1(100),method = "color", diag = FALSE, 
              tl.col="black",tl.cex = 1.2,cl.pos = "b",cl.ratio = 0.2, 
              cl.cex = 1,p.mat = p1.mat, sig.level = c(.001, .01, .05), 
              outline="black",insig = "label_sig",pch.cex = 1.5, 
              pch.col = "black", tl.srt = 45,tl.offset=0.15, title = '(a)', mar=c(0,0,1,0))

garbage<-dev.off()

#Correlation analysis of middle and deep layers

png( 
  filename = "Middle&DeepCor.png",
  width = 2400,
  height = 1800,
  units = "px",
  bg = "white",
  res = 300)
p2.mat <- cor_pmat(data.md.scale)
cor_data2 <- cor(data.md.scale , method="spearman", use = "pairwise")
corrplot(cor_data2, type= "lower",col = col1(100),method = "color", diag = FALSE, 
              tl.col="black",tl.cex = 1.2,cl.pos = "b",cl.ratio = 0.2, 
              cl.cex = 1,p.mat = p2.mat, sig.level = c(.001, .01, .05), 
              outline="black",insig = "label_sig",pch.cex = 1.5, 
              pch.col = "black", tl.srt = 45,tl.offset=0.15, title = '(b)', mar=c(0,0,1,0))
garbage<-dev.off()

#Data importing of figure 7

data <- read.csv("~/Depth vs content.csv")

#Packages loading

library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(tidyverse)
library(patchwork)
library(ggpattern)

#Plotting

theme_set(ggpubr::theme_pubr()+
            theme(legend.position = "right"))

a<- data %>% 
  ggplot(aes(x=SOC, y=Soil.depth)) +
  geom_point(color='#341F16', size=1) +
  geom_smooth(method = "lm", formula = y ~ log(x),) +
  scale_x_continuous(position = "top") +
  annotate('text', label = 'SOC=49.1-8.75 ln(Soil depth)', x = 15, y = 240, size = 4) +
  annotate('text', label = "atop(italic(R)^2==0.95^'***')", x = 25, y = 280, size = 4, parse = TRUE) +
  xlab(label = expression("SOC/g kg"^"-1")) +
  ylab(label = expression("Soil depth/cm")) +
  geom_errorbar(data = data, mapping = aes(xmin= SOC-SESOC, xmax=SOC+SESOC),
                width = 4,
                color = 'black',
                size=0.5) +
  scale_y_reverse() +
  ggtitle('(a)')
a

b<- data %>% 
  ggplot(aes(x=STN, y=Soil.depth)) +
  geom_point(color='#803E33', size=1) +
  geom_smooth(method = "lm", formula = y ~ log(x),) +
  scale_x_continuous(position = "top") +
  annotate('text', label = 'STN=3.58-0.65 ln(Soil depth)', x = 1.5, y = 240, size = 4) +
  annotate('text', label = "atop(italic(R)^2==0.88^'***')", x = 1.5, y = 280, size = 4, parse = TRUE) +
  xlab(label = expression("STN/g kg"^"-1")) +
  ylab(NULL) +
  geom_errorbar(data = data, mapping = aes(xmin= STN-SESTN, xmax=STN+SESTN),
                width = 4,
                color = 'black',
                size=0.5) +
  scale_y_reverse() +
  ggtitle('(b)')
b
c<- data %>% 
  ggplot(aes(x=C.N, y=Soil.depth)) +
  geom_point(color='black', size=1) +
  scale_x_continuous(position = "top") +
  stat_poly_eq(use_label(c("p.value.label")),
               formula = y ~ log(x),parse = TRUE,
               size = 4, 
               label.x = 0.95,  
               label.y = 0.25) +
  xlab(label = expression("C:N")) +
  ylab(label = expression("Soil depth/cm")) +
  geom_errorbar(data = data, mapping = aes(xmin= C.N-SECN, xmax=C.N+SECN),
                width = 4,
                color = 'black',
                size=0.5) +
  scale_y_reverse() +
  ggtitle('(c)')
c
data <- read.csv("~/Depth vs density.csv")
d <- ggplot(data = data,aes(x=Layer, y=Mean, fill=Content.of)) +
  geom_bar(stat = "identity",position = "dodge",width = 0.5) +
  geom_errorbar(aes(ymax = Mean+SE, ymin = Mean-SE), position = position_dodge(0.5), width = 0.15) +
  scale_y_continuous(expand = c(0,0), position = "right") +
  scale_fill_manual(values=c("#341F16","grey")) +
  coord_flip() +
  labs(fill=NULL) +
  ylab(label = expression("Density/kg m"^"-2")) +
  ggtitle('(d)')
d
x <- a+b
y <- c+d
x/y

ggsave(
  filename = "VerticalDistribution.png",
  width = 2187,
  height = 2500,
  units = "px",
  dpi = 300
)

#Data importing of figure 8

data <- read.csv("~/Maindata.csv")

#Packages loading

library(randomForest)
library(rfPermute)
library(A3)

#Data processing

datas <- data[data$Layer=="Surface",]
data.mid <- data[data$Layer=="Middle",]
data.dee <- data[data$Layer=="Deep",]
data2 <- rbind(data.mid,data.dee)

set.seed(1234)
datat <- data[,-1:-5]
datat <- na.roughfix(datat)
SOC.rf <- randomForest(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = datat, importance = TRUE, ntree = 500)
set.seed(1234)
SOC.rfP <- rfPermute(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = datat,importance = TRUE, ntree = 500, num.cores = 4)
importance_SOC.scale <- data.frame(importance(SOC.rfP, scale = TRUE), check.names = FALSE)
importance_SOC.scale
importance_SOC.scale.pval <- (SOC.rfP$pval)[ , , 2]
importance_SOC.scale.pval
importance_SOC.scale <- importance_SOC.scale[order(importance_SOC.scale$'%IncMSE', decreasing = TRUE), ]
importance_SOC.scale
importance_SOC.scale$SOC_name <- rownames(importance_SOC.scale)
importance_SOC.scale$SOC_name <- factor(importance_SOC.scale$SOC_name, levels = importance_SOC.scale$SOC_name)
p1 <- ggplot() +
  geom_col(data = importance_SOC.scale, aes(x = SOC_name, y = `%IncMSE`), width = 0.5, fill = '#341F16', color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand=c(0,0), limit = c(0, 20))
p1
SOC.rf
p1 <- p1 +
  annotate('text', label = 'Total profile', x = 10.5, y = 19.5, size = 5) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 0.64), x = 11, y = 18, size = 4, parse = TRUE)
p1
for (SOC in rownames(importance_SOC.scale)) {
  importance_SOC.scale[SOC,'%IncMSE.pval'] <- importance_SOC.scale.pval[SOC,'%IncMSE']
  if (importance_SOC.scale[SOC,'%IncMSE.pval'] >= 0.05) importance_SOC.scale[SOC,'%IncMSE.sig'] <- ''
  else if (importance_SOC.scale[SOC,'%IncMSE.pval'] >= 0.01 & importance_SOC.scale[SOC,'%IncMSE.pval'] < 0.05) importance_SOC.scale[SOC,'%IncMSE.sig'] <- '*'
  else if (importance_SOC.scale[SOC,'%IncMSE.pval'] >= 0.001 & importance_SOC.scale[SOC,'%IncMSE.pval'] < 0.01) importance_SOC.scale[SOC,'%IncMSE.sig'] <- '**'
  else if (importance_SOC.scale[SOC,'%IncMSE.pval'] < 0.001) importance_SOC.scale[SOC,'%IncMSE.sig'] <- '***'
}
p1 <- p1 +
  geom_text(data = importance_SOC.scale, aes(x = SOC_name, y = `%IncMSE`, label = `%IncMSE.sig`), nudge_y = 0.1)
p1
set.seed(1234)
SOC_forest.pval <- #A3(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = data ,randomForest, p.acc = 0.001)
p1 <- p1 +
    annotate('text', label = sprintf('italic(P) < %.3f', 0.001), x = 11, y = 16.5, size = 4, parse = TRUE)+
    ggtitle('(a)')
p1

set.seed(1234)
datas <- datas[,-1:-5]
datas <- na.roughfix(datas)
SOCs.rf <- randomForest(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = datas, importance = TRUE, ntree = 500)
set.seed(1234)
SOCs.rfP <- rfPermute(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = datas,importance = TRUE, ntree = 500, num.cores = 4)
importance_SOCs.scale <- data.frame(importance(SOCs.rfP, scale = TRUE), check.names = FALSE)
importance_SOCs.scale
importance_SOCs.scale.pval <- (SOCs.rfP$pval)[ , , 2]
importance_SOCs.scale.pval
importance_SOCs.scale <- importance_SOCs.scale[order(importance_SOCs.scale$'%IncMSE', decreasing = TRUE), ]
importance_SOCs.scale
importance_SOCs.scale$SOCs_name <- rownames(importance_SOCs.scale)
importance_SOCs.scale$SOCs_name <- factor(importance_SOCs.scale$SOCs_name, levels = importance_SOCs.scale$SOCs_name)

#Plotting

p2 <- ggplot() +
  geom_col(data = importance_SOCs.scale, aes(x = SOCs_name, y = `%IncMSE`), width = 0.5, fill = '#341F16', color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand=c(0,0), limit = c(0, 25))
p2
SOCs.rf
p2 <- p2 +
  annotate('text', label = 'Surface layer', x = 9, y = 22.5, size = 5) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 0.77), x = 9, y = 21, size = 4, parse = TRUE)
p2

for (SOCs in rownames(importance_SOCs.scale)) {
  importance_SOCs.scale[SOCs,'%IncMSE.pval'] <- importance_SOCs.scale.pval[SOCs,'%IncMSE']
  if (importance_SOCs.scale[SOCs,'%IncMSE.pval'] >= 0.05) importance_SOCs.scale[SOCs,'%IncMSE.sig'] <- ''
  else if (importance_SOCs.scale[SOCs,'%IncMSE.pval'] >= 0.01 & importance_SOCs.scale[SOCs,'%IncMSE.pval'] < 0.05) importance_SOCs.scale[SOCs,'%IncMSE.sig'] <- '*'
  else if (importance_SOCs.scale[SOCs,'%IncMSE.pval'] >= 0.001 & importance_SOCs.scale[SOCs,'%IncMSE.pval'] < 0.01) importance_SOCs.scale[SOCs,'%IncMSE.sig'] <- '**'
  else if (importance_SOCs.scale[SOCs,'%IncMSE.pval'] < 0.001) importance_SOCs.scale[SOCs,'%IncMSE.sig'] <- '***'
}
p2 <- p2 +
  geom_text(data = importance_SOCs.scale, aes(x = SOCs_name, y = `%IncMSE`, label = `%IncMSE.sig`), nudge_y = 0.1)

p2
set.seed(1234)
data
SOCs_forest.pval <- a3(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = datas ,randomForest, p.acc = 0.001)

p2 <- p2 +
  annotate('text', label = sprintf('italic(P) < %.3f', 0.001), x = 9, y = 19.5, size = 4, parse = TRUE)+
  ggtitle('(b)')
p2

set.seed(1234)
data2 <- data2[,-1:-5]
data2 <- na.roughfix(data2)
SOCmd.rf <- randomForest(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = data2, importance = TRUE, ntree = 500)
set.seed(1234)
SOCmd.rfP <- rfPermute(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = data2,importance = TRUE, ntree = 500, num.cores = 4)
importance_SOCmd.scale <- data.frame(importance(SOCmd.rfP, scale = TRUE), check.names = FALSE)
importance_SOCmd.scale
importance_SOCmd.scale.pval <- (SOCmd.rfP$pval)[ , , 2]
importance_SOCmd.scale.pval
importance_SOCmd.scale <- importance_SOCmd.scale[order(importance_SOCmd.scale$'%IncMSE', decreasing = TRUE), ]
importance_SOCmd.scale
importance_SOCmd.scale$SOCmd_name <- rownames(importance_SOCmd.scale)
importance_SOCmd.scale$SOCmd_name <- factor(importance_SOCmd.scale$SOCmd_name, levels = importance_SOCmd.scale$SOCmd_name)
p3 <- ggplot() +
  geom_col(data = importance_SOCmd.scale, aes(x = SOCmd_name, y = `%IncMSE`), width = 0.5, fill = '#341F16', color = NA) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme(panel.grid = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand=c(0,0), limit = c(0, 25))
p3
SOCmd.rf
p3 <- p3 +
  annotate('text', label = 'Middle and deep layer', x = 9, y = 22.5, size = 5) +
  annotate('text', label = sprintf('italic(R^2) == %.2f', 0.69), x = 9, y = 21, size = 4, parse = TRUE)
p3
for (SOCmd in rownames(importance_SOCmd.scale)) {
  importance_SOCmd.scale[SOCmd,'%IncMSE.pval'] <- importance_SOCmd.scale.pval[SOCmd,'%IncMSE']
  if (importance_SOCmd.scale[SOCmd,'%IncMSE.pval'] >= 0.05) importance_SOCmd.scale[SOCmd,'%IncMSE.sig'] <- ''
  else if (importance_SOCmd.scale[SOCmd,'%IncMSE.pval'] >= 0.01 & importance_SOCmd.scale[SOCmd,'%IncMSE.pval'] < 0.05) importance_SOCmd.scale[SOCmd,'%IncMSE.sig'] <- '*'
  else if (importance_SOCmd.scale[SOCmd,'%IncMSE.pval'] >= 0.001 & importance_SOCmd.scale[SOCmd,'%IncMSE.pval'] < 0.01) importance_SOCmd.scale[SOCmd,'%IncMSE.sig'] <- '**'
  else if (importance_SOCmd.scale[SOCmd,'%IncMSE.pval'] < 0.001) importance_SOCmd.scale[SOCmd,'%IncMSE.sig'] <- '***'
}
p3 <- p3 +
  geom_text(data = importance_SOCmd.scale, aes(x = SOCmd_name, y = `%IncMSE`, label = `%IncMSE.sig`), nudge_y = 0.1)
p3

set.seed(1234)

SOCmd_forest.pval <- a3(SOCD ~ MAP + MAT + AI + pH +Sand + Silt + Clay + AGB + NPP + NDVI + Coverage + STND, data = data2 ,randomForest, p.acc = 0.001)
p3 <- p3 +
  annotate('text', label = sprintf('italic(P) < %.3f', 0.001), x = 9, y = 19.5, size = 4, parse = TRUE) +
  ggtitle('(c)')
p3

#Plot patching

library(patchwork)
p1/p2/p3
ggsave(
  filename = "IncMSE.png",
  width = 1600,
  height = 3600,
  units = "px",
  dpi = 300
)
#SEM model performing of surface layer 

data.pca <- data[,-1:-5]
data.class <- data[,1:5]
data.scale <- scale(data.pca)
data.climate <- data.scale[c(1:215), c(1,3)]
data.soil <- data.scale[c(1:215), c(4:7)]
data.vegetation <- data.scale[c(1:215), c(8:10)]
library("FactoMineR")
library("piecewiseSEM")
library("nlme")
res.pcacli <- PCA(data.climate, scale.unit = F, graph = T)
res.pcasoi <- PCA(data.soil, scale.unit = F, graph = T)
res.pcaveg <- PCA(data.vegetation, scale.unit = F, graph = T)
Fcli <- res.pcacli[["ind"]][["coord"]]
Fcli <- Fcli[,-2]
Fsoi <- res.pcasoi[["ind"]][["coord"]]
Fsoi <- Fsoi[,-2:-4]
Fveg <- res.pcaveg[["ind"]][["coord"]]
Fveg <- Fveg[,-2:-4]
Surfacedata.class <- data.class[c(1:215),]
Surfacedata.scale <- data.scale[c(1:215),]
FSurface.PCA<- cbind(Fcli, Fsoi, Fveg)
data.sem <- cbind(Surfacedata.class, Surfacedata.scale, FSurface.PCA)
data.sem <- as.data.frame(data.sem)
Surface.mod <- psem(
  lme(Fsoi ~ Fcli, random = ~ 1 | Researcher, data = data.sem),
  lme(Fveg ~ Fcli + Fsoi, random = ~ 1 | Researcher, data = data.sem),
  lme(SOCD ~ Fcli + Fveg + Fsoi, random = ~ 1 | Researcher, data = data.sem),
  lme(STND ~ Fcli + Fveg + Fsoi, random = ~ 1 | Researcher, data = data.sem),
  data = data.sem)

fisherC(Surface.mod)
summary(Surface.mod)

#SEM model performing of Middle and deep layers

data.climate2 <- data.scale[c(216:419), c(1,3)]
data.soil2 <- data.scale[c(216:419), c(4:7)]
data.vegetation2 <- data.scale[c(216:419), c(8:10)]
res.pcacli2 <- PCA(data.climate2, scale.unit = F, graph = T)
res.pcasoi2 <- PCA(data.soil2, scale.unit = F, graph = T)
res.pcaveg2 <- PCA(data.vegetation2, scale.unit = F, graph = T)
Fcli2 <- res.pcacli2[["ind"]][["coord"]]
Fcli2 <- Fcli2[,-2]
Fsoi2 <- res.pcasoi2[["ind"]][["coord"]]
Fsoi2 <- Fsoi2[,-2:-4]
Fveg2 <- res.pcaveg2[["ind"]][["coord"]]
Fveg2 <- Fveg2[,-2:-4]
MDdata.class <- data.class[c(216:419),]
MDdata.scale <- data.scale[c(216:419),]
FMD.PCA<- cbind(Fcli2, Fsoi2, Fveg2)
MDdata.sem <- cbind(MDdata.class, MDdata.scale, FMD.PCA)
MDdata.sem <- as.data.frame(MDdata.sem)
MD.mod <- psem(
  lme(Fsoi2 ~ Fcli2, random = ~ 1 | Researcher, data = MDdata.sem),
  lme(Fveg2 ~ Fcli2 + Fsoi2, random = ~ 1 | Researcher, data = MDdata.sem),
  lme(SOCD ~ Fcli2 + Fveg2, random = ~ 1 | Researcher, data = MDdata.sem),
  lme(STND ~ SOCD + Fcli2 + Fsoi2 + Fveg2, random = ~ 1 | Researcher,data = MDdata.sem),
  data = MDdata.sem)

fisherC(MD.mod)
summary(MD.mod)

