#This code is to analyse the US National bycatch report database
#Code and plots used for manuscript: Savoca et al., (submitted) Comprehensive assessment of bycatch in U.S. fisheries for use in prioritizing management.

#----REQUIRED LIBRARIES----
install.packages("lattice")
install.packages("tidyverse")
install.packages("scatterpie")
devtools::install_github("wmurphyrd/fiftystater")
install.packages("mapproj")
install.packages("ggjoy")
install.packages("ggpubr")
install.packages("png")

library(lattice)
library(tidyverse)
library(scatterpie)
library(mapproj)
library(fiftystater)
library(ggjoy)
library(ggpubr)
library(png)


#------LOAD DATA-------
#Data can be found on github, contains bycatch summary data for all fisheries and years
data <- read.csv('SummaryData_August2019_AllFisheryYears.csv', header=T) 




#New column titles:
colnames(data) <- c("Fishery_ShortName","Fishery","Region","GearType_specific","GearType_general","Year","Target","Target_HMS","Overfishing_Fm","Overfishing_Fm_numeric",
                    "Overfishing_Bt", "Overfishing_Bt_numeric","TotalBycatch_lbs", "TotalBycatch_inds","Bycatch_ratio",
                    "ESA_num","ESA_lbs","ESA_birdturt","IUCN_num","IUCN_lbs","IUCN_birdturt",
                    "MMPA","VessPers","FisheryYears")

#Normalise data betwen 0 and 1 
range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
data$NORM_Overfishing_Fm <- range01(data$Overfishing_Fm_numeric,na.rm=T)
data$NORM_Overfishing_Bt <- range01(data$Overfishing_Bt_numeric,na.rm=T)
data$NORM_TotalBycatch_lbs <- range01(data$TotalBycatch_lbs,na.rm=T)
data$NORM_TotalBycatch_inds <- range01(data$TotalBycatch_inds,na.rm=T)
data$NORM_Bycatch_ratio <- range01(data$Bycatch_ratio,na.rm=T)
data$NORM_ESA_num <- range01(data$ESA_num,na.rm=T)
data$NORM_ESA_lbs <- range01(data$ESA_lbs,na.rm=T)
data$NORM_ESA_birdturt <- range01(data$ESA_birdturt,na.rm=T)
data$NORM_IUCN_num <- range01(data$IUCN_num,na.rm=T)
data$NORM_IUCN_lbs <- range01(data$IUCN_lbs,na.rm=T)
data$NORM_IUCN_birdturt <- range01(data$IUCN_birdturt,na.rm=T)
data$NORM_MMPA <- range01(data$MMPA,na.rm=T)
data$NORM_MMPA_2 <- data$NORM_MMPA
colnames(data)[25:36] <- c("NORM_Overfishing_Fm",
                           "NORM_Overfishing_Bt", "NORM_TotalBycatch_lbs", "NORM_TotalBycatch_inds","NORM_Bycatch_ratio",
                           "NORM_ESA_num","NORM_ESA_lbs","NORM_ESA_birdturt","NORM_IUCN_num","NORM_IUCN_lbs","NORM_IUCN_birdturt","NORM_MMPA")

#-----RELATIVE BYCATCH INDEX------
#Calculate RBI for each fishery in each year
#Weighted mean, with MMPA category receiving double weighting
data$mean_criteria <- apply(data[,25:36],1,function(x) weighted.mean(x,w=c(rep(1,11),2),na.rm=T))

#List the fisheries with scores in the top 10% 
unique(data$Fishery_ShortName[data$mean_criteria>quantile(data$mean_criteria, probs = c(0.90), na.rm=T)[1]]) 


# Figure 1----

bycatch=read.csv('SummaryData_August2019_AllFisheryYears.csv') %>% 
  select(c(GearType_general,Region)) %>% group_by(GearType_general,Region) %>% 
  summarise(count=n()) %>% 
  spread(GearType_general,count) %>% 
  mutate(x=c(-117.5,-68,-105,-74,-130),y=c(19,36,19,23,45)) 
bycatch[is.na(bycatch)]<-0


crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
NE=c("maine","new hampshire","maryland","massachusetts","connecticut", "rhode island","new jersey", "new york", "west virginia","delaware","virginia")
SE=c("south carolina", "north carolina","georgia","florida","alabama","louisiana","texas","mississippi")
WC=c("washington","oregon","california")
AK=c("alaska")
PI="hawaii"

a=crimes %>% subset(.,state %in% NE) %>% mutate(Region="NE")
b=crimes %>% subset(.,state %in% SE) %>% mutate(Region="SE")
c=crimes %>% subset(.,state %in% WC) %>% mutate(Region="WC")
d=crimes %>% subset(.,state %in% AK) %>% mutate(Region="AK")
e=crimes %>% subset(.,state %in% PI) %>% mutate(Region="PI")

all=do.call("rbind",list(a,b,c,d,e))

master=left_join(crimes,all) 
master$Region[is.na(master$Region)]<-"Other"


p <- ggplot(master, aes(map_id = state)) + 
  geom_map(aes(fill=Region), map = fifty_states,show.legend = FALSE) +#scale_fill_manual("",values=c("AK"="#8da38e","NE"="#3c4d63","PI"="#4d543d","SE"="#557e83","WC"="#69494f","Other"="grey"),guide='none')+
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map()
p=p+  geom_scatterpie(aes(x=x,y=y,r = 4,),data=bycatch,cols=colnames(bycatch)[2:10],color=NA)+coord_fixed() +
  theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = NA,fill=NA),legend.key.size = unit(.5,'lines'))+
  scale_fill_manual(breaks=c("combined gears","dredge","gillnet","line","longline","pots and traps","purse seine","trawl","troll"),
                    values=c("#8da38e","#9f7bb2","#7dac33","#c64f79","#93ccaf","#8e97ee","#3c4d63","grey","#4d543d","#59663e","#ffca33","#557e83","#c5703f","#4d304b","#69494f"))+
  guides(fill=guide_legend(title="Gear types"))+theme(legend.position=c(.18,.5),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=6),legend.title = element_text(size=6))+
  annotate("text",x=-117.5,y=14,label="Alaska",size=2,color="#555555")+annotate("text",x=-68,y=31,label="Northeast",size=2,color="#555555")+
  annotate("text",x=-105,y=14,label="Pacific Islands",size=2,color="#555555")+annotate("text",x=-74,y=18,label="Southeast",size=2,color="#555555")+
  annotate("text",x=-130,y=40,label="West Coast",size=2,color="#555555")+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
#ggtitle("Percentage of fisheries in each gear type in the five bycatch reporting regions")+theme(plot.title = element_text(size=6))

p

png("Fig1.png",width=5, height=5, units="in", res=400)
par(ps=10)
par(mar=c(1,1,1,1))
par(cex=1)
p
dev.off()





# Figure 2 ----
# get silhouette images for figures
imgshark <- png::readPNG("./shark.png")
rastshark <- grid::rasterGrob(imgshark, interpolate = T)
imgcrab <- png::readPNG("./crab.png")
rastcrab <- grid::rasterGrob(imgcrab, interpolate = T)
imgjelly <- png::readPNG("./jelly.png")
rastjelly <- grid::rasterGrob(imgjelly, interpolate = T)
imgdolphin <- png::readPNG("./dolphin.png")
rastdolphin <- grid::rasterGrob(imgdolphin, interpolate = T)
imgpinniped <- png::readPNG("./pinniped.png")
rastpinniped <- grid::rasterGrob(imgpinniped, interpolate = T)
imgseaturtle <- png::readPNG("./seaturtle.png")
rastseaturtle <- grid::rasterGrob(imgseaturtle, interpolate = T)
imgfulmar <- png::readPNG("./fulmar.png")
rastfulmar <- grid::rasterGrob(imgfulmar, interpolate = T)
imgalbatross <- png::readPNG("./albatross.png")
rastalbatross <- grid::rasterGrob(imgalbatross, interpolate = T)

#read in data
d1 <- read_csv("SummaryData_September2019_AnalysisExport_AllFisheryYears_MMPAweighted.csv")
d1$Year <- as.factor(d1$Year)
d1$Region <- as.factor(d1$Region)


# identifying quantiles for break points
quantile(d1$Bycatch_ratio, probs = c(0.5, 0.75), na.rm = TRUE)
quantile(d1$TotalBycatch_inds, probs = c(0.5, 0.75), na.rm = TRUE)

#adding columms changing continuous values to discrete for figure
d1 <- d1 %>% mutate(
  BR_ratio_cat = cut(Bycatch_ratio, breaks=c(-Inf, 0.1480491, 0.3037714, Inf), 
                     labels=c("low (<0.15)","moderate (0.15-0.30)","high (>0.30)")),
  TotalBycatch_SBST_cat = cut(TotalBycatch_inds, breaks=c(-Inf, 0, 26.875, Inf),
                              labels=c("none","moderate (1-26)","high (>26)")),
  MMPA_cat = case_when(MMPA == 1 ~ "III",
                       MMPA == 2 ~ "II",
                       MMPA == 3 ~ "I"))

# defining the color palette
HW_palette <- c("#9f7bb2", "#7dac33","#c64f79","#93ccaf","#8e97ee", "#59663e","#ffca33", "#c5703f","#4d304b")

# bycatch ratio for fish and inverts (A)
BR_gear <- ggplot(filter(d1, BR_ratio_cat != "NA"), 
                  aes(BR_ratio_cat)) +
  geom_bar(aes(fill = GearType_general)) +
  ylab("Number of fisheries") +
  xlab("Bycatch ratio of fish and invertebrates") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=HW_palette) +
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12)) +
  annotation_custom(rastshark, ymin = 160, ymax = 190, xmin = 0, xmax = 5) +
  annotation_custom(rastcrab, ymin = 140, ymax = 160, xmin = 1) +
  annotation_custom(rastjelly, ymin = 140, ymax = 160, xmin = 2) 
BR_gear


#bycatch of seabirds and sea turtles (B)
B_indSBST_gear <- ggplot(filter(d1, TotalBycatch_SBST_cat != "NA"), 
                         aes(TotalBycatch_SBST_cat)) +
  geom_bar(aes(fill = GearType_general)) +
  ylab("Number of fisheries") +
  xlab("Total bycatch of seabirds and sea turtles") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=HW_palette) +
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12)) +
  annotation_custom(rastfulmar, ymin = 145, ymax = 170, xmin = 2) +
  annotation_custom(rastseaturtle, ymin = 170, ymax = 210, xmin = 1.7)
B_indSBST_gear 


#MMPA category ranking distribution (C)
MMPA_gear <- ggplot(filter(d1, MMPA_cat != "NA"),
                    aes(fct_relevel(MMPA_cat, "III", "II", "I"))) +
  geom_bar(aes(fill = GearType_general)) +
  ylab("Number of fisheries") +
  xlab("Marine Mammal Protection Act Category") +
  guides(fill=guide_legend(title="gear type")) +
  scale_fill_manual(values=HW_palette) +
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", size=12),
        axis.text.y  = element_text(size=12),
        axis.text.x = element_text(size=11),
        axis.title.y = element_text(face="bold",size=12),
        legend.text=element_text(size=10),
        strip.text.x = element_text(size = 12)) +
  annotation_custom(rastpinniped, ymin = 230, ymax = 270, xmin = 2) +
  annotation_custom(rastdolphin, ymin = 175, ymax = 215, xmin = 1.5)
MMPA_gear

Fig_2 <- ggarrange(BR_gear, B_indSBST_gear, MMPA_gear,
                   labels = c("A", "B", "C"), # THIS IS SO COOL!!
                   common.legend = TRUE, legend="top",
                   ncol = 3, nrow = 1)
Fig_2

dev.copy2pdf(file="Figure_2.pdf", width=14, height=8)




# Figure 3 ----

# Get 50 and 75% quantile breaks for all fishery-years scores
quantile(d1$mean_criteria, probs = c(0.5, 0.75), na.rm = TRUE)


# overall histogram of mean criteria score (A)
mean_score_hist <- ggplot(d1, aes(mean_criteria)) +
  geom_histogram(binwidth = 0.05, color="black", fill="gray80") +
  xlab("Relative Bycatch Index (RBI)") +
  ylim(-10,160) +
  geom_vline(xintercept = c(0.08032846, 0.15452661), color = "blue", linetype = "dashed") +
  theme_classic(base_size = 16)
mean_score_hist 

col.pal <- colorRampPalette(c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" ,"#F7F7F7", "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F"))
col.pal.2 <- col.pal(2)


#density plot by gear type (B)
dens_by_GT <- ggplot(d1, aes(mean_criteria, fct_reorder(GearType_general, mean_criteria, .desc = TRUE), 
                             fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.85,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0),
                               point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,
                               show.legend = FALSE) +
  ylab("Gear Type") +
  scale_fill_gradientn(colours = c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F"), 
                       name = NULL, limits = c(-0.1, 0.5)) +
  xlab("RBI") +
  theme_classic(base_size = 16)
dens_by_GT


#density plot by region (C)
dens_by_region <- ggplot(d1, aes(mean_criteria, fct_reorder(Region, mean_criteria, .desc = TRUE), 
                                 fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.85,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0),
                               point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,
                               show.legend = FALSE) +
  ylab("Region") +
  scale_fill_gradientn(colours = c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F"), 
                       name = NULL, limits = c(-0.1, 0.5)) +
  xlab("RBI") +
  theme_classic(base_size = 16)
dens_by_region + guides(size = FALSE)

col.pal <- colorRampPalette(c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" ,"#F7F7F7", "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F","#67001F"))

#density plot by year (D)
dens_by_year <- ggplot(d1, aes(mean_criteria, fct_relevel(Year, "2015", "2014", "2013", "2012", "2011", "2010"), 
                               fill = ..x..)) +
  geom_density_ridges_gradient(scale = 0.85,
                               jittered_points = TRUE,
                               position = position_points_jitter(width = 0.05, height = 0),
                               point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.3,
                               show.legend = FALSE) +
  ylab("Year") +
  scale_fill_gradientn(colours = c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F","#67001F"), 
                       name = NULL, limits = c(-0.1, 0.5)) +
  xlab("RBI") +
  theme_classic(base_size = 16)
dens_by_year


#combine plots into one figure
Fig_3 <- ggarrange(mean_score_hist,                                        # First row with scatter plot
                      ggarrange(dens_by_GT, dens_by_region, dens_by_year, 
                                ncol = 3, labels = c("B", "C", "D")), # Second row with box and dot plots
                      nrow = 2, labels = "A" )                    # Labels of the scatter plot
Fig_3

dev.copy2pdf(file="Figure_3.pdf", width=11, height=12)





# Figure 4----
#First select fisheries with 6 years of data
short <- as.data.frame(table(data$Fishery))
colnames(short) <- c("Fishery","freq")
d2 <- left_join(data[,c(1,2,6,38)],short,by="Fishery")
tiff('Figure4.tiff',res=300,units="in",width=11,height=14,bg="transparent")
levelplot(d2$mean_criteria[d2$freq==6]~d2$Year[d2$freq==6]*d2$Fishery_ShortName[d2$freq==6],ylab="",xlab="",main="",scales=list(cex=1, tck=c(1,0)), col.regions=col.pal,panel.abline(v=5))
dev.off()



#Figure S1 ----
quartz()
col.pal <- colorRampPalette(c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"))
tiff('FigureS1.tiff',res=400,units="in",width=9,height=16,bg="transparent")
levelplot(data$mean_criteria~data$Year*data$Fishery_ShortName, ylab="",xlab="",main="",scales=list(cex=1, tck=c(1,0)), col.regions=col.pal)
dev.off()



# Figure S2 ----
par(mfrow=c(3,4),mar=c(4,2,2,2))
hist(data$Overfishing_Fm_numeric,main="",ylab="Frequency",xlab="Species subject to overfishing",col="grey")
hist(data$Overfishing_Bt_numeric,main="",xlab="Species overfished",col="grey")
hist(data$TotalBycatch_inds,main="",xlab="Seabirds and sea turtles bycaught",col="grey")
hist(data$TotalBycatch_lbs,main="",xlab="Fish and invertebrates bycaught (lbs)",col="grey")
hist(data$Bycatch_ratio,main="",ylab="Frequency",xlab="Bycatch Ratio",col="grey")
hist(data$ESA_num,main="",xlab="ESA listed species bycaught",col="grey")
hist(data$ESA_lbs,main="",xlab="ESA listed fish and invertebrates bycaught (lbs)",col="grey")
hist(data$ESA_birdturt,main="",xlab="ESA listed seabirds and sea turtles bycaught",col="grey")
hist(data$IUCN_num,main="",xlab="IUCN listed species bycaught",col="grey")
hist(data$IUCN_lbs,main="",ylab="Frequency",xlab="IUCN listed fish and invertebrates bycaught (lbs)",col="grey")
hist(data$IUCN_birdturt,main="",xlab="IUCN listed seabirds and sea turtles bycaught",col="grey")
hist(data$MMPA,main="",xlab="MMPA Category Ranking (inverted)",col="grey")




# Sensitivity analyses, and Figure S3----
#Sensitivity analysis: find out the relative impact of each criteria on the RBI 
#This works by randomly changing the criteria value to be itself, or +/- 10%
#1000 iterations
sens_anal <- as.data.frame(matrix(NA,nrow=466000,ncol=13)) 
counter=1
for (i in 1:1000){
  c1 <- data[,25] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,25]
  c2 <- data[,26] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,26]
  c3 <- data[,27] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,27]
  c4 <- data[,28] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,28]
  c5 <- data[,29] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,29]
  c6 <- data[,30] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,30]
  c7 <- data[,31] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,31]
  c8 <- data[,32] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,32]
  c9 <- data[,33] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,33]
  c10 <- data[,34] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,34]
  c11 <- data[,35] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,35]
  c12 <- data[,36] + sample(c(1,-0.1,0.1),size=1,replace=TRUE)*data[,36]
  score <- rowMeans(cbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12),dims=1)
  
  c2 = counter+465
  sens_anal[counter:c2,1:12] <- cbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12)
  sens_anal[counter:c2,13] <- score
  counter= counter + 466
}
colnames(sens_anal) <- c("Overfishing_Fm",
                         "Overfishing_Bt", "TotalBycatch_lbs", "TotalBycatch_inds","Bycatch_ratio",
                         "ESA_num","ESA_lbs","ESA_birdturt","IUCN_num","IUCN_lbs","IUCN_birdturt","MMPA",'Mean_Score')
#Standardise output
sens_anal$Overfishing <- (sens_anal$Overfishing_Fm - ((max(sens_anal$Overfishing_Fm,na.rm=T) + min(sens_anal$Overfishing_Fm,na.rm=T)) / 2)) / ((max(sens_anal$Overfishing_Fm,na.rm=T) - min(sens_anal$Overfishing_Fm,na.rm=T)) / 2)
sens_anal$Overfished <- (sens_anal$Overfishing_Bt - ((max(sens_anal$Overfishing_Bt,na.rm=T) + min(sens_anal$Overfishing_Bt,na.rm=T)) / 2)) / ((max(sens_anal$Overfishing_Bt,na.rm=T) - min(sens_anal$Overfishing_Bt,na.rm=T)) / 2)
sens_anal$TotalBycatch_lbs <- (sens_anal$TotalBycatch_lbs - ((max(sens_anal$TotalBycatch_lbs,na.rm=T) + min(sens_anal$TotalBycatch_lbs,na.rm=T)) / 2)) / ((max(sens_anal$TotalBycatch_lbs,na.rm=T) - min(sens_anal$TotalBycatch_lbs,na.rm=T)) / 2)
sens_anal$TotalBycatch_inds <- (sens_anal$TotalBycatch_inds - ((max(sens_anal$TotalBycatch_inds,na.rm=T) + min(sens_anal$TotalBycatch_inds,na.rm=T)) / 2)) / ((max(sens_anal$TotalBycatch_inds,na.rm=T) - min(sens_anal$TotalBycatch_inds,na.rm=T)) / 2)
sens_anal$Bycatch_ratio <- (sens_anal$Bycatch_ratio - ((max(sens_anal$Bycatch_ratio,na.rm=T) + min(sens_anal$Bycatch_ratio,na.rm=T)) / 2)) / ((max(sens_anal$Bycatch_ratio,na.rm=T) - min(sens_anal$Bycatch_ratio,na.rm=T)) / 2)
sens_anal$ESA_species <- (sens_anal$ESA_num - ((max(sens_anal$ESA_num,na.rm=T) + min(sens_anal$ESA_num,na.rm=T)) / 2)) / ((max(sens_anal$ESA_num,na.rm=T) - min(sens_anal$ESA_num,na.rm=T)) / 2)
sens_anal$ESA_fish_lbs <- (sens_anal$ESA_lbs - ((max(sens_anal$ESA_lbs,na.rm=T) + min(sens_anal$ESA_lbs,na.rm=T)) / 2)) / ((max(sens_anal$ESA_lbs,na.rm=T) - min(sens_anal$ESA_lbs,na.rm=T)) / 2)
sens_anal$ESA_birdturtle <- (sens_anal$ESA_birdturt - ((max(sens_anal$ESA_birdturt,na.rm=T) + min(sens_anal$ESA_birdturt,na.rm=T)) / 2)) / ((max(sens_anal$ESA_birdturt,na.rm=T) - min(sens_anal$ESA_birdturt,na.rm=T)) / 2)
sens_anal$IUCN_species <- (sens_anal$IUCN_num - ((max(sens_anal$IUCN_num,na.rm=T) + min(sens_anal$IUCN_num,na.rm=T)) / 2)) / ((max(sens_anal$IUCN_num,na.rm=T) - min(sens_anal$IUCN_num,na.rm=T)) / 2)
sens_anal$IUCN_fish_lbs <- (sens_anal$IUCN_lbs - ((max(sens_anal$IUCN_lbs,na.rm=T) + min(sens_anal$IUCN_lbs,na.rm=T)) / 2)) / ((max(sens_anal$IUCN_lbs,na.rm=T) - min(sens_anal$IUCN_lbs,na.rm=T)) / 2)
sens_anal$IUCN_birdturtle <- (sens_anal$IUCN_birdturt - ((max(sens_anal$IUCN_birdturt,na.rm=T) + min(sens_anal$IUCN_birdturt,na.rm=T)) / 2)) / ((max(sens_anal$IUCN_birdturt,na.rm=T) - min(sens_anal$IUCN_birdturt,na.rm=T)) / 2)
sens_anal$MMPA <- (sens_anal$MMPA - ((max(sens_anal$MMPA,na.rm=T) + min(sens_anal$MMPA,na.rm=T)) / 2)) / ((max(sens_anal$MMPA,na.rm=T) - min(sens_anal$MMPA,na.rm=T)) / 2)

#Build linear model with output
sens_m1 <- lm(log(Mean_Score) ~ Overfishing + Overfished + TotalBycatch_lbs + TotalBycatch_inds 
              +Bycatch_ratio + ESA_species + ESA_fish_lbs + ESA_birdturtle + IUCN_species + IUCN_fish_lbs + IUCN_birdturtle + MMPA,
              data = sens_anal)
#Plot Figure S3: note that random permutations above may change results slightly
par(mar=c(8,4,2,2))
barplot(sqrt((sens_m1$coefficients[2:13])^2),las=2,ylab="Relative Impact",cex.axis=0.5)




