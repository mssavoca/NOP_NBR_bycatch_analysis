#Analysis for  M. Savoca: bycatch database analysis
#Simple code for Heather to edit for RShiny

#------Load in data-------
data <- read.csv('data/SummaryData_August2019_AllFisheryYears.csv', header=T)

# The names are long so renaming here.
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
colnames(data)[25:36] <- c("NORM_Overfishing_Fm",
                           "NORM_Overfishing_Bt", "NORM_TotalBycatch_lbs", "NORM_TotalBycatch_inds","NORM_Bycatch_ratio",
                           "NORM_ESA_num","NORM_ESA_lbs","NORM_ESA_birdturt","NORM_IUCN_num","NORM_IUCN_lbs","NORM_IUCN_birdturt","NORM_MMPA")

#-----Fisheries Scores------
#Get average across criteria for each fishery year
data$mean_criteria <- apply(data[,25:36],1,function(x) weighted.mean(x,w=c(rep(1,11),2),na.rm=T)) #Here 'w' refers to the weights.
write.csv(data,"data/cleaned_stephs_code_2.csv")

#----Score Plot----
#All Fisheries, All Years:
col.pal <- colorRampPalette(c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" ,"#F7F7F7", "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"))
levelplot(data$mean_criteria~data$Year*rev(data$Fishery_ShortName),ylab="",xlab="",main="",scales=list(cex=1, tck=c(1,0)), col.regions=col.pal)

q=data %>% select(Year,mean_criteria,Fishery_ShortName) %>% spread(Year,mean_criteria) %>% mutate(Fishery_ShortName=as.character(Fishery_ShortName)) %>% arrange(desc(Fishery_ShortName))
rownames(q)=q$Fishery_ShortName
q=q %>% .[,2:ncol(.)]


col.pal <- colorRampPalette(c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"))

par(mar = c(12, 12, 12, 12))
library(d3heatmap)


par(ps=10) #settings before layout
layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=7)

par(cex=1) # layout has the tendency change par()$cex, so this step is important for control

par(mar=c(4,4,1,1)) # I usually set my margins before each plot

d3heatmap(q, na.rm=T,Rowv = FALSE, Colv=FALSE, colors=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"),
          show_grid=F, yaxis_width=300,show_color_legend=T,na_color="white",row_side_palette=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F")

)

box()
par(mar=c(4,4,0,1))
image(x=levs, y=1, z=as.matrix(levs), col=col.pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")

 box()
 
 
 
 
 test=par(mfrow=c(1,2))
 d3heatmap(q, na.rm=T,Rowv = FALSE, Colv=FALSE, colors=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"),
           show_grid=F, yaxis_width=300,show_color_legend=T,na_color="white",row_side_palette=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F")
           
 )
 image(x=levs, y=1, z=as.matrix(levs), col=col.pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
 par(test)

# par(mar=c(4,4,0,1)) # I usually set my margins before each plot

ncolors <- 100
breaks <- seq(min(data$mean_criteria),max(data$mean_criteria),,ncolors+1)
levs <- breaks[-1] - diff(breaks)/2
image(x=levs, y=1, z=as.matrix(levs), col=col.pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
# box()








d3heatmap(q, na.rm=T,Rowv = FALSE, Colv=FALSE, colors=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"),
          show_grid=F,yaxis_width=300,na_color="white",row_side_palette=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"),
          show_color_legend=T
          
)

# library(heatmaply)

#---END----

