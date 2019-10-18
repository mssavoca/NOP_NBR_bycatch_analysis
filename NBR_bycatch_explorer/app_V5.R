##### Defining global objects####
# source functions
source("2_load_libraries.R")
library(tidyverse)
library(plotly)
library(d3heatmap)
library(fields)
library(shinyBS)
library(markdown)
master=read.csv("data/All_bycatch_data_2010_2015.csv") %>% select(-c(CV,FOOTNOTE.S.,FISHERY.TYPE.GENERAL,FISHERY.TYPE.SPECIFIC)) %>% .[complete.cases(.[,c(6,9,10)]),] %>% mutate(NUM.FISH=rep(1,nrow(.)))
master_raw=read.csv("data/All_bycatch_data_2010_2015.csv") %>% select(-c(CV,FOOTNOTE.S.,FISHERY.TYPE.GENERAL,FISHERY.TYPE.SPECIFIC)) %>% .[complete.cases(.[,c(6,9,10)]),] %>% mutate(NUM.FISH=rep(1,nrow(.)))
master[master=="Pot"]<-"pot"
master[master=="NW"]<-"WC"
master[master=="SW"]<-"WC"

### code to split mammals by year ####
# a=master %>% filter(GROUP=="marine mammal") %>% filter(UNIT=="INDIVIDUAL")
# new=list()
# for(i in 1:nrow(a)){
#   print(i)
#   if(nchar(as.character(a$YEAR[i]))>4){
#     b=strsplit(as.character(a$YEAR[i]),"-")
#     c=lapply(b,function(x)paste0(x,"-01-01"))
#     d=interval(c[[1]][1],c[[1]][2])
#     e=time_length(d,unit="year")+1
#     bycatch=a$TOTAL.FISHERY.BYCATCH.MM[i]/e
#     f=a %>% slice(rep(i,each=e))
#     f$TOTAL.FISHERY.BYCATCH.MM=bycatch
#     f$YEAR=seq(b[[1]][1],b[[1]][2])
#     new[[length(new)+1]] <- f
#   }
# }
# 
# test=do.call("rbind",new)
# other=master%>% filter(GROUP=="marine mammal") %>% filter(UNIT=="INDIVIDUAL") %>% filter(nchar(as.character(YEAR))==4)
# final=rbind(test,other)
# write.csv(final,"data/mammals_by_year.csv",row.names = F)
#####
mammals=read.csv("data/mammals_by_year.csv")
rbi=read.csv("data/cleaned_stephs_code_1.csv")

group=unique(master$GROUP)%>% .[complete.cases(.)]
year=c(2010,2011,2012,2013,2014,2015)
region=as.factor(master$REGION) %>% unique()
fishery=unique(master$FISHERY)%>% .[complete.cases(.)] %>% as.character() %>% sort()
fishery=c("Don't filter",fishery)
species=unique(master$SCIENTIFIC.NAME)  %>% .[complete.cases(.)] %>% as.character()%>% sort()
species=c("Don't filter",species)
gear=unique(master$FISHERY.TYPE)%>% .[complete.cases(.)] %>% as.character()%>% sort()
gear=c("Don't filter",gear)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                     title = "National Bycatch Database Explorer",
                     titleWidth = 350
                    ),
                    dashboardSidebar(
                      width = 200,
                  sidebarMenu(id = 'sidebarmenu',
                              menuItem("Relative Bycatch Index",tabName = 'rbi',icon=icon("award")),
                              conditionalPanel("input.sidebarmenu==='rbi'",
                                               sliderInput("mmpa","Adjust MMPA weighting",min=1,max=5,step=1,value=2),
                                               # shinyBS::bsTooltip("mmpa", "The wait times will be broken into this many equally spaced bins",
                                               #           "right", options = list(container = "body"))
                                               bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small"),
                                               bsPopover(id = "q1", title = "",
                                                         content = "How should we explain MMPA and why we might adjust it?",
                                                         placement = "right", 
                                                         trigger = "hover", 
                                                         options = list(container = "body"))
                                               
                                               ),
                              menuItem("Visualize by species group", tabName='species',icon=icon("fish")),
                              conditionalPanel("input.sidebarmenu ==='species'",
                                               #checkboxInput("sp_region", "Subdivide by region",value=FALSE),
                                               radioButtons(inputId="choice_sp", label="How would you like to subdivide the data?", selected = "Don't subdivide",
                                                            choices=c("Region","Gear type", "Don't subdivide"))),
                                               #checkboxInput("sp_region", "Subdivide by fishing type",value=FALSE)),
                               menuItem("Visualize by gear type", tabName='fishing',icon=icon("ship",lib='font-awesome')),
                              conditionalPanel("input.sidebarmenu ==='fishing'",
                                               radioButtons(inputId="choice_gear", label="How would you like to subdivide the data?", selected = "Don't subdivide",
                                                            choices=c("Region", "Don't subdivide")),
                                               radioButtons(inputId="choice_metric", label="What metric would you like to see?", selected = "FISHERY.BYCATCH.RATIO",
                                                           choices=c("Bycatch ratio"="FISHERY.BYCATCH.RATIO",
                                                                     "Total catch"="TOTAL.CATCH",
                                                                     "Total landings"="TOTAL.FISHERY.LANDINGS",
                                                                     "Number of fisheries"="NUM.FISH"))),
                              menuItem("Explore raw data", tabName='raw',icon=icon("poo",lib='font-awesome')),
                              conditionalPanel("input.sidebarmenu ==='raw'",
                                               selectInput("raw_species","Filter species",species,width = "100%"),
                                               selectInput("raw_fishery","Filter fishery",fishery,width = "100%"),
                                               selectInput("raw_gear","Filter gear",gear,width = "100%"),
                                               div(style="text-align:center",downloadButton("downloadDataF", label = h6(style="color:black","Download dataset")))
                              ),
                              div(style="text-align:center",url <- a(tags$span(style="color:dodgerblue",h4("Read the paper")), href="https://media.giphy.com/media/qaoutfIYJYxr2/source.gif"))
                               )),
                    
   dashboardBody(
     tabItems(
       tabItem(tabName = "rbi",
              fluidRow(
                column(h4(style="text-align:center;","This app explores relative bycatch performance in US fisheries."),width = 12),
                column(h5(""),width=1,plotOutput("scale",height = '800px'),style = "background-color:white;"),
                column(h5(""),width=11,d3heatmapOutput("heatmap",height = '800px'),style = "background-color:white;",
                       absolutePanel(draggable=T,top = 0, left = 0, right = 0,tags$div(h2(style="text-align:center;color:red;padding:0px;border-radius: 0px; ",tags$b(tags$em("EXPLORATORY"))))))
                # absolutePanel(div(style="text-align:center;color:red;padding:0px;border-radius: 0px; ",tags$b(tags$em("placeholder"))),draggable=T,top=350, right=50)
                # absolutePanel(draggable=T,top = 0, left = 0, right = 0,div(style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",HTML(markdownToHTML(fragment.only=TRUE,text="placeholder"))))
                
              )),
       tabItem(tabName = "species",
        fluidRow(
            column(h4(style="text-align:center;","This app explores relative bycatch performance in US fisheries."),width = 12),
             column(h5("Fish and invertebrates"),width=4,plotOutput("Fish")),
             column(h5("Mammals"),width=4,plotOutput("Mammals")),
             column(h5("Seabirds and sea turtles"),width=4,plotOutput("SBST"))
        )),
       tabItem(tabName = "fishing",
               fluidRow(
                 column(h4(style="text-align:center;","This app explores relative bycatch performance in US fisheries."),width = 12),
                 column(h5(""),width=12,plotOutput("gear_ll",height = '800px')))
               ),
       tabItem(tabName = "raw",
               fluidRow(
                 column(h4(style="text-align:center;","This app explores relative bycatch performance in US fisheries."),width = 12),
                 column(h5(""),width=12,DT::dataTableOutput("rawTable"))
               ))
     ))

             
)
   


server <- shinyServer(function(input, output,session) {
  
  output$heatmap<-renderD3heatmap({
    a=rbi %>% mutate(mean_criteria = apply(.[,26:37],1,function(x) weighted.mean(x,w=c(rep(1,11),input$mmpa),na.rm=T))) #Here 'w' refers to the weights.
    
    q=a %>% select(Year,mean_criteria,Fishery_ShortName) %>% spread(Year,mean_criteria) %>% mutate(Fishery_ShortName=as.character(Fishery_ShortName)) %>% arrange(desc(Fishery_ShortName))
    rownames(q)=q$Fishery_ShortName
    q=q %>% .[,2:ncol(.)]
    
    d3heatmap(q, na.rm=T,Rowv = FALSE, Colv=FALSE, colors=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"),
              xlab=w,
              show_grid=F, yaxis_width=400,show_color_legend=T,na_color="white",row_side_palette=c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F")
              
    )
  })
  
  output$scale<-renderPlot({
    # par(mar=c(1,.1,.1,.1))
    a=rbi %>% mutate(mean_criteria = apply(.[,26:37],1,function(x) weighted.mean(x,w=c(rep(1,11),input$mmpa),na.rm=T))) #Here 'w' refers to the weights.
    col.pal <- colorRampPalette(c("#053061" ,"#2166AC", "#4393C3",  "#D1E5F0" , "#FDDBC7", "#F4A582" ,"#D6604D" ,"#B2182B","#B2182B","#67001F"))
    ncolors <- 100
    breaks <- seq(min(a$mean_criteria,na.rm = T),max(a$mean_criteria,na.rm = T),,ncolors+1)
    levs <- breaks[-1] - diff(breaks)/2
    # image(x=levs, y=1, z=as.matrix(levs), col=col.pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
    par(mar=c(.1,.1,.1,.1))
    image.plot(x=levs, y=1,smallplot= c(0,.2,.2,1), z=as.matrix(levs), legend.only = TRUE,col=col.pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n",axis.args = list(cex.axis = .6))
    
  })
  
  output$placeholder=renderText({
    "test"
  })

   output$Fish<-renderPlot({
    value=input$choice_sp
    
    if(value=="Don't subdivide"){
      a=master %>% filter(GROUP=="invertebrate"|GROUP=="fish") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% filter(UNIT=="POUND") %>% 
        group_by(YEAR,FISHERY) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.FISH.INVERT)) %>% group_by(YEAR) %>% summarise(newcol=sum(newcol))
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+ theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "black"))+ylab("Total bycatch (lbs)")+xlab("Year")
      }
    if(value=="Region"){
      a=master %>% filter(GROUP=="invertebrate"|GROUP=="fish") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% filter(UNIT=="POUND") %>% 
        group_by(YEAR,FISHERY,REGION) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.FISH.INVERT)) %>% group_by(YEAR,REGION) %>% summarise(newcol=sum(newcol))
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity", position = position_dodge())+ theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "black"))+ylab("Total bycatch (lbs)")+xlab("Year")+
        scale_fill_manual("",values=c("AK"="#7489ff","PI"="#c2c700","SE"="#00683b","WC"="#b45300","NE"="#afcf9d"),labels=c("Alaska","Pacific Islands","Southeast","Westcoast","Northeast"))
      }
    if(value=="Gear type"){
      a=master %>% filter(GROUP=="invertebrate"|GROUP=="fish") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% filter(UNIT=="POUND") %>% 
        group_by(YEAR,FISHERY,FISHERY.TYPE) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.FISH.INVERT)) %>% group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=sum(newcol))
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol,fill=FISHERY.TYPE),stat="identity", position = position_dodge())+ theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "black"))+ylab("Total bycatch (lbs)")+xlab("Year")+
              scale_fill_manual("",values=c("jig"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pot"="#59663e","seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
    }
    b
   })
   
   output$Mammals<-renderPlot({
     value=input$choice_sp

     if(value=="Don't subdivide"){
       a=mammals %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.MM)) %>% group_by(YEAR) %>% summarise(newcol=sum(newcol))
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+ theme_bw()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.border = element_rect(colour = "black"))+ylab("Total bycatch (individuals)")+xlab("Year")
     }
     if(value=="Region"){
       a=mammals %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY,REGION) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.MM)) %>% group_by(YEAR,REGION) %>% summarise(newcol=sum(newcol))
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity", position = position_dodge())+ theme_bw()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.border = element_rect(colour = "black"))+ylab("Total bycatch (individuals)")+xlab("Year")+
         scale_fill_manual("",values=c("AK"="#7489ff","PI"="#c2c700","SE"="#00683b","WC"="#b45300","NE"="#afcf9d"),labels=c("Alaska","Pacific Islands","Southeast","Westcoast","Northeast"))
     }
     if(value=="Gear type"){
       a=mammals %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY,FISHERY.TYPE) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.MM)) %>% group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=sum(newcol))
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol,fill=FISHERY.TYPE),stat="identity", position = position_dodge())+ theme_bw()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.border = element_rect(colour = "black"))+ylab("Total bycatch (individuals)")+xlab("Year")+
         scale_fill_manual("",values=c("jig"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pot"="#59663e","seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
     }
     b
   })
   
   output$SBST<-renderPlot({
     value=input$choice_sp
     
     if(value=="Don't subdivide"){
       a=master %>% filter(GROUP=="seabird"|GROUP=="sea turtle") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015)%>% filter(UNIT=="INDIVIDUAL") %>% 
          group_by(YEAR,FISHERY) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.SBST)) %>% group_by(YEAR) %>% summarise(newcol=sum(newcol))
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+ theme_bw()+
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Total bycatch (individuals)")+xlab("Year")
     }
     if(value=="Region"){
       a=master %>% filter(GROUP=="seabird"|GROUP=="sea turtle") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015)%>% filter(UNIT=="INDIVIDUAL") %>% 
         group_by(YEAR,FISHERY,REGION) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.SBST)) %>% group_by(YEAR,REGION) %>% summarise(newcol=sum(newcol))
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity", position = position_dodge())+ theme_bw()+
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Total bycatch (individuals)")+xlab("Year")+
         scale_fill_manual("",values=c("AK"="#7489ff","PI"="#c2c700","SE"="#00683b","WC"="#b45300","NE"="#afcf9d"),labels=c("Alaska","Pacific Islands","Southeast","Westcoast","Northeast"))
     }
     if(value=="Gear type"){
       a=master %>% filter(GROUP=="seabird"|GROUP=="sea turtle") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015)%>% filter(UNIT=="INDIVIDUAL") %>% 
         group_by(YEAR,FISHERY,FISHERY.TYPE) %>% summarise(newcol=mean(TOTAL.FISHERY.BYCATCH.SBST)) %>% group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=sum(newcol))
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol,fill=FISHERY.TYPE),stat="identity", position = position_dodge())+ theme_bw()+
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Total bycatch (individuals)")+xlab("Year")+
         scale_fill_manual("",values=c("jig"="#9f7bb2","dredge"="#7dac33","gillnet"="#c64f79","line"="#93ccaf","longline"="#8e97ee","pot"="#59663e","seine"="#ffca33","trawl"="#c5703f","troll"="#4d304b"))
     }
     b
   })
   
   metric=reactive({
     a=input$choice_metric
     b=grep(a,colnames(master),value=T)
     return(b)
   })


   output$gear_ll<-renderPlot({
     value=input$choice_gear
     
     if(metric()=="FISHERY.BYCATCH.RATIO"){
       a=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=mean(FISHERY.BYCATCH.RATIO,na.rm=T)) 

       aa=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,REGION) %>% summarise(newcol=mean(FISHERY.BYCATCH.RATIO,na.rm=T)) 

     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Bycatch ratio")+xlab("Year")
     }
     if(value=="Region"){
       b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity", position = position_dodge())+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Bycatch ratio")+xlab("Year")+
         scale_fill_manual("",values=c("AK"="#7489ff","PI"="#c2c700","SE"="#00683b","WC"="#b45300","NE"="#afcf9d"),labels=c("Alaska","Pacific Islands","Southeast","Westcoast","Northeast"))
     }
     }
     
     if(metric()=="TOTAL.FISHERY.LANDINGS"){
       a=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=sum(TOTAL.FISHERY.LANDINGS,na.rm=T))
       
       aa=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,REGION) %>% summarise(newcol=sum(TOTAL.FISHERY.LANDINGS,na.rm=T))
       
       if(value=="Don't subdivide"){
         b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total landings")+xlab("Year")
       }
       if(value=="Region"){
         b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity", position = position_dodge())+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total landings")+xlab("Year")+
           scale_fill_manual("",values=c("AK"="#7489ff","PI"="#c2c700","SE"="#00683b","WC"="#b45300","NE"="#afcf9d"))
       }
     }
     
     if(metric()=="TOTAL.CATCH"){
       a=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=sum(TOTAL.CATCH,na.rm=T))
       
       aa=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,REGION) %>% summarise(newcol=sum(TOTAL.CATCH,na.rm=T))
       
       if(value=="Don't subdivide"){
         b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total catch")+xlab("Year")
       }
       if(value=="Region"){
         b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity", position = position_dodge())+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total catch")+xlab("Year")+
           scale_fill_manual("",values=c("AK"="#7489ff","PI"="#c2c700","SE"="#00683b","WC"="#b45300","NE"="#afcf9d"))
       }
  
     }
     
     if(metric()=="NUM.FISH"){
       a=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,FISHERY) %>% summarise(newcol=n()) %>% distinct() %>% group_by(YEAR,FISHERY.TYPE)%>% summarise(newcol=n())
       
       aa=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,REGION,FISHERY) %>% summarise(newcol=n()) %>% distinct() %>% group_by(YEAR,FISHERY.TYPE,REGION)%>% summarise(newcol=n())
       
       if(value=="Don't subdivide"){
         b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Number of fisheries")+xlab("Year")
       }
       if(value=="Region"){
         b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity", position = position_dodge())+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Number of fisheries")+xlab("Year")+
           scale_fill_manual("",values=c("AK"="#7489ff","PI"="#c2c700","SE"="#00683b","WC"="#b45300","NE"="#afcf9d"))
       }
       
     }
     
     b
   })
   # }, height = function() {
   #   session$clientData$output_gear_ll_width
   #   })

   
   output$rawTable<-DT::renderDataTable({
     a=master %>% select(-c(TOTAL.FISHERY.BYCATCH.MM,TOTAL.FISHERY.BYCATCH.SBST,NUM.FISH,TOTAL.FISHERY.BYCATCH.FISH.INVERT,OBSERVER.COVERAGE,TOTAL.FISHERY.LANDINGS,TOTAL.CATCH))
     fish=input$raw_fishery
     sp=input$raw_species
     # 
     # if(input$raw_species=="Don't filter" & input$raw_fishery=="Don't filter"& input$raw_gear=="Don't filter"){
     #   b=a
     # } else if(input$raw_species=="Don't filter" & input$raw_fishery!="Don't filter"& input$raw_gear!="Don't filter"){
     #   b=a %>% filter(FISHERY==input$raw_fishery)
     # } else if(input$raw_species!="Don't filter" & input$raw_fishery=="Don't filter"& input$raw_gear!="Don't filter"){
     #   b=a %>% filter(SCIENTIFIC.NAME==input$raw_species)
     # } else if(input$raw_species!="Don't filter" & input$raw_fishery!="Don't filter"){
     #   b=a %>% filter(FISHERY==input$raw_fishery & SCIENTIFIC.NAME==input$raw_species)
     # }
     
     if(input$raw_species!="Don't filter"){
       a=a %>% filter(SCIENTIFIC.NAME==input$raw_species)
     }
     if(input$raw_fishery!="Don't filter"){
       a=a %>% filter(FISHERY==input$raw_fishery)
     }
     
     if(input$raw_gear!="Don't filter"){
       a=a %>% filter(FISHERY.TYPE==input$raw_gear)
     }
     
     datatable(a,options=list(scrollX=TRUE))
   })

   
   filtered_data=reactive({
     a=master %>% select(-c(TOTAL.FISHERY.BYCATCH.MM,TOTAL.FISHERY.BYCATCH.SBST,NUM.FISH,TOTAL.FISHERY.BYCATCH.FISH.INVERT,OBSERVER.COVERAGE,TOTAL.FISHERY.LANDINGS,TOTAL.CATCH))
     if(input$raw_species!="Don't filter"){
       a=a %>% filter(SCIENTIFIC.NAME==input$raw_species)
     }
     if(input$raw_fishery!="Don't filter"){
       a=a %>% filter(FISHERY==input$raw_fishery)
     }
     
     if(input$raw_gear!="Don't filter"){
       a=a %>% filter(FISHERY.TYPE==input$raw_gear)
     }
     return(a)
   })
   
    output$downloadDataF <- downloadHandler(
      filename = function() {
        paste("National_Bycatch_Database", ".csv", sep = "")
      },
      content = function(file) {
        
        write.csv(filtered_data(), file, row.names = FALSE)
      })

  
})

shinyApp(ui = ui, server = server)

