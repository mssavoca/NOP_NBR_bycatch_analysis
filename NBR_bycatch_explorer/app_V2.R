##### Defining global objects####
# source functions
source("2_load_libraries.R")
library(tidyverse)
library(plotly)
master=read.csv("data/All_bycatch_data_2010_2015.csv") %>% select(-c(CV,FOOTNOTE.S.,FISHERY.TYPE.GENERAL,FISHERY.TYPE.SPECIFIC)) %>% .[complete.cases(.[,c(6,9,10)]),] %>% mutate(NUM.FISH=seq(1:nrow(.)))
master[master=="Pot"]<-"pot"
master[master=="NW"]<-"WC"
master[master=="SW"]<-"WC"



ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                     title = "National Bycatch Database Explorer",
                     titleWidth = 200
                    ),
                    dashboardSidebar(
                      width = 200,
                  sidebarMenu(id = 'sidebarmenu',
                              menuItem("Visualize by species group", tabName='species',icon=icon("clock-o",lib='font-awesome')),
                              conditionalPanel("input.sidebarmenu ==='species'",
                                               #checkboxInput("sp_region", "Subdivide by region",value=FALSE),
                                               radioButtons(inputId="choice_sp", label="How would you like to subdivide the data?", selected = "Don't subdivide",
                                                            choices=c("Region","Fishery type", "Don't subdivide"))),
                                               #checkboxInput("sp_region", "Subdivide by fishing type",value=FALSE)),
                               menuItem("Visualize by fishing type", tabName='fishing',icon=icon("clock-o",lib='font-awesome')),
                              conditionalPanel("input.sidebarmenu ==='fishing'",
                                               radioButtons(inputId="choice_gear", label="How would you like to subdivide the data?", selected = "Don't subdivide",
                                                            choices=c("Region", "Don't subdivide")),
                                               radioButtons(inputId="choice_metric", label="What metric would you like to see?", selected = "FISHERY.BYCATCH.RATIO",
                                                           choices=c("Bycatch ratio"="FISHERY.BYCATCH.RATIO",
                                                                     "Total catch"="TOTAL.CATCH",
                                                                     "Total landings"="TOTAL.FISHERY.LANDINGS",
                                                                     "Number of fisheries"="NUM.FISH"))),
                              menuItem("Explore raw data", tabName='raw',icon=icon("clock-o",lib='font-awesome')),
                              div(style="text-align:center",downloadButton("downloadData", label = h6(style="color:black","Download dataset")))
                               )),
                    
   dashboardBody(
     tabItems(
       tabItem(tabName = "species",
        fluidRow(
             column(h5("Fish and invertebrates"),width=4,plotOutput("Fish")),
             column(h5("Mammals"),width=4,plotOutput("Mammals")),
             column(h5("Seabirds and sea turtles"),width=4,plotOutput("SBST"))
        )),
       tabItem(tabName = "fishing",
               fluidRow(
                 # column(h5("Catch:Bycatch Ratio"),width=4,plotOutput("gear_BR")),
                 # column(h5("Total Catch"),width=4,plotOutput("gear_TC")),
                 # column(h5("Total Landings"),width=4,plotOutput("gear_TL"))
                 column(h5(""),width=12,plotOutput("gear_ll")))
                 # column(h5("Trawl"),width=4,plotOutput("gear_tr")),
                 # column(h5("Pot"),width=4,plotOutput("gear_pt"))),
               # fluidRow(
               #   column(h5("Gillnet"),width=4,plotOutput("gear_gn")),
               #   column(h5("Jig"),width=4,plotOutput("gear_jg")),
               #   column(h5("Troll"),width=4,plotOutput("gear_trol"))),
               # fluidRow(
               #   column(h5("Line"),width=4,plotOutput("gear_ln")),
               #   column(h5("Dredge"),width=4,plotOutput("gear_dr")),
               #   column(h5("Seine"),width=4,plotOutput("gear_se")))
               ),
       tabItem(tabName = "raw",
               fluidRow(
                 column(h5(""),width=10,DT::dataTableOutput("rawTable"))
               ))
     ))

             
)
   


server <- shinyServer(function(input, output) {

   output$Fish<-renderPlot({
    a=master %>% filter(GROUP=="invertebrate" | GROUP=="fish") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015)
    value=input$choice_sp
    
    if(value=="Don't subdivide"){
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.FISH.INVERT),stat="identity")+ theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
      }
    if(value=="Region"){
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.FISH.INVERT,fill=REGION),stat="identity")+ theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
      }
    if(value=="Fishery type"){
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.FISH.INVERT,fill=FISHERY.TYPE),stat="identity")+ theme_bw()+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
    }
    b
   })
   
   output$Mammals<-renderPlot({
     a=master %>% filter(GROUP=="marine mammal") 
     value=input$choice_sp
     
     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.MM),stat="identity")+ theme_bw()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
     }
     if(value=="Region"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.MM,fill=REGION),stat="identity")+ theme_bw()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
     }
     if(value=="Fishery type"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.MM,fill=FISHERY.TYPE),stat="identity")+ theme_bw()+
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
     }
     b
   })
   
   output$SBST<-renderPlot({
     a=master %>% filter(GROUP=="seabird"|GROUP=="sea turtle") %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015)
     value=input$choice_sp
     
     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.SBST),stat="identity")+ theme_bw()+
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
     }
     if(value=="Region"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.SBST,fill=REGION),stat="identity")+ theme_bw()+
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
     }
     if(value=="Fishery type"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.SBST,fill=FISHERY.TYPE),stat="identity")+ theme_bw()+
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Total bycatch")+xlab("Year")
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
       b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               panel.border = element_rect(colour = "black"))+ylab("Bycatch ratio")+xlab("Year")
     }
     }
     
     if(metric()=="TOTAL.FISHERY.LANDINGS"){
       a=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=mean(TOTAL.FISHERY.LANDINGS,na.rm=T))
       
       aa=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,REGION) %>% summarise(newcol=mean(TOTAL.FISHERY.LANDINGS,na.rm=T))
       
       if(value=="Don't subdivide"){
         b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total landings")+xlab("Year")
       }
       if(value=="Region"){
         b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total landings")+xlab("Year")
       }
     }
     
     if(metric()=="TOTAL.CATCH"){
       a=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=mean(TOTAL.CATCH,na.rm=T))
       
       aa=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,REGION) %>% summarise(newcol=mean(TOTAL.CATCH,na.rm=T))
       
       if(value=="Don't subdivide"){
         b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total catch")+xlab("Year")
       }
       if(value=="Region"){
         b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Total catch")+xlab("Year")
       }
  
     }
     
     if(metric()=="NUM.FISH"){
       a=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE) %>% summarise(newcol=n())
       
       aa=master %>% filter(YEAR==2010|YEAR==2011|YEAR==2012|YEAR==2013|YEAR==2014|YEAR==2015) %>% 
         group_by(YEAR,FISHERY.TYPE,REGION) %>% summarise(newcol=n())
       
       if(value=="Don't subdivide"){
         b=ggplot(a) +geom_bar(aes(x=YEAR,y=newcol),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Number of fisheries")+xlab("Year")
       }
       if(value=="Region"){
         b=ggplot(aa) +geom_bar(aes(x=YEAR,y=newcol,fill=REGION),stat="identity")+facet_wrap(~FISHERY.TYPE)+ theme_bw() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "black"))+ylab("Number of fisheries")+xlab("Year")
       }
       
     }
     
     b
   })

   
   output$rawTable<-DT::renderDataTable({
     datatable(master)
   })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("National_Bycatch_Database", ".csv", sep = "")
      },
     content = function(file) {
        write.csv(master, file, row.names = FALSE)
      })

  
})

shinyApp(ui = ui, server = server)
