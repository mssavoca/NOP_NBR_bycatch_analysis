##### Defining global objects####
# source functions
source("2_load_libraries.R")
library(tidyverse)
library(plotly)
master=read.csv("data/All_bycatch_data_2010_2015.csv") %>% select(-c(CV,FOOTNOTE.S.,FISHERY.TYPE.GENERAL,FISHERY.TYPE.SPECIFIC))
# library(rpivotTable)
# 
# rpivotTable(master)
# if (!require("DT")) install.packages('DT')
# library(DT)
# datatable(master)
# 
# 
# ## idea 1. values by year TOTAL.FISHERY.BYCATCH.FISH.INVERT, TOTAL.FISHERY.LANDINGS, TOTAL.CATCH , FISHERY.BYCATCH.RATIO, TOTAL.FISHERY.BYCATCH.SBST, TOTAL.FISHERY.BYCATCH.MM
# ## perhaps subset by region, fishery type
# install.packages("dygraphs")
# library(dygraphs)
# test=master %>% select(c(YEAR,TOTAL.FISHERY.BYCATCH.FISH.INVERT,GROUP))%>% filter(GROUP=="fish") %>% group_by(YEAR) %>% summarise(catch=sum(TOTAL.FISHERY.BYCATCH.FISH.INVERT,na.rm=T))  %>% as.data.frame()%>% .[complete.cases(.),] %>% mutate(YEAR=as.Date(YEAR,format="%Y")) %>% mutate(YEAR=year(YEAR))
# dygraph(test)%>% 
#   dyRangeSelector(dateWindow = c("2009", "2011"))

## idea 2. different tabs for groups: mammal, sea turtle, sea bird, fish bycatch charts (or different charts on same tab)
# total group bycatch by year
# average fishery bycatch ratio by year
# by region

## idea 3. explore by fishing type
# total type bycatch by year

## fields that are interesting
# group
# year (reported differently for groups)
# region
# fishery type
# total.fishery bycatch fish invert (reported differently for groups)
# total fishery bycatch sbst (reported differently for groups)
# total fishery bycatch marine mammals (reported differently for groups)
# total bycatch ratio

## explore by species group tab
# three bar gaphs showing bycatch across time per group
# option to stack bar by region or fishery type

## explore by fishery type tab
# total bycatch ratio across time
# total catch across time
# total landings across time
## option to subset by region


#div(style="text-align:center",downloadButton("downloadData", label = h6(style="color:black","Download")))
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
                                                            choices=c("Region", "Don't subdivide"))),
                              menuItem("Explore raw data", tabName='raw',icon=icon("clock-o",lib='font-awesome'))
                               )),
                    
   dashboardBody(
     tabItems(
       tabItem(tabName = "species",
        fluidRow(
             column(h5("Fish and invertebrates"),width=4,plotOutput("Fish")),
             column(h5("Mammals"),width=4,plotOutput("Mammals")),
             column(h5("Seabirds and Sea turtles"),width=4,plotOutput("SBST"))
        )),
       tabItem(tabName = "fishing",
               fluidRow(
                 column(h5("Catch:Bycatch Ratio"),width=4,plotOutput("gear_BR")),
                 column(h5("Total Catch"),width=4,plotOutput("gear_TC")),
                 column(h5("Total Landings"),width=4,plotOutput("gear_TL"))
               )),
       tabItem(tabName = "raw",
               fluidRow(
                 column(h5(""),width=10,DT::dataTableOutput("rawTable"))
               ))
     ))

             
)
   


server <- shinyServer(function(input, output) {

   output$Fish<-renderPlot({
    a=master %>% filter(GROUP=="invertebrate" | GROUP=="fish") 
    value=input$choice_sp
    
    if(value=="Don't subdivide"){
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.FISH.INVERT),stat="identity")
      }
    if(value=="Region"){
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.FISH.INVERT,fill=REGION),stat="identity")
      }
    if(value=="Fishery type"){
      b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.FISH.INVERT,fill=FISHERY.TYPE),stat="identity")
    }
    b
   })
   
   output$Mammals<-renderPlot({
     a=master %>% filter(GROUP=="marine mammal") 
     value=input$choice_sp
     
     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.MM),stat="identity")
     }
     if(value=="Region"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.MM,fill=REGION),stat="identity")
     }
     if(value=="Fishery type"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.MM,fill=FISHERY.TYPE),stat="identity")
     }
     b
   })
   
   output$SBST<-renderPlot({
     a=master %>% filter(GROUP=="seabird"|GROUP=="sea turtle") 
     value=input$choice_sp
     
     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.SBST),stat="identity")
     }
     if(value=="Region"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.SBST,fill=REGION),stat="identity")
     }
     if(value=="Fishery type"){
       b=ggplot(a) +geom_bar(aes(x=YEAR,y=TOTAL.FISHERY.BYCATCH.SBST,fill=FISHERY.TYPE),stat="identity")
     }
     b
   })
   
   output$gear_BR<-renderPlot({
     a=master
     value=input$choice_gear
     
     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=FISHERY.TYPE,y=FISHERY.BYCATCH.RATIO),stat="identity")
     }
     if(value=="Region"){
       b=ggplot(a) +geom_bar(aes(x=FISHERY.TYPE,y=FISHERY.BYCATCH.RATIO,fill=REGION),stat="identity")
     }
     b
   })
   
   output$gear_TC<-renderPlot({
     a=master
     value=input$choice_gear
     
     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=FISHERY.TYPE,y=TOTAL.CATCH),stat="identity")
     }
     if(value=="Region"){
       b=ggplot(a) +geom_bar(aes(x=FISHERY.TYPE,y=TOTAL.CATCH,fill=REGION),stat="identity")
     }
     b
   })
   
   output$gear_TL<-renderPlot({
     a=master
     value=input$choice_gear
     
     if(value=="Don't subdivide"){
       b=ggplot(a) +geom_bar(aes(x=FISHERY.TYPE,y=TOTAL.FISHERY.LANDINGS),stat="identity")
     }
     if(value=="Region"){
       b=ggplot(a) +geom_bar(aes(x=FISHERY.TYPE,y=TOTAL.FISHERY.LANDINGS,fill=REGION),stat="identity")
     }
     b
   })
   
   output$rawTable<-DT::renderDataTable({
     datatable(master)
   })
 
  
})

shinyApp(ui = ui, server = server)
