library("shinydashboard")
library("shiny")
library("leaflet")
source("maps.R")
source("Plots.R")



ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "South Caucasus"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Population",
               radioButtons("btn",
                            label=NULL,
                            choices=list("Population"= "pop5" ,
                                         "Migration"= "mig" ,
                                         "Urban Population"="up"))),
      menuItem("GDP",
               radioButtons("btn2",
                            label=NULL,
                            choices=list("Nominal"= "nominal" ,
                                         "Per Capita"="pc",
                                         "GDP Change"="gdp"))),
      menuItem("Tourism",
               radioButtons("btn1",
                            label=NULL,
                            choices=list("Visitors"= "visitors" ,
                                         "Revenue"="revenue"))),
      menuItem("Sports",
               radioButtons("btn3",
                       label = NULL,
                       choices=list("Chess"= "chess" ,
                                     "Medals"="medals")
               ))  
    )),
  dashboardBody(color = "blue",
                fluidRow(tabBox(width = 12,
                                tabPanel("Overview",
                                      fluidRow(column(4,box(width =12,height = "455px",
                                                      status = "primary",
                                                leafletOutput("map1"))),
                                       column(8,box(width =12,height = "455px",
                                             status = "primary",
                                             plotlyOutput("dens1") 
                                               ))),
                              fluidRow(column(4,box(width =12,height = "455px",
                                                      status = "primary",
                                                    leafletOutput("map2"))),
                                         column(8,box(width =12,height = "455px",
                                                      status = "primary",
                                                      plotlyOutput("dens2") 
                                         ))),
                             fluidRow(column(4,box(width =12,height = "455px",
                                                   status = "primary",
                                                   leafletOutput("map3"))),
                                      column(8,box(width =12,height = "455px",
                                                   status = "primary",
                                                   plotlyOutput("dens3") 
                                      )))
                             ),
                
                             tabPanel("Comparison",
                                     tabBox(width = 12,
                                            tabPanel("Population",
                                fluidRow(column(1),
                                   column(10,box(width =12,height = "455px",
                                             status = "primary",
                                             plotlyOutput("output1")
                                             )),
                                           column(1))),
                                tabPanel("GDP",
                                  fluidRow(column(1),
                                           column(10,box(width =12,height = "455px",
                                                         status = "primary",
                                                         plotOutput("output2")
                                           )),
                                           column(1))),
                                tabPanel("Military Budget",
                                  fluidRow(column(1),
                                           column(10,box(width =12,height = "455px",
                                                         status = "primary",
                                                         plotlyOutput("output3")
                                           )),
                                           column(1))),
                                tabPanel("Crime",
                                  fluidRow(column(1),
                                           column(10,box(width =12,height = "455px",
                                                         status = "primary",
                                                         plotlyOutput("output4")
                                           )),
                                           column(1))),
                                tabPanel("Tourism",
                                  fluidRow(column(1),
                                           column(10,box(width =12,height = "455px",
                                                         status = "primary",
                                                         plotlyOutput("output5")
                                           )),
                                           column(1))),
                                tabPanel("Sports",
                                  fluidRow(column(1),
                                           column(10,box(width =12,height = "455px",
                                                         status = "primary",
                                                         plotlyOutput("output6")
                                           )),
                                           column(1))))
                                )
                )
  ),
  tags$head(
    tags$style(HTML("
          .content-wrapper {
            background-color: #2F4858 !important;
          }

        "))
  )
)
)


server <- function(input,output){
  output$output1 <- renderPlotly({
    if (input$btn == "mig"){
      migration_plot
    } else if (input$btn =="up"){
      urban_plot
    }else if (input$btn =="pop5"){
      population_plot
    }
  })
  output$output2 <- renderPlot({
    if (input$btn2 == "nominal"){
      nominal_gdp
    } else if (input$btn2 =="gdp"){
      change_plot
    } else if(input$btn2 =="pc"){
      pc_plot
    }
  })
  output$output3 <- renderPlotly(military_plot)
  output$output4 <- renderPlotly(crime_plot)
  output$output5 <- renderPlotly({
    if (input$btn1 == "visitors"){
      tourism_plot
    } else if (input$btn1 =="revenue"){
      revenue_plot
    }
  })
  output$output6 <- renderPlotly({
    if (input$btn3 == "chess"){
      chess_plot
    }  else if(input$btn3 == "medals"){
      medal_plot
    }
    })
  output$map1 <- renderLeaflet(armenia_map)
  output$map2 <- renderLeaflet(georgia_map)
  output$map3 <- renderLeaflet(azer_map)
  output$dens1 <- renderLeaflet(am_density)
  output$dens2 <- renderLeaflet(ge_density)
  output$dens3 <- renderLeaflet(az_density)
} 

 

shinyApp(ui = ui, server = server)





