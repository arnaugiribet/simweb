
## app.R ##
library(data.table)
library(shiny)
library(DT)
library(modules)
library(shinydashboard)
library(bslib)
library(dplyr)
library(shinyWidgets)
library(shinyvalidate)
library(shinyjs)
#v2
library(shinycssloaders)
library(tableHTML)
library(fresh)

for(module in paste('modules',list.files('modules'),sep='/')) source(module)
source('funcions/funcionsECO.R')

my_theme = create_theme(
  adminlte_color(
    light_blue = "#BF0000"
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Simulador Web"),
  dashboardSidebar(
    #h3("Selecció d'Impost"),
    selectInput("versio", label = h3("Versió Simulador"),
                choices = list("Versió 1" = "v1",
                               "Versió 2" = "v2"),
                selected = 'v2')
  ),
  dashboardBody(
    useShinyjs(),
    use_theme(my_theme),
    
    uiOutput('ui')
  )
)

server <- function(input, output, session){
  
  observeEvent(input$versio,{
    if(input$versio == "v1"){
      output$ui <- renderUI({successionsV1ModuleUI("v1")})
    }
    if(input$versio == "v2"){
      output$ui <- renderUI({successionsV2ModuleUI("v2")})
    }
  })
  
  successionsV2ModuleServer('v1')
  successionsV2ModuleServer('v2')
  
}


shinyApp(ui, server)
