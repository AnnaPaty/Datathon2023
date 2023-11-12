library(shiny)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(dygraphs)
library(datamods)

dashboardPage(
  skin = "blue", 
  dashboardHeader(title = "Time Models"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("codeProd", "Choose a product:",
                  choices = c("BOMBA DE UN SOLO USO / TERAPIA PRESI\xd3N NEGATIVA-40",
                              "AP\xd3SITO DE ESPUMA DE POLIURETANO Y POLIACRILATO CON SILICONA-14",
                              "EQ. DE PRESION NEGATIVA CON INSTILACI\xd3N-33",
                              "AP\xd3SITO TRANSPARENTE ADHESIVO-24",
                              "AP\xd3SITO DE ESPUMA DE POLIURETANO Y POLIACRILATO CON SILICONA-13" ,
                              "BOMBA DE UN SOLO USO / TERAPIA PRESI\xd3N NEGATIVA C/ APOSITO-28",
                              "APOSITO ABSORBENTE S/ ADHESIVO-25",
                              "APOSITO C/ CARBON Y PLATA-4",
                              "APOSITO C/ CARBON Y PLATA-6",
                              "APOSITO DE ALGINATO C/ PLATA-6",                                                   
                              "APOSITO DE ESPUMA POLIURETANO-8",                                                  
                              "EQ. DE PRESI\xd3N NEGATIVA ABDOMINAL-29",                                          
                              "APOSITO DE ESPUMA POLIURETANO / SACRO-11",                                         
                              "APOSITO DE FIBRAS DE POLIACRILATO-3",                                              
                              "APOSITO DE FIBRAS DE POLIACRILATO C/PLATA-3",                                      
                              "APOSITO DE FIBRAS DE POLIACRILATO C/PLATA-6",                                      
                              "APOSITO DE HIDROCOLOIDE-3",                                                        
                              "APOSITO DE HIDROCOLOIDE-6",                                                        
                              "APOSITO PARA PEQUE\xd1AS HERIDAS REDONDO-19",                                      
                              "BOMBA DE UN SOLO USO / TERAPIA PRESI\xd3N NEGATIVA C/ APOSITO-27",                 
                              "AP\xd3SITO ABSORBENTE C/ ADHESIVO-23",                                             
                              "AP\xd3SITO ABSORBENTE C/ ADHESIVO-22",                                             
                              "AP\xd3SITO TRANSPARENTE ADHESIVO-20",                                              
                              "APOSITO DE HIDROCOLOIDE-7",                                                        
                              "APOSITO DE HIDROCOLOIDE-9",                                                        
                              "APOSITO DE HIDROCOLOIDE C/ PLATA-2",                                               
                              "APOSITO DE HIDROCOLOIDE C/ PLATA-5",                                               
                              "APOSITO DE HIDROFIBRA  / CINTA-18",                                                
                              "APOSITO DE HIDROFIBRA DE HIDROCOLOIDE C/ ESPUMA POLIURETANO-10",                   
                              "APOSITO DE HIDROFIBRA DE HIDROCOLOIDE C/ ESPUMA POLIURETANO-12",                   
                              "APOSITO DE HIDROFIBRA DE HIDROCOLOIDE C/ PLATA-3",                                 
                              "APOSITO DE HIDROFIBRA HIDROCOLOIDE P/ BOMBA DE UN SOLO USO-26",                    
                              "APOSITO ESPUMA DE POLIVINILO ALCOHOL  P/ TERAPIA CONVENCIONAL PRESION NEGATIVA-34",
                              "APOSITO MALLADO C/ PARAFINA-38",                                                   
                              "APOSITO MALLADO C/ PARAFINA-39",                                                   
                              "APOSITO MALLADO C/ SILICONA-0",                                                    
                              "ELIMINADOR DE ADHESIVOS MEDICOS DE LA PIEL-17",                                    
                              "EQ. DE PRESION NEGATIVA / Espuma de poliuretano-32",                               
                              "EQ. DE PRESION NEGATIVA / Espuma de poliuretano-36",                               
                              "EQ. DE PRESION NEGATIVA / Espuma de poliuretano con plata-35",                     
                              "GEL P/ LIMPIEZA  Y DESCONTAMINACION DE HERIDAS-15",                                
                              "HIDROGEL-37",                                                                      
                              "SOLUCION P/ LIMPIEZA Y DESCONTAMINACION DE HERIDAS-16",                            
                              "TIRA DE GASA ORILLADA TAPONAMIENTOS ESTERIL 1cm.-30",                              
                              "TIRA DE GASA ORILLADA TAPONAMIENTOS ESTERIL 2cm.-31",                              
                              "AP\xd3SITO MALLADO C/ OLIGOSAC\xc1RIDOS-1",                                        
                              "AP\xd3SITO DE ESPUMA DE POLIURETANO C/ SILICONA-3",                                
                              "AP\xd3SITO ADHESIVO ABSORBENTE IMPERMEABLE C/ BOLSA-21" ),
                  selected = "GEL P/ LIMPIEZA  Y DESCONTAMINACION DE HERIDAS-15"),
      menuItem("Predicted expenses by product", tabName = "predByProd")
    )
  ),
  dashboardBody(
    
    ########### TAB1 ################
    tabItem(tabName = "predByProd",
            fluidRow(
              tabBox(title = "Predicted expenses by product",
                     id = "predExpByProd",
                     tabPanel(
                       title = "Performance Metrics",
                       tableOutput("metricasperf"),
                       selectInput("modelInput", 
                                   "Select the best model for your product:",
                                   choices = c("ETS(filter models)",
                                               "AUTO.ARIMA(sarima models)",
                                               "PROPHET (deep learning)"))
                     )
              ),
              tabBox(title = "Model Plot",
                     id = "predExpByProdPlot",
                     tabPanel(title = "Data Visualization",
                              conditionalPanel(
                                condition = "input.modelInput != 'PROPHET (deep learning)'",
                                plotOutput("plot1")
                              ),
                              conditionalPanel(
                                condition = "input.modelInput == 'PROPHET (deep learning)'",
                                dygraphOutput("plot2")
                              )
                     ), 
                     tabPanel(title = "Predictions values",
                              id = "predExpByProdTable",
                              tabPanel(title = "Data Visualization",
                                       conditionalPanel(
                                         condition = "input.modelInput != 'PROPHET (deep learning)'",
                                         tableOutput("table1")
                                       ),
                                       conditionalPanel(
                                         condition = "input.modelInput == 'PROPHET (deep learning)'",
                                         dataTableOutput("table2")
                                       )
                              )
                     )
                     
              )
              
            )
    )#dashboardBody
  )
)