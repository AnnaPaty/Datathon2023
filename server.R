library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(data.table)

function(input, output, session) { 
  # Read data from csv and adapt it to R
  dd <- fread("D:/Datathon/consumo_material_clean.csv", col.names = c("codigo", "fechapedido", "numero", "referencia", "cantidadcompra", "unidadesconsumocontenidas", "precio", "importelinea", "tipocompra", "origen", "tgl", "producto"))
  dd$fechapedido <- as.Date(dd$fechapedido, format = "%d/%m/%y")
  dd[dd$tgl == "", "tgl"] <- "Sin distribución"
  dd$tgl <- as.factor(dd$tgl)
  dd$tipocompra <- as.factor(dd$tipocompra)
  dd$monthyear <- format(dd$fechapedido, "%Y-%m")
  dd$year <- format(dd$fechapedido, "%Y")
  dd$year <- as.Date(paste(dd$year, "-01-01", sep = ""), format = "%Y-%m-%d")
  dd$month <- as.factor(format(dd$fechapedido, "%m"))
  dd[,c('regio', 'hospital', "departament")] <- as.data.frame(str_split_fixed(dd$origen, "-",3))
  dd$classification <- substr(dd$codigo, 1, 1)
  dd$producto <- as.factor(dd$producto)
  
  # Create render complete serie
  complete_serie <- reactive({
    complete_filtered <- dd %>% 
        select("producto", "monthyear", "importelinea") %>% 
        group_by(producto, monthyear) %>% 
        summarise(importelinea = sum(importelinea)) %>% 
        filter(producto %in% input$codeProd)
    
    window(ts(complete_filtered[, c("importelinea")], 
              start = c(as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][1]),
                        as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][2])), freq = 12),
           start = c(as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][1]),
                     as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][2])))
  })

  
  # Create render complete serie
  complete_serie_minus12 <- reactive({
    complete_filtered <- dd %>% 
      select("producto", "monthyear", "importelinea") %>% 
      group_by(producto, monthyear) %>% 
      summarise(importelinea = sum(importelinea, na.rm = TRUE)) %>% 
      filter(producto %in% input$codeProd)
    
    window(ts(complete_filtered[, c("importelinea")], 
              start = c(as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][1]),
                        as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][2])), freq = 12),
           start = c(as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][1]),
                     as.numeric(strsplit(min(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][2])),
           end = c(as.numeric(strsplit(max(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][1]) - 1,
                   as.numeric(strsplit(max(complete_filtered$monthyear, na.rm = TRUE), "-")[[1]][2])))
  })
  
  
  #   ###############################################################*
  #   ############### PROPHET #######################################*
  #   ###############################################################*
  
  library(prophet)
  
  complete_filtered_data <- reactive({ dd %>% 
      select("producto", "monthyear", "importelinea") %>% 
      group_by(producto, monthyear) %>% 
      summarise(importelinea = sum(importelinea, na.rm = TRUE)) %>% 
      filter(producto %in% input$codeProd)
  })
  
  complete_filtered_data_minus12 <- reactive({ dd %>% 
      select("producto", "monthyear", "importelinea") %>% 
      group_by(producto, monthyear) %>% 
      summarise(importelinea = sum(importelinea, na.rm = TRUE)) %>% 
      filter(producto %in% input$codeProd) %>%
      filter(monthyear < "2023-01")
  })
  
  observe({
    npasos <<- (2023-as.numeric(strsplit(max(complete_filtered_data()$monthyear, na.rm = TRUE), "-")[[1]][1]))*12 +
      12-as.numeric(strsplit(max(complete_filtered_data()$monthyear, na.rm = TRUE), "-")[[1]][2])
  })
  
  dfprophet <- reactive({
    df <- mutate(
      complete_filtered_data(),
      ds = monthyear,
      y = importelinea
    )
    df$ds <- as.Date(paste(df$ds,"-01", sep=""), format = "%Y-%m-%d")
    df <- column_to_rownames(df, var = "monthyear")
    m <- prophet()
    m <- add_country_holidays(m,"ES")
    fit.prophet(m,df)
    
  })
  
  dfprophet_minus12 <- reactive({
    df <- mutate(
      complete_filtered_data_minus12(),
      ds = monthyear,
      y = importelinea
    )
    df$ds <- as.Date(paste(df$ds,"-01", sep=""), format = "%Y-%m-%d")
    df <- column_to_rownames(df, var = "monthyear")
    m <- prophet()
    m <- add_country_holidays(m,"ES")
    fit.prophet(m,df)
    
  })
  
  forecast <- reactive({
    future <- make_future_dataframe(dfprophet(), periods = npasos)
    predict(dfprophet(), future)
  })
  
  
output$plot1 <- renderPlot({
  if(input$modelInput == "ETS(filter models)"){
    library(forecast)
    fit <- ets(complete_serie(), allow.multiplicative.trend=T)
    plot(predict(fit,h=npasos),ylab="")
  }else if(input$modelInput == "AUTO.ARIMA(sarima models)"){
    library(forecast)
    fit <- auto.arima(complete_serie())
    plot(forecast::forecast(fit,h=npasos))
  }
})

output$table1 <- renderTable({
  if(input$modelInput == "ETS(filter models)"){
    library(forecast)
    fit <- ets(complete_serie(), allow.multiplicative.trend=T)
    predictions <- predict(fit,h=npasos)$mean
  }else if(input$modelInput == "AUTO.ARIMA(sarima models)"){
    library(forecast)
    fit <- auto.arima(complete_serie())
    predictions <- forecast::forecast(fit,h=npasos)$mean
  }
  
  if(length(predictions) < 12) {
    combined <- c(predictions, tail(complete_serie(), 12-length(predictions)))
  } else {
    combined <- predictions
  }
  data.frame(Month = 12:1, Values = combined)
  
})

output$plot2 <- renderDygraph({
  if(input$modelInput == "PROPHET (deep learning)"){
  dyplot.prophet(dfprophet(),forecast())
  }
})

output$table2 <- renderDataTable({
  if(input$modelInput == "PROPHET (deep learning)"){
    data.frame(forecast()[c("ds","yhat","yhat_lower","yhat_upper")])
  }
}, options = list(pageLength=12))

  ###### Final

  RMSPE <- function(obs,pr){
    r <- sqrt(sum(((obs-pr)/obs)^2)/(12))
    return(r)
  }

  MAPE <- function(obs,pr){
    r <- sum(abs((obs-pr)/obs)/(12))
    return(r)
  }

  MAE <- function(obs,pr){
    r <- sum(abs(obs-pr))/(12)
    return(r)
  }

  RMSE <- function(obs,pr){
    r <- sqrt(sum(((obs-pr))^2)/(12))
    return(r)
  }

  nRMSE <- function(obs,pr){
    r <- RMSE(obs,pr)/abs(mean(obs))
    return(r)
  }

  nMAE <- function(obs,pr){
    r <- MAE(obs,pr)/abs(mean(obs))
  }
  
  pr_ets <- reactive({
    library(forecast)
    fitets <- forecast::ets(complete_serie_minus12(), allow.multiplicative.trend = T)
    as.matrix(forecast::forecast(fitets, h = 12)$mean)

  })

  pr_autoarima <- reactive({
    library(forecast)
    fitauto <- auto.arima(complete_serie_minus12())
    as.matrix(forecast::forecast(fitauto,h=12)$mean)
  })

  pr_prophet <- reactive({
    future <- make_future_dataframe(dfprophet_minus12(), periods = 12)
    forecast <- predict(dfprophet(), future)$yhat
    as.matrix(forecast)[1:12,1]
  })

  obs <- reactive({
    as.matrix(tail(complete_serie(), 12))
  })

  output$metricasperf <- renderTable({
    c_mape <- c(MAPE(obs(),pr_ets()), MAPE(obs(),pr_autoarima()), MAPE(obs(),pr_prophet()))
    c_rmspe <- c(RMSPE(obs(),pr_ets()), RMSPE(obs(),pr_autoarima()), RMSPE(obs(),pr_prophet()))
    c_mae <- c(MAE(obs(),pr_ets()), MAE(obs(),pr_autoarima()), MAE(obs(),pr_prophet()))
    c_rmse <- c(RMSE(obs(),pr_ets()), RMSE(obs(),pr_autoarima()), RMSE(obs(),pr_prophet()))
    c_nmae <- c(nMAE(obs(),pr_ets()), nMAE(obs(),pr_autoarima()), nMAE(obs(),pr_prophet()))
    c_nrmse <- c(nRMSE(obs(),pr_ets()), nRMSE(obs(),pr_autoarima()), nRMSE(obs(),pr_prophet()))

    r <- data.frame(MODEL = c("ets", "Auto.arima", "Prophet"),
                    MAPE = c_mape, RMSPE = c_rmspe, 
                    MAE = c_mae, RMSE = c_rmse,
                    nMAE = c_nmae, nRMSE = c_nrmse)
  })


 }
 
