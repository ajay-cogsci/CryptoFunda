library(quantmod)
library(PerformanceAnalytics)


server <- shinyServer(function(input, output){
  observe({
    price <- getSymbols(req(input$stockInput),
                        type="line",
                        
                        theme=chartTheme('white'),
                        auto.assign = F)
    
    
    
    
    
    #choice for technical indicator plot
    
    technical_indicator <- req(input$ti)
    
    if(technical_indicator=="RSI(7)") {
     
      
      output$distPlot5 <- renderPlot({ chart_Series(price) 
        dropTA(all = TRUE)
        addRSI(n=7)})
      
    }
      
    
    if(technical_indicator=="RSI(14)") {
      
      
      output$distPlot5 <- renderPlot({ chart_Series(price)
        dropTA(all = TRUE)
        addRSI(n=14)})
      
    }
    
    if(technical_indicator=="RSI(21)") {
      
      output$distPlot5 <- renderPlot({ chart_Series(price)
        dropTA(all = TRUE)
        addRSI(n=21)})
      
    }
    
    if(technical_indicator=="MACD") {
      
      
      output$distPlot5 <- renderPlot({ chart_Series(price)
        dropTA(all = TRUE)
        addMACD()})
      
    }
    
    if(technical_indicator=="ADX") {
      
      
      output$distPlot5 <- renderPlot({ chart_Series(price)
        dropTA(all = TRUE)
        addADX()})
      
    }
    
    if(technical_indicator=="None") {
      
      
      output$distPlot5 <- renderPlot({ chartSeries(price,   theme = chartTheme("black"))})
      
    }
    
 
    
    
    
    #for showing info
    
    
    if(technical_indicator=="RSI(7)") {
      
      output$tech_ind_info <- renderText({"The relative strength index (RSI) is a momentum indicator used in technical analysis that measures the magnitude of recent price changes to evaluate overbought or oversold conditions in the price of a stock or other asset. The RSI is displayed as an oscillator (a line graph that moves between two extremes) and can have a reading from 0 to 100. "})
  }

    
    if(technical_indicator=="RSI(14)") {
      
      
      output$tech_ind_info <- renderText({"The relative strength index (RSI) is a momentum indicator used in technical analysis that measures the magnitude of recent price changes to evaluate overbought or oversold conditions in the price of a stock or other asset. The RSI is displayed as an oscillator (a line graph that moves between two extremes) and can have a reading from 0 to 100. "})

    }
    
    if(technical_indicator=="RSI(21)") {
      
      output$tech_ind_info <- renderText({"The relative strength index (RSI) is a momentum indicator used in technical analysis that measures the magnitude of recent price changes to evaluate overbought or oversold conditions in the price of a stock or other asset. The RSI is displayed as an oscillator (a line graph that moves between two extremes) and can have a reading from 0 to 100. "})
      
    }
    
    if(technical_indicator=="MACD") {
      
      output$tech_ind_info <- renderText({"Moving average convergence divergence (MACD) is a trend-following momentum indicator that shows the relationship between two moving averages of a security's price. The MACD is calculated by subtracting the 26-period exponential moving average (EMA) from the 12-period EMA."})
      
    }
    
    if(technical_indicator=="ADX") {
      
      output$tech_ind_info <- renderText({"ADX is plotted as a single line with values ranging from a low of zero to a high of 100. ADX is non-directional; it registers trend strength whether price is trending up or down. The indicator is usually plotted in the same window as the two directional movement indicator (DMI) lines, from which ADX is derived."})
      
      
    }
    
    if(technical_indicator=="None") {
      
      output$tech_ind_info <- renderText({"No technical indicator selected !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "})
      
    }
    
    
    
    
    
    
    
    
    
    
    goat <-  price[,4]
    
    r <- goat/Lag(goat) - 1
    delta<-0.005
    signal <-c(NA) # first signal is NA
    
    for (i in 2: length(Cl(price))){
      if (r[i] > delta){
        signal[i]<- 1
      } else if (r[i]< -delta){
        signal[i]<- -1
      } else
        signal[i]<- 0
    }
    signal<-reclass(signal,Cl(price))
    
    trade1 <- Lag(signal)
    ret1<-dailyReturn(price)*trade1
    names(ret1) <- 'Naive'
    
    
    
    day <-14
    
    
    signal <- c()                    #initialize vector
    rsi <- RSI(goat, day)     #rsi is the lag of RSI
    signal [1:day+1] <- 0            #0 because no signal until day+1
    
    for (i in (day+1): length(goat)){
      if (rsi[i] < 30){             #buy if rsi < 30
        signal[i] <- 1
      }else {                       #no trade all if rsi > 30
        signal[i] <- 0
      }
    }
    signal<-reclass(signal,Cl(price))
    trade2 <- Lag(signal)
    
    #construct a new variable ret1
    ret1 <- dailyReturn(price)*trade1
    names(ret1) <- 'Naive'
    # construct a new variable ret2
    ret2 <- dailyReturn(price)*trade2
    names(ret2) <- 'RSI'
    
    retall <- cbind(ret1, ret2)
    
    
    
    plot.macd = TRUE
    ndrawdowns = 3
    short.pos = 0 # size of short position. Set to 0 to disallow shorting
    
    
    data=price[,6] # column 6 to use distribution-adjusted closing prices
    macd = MACD(data, nFast=12, nSlow=26,nSig=9,maType=SMA,percent = FALSE)
    
    signal = Lag(ifelse(macd$macd < macd$signal, short.pos, 1))
    returns = ROC(data)*signal
    names(returns) <- 'MACD'
    portfolio = exp(cumsum(returns))
    table.Drawdowns(returns, top=ndrawdowns)
    table.DownsideRisk(returns)
    
    
    retall_macd <- cbind(ret1, returns)
    
    
    
    
    
    
    adx = ADX(price[,2:4])
    signal <- Lag(ifelse(adx$DIp < adx$DIn, -1, 1))
    ret_m<-dailyReturn(price)*signal
    names(ret_m) <- 'ADX'
    retall_adx <- cbind(ret1, ret_m)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$distPlot <- renderPlot({charts.PerformanceSummary(retall, 
                                                             main="Naive v.s. RSI")
      
      
      
      
    })
    
    output$distPlot2 <- renderPlot({ charts.PerformanceSummary(retall_macd, 
                                                               main="Naive v.s. MACD")
      
    })
    
    
    output$distPlot3 <- renderPlot({charts.PerformanceSummary(retall_adx,
                                                              main="Naive v.s. ADX") })
      
    
  
})})

### ui.R
library(shiny)
tech_ind <- c("None", "RSI(7)", "RSI(14)", "RSI(21)", "MACD", "ADX")

ui <- shinyUI(fluidPage(
  titlePanel( h1("CoinKart: Technical Indicators for Cryptocurrency", align = "center")
  ),
  sidebarLayout(
    sidebarPanel(
      
      #This is a dropdown to select the stock
      selectInput("stockInput",
                  "Pick your cryptocurrency pair:",
                  choices = c("BTC-INR","ETH-INR","MATIC-INR","ALGO-INR","EOS-INR","EGLD-INR"),
                  selected = "BTC-INR"),
      radioButtons("ti", "Display Technical Indicators", tech_ind),
      p(textOutput("tech_ind_info"))
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      
      fluidRow(plotOutput("distPlot5")))
    
    ),
    
    
    titlePanel(h3("Backtesting Trading Strategy using Technical Indicator", align = "center")),
    

  splitLayout(plotOutput("distPlot"), plotOutput("distPlot2"), plotOutput("distPlot3")))
    
      
  )
      
     
    
  

shiny::shinyApp(ui, server)