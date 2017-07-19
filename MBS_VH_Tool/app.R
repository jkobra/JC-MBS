library(plotly)
library(shiny)
library(xts)
library(lubridate)
#library(rhandsontable)

setwd("~/MBS Valuation/MBS_VH_Tool/")

# Inputs ------------------------------------------------------------------

#List all of the coupons (without '.'s) and their stories - This should be changed accordingly if underlying data changes
coupons <- c("3", "3.5", "4", "4.5")         #MBS coupons go here
stories <- c("LLB", "MLB", "HLB", "HLB2")  #MBS loan balance stories go here

#DO NOT CHANGE
all.plot.strings <- apply(expand.grid(coupons, stories), 1, paste, collapse="")


# functions ---------------------------------------------------------------

convertDate <- function(x){
  # Function that takes excel data in format of "mm/dd/yyyy and transforms to 
  # A standard date/time format for the Lubridate package
  
  vec <- strsplit(x,"/")
  mm <- unlist(vec)[3*(1:length(vec))-2]
  dd <- unlist(vec)[3*(1:length(vec))-1]
  yyyy <- unlist(vec)[3*(1:length(vec))]
  return(paste(yyyy,mm,dd,sep="-"))
}

# User Interface ----------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # HTML Design and Layout
  titlePanel("MBS Valuation and Hedging Tool"),
  mainPanel(
    tabsetPanel(
      # tabPanel("Hedging",
      #          br(),
      #          mainPanel(
      #            h2("Hedge Ratio Calculation Table"),
      #            br(),
      #            helpText("Click a to enter data in the cell"),
      #            br(),
      #            column(8,
      #              rHandsontableOutput("HedgeTable")
      #            )
      #          )
      #         ),
      
      
      #Regression Tab Layout
      tabPanel("Regression",
               br(),
               sidebarLayout(
                 sidebarPanel = sidebarPanel(width = 5,
                                             verticalLayout(
                                               fluidRow(
                                                 selectInput("regressionCoupon", label = "Coupon (%)", choices = coupons),
                                                 dateInput("regr.start.date", label = "Start Date", value = "2014-01-01"),
                                                 dateInput("regr.end.date", label = "End Date", value = "2017-10-01")
                                               ),
                                               fluidRow(
                                                 verbatimTextOutput("RegressionInfo")
                                               )
                                             )
                 ),
                 mainPanel = mainPanel(width = 7, htmltools::div(plotlyOutput("RegPlot", width = 1200, height = 650)))
               )
      ),
      
      
      #Z-Score Tab Layout
      tabPanel("Z-Score",
               fluidRow(
                 br(),
                 sidebarLayout(
                   mainPanel = mainPanel(width = 7,
                                         htmltools::div(style = "display:inline-block", plotlyOutput("ZScorePlot", width = 1200, height = 600))),
                   sidebarPanel = sidebarPanel(width = 4,
                                               h3("Analysis Inputs"),
                                               selectInput("plot.string", label = "Coupon & Story", choices = all.plot.strings),
                                               dateInput("zs.start.date", label = "Start Date", value = "2014-01-01"),
                                               dateInput("zs.end.date", label = "End Date", value = "2017-10-01"),
                                               helpText("Note: The Treasury Yield is NOT a Z-Score")
                   )
                 )
               )
      )
    )
  )
)


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  LOAS.data <- reactive({
    #Import Data From .CSV
    temp <- read.csv(paste("~/MBS Valuation/MBS_VH_Tool/Data/LOAS.csv", sep=""), header=T, stringsAsFactors = F)
    
    #Convert Data to a time series (.xts) object, reformat Excel data to R date
    temp <- xts(temp[,-c(1)], order.by = as_date(convertDate(temp$Date)))
    colnames(temp) <- gsub("X", "",colnames(temp)) 
    
    #Return the Data
    return(temp)
  })
  
  LOAD.data <- reactive({
    temp <- read.csv(paste("~/MBS Valuation/MBS_VH_Tool/Data/LOAD.csv", sep=""), header=T, stringsAsFactors = F)
    temp <- xts(temp[,-c(1)], order.by = as_date(convertDate(temp$Date)))
    colnames(temp) <- gsub("X", "",colnames(temp)) 
    return(temp)
  })
  
  tsy.data <- reactive({
    temp <- read.csv(paste("~/MBS Valuation/MBS_VH_Tool/Data/tyields.csv", sep=""), header=T, stringsAsFactors = F)
    temp <- xts(temp[,c(2)], order.by = as_date(convertDate(temp$Date)))
    colnames(temp) <- c("yield")
    return(temp)
  })
  
  #Create storage variables for MBS .csv data
  cpn.story.data <- reactive({                  #Aggregate Data for each coupon's story
    returnList <- list()
    
    for(coupon in coupons){
      for(story in stories){
        
        #Read csv data into variable
        temp <- read.csv(paste("~/MBS Valuation/MBS_VH_Tool/Data/",coupon,story,".csv", sep=""), header=T, stringsAsFactors = F)
        
        #Remove NA data after converting to standard format
        date1 <- na.omit(as_date(convertDate(temp$Date)))
        pseries <- na.omit(temp$Payup..ticks.)
        
        date2 <- na.omit(as_date(convertDate(temp$Date.1)))
        oSeries <- na.omit(temp$X.OAS)
        
        #Place data into time series
        payup <- xts(pseries, date1)
        payup <- payup[paste(input$regrstart.date,"/", input$regr.end.date, sep="")]
        
        percent.oas <- xts(oSeries, date2)
        percent.oas <- percent.oas[paste(input$regr.start.date,"/", input$regr.end.date, sep="")]
        
        #Merge both sets into one time series
        new.series <- merge.xts(payup, percent.oas) 
        new.series <- new.series[complete.cases(new.series)]
        
        #Calculate model.payup, actual payup and payback
        model.payup <- 32.0 * (new.series$payup / new.series$percent.oas)
        
        actual.payup <- new.series$payup * 32
        
        payback = model.payup-actual.payup
        LOAS <- LOAS.data()[,paste(coupon,story,sep="")]
        
        #Add new calculations to the new time series
        new.series <- merge(new.series, model.payup, actual.payup, payback, LOAS)
        colnames(new.series) <- c("payup.ticks", "percent.oas", "model.payup", "actual.payup", "payback", "LOAS")
        
        returnList[[paste(coupon,story, sep=".")]] <- new.series
      }
    }
    return(returnList)
  })
  
  cpn.data <- reactive({         #Aggregate Data for each Coupon, tracks the model.payups and paybacks
    returnList <- list()
    
    #Begin cycling through all .csv's of MBS story data
    for(coupon in coupons){
      
      #Pre allocate the data frame for the current MBS coupon
      returnList[[coupon]] = data.frame()
      
      for(story in stories){
        temp <- cpn.story.data()[[paste(coupon,story, sep=".")]]
        
        #Remove time series format and add the core data to the storage variable
        returnList[[coupon]] <- rbind(returnList[[coupon]], merge(coredata(merge(temp$model.payup, temp$payback)),story))
        
      }
      colnames(returnList[[coupon]]) <- c("payup", "payback", "story")
      
    }
    return(returnList)
  })
  
  z.data <- reactive({           #Stores the z-score time series of the %OAS and model.payups by each coupon's story
    returnList <- list()
    
    for(coupon in coupons){
      for(story in stories){
        
        temp <- cpn.story.data()[[paste(coupon,story, sep=".")]]
        
        #Calculate model.payup and %OAS z-scores
        returnList[[paste(coupon,story, sep="")]] <- scale(temp[paste(input$zs.start.date,"/", input$zs.end.date, sep=""),c("LOAS", "percent.oas")])
      }
    }
    return(returnList)
  })
  
  regression.data <- reactive({  #Records the data from regressing model.payups onto paybacks
    returnList <- list()
    
    for(coupon in coupons){
      dataList <- list()
      dataList$model <- lm(data = cpn.data()[[coupon]], payback~payup)
      maxDataPoints <- length(cpn.data()[[coupon]]$payback)
      fitValues <- predict.lm(dataList$model, newdata = data.frame(payup = cpn.data()[[coupon]][,1][1:maxDataPoints]))
      dataList$plot <- plot_ly(data = cpn.data()[[coupon]], x=~payup) %>%      #Build Plotly plot
        add_markers(y=~payback, color=~story, colors="Set2") %>%                          #Add data points
        add_lines(x=~payup, y=fitValues, name = "Regression", line = list(color='rgb(255, 12, 24)', width=3)) %>%                 #Add linar regression line
        layout(                                              #Add Chart Features
          title = paste(coupon,"% Coupon Payback vs. Payup", sep=""),
          xaxis = list(title = "Model Payup"),
          yaxis = list(title = "Payback (Model-Actual)")
        )
      returnList[[coupon]] = dataList
    }
    return(returnList)
  })
  
  get_plot <- reactive({
    ts = z.data()[[input$plot.string]]
    data = data.frame(date = index(ts), LOAS = coredata(ts$LOAS), percent.oas = coredata(ts$percent.oas))
    tsy <- data.frame(date = index(tsy.data()), yield = coredata(tsy.data()$yield))
    p <- plot_ly(data, x = ~date) %>%
      add_trace(y = ~LOAS, name = "LOAS (Left)",  mode = "lines") %>%
      add_trace(y = ~percent.oas, name = "% Theoretical OAS (Left)",  mode = "lines") %>%
      add_trace(data = tsy, x =~date, y=~yield, name = "10yr Treasury Yield (Right)", yaxis="y2", mode = "lines") %>%
      layout(
        title = paste(input$plot.string, "Z-Score Data"),
        xaxis = list(
          title = "Date"
        ),
        yaxis = list(
          title = "Z-Score"
        ),
        yaxis2 = list(
          title = "Yield (%)",
          side = "right",
          overlaying = "y"
        )
      )
    return(p)
  })
  
  ### Outputs
  
  # output$HedgeTable <- renderRHandsontable({
  #   
  # })
  
  output$RegressionInfo <- renderPrint({
    summary(regression.data()[[input$regressionCoupon]]$model)
  })
  
  output$ZScorePlot <- renderPlotly({
    get_plot()
  })
  
  output$RegPlot <- renderPlotly({
    regression.data()[[input$regressionCoupon]][["plot"]]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

