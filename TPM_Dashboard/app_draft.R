#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
###--------
load("Output.RData")
names <- c("Region", "Subject Area")
yvar <- c("Number of Teachers","Number of New Teachers","Number of Leavers","Attrition", "Student/Teacher Ratio","Not Fully Certified Rate", "Number Not Certified")
filter <- c("Elementary", "Middle School","High School")
subject <- unique(TPMsubtransformed$`Subject Area`)
region <- unique(TPMsubtransformed$Region)
region <- region[!region %in% c("Null", "Xxother")]

library(shiny)
library(ggplot2)
library(dplyr)
library(markdown)
library(plotly)

# Define UI for application that draws a histogram
ui <- navbarPage("Teacher Predictor Model",
    tabPanel("By Region",
    
    # Application title
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(selectInput("Variable", 
                                               label = "Y-axis Variable:",
                                               choices = yvar,selected = "Number of Teachers"),
                        sliderInput("range", "Year Range", min = 2010, max = current_year+5, value = c(2013,current_year+1), sep = ""),
                        checkboxGroupInput("filter", label = "Grade Band", choiceNames = filter, choiceValues = filter, selected = "Elementary"),
                        checkboxGroupInput("region", label = "Region", choiceNames = region, choiceValues = region, selected = region[1:5]),
                        checkboxGroupInput("subject", label = "Subject", choiceNames = subject, choiceValues = subject, selected = subject[1:5]),
                        downloadButton('downloadData', 'Save Output')),
    
            # Show a plot of the generated distribution
            mainPanel(tabsetPanel(type = "tabs", 
                                  tabPanel("Line Plot", plotlyOutput("distPlot")),
                                  tabPanel("Bar Plot", plotlyOutput("distPlotbar"))),
                      DT::dataTableOutput("view")
               
            )
        )
    ),
    
    tabPanel("About"
             
             )
)

server <- function(input, output) {
    dat <- reactive({datfilter <- TPMsubtransformed %>%
        filter(Category %in% input$filter & Region %in% input$region, !Region %in% c("Xxother","Null")) %>%
        group_by(Region, Year) %>% 
        summarise(enrollment_count = mean(enrollment_count, na.rm = TRUE), 
                  `Number of Teachers` = sum(`Number of Teachers`, na.rm = TRUE), 
                  `Number of Leavers` = sum(`Number of Leavers`, na.rm = TRUE),
                  `Number of New Teachers` = sum(`Number of New Teachers`, na.rm = TRUE),
                  `Number Not Certified` = sum(`Number Not Certified`,na.rm = TRUE)) %>%
        mutate(`Student/Teacher Ratio` = `Number of Teachers`/enrollment_count, 
               Attrition = `Number of Leavers`/`Number of Teachers`,
               `Not Fully Certified Rate` = `Number Not Certified`/`Number of Teachers`)
    return(datfilter)})
    
    pred <- reactive({
        dat <- as.data.frame(dat())
        
        #Model Functions
        model_fun <- function(var, name, type, subset = FALSE, round = FALSE, ponly = FALSE, currentyear = current_year){
            datsub <- data.frame(x = dat[,var], Region = dat$Region, Year = dat$Year, teach_count = dat$`Number of Teachers`, enr = dat$enrollment_count)
            datsub$trendyear <- ifelse(datsub$Year > currentyear, currentyear + 1, datsub$Year)
            if(type == "two"){
                lm1 <- lm(x ~ Region, data = datsub[datsub$Year %in% c((currentyear-1):currentyear),])
            } else if(type == "four"){
                lm1 <- lm(x ~ Region, data = datsub[datsub$Year %in% c((currentyear-3):currentyear),])
            } else if(type == "trend") {
                lm1 <- lm(x ~ Region*as.numeric(trendyear), data = datsub[datsub$Year %in% c((currentyear-3):currentyear),])
            }
            datsub$p <- predict(object = lm1, newdata = datsub)
            
            datsub$p <- ifelse(datsub$Year <= currentyear, datsub$x, datsub$p)
            
            if(round == TRUE){
                datsub$p <- round(datsub$p,4)
            }
            if(subset == TRUE){
                datsub <- subset(datsub, select = c(Region, Year, p))
            } 
            
            if(ponly == TRUE){
                p_out <- datsub$p
            } else{
                names(datsub)[names(datsub) == "p"] <- name
                return(datsub)
            }
            
            
        }
        attrition_model_type <- "two"
        st_ratio_model_type <- "trend"
        
        #make Predictions
        if(input$Variable %in% c("Attrition","Number of Leavers")){
            current_year <- current_year - 1
        } else {
            current_year <- current_year
        }

        if(input$Variable %in% c("Attrition")){
            datsub <- model_fun(input$Variable,input$Variable, attrition_model_type, subset = TRUE, round = TRUE)
            
        } else if(input$Variable %in% c("Not Fully Certified Rate")){
            datsub <- model_fun(input$Variable,input$Variable,"two", subset = TRUE, round = TRUE)
            
        } else if(input$Variable %in% c("Student/Teacher Ratio")){
            datsub <- model_fun(input$Variable,input$Variable,st_ratio_model_type, subset = TRUE, round = TRUE)
            
        } else if(input$Variable %in% c("Number of Teachers")){
            datsub <- model_fun(var = "Student/Teacher Ratio", name = "p_st", type = st_ratio_model_type)
            
            datsub$p <- datsub$p_st * datsub$enr
            datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$teach_count, datsub$p))
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- input$Variable
            
        } else if(input$Variable %in% c("enrollment_count")){
            datsub <- data.frame(Region = dat$Region, Year = dat$Year, p = dat$enrollment_count)
            datsub <- subset(datsub, select = c(Region, Year, p))
            datsub$p <- round(datsub$p)
            names(datsub)[names(datsub) == "p"] <- input$Variable1
            
        } else if(input$Variable %in% c("Number of Leavers")){
            datsub <- data.frame(x = dat$enrollment_count, ar = dat$Attrition, st = dat$`Student/Teacher Ratio`, 
                                 teach_count = dat$`Number of Teachers`, nl = dat$`Number of Leavers`,  
                                 Region = dat$Region, Year = dat$Year)
            
            #Predict Number of teachers
            st <- model_fun(var = "Student/Teacher Ratio", name = "p_st", type = st_ratio_model_type, ponly = TRUE, currentyear = current_year + 1)
            datsub$p_st <- ifelse(datsub$Year <= current_year + 1, datsub$st, st)
            datsub$p_nt <- datsub$p_st * datsub$x
            
            #Predict number of leavers
            p_ar <- model_fun(var = "Attrition", name = "p_st", type = attrition_model_type, ponly = TRUE, currentyear = current_year)
            datsub$p_ar <- ifelse(datsub$Year <= current_year, datsub$ar, p_ar)
            
            datsub$p <- round(datsub$p_ar * datsub$p_nt)
            datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$nl, datsub$p))
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- "Number of Leavers"
            
        } else if(input$Variable %in% c("Number of New Teachers")){
            
            datsub <- data.frame(x = dat$enrollment_count, ar = dat$Attrition, st = dat$`Student/Teacher Ratio`, 
                                 teach_count = dat$`Number of Teachers`, nl = dat$`Number of Leavers`,  
                                 Region = dat$Region, Year = dat$Year, nnt = dat$`Number of New Teachers`)
            
            #Predict Number of teachers
            st <- model_fun(var = "Student/Teacher Ratio", name = "p_st", type = st_ratio_model_type, ponly = TRUE, currentyear = current_year)
            datsub$p_st <- ifelse(datsub$Year <= current_year, datsub$st, st)
            datsub$p_nt <- datsub$p_st * datsub$x
            
            #Predict number of leavers
            p_ar <- model_fun(var = "Attrition", name = "p_st", type = attrition_model_type, ponly = TRUE, currentyear = current_year-1)
            datsub$p_ar <- ifelse(datsub$Year <= current_year-1, datsub$ar, p_ar)
            
            datsub$p <- round(datsub$p_ar * datsub$p_nt)
            datsub$p <- round(ifelse(datsub$Year <= current_year - 1, datsub$nl, datsub$p))
            names(datsub)[names(datsub) == "p"] <- "Number of Leavers"
            
            datsub_temp <- datsub
            datsub_temp$p_nt_last_year <- datsub_temp$p_nt
            datsub_temp$lvrs_last_year <- datsub_temp$`Number of Leavers`
            datsub_temp$Year <- as.numeric(datsub_temp$Year + 1)
            datsub_temp <- subset(datsub_temp, select = c(Year, p_nt_last_year, lvrs_last_year, Region))
            datsub <- merge(datsub, datsub_temp, all.x = TRUE)
            
            #Predict Number of new Teachers
            datsub$p <- (datsub$p_nt - datsub$p_nt_last_year) + datsub$lvrs_last_year
            datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$nnt, datsub$p))
            datsub <- datsub[!datsub$Year == min(datsub$Year),]
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- "Number of New Teachers"
            
        } else if(input$Variable %in% c("Number Not Certified")){
            datsub <- data.frame(x = dat$enrollment_count, st = dat$`Student/Teacher Ratio`, teach_count = dat$`Number of Teachers`,   
                                 Region = dat$Region, Year = dat$Year, nfcr = dat$`Not Fully Certified Rate`, 
                                 nnfc = dat$`Number Not Certified`)
            
            #Predict Number of teachers
            st <- model_fun(var = "Student/Teacher Ratio", name = "p_st", type = st_ratio_model_type, ponly = TRUE, currentyear = current_year)
            datsub$p_st <- ifelse(datsub$Year <= current_year, datsub$st, st)
            datsub$p_nt <- datsub$p_st * datsub$x
            datsub$p_nt <- round(datsub$p_nt)
            
            #Predict certification rate
            p_nfcr <- model_fun(var = "Not Fully Certified Rate", name = "p_st", type = "two", ponly = TRUE, currentyear = current_year)
            datsub$p_nfcr <- ifelse(datsub$Year <= current_year, datsub$nfcr, p_nfcr)
            
            
            datsub$p <- round(datsub$p_nfcr * datsub$p_nt)
            datsub$p <- ifelse(datsub$Year <= current_year, datsub$nnfc, datsub$p)
            
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- input$Variable
            
            
        }
        datsub <- datsub[datsub$Year %in% c(input$range[1]:input$range[2]),]
        return(datsub)
    })
    
    output$distPlot <- renderPlotly({
        # generate subsets based on user input
        dat <- as.data.frame(dat())
        datsub <- as.data.frame(pred())
        if(input$Variable %in% c("Attrition", "Number of Leavers")){
            current_year <- current_year - 1
        } else{
            current_year <- current_year
        }
        dat[,input$Variable] <- ifelse(test = dat$Year > current_year,
               datsub[,input$Variable], 
               dat[,input$Variable])
        
        # draw the line graph
        plot_ly(datsub, x = ~Year, y = ~datsub[,input$Variable], color = ~Region,type = 'scatter', mode = 'line',
                hoverinfo = "text",
                hovertext = paste("Year : ", datsub[,"Year"],
                                  "<br>", as.character(input$Variable)," : ", datsub[,input$Variable],
                                  "<br>Region : ", datsub[,"Region"],sep = "")) %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.2), yaxis = list(title = as.character(input$Variable),titlefont="f"),shapes = list(
                list(
                    x0 = ifelse(current_year >= input$range[1],ifelse(current_year < input$range[2],current_year + .90,input$range[2]+.1),input$range[1] - 0.1), 
                    x1 = ifelse(current_year < input$range[2],input$range[2] + .1,input$range[2]+.1), 
                    y0 = 0.025, 
                    y1 = .975, 
                    type = "rect", 
                    xref = "x", 
                    yref = "paper", 
                    opacity = ifelse(current_year < input$range[2],0.2,0), 
                    fillcolor = "#d3d3d3"
                )
            ))
    }) 
    output$distPlotbar <- renderPlotly({
        # generate subsets based on user input
        dat <- as.data.frame(dat())
        datsub <- as.data.frame(pred())
        if(input$Variable %in% c("Attrition", "Number of Leavers")){
            current_year <- current_year - 1
        } else{
            current_year <- current_year
        }
        dat[,input$Variable] <- ifelse(test = dat$Year > current_year,
                                       datsub[,input$Variable], 
                                       dat[,input$Variable])
        
        # draw the line graph
        plot_ly(datsub, x = ~Year, y = ~datsub[,input$Variable], color = ~Region,type = 'bar',
                hoverinfo = "text",
                hovertext = paste("Year : ", datsub[,"Year"],
                                  "<br>", as.character(input$Variable)," : ", datsub[,input$Variable],
                                  "<br>Region : ", datsub[,"Region"],sep = "")) %>% 
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.2), 
                   yaxis = list(title = as.character(input$Variable),titlefont="f"),
                   barmode ='stack')
    }) 
    
    output$view <- DT::renderDataTable({
        DT::datatable(pred())
    })
    
    output$comparison_scatter <- renderPlotly({
        plot_ly(TPMsubtransformed, x = ~TPMsubtransformed[,input$Variable_X], y = ~TPMsubtransformed[,input$Variable_Y], type = "scatter", mode = 'markers', color = ~Region) %>%
            layout(xaxis = list(title = as.character(input$Variable_X),titlefont="f"),
                   yaxis = list(title = as.character(input$Variable_Y),titlefont="f"))
    })
    
    output$downloadData <- downloadHandler(
          filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
          },
          content = function(con) {
            write.csv(pred(), con)
    })
    
    output$downloadData_subj <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(pred_subj(), con)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
