#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
###--------
load("Output.RData")
names <- c("Region", "Subject Area")
yvar <- c("Number of Teachers","Number of New Teachers","Number of Leavers","Attrition", "Student/Teacher Ratio","Not Fully Certified Rate","enrollment_count", "Number Not Certified")
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
                         selectInput("reg_or_sub", 
                                     label = "Breakdown by:",
                                     choices = c("Subject","Region"),selected = "Region"),
                        sliderInput("range", "Year Range", min = 2010, max = current_year+5, value = c(2013,current_year+1), sep = ""),
                        radioButtons("filter", label = "Level", choices = filter, selected = "Elementary"),
                        checkboxGroupInput("region", label = "Region", choiceNames = region, choiceValues = region, selected = region[1:5])),
    
            # Show a plot of the generated distribution
            mainPanel(
               plotlyOutput("distPlot"),
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
        group_by(Region, Year, Category) %>% 
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
        
        #make Predictions
        if(input$Variable %in% c("Attrition","Number of Leavers")){
            current_year <- current_year - 1
        } else {
            current_year <- current_year
        }
        
        
        
        if(input$Variable %in% c("Attrition","Not Fully Certified Rate")){
            datsub <- data.frame(x = dat[,input$Variable], Region = dat$Region, Year = dat$Year)
            lm1 <- lm(x ~ Region, data = datsub[datsub$Year %in% c((current_year-1):current_year),])
            datsub$p <- predict(object = lm1, newdata = datsub)
            datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$x, datsub$p),4)
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- input$Variable
            
        } else if(input$Variable %in% c("Student/Teacher Ratio")){
            datsub <- data.frame(x = dat[,input$Variable], Region = dat$Region, Year = dat$Year)
            lm1 <- lm(x ~ Region, data = datsub[datsub$Year %in% c((current_year-3):current_year),])
            datsub$p <- predict(object = lm1, newdata = datsub)
            datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$x, datsub$p),4)
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- input$Variable
            
        } else if(input$Variable %in% c("Number of Teachers")){
            datsub <- data.frame(x = dat$enrollment_count, st = dat$`Student/Teacher Ratio`, teach_count = dat$`Number of Teachers`, Region = dat$Region, Year = dat$Year)
            
            lm1 <- lm(st ~ Region, data = datsub[datsub$Year %in% c((current_year-3):current_year),])
            datsub$p_st <- predict(object = lm1, newdata = datsub)
            datsub$p_st <- ifelse(datsub$Year <= current_year, datsub$st, datsub$p_st)
            datsub$p <- datsub$p_st * datsub$x
            datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$teach_count, datsub$p))
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- input$Variable
            
        } else if(input$Variable %in% c("enrollment_count")){
            datsub <- data.frame(Region = dat$Region, Year = dat$Year, p = dat$enrollment_count)
            datsub <- subset(datsub, select = c(Region, Year, p))
            datsub$p <- round(datsub$p)
            names(datsub)[names(datsub) == "p"] <- input$Variable
        } else if(input$Variable %in% c("Number of Leavers")){
            
            datsub <- data.frame(x = dat$enrollment_count, ar = dat$Attrition, st = dat$`Student/Teacher Ratio`, 
                                 teach_count = dat$`Number of Teachers`, nl = dat$`Number of Leavers`,  
                                 Region = dat$Region, Year = dat$Year)
            
            #Predict Number of teachers
            lm1 <- lm(st ~ Region, data = datsub[datsub$Year %in% c((current_year-2):current_year + 1),])
            datsub$p_st <- predict(object = lm1, newdata = datsub)
            datsub$p_st <- ifelse(datsub$Year <= current_year + 1, datsub$st, datsub$p_st)
            datsub$p_nt <- datsub$p_st * datsub$x
            
            #Predict number of leavers
            lm2 <- lm(ar ~ Region, data = datsub[datsub$Year %in% c((current_year-3):current_year),])
            datsub$p_ar <- predict(object = lm2, newdata = datsub)
            datsub$p_ar <- ifelse(datsub$Year <= current_year, datsub$ar, datsub$p_ar)
            
            datsub$p <- round(datsub$p_ar * datsub$p_nt)
            datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$nl, datsub$p))
            datsub <- subset(datsub, select = c(Region, Year, p))
            names(datsub)[names(datsub) == "p"] <- "Number of Leavers"
            
        } else if(input$Variable %in% c("Number of New Teachers")){
            
            datsub <- data.frame(x = dat$enrollment_count, ar = dat$Attrition, st = dat$`Student/Teacher Ratio`, 
                                 teach_count = dat$`Number of Teachers`, nl = dat$`Number of Leavers`,  
                                 Region = dat$Region, Year = dat$Year, nnt = dat$`Number of New Teachers`)
            
            #Predict Number of teachers
            lm1 <- lm(st ~ Region, data = datsub[datsub$Year %in% c((current_year-3):current_year),])
            datsub$p_st <- predict(object = lm1, newdata = datsub)
            datsub$p_st <- ifelse(datsub$Year <= current_year, datsub$st, datsub$p_st)
            datsub$p_nt <- datsub$p_st * datsub$x
            
            #Predict number of leavers
            lm2 <- lm(ar ~ Region, data = datsub[datsub$Year %in% c((current_year-4):current_year-1),])
            datsub$p_ar <- predict(object = lm2, newdata = datsub)
            datsub$p_ar <- ifelse(datsub$Year <= current_year-1, datsub$ar, datsub$p_ar)
            datsub$p <- round(datsub$p_ar * datsub$p_nt)
            datsub$p <- round(ifelse(datsub$Year <= current_year-1, datsub$nl, datsub$p))
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
            lm1 <- lm(st ~ Region, data = datsub[datsub$Year %in% c((current_year-3):current_year),])
            datsub$p_st <- predict(object = lm1, newdata = datsub)
            datsub$p_st <- ifelse(datsub$Year <= current_year, datsub$st, datsub$p_st)
            datsub$p_nt <- datsub$p_st * datsub$x
            
            #Predict certification rate
            lm2 <- lm(nfcr ~ Region, data = datsub[datsub$Year %in% c((current_year-1):current_year),])
            datsub$p_nfcr <- predict(object = lm2, newdata = datsub)
            datsub$p_nfcr <- ifelse(datsub$Year <= current_year, datsub$nfcr, datsub$p_nfcr)
            
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
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.2), yaxis = list(title = as.character(input$Variable),titlefont="f"))
    }) 
    
    output$distPlot2 <- renderPlot({
        # generate subsets based on user input
        dat <- TPMsubtransformed %>%
            filter(Category %in% input$filter2 & TPMsubtransformed$`Subject Area` %in% input$subject) %>%
            group_by(`Subject Area`, Year, Category) %>% 
            summarise(enrollment_count = mean(enrollment_count, na.rm = TRUE), `Number of Teachers` = sum(`Number of Teachers`, na.rm = TRUE), `Number of Leavers` = sum(`Number of Leavers`, na.rm = TRUE)) %>%
            mutate(`Student/Teacher Ratio` = `Number of Teachers`/enrollment_count, Attrition = `Number of Leavers`/`Number of Teachers`)
        dat <- as.data.frame(dat)
        current_year <- current_year
        
        # draw the line graph
        
        
        ggplot(data = NULL) + 
            stat_summary(aes(x = dat$Year, y = dat[,input$Variable2], group = dat[,"Subject Area"], color = dat[,"Subject Area"]),fun.y = identity, geom = "line", size  = .7) + 
            stat_summary(aes(x = dat$Year, y = dat[,input$Variable2], group = dat[,"Subject Area"], color = dat[,"Subject Area"]),fun.y = identity, geom = "point", size  = 2) +
            xlim(low = input$range2[1], high = input$range2[2]) +
            labs(color = "Subject Area") +
            ylab(input$Variable2) +
            xlab("Year") +
            theme(legend.position="top")
    }) 
    
    output$view <- DT::renderDataTable({
        DT::datatable(pred())
    })
    
    output$view_sub <- DT::renderDataTable({pred()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
