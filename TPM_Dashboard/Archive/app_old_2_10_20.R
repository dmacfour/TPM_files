#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

load("~/TPM_SPM_Analysis/Images/Output.RData")
names <- c("Region", "Subject Area")
yvar <- c("Number of Teachers","Number of New Teachers","Number of Leavers","Attrition", "Student/Teacher Ratio","Not Fully Certified Rate")
filter <- c("Elementary", "Middle School","High School")
subject <- unique(TPMsubtransformed$`Subject Area`)
region <- unique(TPMsubtransformed$Region)
TPMsubtransformed$weight <- TPMsubtransformed$`Number of Teachers`

library(shiny)
library(ggplot2)
library(dplyr)
library(markdown)

# Define UI for application that draws a histogram
ui <- navbarPage("Teacher Predictor Model",
    tabPanel("Regions",
    
    # Application title
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(selectInput("Variable", 
                                               label = "Y-axis Variable:",
                                               choices = yvar,selected = "Number of Teachers"),
            sliderInput("range", "Year Range", min = 2010, max = 2025, value = c(2012,current_year+1), sep = ""),
            radioButtons("filter", label = "Level", choices = filter, selected = "Elementary"),
            checkboxGroupInput("region", label = "Region", choiceNames = region, choiceValues = region, selected = region)),
    
            # Show a plot of the generated distribution
            mainPanel(
               plotOutput("distPlot"),
               DT::dataTableOutput("view")
               
            )
        )
    ),
    tabPanel("Subject Area",
             
             
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(selectInput("Variable2", 
                                          label = "Y-axis Variable:",
                                          choices = yvar),
                              sliderInput("range2", "Year Range", min = 2010, max = 2025, value = c(2012,current_year+1), sep = ""),
                              radioButtons("filter2", label = "Level", choices = filter, selected = "Elementary"),
                              checkboxGroupInput("subject", label = "Subject Area", choiceNames = subject, choiceValues = subject, selected = c("Art","Science","Social Studies","Mathematics"))),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("distPlot2"),
                     DT::dataTableOutput("view_sub")
                     
                 )
             )
    ),
    tabPanel("About"
             
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dat <- reactive({datfilter <- TPMsubtransformed %>%
        filter(Category %in% input$filter & Region %in% input$region) %>%
        group_by(Region, Year, Category) %>% 
        summarise(enrollment_count = mean(enrollment_count, na.rm = TRUE), 
                  `Number of Teachers` = sum(`Number of Teachers`, na.rm = TRUE), 
                  `Number of Leavers` = sum(`Number of Leavers`, na.rm = TRUE),
                  `Number of New Teachers` = sum(`Number of New Teachers`,na.rm = TRUE),
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
        } else{
            current_year <- current_year
        }
        datsub <- data.frame(x = dat[,input$Variable], Region = dat$Region, Year = dat$Year)
        lm1 <- lm(x ~ Region, data = datsub[datsub$Year %in% c((current_year-1):current_year),])
        datsub$p <- predict(object = lm1, newdata = datsub)
        datsub$p <- round(ifelse(datsub$Year <= current_year, datsub$x, datsub$p),4)
        datsub <- subset(datsub, select = -c(x))
        return(datsub)
    })
    output$distPlot <- renderPlot({
        # generate subsets based on user input
        dat <- as.data.frame(dat())
        datsub <- as.data.frame(pred())
        if(input$Variable %in% c("Attrition","Number of Leavers")){
            current_year <- current_year -1
        } else{
            current_year <- current_year
        }
        # draw the line graph
        
        ggplot(data = NULL) + 
            stat_summary(aes(x = dat[dat$Year <= current_year,]$Year, y = dat[dat$Year <= current_year,input$Variable], group = dat[dat$Year <= current_year,"Region"], color = dat[dat$Year <= current_year,"Region"]),fun.y = identity, geom = "line", size  = .9) + 
            stat_summary(aes(x = dat[dat$Year <= current_year,]$Year, y = dat[dat$Year <= current_year,input$Variable], group = dat[dat$Year <= current_year,"Region"], color = dat[dat$Year <= current_year,"Region"]),fun.y = identity, geom = "point", size  = 2) +
            stat_summary(aes(x = datsub[datsub$Year >= current_year,]$Year, y = datsub[datsub$Year >= current_year,"p"], group = datsub[datsub$Year >= current_year,"Region"], color = datsub[datsub$Year >= current_year,"Region"]), linetype = "longdash",fun.y = identity, geom = "line", size  = .9) +
            stat_summary(aes(x = datsub[datsub$Year >= current_year,]$Year, y = datsub[datsub$Year >= current_year,"p"], group = datsub[datsub$Year >= current_year,"Region"], color = datsub[datsub$Year >= current_year,"Region"]), fun.y = identity, geom = "point", size  = 2) +
            xlim(low = input$range[1], high = input$range[2]) +
            labs(color = "Region") +
            ylab(input$Variable) +
            xlab("Year") +
            theme(legend.position="top")
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
