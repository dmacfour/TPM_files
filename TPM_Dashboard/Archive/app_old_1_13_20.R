#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

load("~/TPM_SPM_Analysis/Images/Output.RData")
names <- c("Region", "Category")
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
                                               choices = yvar),
            sliderInput("range", "Year Range", min = 2010, max = 2025, value = c(2012,current_year), sep = ""),
            checkboxGroupInput("filter", label = "Level", choiceNames = filter, choiceValues = filter, selected = filter),
            checkboxGroupInput("region", label = "Region", choiceNames = region, choiceValues = region, selected = region)),
    
            # Show a plot of the generated distribution
            mainPanel(
               plotOutput("distPlot"),
               DT::dataTableOutput("view")
               
            )
        )
    ),
    tabPanel("Subject Area",
             
             # Application title
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(selectInput("Variable2", 
                                          label = "Y-axis Variable:",
                                          choices = yvar),
                              sliderInput("range2", "Year Range", min = 2010, max = 2025, value = c(2012,current_year), sep = ""),
                              checkboxGroupInput("filter2", label = "Level", choiceNames = filter, choiceValues = filter, selected = filter),
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

    output$distPlot <- renderPlot({
        # generate subsets based on user input
        dat <- TPMsubtransformed[TPMsubtransformed$Category %in% input$filter & TPMsubtransformed$Region %in% input$region,] 
        x <- dat$Year
        y <- dat[,input$Variable]
        r <- dat[,"Region"]
        min <- input$range[1]
        max <- input$range[2]
        current_year <- current_year
        
        # draw the line graph
        if (input$Variable == "Number of Teachers"| input$Variable == "Number of New Teachers" | input$Variable == "Number of Leavers") {
            ggplot(data = NULL) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = sum, geom = "line", size  = .7) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = sum, geom = "point", size  = 2) +
                xlim(low = min, high = max) +
                labs(color = input$subset) +
                ylab(input$Variable) +
                xlab("Year") +
                theme(legend.position="top")
        } else {
            ggplot(data = NULL) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = mean, geom = "line", size  = .7) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = mean, geom = "point", size  = 2) +
                xlim(low = min, high = max) +
                labs(color = input$subset) +
                ylab(input$Variable) +
                xlab("Year") +
                theme(legend.position="top")
        }
        
        
        
    }) 
    
    output$distPlot2 <- renderPlot({
        # generate subsets based on user input
        dat <- TPMsubtransformed[TPMsubtransformed$Category %in% input$filter2 & TPMsubtransformed$`Subject Area` %in% input$subject,] 
        x <- dat$Year
        y <- dat[,input$Variable2]
        r <- dat[,"Subject Area"]
        min <- input$range2[1]
        max <- input$range2[2]
        current_year <- current_year
        
        # draw the line graph
        if (input$Variable2 == "Number of Teachers"| input$Variable2 == "Number of New Teachers" | input$Variable2 == "Number of Leavers") {
            ggplot(data = NULL) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = sum, geom = "line", size  = .7) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = sum, geom = "point", size  = 2) +
                xlim(low = min, high = max) +
                labs(color = input$subset) +
                ylab(input$Variable2) +
                xlab("Year") + 
                theme(legend.position="top")
        } else {
            ggplot(data = NULL) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = mean, geom = "line", size  = .7) + 
                stat_summary(aes(x = x, y = y, group = r, color = r),fun.y = mean, geom = "point", size  = 2) +
                xlim(low = min, high = max) +
                labs(color = input$subset) +
                ylab(input$Variable2) +
                xlab("Year") +
                theme(legend.position="top")
        }
        
        
        
    }) 
    
    output$view <- DT::renderDataTable({
        if(input$Variable == "Number of Teachers"| input$Variable == "Number of New Teachers" | input$Variable == "Number of Leavers"){
            DT::datatable(TPMsubtransformed[,c("Year","Region","Category",input$Variable)] %>%
                              filter(Category %in% input$filter, Region %in% input$region, Year %in% input$range[1]:input$range[2]) %>%
                              group_by(Year, Category, Region) %>%
                              summarise_all(.funs = sum, na.rm = TRUE))
        } else if(input$Variable == "Attrition"){
            DT::datatable(TPMsubtransformed[,c("Year","Region","Category", "Number of Teachers", "Number of Leavers",input$Variable)] %>%
                              filter(Category %in% input$filter, Region %in% input$region, Year %in% input$range[1]:input$range[2]) %>%
                              group_by(Year, Category, Region)  %>%
                              summarise(a = sum(`Number of Teachers`, na.rm = TRUE), b = sum(`Number of Leavers`, na.rm = TRUE)) %>%
                              mutate(Attrition = b/a))
        } else if(input$Variable == "Student/Teacher Ratio"){
            DT::datatable(TPMsubtransformed[,c("Year","Region","Category", "Number of Teachers", "enrollment_count",input$Variable)] %>%
                              filter(Category %in% input$filter, Region %in% input$region, Year %in% input$range[1]:input$range[2]) %>%
                              group_by(Year, Category, Region)  %>%
                              summarise(a = sum(`Number of Teachers`, na.rm = TRUE), b = sum(enrollment_count, na.rm = TRUE)) %>%
                              mutate(`Student/Teacher Ratio` = a/b))
        } else if(input$Variable == "Not Fully Certified Rate"){
            DT::datatable(TPMsubtransformed[,c("Year","Region","Category", "Number of Teachers", "enrollment_count","Number Not Certified",input$Variable)] %>%
                              filter(Category %in% input$filter, Region %in% input$region, Year %in% input$range[1]:input$range[2]) %>%
                              group_by(Year, Category, Region)  %>%
                              summarise(a = sum(`Number of Teachers`, na.rm = TRUE), b = sum(`Number Not Certified`, na.rm = TRUE)) %>%
                              mutate(`Not Fully Certified Rate` = b/a))
        }
        
        
        
    })
    
    output$view_sub <- DT::renderDataTable({
        if(input$Variable2 == "Number of Teachers"| input$Variable2 == "Number of New Teachers" | input$Variable2 == "Number of Leavers"){
            DT::datatable(TPMsubtransformed[,c("Year","Subject Area","Category",input$Variable2)] %>%
                              filter(Category %in% input$filter2, TPMsubtransformed$`Subject Area` %in% input$subject, Year %in% input$range2[1]:input$range2[2]) %>%
                              group_by(Year, Category, `Subject Area`) %>%
                              summarise_all(.funs = sum, na.rm = TRUE))
        } else if(input$Variable == "Attrition"){
            DT::datatable(TPMsubtransformed[,c("Year","Subject Area","Category", "Number of Teachers", "Number of Leavers", input$Variable2)] %>%
                              filter(Category %in% input$filter2, TPMsubtransformed$`Subject Area` %in% input$subject, Year %in% input$range2[1]:input$range2[2]) %>%
                              group_by(Year, Category, `Subject Area`) %>%
                              summarise(a = sum(`Number of Teachers`, na.rm = TRUE), b = sum(`Number of Leavers`, na.rm = TRUE)) %>%
                              mutate(Attrition = b/a))
        } else if(input$Variable == "Student/Teacher Ratio"){
            DT::datatable(TPMsubtransformed[,c("Year","Subject Area","Category", "Number of Teachers", "enrollment_count", input$Variable2)] %>%
                              filter(Category %in% input$filter2, TPMsubtransformed$`Subject Area` %in% input$subject, Year %in% input$range2[1]:input$range2[2]) %>%
                              group_by(Year, Category, `Subject Area`) %>%
                              summarise(a = sum(`Number of Teachers`, na.rm = TRUE), b = sum(enrollment_count, na.rm = TRUE)) %>%
                              mutate(`Student/Teacher Ratio` = a/b))
        }
        
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
