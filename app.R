#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(shinyalert)
library(broom)
playoff <- read_csv("./playoff.csv")
playoff <- playoff %>%
    mutate(Conference = as.factor(Conference)) %>% 
    rename(GamesPlayed = "GamesPlayed",
           Wins="Wins",
           PowerRating = "PowerRating",
           EFGOffense = "EFG_O",
           EFGDefense = "EFG_D",
           TurnOverRate = "TurnoverRate",
           StealRate = "StealRate",
           ORB = "ORB",
           DRB = "DRB",
           FTR = "FTR",
           FTRA = "FTRA",
           TwoPointsPercent = "Two_Point_Percent",
           TwoPointsPercentAllowed = "Two_Point_Percent_Allowed",
           ThreePointsPercent = "Three_Point_Percent",
           ThreePointPercentAllowed = "Three_Point_Percent_Allowed",
           AdjustedTempo = "Adjusted_Tempo")

playoffNew <- playoff %>% 
    select(GamesPlayed,
           Wins,
           PowerRating,
           EFGOffense,
           EFGDefense,
           TurnOverRate,
           StealRate,
           ORB,
           DRB,
           FTR,
           FTRA,
           TwoPointsPercent,
           TwoPointsPercentAllowed,
           ThreePointsPercent,
           ThreePointPercentAllowed,
           AdjustedTempo,
           AdjustedOffensiveEfficiency ,
           AdjustedDefensiveEfficiency, 
           WinPercentage, 
           POSTSEASON) 

playoffcat <- playoff %>% 
    select(PowerRating,
           EFGOffense,
           EFGDefense,
           TurnOverRate,
           StealRate,
           ORB,
           DRB,
           FTR,
           FTRA,
           TwoPointsPercent,
           TwoPointsPercentAllowed,
           ThreePointsPercent,
           ThreePointPercentAllowed,
           AdjustedTempo,
           AdjustedOffensiveEfficiency ,
           AdjustedDefensiveEfficiency) 

playoffWinPct <- playoff %>% 
    select(WinPercentage) 

ui <- fluidPage(

    useShinyalert(),
    
    titlePanel(title = div(img(src ="MarchMadness.png", align = "left"))), 
    
    sidebarLayout(
        sidebarPanel(
        
        selectInput("conference",
                    "Conference:", 
                    levels(playoff$Conference)),
        
        checkboxGroupInput("var", 
                           "Choose Variables for Data Table:", 
                           choices = c(colnames(playoff)),
                           selected =  "TEAM"),
        
        selectInput("X",
                    "Choose X Variable for Linear Regression Plot:",
                    choices = c(colnames(playoffcat)),
                    selected = "ORB"),
        
        selectInput("varX",
                    "Choose X Variable for Regression Output",
                    choices = c(colnames(playoffNew))),
        
        selectInput("varY",
                    "Choose Y Variable for Regression Output",
                    choices = c(colnames(playoffWinPct))),
        
        selectInput("Playoff", 
                    "Choose X Variable for Boxplots:", 
                    choices = c(colnames(playoffNew))),
                    
        checkboxGroupInput(
                        "POSTSEASON",
                        "Postseason Group Reached for Boxplots:",
                        choices = c("2ND" = "2nd",
                                    "Champions" = "champions",
                                    "E8" = "E8",
                                    "F4" = "F4",
                                    "R32" = "R32",
                                    "R64" = "R64",
                                    "R68" = "R68",
                                    "S16" = "S16"),
                        inline = TRUE,
                        selected = "2ND")
                    
        
        
        ),
    
        mainPanel(dataTableOutput("table"), 
                  plotOutput("myplot"),
                  tableOutput("regs"), 
                  plotOutput("distPlot"),
                  verbatimTextOutput("text"))
    ) 
)

server <- function(input, output) {
    
newplayoff <- reactive({filter(playoff, Conference == input$conference) %>% select(input$var)}) 

new_playoff <- reactive({filter(playoff, Conference == input$conference)})


shinyalert(
    "Enter your Name", type = "input",
    imageUrl = "MarchMadness.png", imageWidth = 200, imageHeight = 200,
    callbackR = function(x) { message("Hi ", x, "! Welcome to March Madness!") },
    callbackJS = "function(x) { alert('Hi ' + x + '! Welcome to March Madness!'); }"
)


output$myplot <- renderPlot({
    ggplot(new_playoff(),aes_string(x = input$X, y = "WinPercentage")) + geom_point(size = 3) + geom_smooth(method = lm, se = FALSE) + 
        geom_text(aes(label=TEAM), nudge_x = .5, check_overlap = TRUE) +
        ggtitle("Linear Regression Plot")
})


output$table <- renderDataTable(
    newplayoff())

output$regs <- renderTable({
    
    my_formula <- as.formula(paste(input$varY, "~", input$varX))
    
    sim1 <- lm(my_formula, data = playoffNew)
    
    tidy(sim1)
    
},
caption = "Linear Regression Model Output",
caption.placement = getOption("xtable.caption.placement", "top"), 
caption.width = getOption("xtable.caption.width", NULL))

output$text <- renderText({
    sprintf("You have selected %s on postseason :)\n",input$POSTSEASON)
})

output$distPlot <- renderPlot({
    playoffNew %>%
        mutate(select_val = POSTSEASON %in% input$POSTSEASON
        ) %>%
        ggplot(aes_string(x="POSTSEASON", y = input$Playoff, fill = "select_val")) + 
        geom_boxplot(color = "black")+ 
        ggtitle("Postseason Boxplot")+
        guides(fill= FALSE)+
        geom_point(aes( fill=POSTSEASON ),binaxis = "y",  method="histodot",stackdir = "center", binwidth=0.1, dotsize=1.75)
})

    
}

shinyApp(ui = ui, server = server)