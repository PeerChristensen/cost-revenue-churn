
library(shiny)
library(tidyverse)
library(scales)
library(shinythemes)
library(plotly)

preds <- read_csv("predictions.csv") %>%
    mutate_if(is.character,factor)

ks <- function (x) { number_format(accuracy = 1,
                                   scale    = 1/1000,
                                   suffix   = "k",
                                   big.mark = ",")(x) }

theme_set(theme_minimal() +
              theme(plot.background  = element_rect(fill="black"),
                    panel.background = element_rect(fill="black"),
                    panel.grid       = element_line(colour="grey20"),
                    strip.text       = element_text(colour="snow",size = 14),
                    axis.text        = element_text(colour="snow",size=14),
                    axis.title       = element_text(colour="snow",size=14),
                    legend.text      = element_text(colour="snow"),
                    legend.title     = element_text(colour="snow"))
)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
                tags$style(HTML(".tabbable > .nav > li   > a {border-color: #dc3912; background-color: black; width:100px}
    .tabbable > .nav > li[class=active]    > a {background-color: #dc3912}
  ")),

    # Application title
    titlePanel("Cost-Revenue Calculation for Churn Prevention",windowTitle="Churn Cost-Revenue"),br(),
    tabsetPanel(
        tabPanel(h5("App"), fluid = TRUE,
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(width=3,
                                  fileInput("file","Select file", accept = ".csv"),
                                  #selectInput("sep",label="Separator",choices = c(",",";")),
                                  radioButtons("sep", "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";"),
                                               selected = ","),
                                  uiOutput("truth_sel"),
                                  uiOutput("churn_prob_sel"),
                                  hr(),
                                  numericInput("discount",label = h5("Discount/Offer"),100),
                                  numericInput("prob_accept",label = h5("Probability of acceptance"),0.5,min = 0, max = 1,step = .1),
                                  hr(),
                                  numericInput("tp_val",label = h5("True Positive Value"),2500),
                                  numericInput("fp_val",label = h5("False Positive Value"),0),
                                  numericInput("tn_val",label = h5("True Negative Value"),0),
                                  numericInput("fn_val",label = h5("False Negative Value"),-2500),
                                  hr(),
                                  fluidRow(
                                      column(12, align="right",
                                  actionButton("go", label = h5("Go"),class = "btn-primary btn-lg",width="25%",
                                               style="background: #dc3912; border-color: #dc3912;display:center-align")
                                      ))
                     ),
                     
                     mainPanel(
                         uiOutput("header") ,
                         div(tableOutput("max_point"), style = "font-size:150%"),
                         hr(),
                         plotlyOutput("plot",height = "600px"), 
                         br()
                         
                     )
                 )
        ),
        tabPanel(h5("Demo"), fluid = TRUE,

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=3,
            numericInput("discount_d",label = h5("Discount/Offer"),100),
            numericInput("prob_accept_d",label = h5("Probability of acceptance"),0.5,min = 0, max = 1,step = .1),
            hr(),
            numericInput("tp_val_d",label = h5("True Positive Value"),2500),
            numericInput("fp_val_d",label = h5("False Positive Value"),0),
            numericInput("tn_val_d",label = h5("True Negative Value"),0),
            numericInput("fn_val_d",label = h5("False Negative Value"),-2500),
            hr(),
            fluidRow(
                column(12, align="right",
            actionButton("go_d", label = h5("Go"),class = "btn-primary btn-lg",width="25%",
                         style="background: #dc3912; border-color: #dc3912")
            ))
        ),

         mainPanel(
             uiOutput("header_d") ,
            div(tableOutput("max_point_d"), style = "font-size:150%"),
            hr(),
            plotlyOutput("plot_d",height = "600px"), 
            br()

        )
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- reactive({
        file <- input$file
        if (is.null(file)) {
            
            return(NULL)
        }
        
        if (input$sep == ",") {
            read_csv(file$datapath) %>%
                mutate_if(is.character,factor) 
            
        }
        else if (input$sep == ";") {
            read_csv2(file$datapath) %>%
                mutate_if(is.character,factor)
        }
    })

    output$truth_sel <- renderUI({
        cols <- names(df())
        selectInput("truth", h5("True class column (Possible values are 'Yes' / 'No')"),choices=cols)  
        
    })
    
    output$churn_prob_sel <- renderUI({
        cols <- names(df())
        selectInput("churn_prob", h5("Churn probability column"),choices=cols)  
        
    })
    
    observeEvent(input$go, {
        if (is.null(input$file)) {
            return(NULL)
        }
        # update values
        tp_val <- input$tp_val - input$discount
        fp_val <- input$fp_val - input$discount
        tn_val <- input$tn_val
        fn_val <- input$fn_val
        
        # calculate
        results <- NULL
        for (i in seq(0,1,0.01)) {
            
            preds_i <- df() %>% mutate(predict = if_else(get(input$churn_prob) > i,"Yes","No"))
          
            tp <- preds_i %>% filter(predict == "Yes" & get(input$truth) == "Yes") %>% nrow()
            fp <- preds_i %>% filter(predict == "Yes" & get(input$truth) == "No")  %>% nrow()
            tn <- preds_i %>% filter(predict == "No"  & get(input$truth) == "No")  %>% nrow()
            fn <- preds_i %>% filter(predict == "No"  & get(input$truth) == "Yes") %>% nrow()
         
            payoff <- ((tp * tp_val) * input$prob_accept) + ((fp * fp_val) * input$prob_accept) + (fn * fn_val) + (tn * tn_val)
            
            result <- tibble(threshold = i, payoff,tp,fp,tn,fn)
            
            results <- rbind(results,result)
        }
        max_point <- results %>% 
            filter(payoff == max(payoff)) %>%
            rename(Threshold         = threshold,
                   Revenue           = payoff,
                   `True positives`  = tp,
                   `False positives` = fp,
                   `True negatives`  = tn,
                   `False negatives` = fn) 
        
        output$max_point <- renderTable({
            
            max_point$Revenue = format(max_point$Revenue,0)
            max_point
        }, hover=T, striped = T)
        
        output$header <- renderUI({
            h3("Highest Revenue")
            
        })
        
        output$plot <- renderPlotly({
            
            ggplotly(
                results %>%
                    ggplot(aes(x=threshold,y=payoff)) +
                    geom_line(color="#dc3912",size = 1) +
                    scale_y_continuous(labels = ks) +
                    labs(x = "Threshold", y = "Revenue"))
            
        })
        
    })
    
    observeEvent(input$go_d, {
    
    # update values
        tp_val_d <- input$tp_val_d - input$discount_d
        fp_val_d <- input$fp_val_d - input$discount_d
        tn_val_d <- input$tn_val_d
        fn_val_d <- input$fn_val_d
        
    # calculate
        results <- NULL
        for (i in seq(0,1,0.01)) {
            
            preds_i <- preds %>% mutate(predict = if_else(Yes > i,"Yes","No"))
            
            tp <- preds_i %>% filter(predict == "Yes" & Churn == "Yes") %>% nrow()
            fp <- preds_i %>% filter(predict == "Yes" & Churn == "No")  %>% nrow()
            tn <- preds_i %>% filter(predict == "No"  & Churn == "No")  %>% nrow()
            fn <- preds_i %>% filter(predict == "No"  & Churn == "Yes") %>% nrow()
            
            payoff <- ((tp * tp_val_d) * input$prob_accept_d) + ((fp * fp_val_d) * input$prob_accept_d) + (fn * fn_val_d) + (tn * tn_val_d)
            
            result <- tibble(threshold = i, payoff,tp,fp,tn,fn)
            
            results <- rbind(results,result)
        }
        
        max_point <- results %>% 
            filter(payoff == max(payoff)) %>%
            rename(Threshold         = threshold,
                   Revenue           = payoff,
                   `True positives`  = tp,
                   `False positives` = fp,
                   `True negatives`  = tn,
                   `False negatives` = fn) 
            
        
        output$max_point_d <- renderTable({
            
            max_point$Revenue = format(max_point$Revenue,0)
            max_point
            }, hover=T, striped = T)
        
        output$header_d <- renderUI({
            h3("Highest Revenue")
            
        })
        
    output$plot_d <- renderPlotly({
        
        ggplotly(
        results %>%
            ggplot(aes(x=threshold,y=payoff)) +
            geom_line(color="#dc3912",size = 1) +
            scale_y_continuous(labels = ks) +
            labs(x = "Threshold", y = "Revenue"))
       
    })
    
})
}

# Run the application 
shinyApp(ui = ui, server = server)
