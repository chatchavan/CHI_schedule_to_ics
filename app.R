library(shiny)
library(readr)
# library(shinyjs)


ui <- fluidPage(
    titlePanel("Convert your exported CHI 2021 schedule to iCal"),
    fixedPage(
        selectInput("timezone", "Choose target time zone:",
                    choices = OlsonNames()),
        fileInput("file1", "Choose CSV File exported from the CHI program",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        selectInput("ics", "Choose .ics to download:",
                choices = NA),
        downloadButton("download", "Download")
    )
)


server <- function(input, output) {

    globals <- reactiveValues()
    
    observeEvent(input$file1, {
        
        # process data
        df_target_ <- 
            tidy_schedule(input$file1$datapath, input$timezone) %>% 
            mutate(start_ymd = str_c(year(start_date_target), month(start_date_target), day(start_date_target), sep = "-"))
        
        # update the ics selection
        choices_ymd <- 
            df_target_ %>%   
            pull(start_ymd) %>% 
            unique()
        
        updateSelectInput(inputId = "ics", choices = c("all", choices_ymd))
        
        # make the results available for further controls
        globals$df_target <- df_target_
    })
    
    # file download
    output$download <- downloadHandler(
        filename = function() {
            paste(input$ics, ".ics", sep = "")
        },
        content = function(file) {
            df_target_ <- globals$df_target
            
            if (input$ics != "all")
                df_target_ <-  
                    df_target_ %>% 
                    filter(start_ymd == input$ics)
            
            write_ics(df_target_, file)
        }
    )
}

# Run the application 
shinyApp(ui, server = server)
