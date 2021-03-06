library(shiny)
library(makeup)
library(homodatum)
library(hgchmagic)

# options(rsconnect.locale = 'es_ES.UTF-8')

# system("locale -a", intern = TRUE)

dat <- homodatum:::Dat("04/21?2020?")
makeup(as.Date(dat), locale = "de-DE")
# makeup(as.Date(dat), locale = "ru-RU")

#Sys.setlocale("LC_ALL", locale = "ru_RU.UTF-8")
#Sys.setlocale("LC_ALL", locale = "russian")
#Sys.setlocale("LC_CTYPE", locale = "russian")

#Sys.setlocale("LC_ALL", "C")
#Sys.setlocale("LC_ALL", "en_US.UTF-8")

#makeup::guess_date_fmt("2020-05-21", locale = "ru-RU")
#makeup::guess_date_fmt("2020-05-21", locale = "es-ES")

ui <- fluidPage(
  column(4,
         selectInput("locale", "Locale", choices = c("ru-RU","en-US", "es-ES","de-DE")),
         dateInput("date", "Some date", value = "2020-04-21"),
         verbatimTextOutput("formatted_date"),
         textInput("custom_date", "Some date", value = "04/21?2020?"),
         verbatimTextOutput("formatted_custom_date")
  ),
  column(8,
         highchartOutput("chart"),
         verbatimTextOutput("debug")
  )
)

d1 <- data.frame(fecha = c("2020/05/03","2020/05/04"),
                 vals = 1:2)


server <- function(input, output, session){

  output$formatted_date <- renderPrint({
    makeup(as.Date(input$date), locale = input$locale)
  })

  output$formatted_custom_date <- renderPrint({
    dat <- homodatum:::Dat(input$custom_date)
    makeup(as.Date(dat), locale = input$locale, format = "%B %d %Y")
  })

  output$chart <- renderHighchart({
    hgch_line_DatNum(d1, locale = input$locale, format_dat = "%B %d %Y")
  })

  output$debug <- renderPrint({
    d1
  })

}

shinyApp(ui, server)
