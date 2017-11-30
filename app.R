

library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(formattable)


devtools::install_github("rosseji/shiny.semantic@develop")
devtools::install_github("trendlock/SAFEnote")

library(shiny.semantic)
library(SAFEnote)







Threshold <- 200000
Discount <- .2
SAFE.Investment <- 50000

df_assume <- tibble(What = c("Threshold", "Discount", "SAFE.Investment"),
             `How Much` = c("$200K", "20%", "$50k"))

ui <- fluidPage(
  semanticPage(
    title = "SAFENote Simulator",


    br(),
    h1(class = "ui header", "SAFENote Simulator",
       div(class = "sub header", "By TRENDLOCK")),
    div(class = "ui divider"),
    div(class = "ui grid",
        div(class = "four wide column",
            uicard(
              div(class = "ui horizontal divider", uiicon("tag"), "Non-Adjustable Assumptions"),

              align = "center",
              formattableOutput("assume", width = "70%")
            ),
            uisegment(
              div(class = "ui horizontal divider", uiicon("tag"), "Adjustable Inputs"),
              sliderInput("Cap", "Cap", min = 0, max = 10, value = 2, pre = "$", post = "M"),
              sliderInput("Existing.no.shareholders", "Existing No of Shareholders", min = 0, max = 8, value = 3, post = " Shareholders"),
              textInput("Existing.no.shares.issued", "Existing No of Shares Issued",  value = 1000),
              sliderInput("Pre.cash.valuation", "Pre Cash Valuation", min = 0, max = 10, value = 1, step = 0.1, pre = "$", post = "M"),
              sliderInput("New.Investment", "New Investment", min = 0, max = 4, value = .25, step = 0.05, pre = "$", post = "M")

              )
            ),
        div(class = "eight wide column",

            div(class = "ui horizontal divider", uiicon("tag"), "Shares on Issue"),
            plotlyOutput("on_issue_plot") %>% withSpinner(),
            div(class = "ui horizontal divider", uiicon("tag"), "Percentage of Equity Issued"),
            plotlyOutput("main_plot") %>% withSpinner()


            ),
        div(class = "four wide column",
            uisegment(
              div(class = "ui horizontal divider", uiicon("tag"), "All Values"),
              align = "center",
              formattableOutput("inputs_tbl", width = "70%")
            )

        )),
    div(class = "ui grid",
        div(class = "four wide column",
            div(class = "ui horizontal divider", uiicon("tag"), "Pre-Raise Details"),
            align = "center",
            downloadButton("csv_preraise"),
            formattableOutput("preraise")
            
            ),
        div(class = "four wide column",
            div(class = "ui horizontal divider", uiicon("tag"), "Post-Raise Details"),
            align = "center",
            downloadButton("csv_postraise"),
            formattableOutput("postraise")
            
        )
        )
        
    )

)

server <- function(input, output) {


  output$assume <- renderFormattable(
    formattable(df_assume)
  )


  output$main_plot <- renderPlotly({


    Cap <- input$Cap * 1000000
    Existing.no.shareholders <- input$Existing.no.shareholders
    Existing.no.shares.issued <- input$Existing.no.shares.issued %>% as.numeric()
    Pre.cash.valuation <- input$Pre.cash.valuation * 1000000
    New.Investment <-  input$New.Investment * 1000000


    preraise_table <- lister(number = Existing.no.shareholders)
    preraise_table <- preraise_table %>%
      mutate(shares = round(Existing.no.shares.issued/Existing.no.shareholders),
             percent = shares/Existing.no.shares.issued*100)

    #### A few more calcs #########
    price.per.share <- Pre.cash.valuation/Existing.no.shares.issued
    shares.issued <- round(New.Investment/price.per.share)
    SAFE.triggered <- if_else(New.Investment >= Threshold, TRUE, FALSE)
    Discount.Price <- price.per.share*(1-Discount)
    SAFE.Price <- Cap/Existing.no.shares.issued
    Cap.triggered <- if_else(SAFE.Investment/SAFE.Price > SAFE.Investment/Discount.Price, TRUE, FALSE)
    Westpac.Shares <- round(max(SAFE.Investment/Discount.Price, SAFE.Investment/SAFE.Price))*SAFE.triggered
    Total.Shares.Post.Raise <- sum(preraise_table$shares) + shares.issued + Westpac.Shares

    ls <- list(

      list(name = "Number of Existing Shareholders",  val = Existing.no.shareholders),
      list(name = "Fully-diluted number of shares pre-raise",  val = Existing.no.shares.issued),
      list(name = "Pre-Cash Valuation",  val = Pre.cash.valuation),
      list(name = "Valuation Cap",  val = Cap),
      list(name = "New Investment Amount",  val = New.Investment),
      list(name = "Standard Price Per Share",  val = round(price.per.share, digits = 3)),
      list(name = "Number of Shares issue to new investor",  val = shares.issued),
      list(name = "Was the SAFE note triggered?",  val = SAFE.triggered),
      list(name = "Discounted Share Price",  val = round(Discount.Price, digits = 3)),
      list(name = "SAFE Price (from Cap)",  val = round(SAFE.Price, digits = 3)),
      list(name = "Was the Cap triggered?",  val = Cap.triggered),
      list(name = "Number of Shares Issued to Westpac",  val = Westpac.Shares),
      list(name = "Total Shares on Issued post-raise",  val = Total.Shares.Post.Raise)
    )

    output$inputs_tbl <-  renderFormattable({
      ls %>%
        map( ~ enframe(.x[["val"]]) %>%
               mutate(name = .x[["name"]],
                      value = as.character(value))) %>%
        bind_rows() %>%
        `colnames<-`(c("What", "Info")) %>%
        formattable()
    })


    ##### Building basic df for New Investor  #####
    Shareholder <- c("New Investor")
    shares <- c(shares.issued)
    New.Investor <- tibble(Shareholder, shares)


    ######B Building basic df for Westpac  #########
    Shareholder <- c("Westpac")
    shares <- c(Westpac.Shares)
    Westpac <- tibble(Shareholder, shares)

    ##########  The Post-Raise Equity Table   #####
    postraise_table <- preraise_table %>%
      select(Shareholder, shares) %>%
      bind_rows(New.Investor, Westpac) %>%
      mutate(percent = shares/Total.Shares.Post.Raise*100)

    ######  Making the data tidy #########
    preraise_table. <- preraise_table %>%
      mutate(stage = "Pre-Raise")
    
    output$preraise <- renderFormattable({
      formattable(preraise_table.)
    })
    
    output$csv_preraise <- downloadHandler(
      filename <- function() {
        paste('preraise-tbl-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write_excel_csv(drop_read_csv(preraise_table.), con)
      })
    
    
    postraise_table. <- postraise_table %>%
      mutate(stage = "Post-Raise")
    
    
    output$postraise <- renderFormattable({
      formattable(postraise_table)
    })
    
    
    output$csv_postraise <- downloadHandler(
      filename <- function() {
        paste('postraise-tbl-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write_excel_csv(drop_read_csv(postraise_table.), con)
      })

    tidy_table <- bind_rows(preraise_table., postraise_table.)
    tidy_table$stage <- factor(tidy_table$stage, levels = c("Pre-Raise", "Post-Raise"))



    output$on_issue_plot <- renderPlotly({
      p <- ggplot(tidy_table, aes(x = stage, y = shares, fill = Shareholder))+
        geom_bar(stat = "identity")
        # labs(title = "Shares on Issue")
      ggplotly(p)
    })


    p <- ggplot(tidy_table, aes(x = stage, y = percent, fill = Shareholder))+
      geom_bar(stat = "identity")
      # labs(title = "Percentage of Equity Issued")
    ggplotly(p)

  })




}

shinyApp(ui = ui, server = server)

