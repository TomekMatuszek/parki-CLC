library(shiny)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(ggplot2)

path = tempfile(fileext = ".xlsx")
download.file("https://www.dropbox.com/s/ot8y5sfx0h7i09d/parki_CLC.xlsx?dl=1", destfile = path, mode = "wb")
parki_CLC = read_excel(path)
parki_nazwy = sort(unique(parki_CLC$nazwa))

path = tempfile(fileext = ".xlsx")
download.file("https://www.dropbox.com/s/dvfcyxv027ks6s0/clc_legenda.xlsx?dl=1", destfile = path, mode = "wb")
CLC_legenda = read_excel(path)

CLC_legenda$CODE_18 = as.character(CLC_legenda$CODE_18)

ui = verticalLayout(
  tags$head(
    tags$style("body{height:500px; width:800px; margin:auto}")),
  span(titlePanel("Pokrycie terenu polskich parków narodowych"), 
       style = "font-size:150%;text-align:center;color:#dddddd"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type = "text/css", "label{display:table-cell;vertical-align:middle;;text-align:center;padding-right:20px;padding-left:70px} .selectize-input{display:table-cell; width:400px; vertical-align:middle} .form-group{display:table-row}")),
      selectInput(inputId = "park", label = "wybierz park narodowy: ", choices = parki_nazwy), 
                  style = "font-size:150%;color:#dddddd;text-align:center"),
    mainPanel(plotOutput(outputId = "wykres"))
  ),
  setBackgroundColor("#2e3039")
)

server = function(input, output){
  pow_parkow = group_by(parki_CLC, nazwa) %>% summarise(pow = sum(area))
  parki_CLC = inner_join(parki_CLC, pow_parkow, by = "nazwa")
  parki_CLC = inner_join(parki_CLC, CLC_legenda, by = "CODE_18")
  parki_CLC$legenda = paste0(parki_CLC$CLC, " [", round(parki_CLC$area / parki_CLC$pow * 100, 2), "%]")
  
  parki_CLC$hex = sapply(strsplit(parki_CLC$RGB, "-"), function(kolory) rgb(kolory[[1]], kolory[[2]], kolory[[3]], maxColorValue = 255))
  
  output$wykres = renderPlot(
    {
      cols = as.vector(parki_CLC[parki_CLC$nazwa == input$park, ])
      cols = cols[order(cols$CLC),]$hex
      ggplot(parki_CLC[parki_CLC$nazwa == input$park, ], aes(x = "", y = area / pow, fill = legenda)) +
        geom_bar(stat = "identity", color = "#2e3039") +
        #labs(title = paste(input$park, "- pokrycie terenu")) +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = cols) +
        guides(fill = guide_legend(ncol = 2)) +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_rect(fill = "#2e3039", color = NA),
              panel.background = element_rect(fill = "#2e3039"),
              panel.grid = element_blank(),
              plot.title = element_text(color = "#dddddd", hjust = 1),
              legend.position = "bottom",
              legend.direction = "vertical",
              legend.title = element_blank(),
              legend.text = element_text(color = "#dddddd", size = 12),
              legend.background = element_rect(fill = "#2e3039"),
              legend.key = element_blank()
              )
    }, bg="transparent"
  )
}

shinyApp(ui = ui, server = server)
