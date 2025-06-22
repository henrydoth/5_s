library(shiny)
library(ggplot2)
library(dplyr)

# Đảm bảo dữ liệu tối thiểu có sẵn trong app
data <- data.frame(
  Tieu_chi = c("Khô ráo", "Bong tróc", "Sạch", "Trơn", "Rác"),
  Ty_le = c(61.2, 46.8, 35.1, 35.1, 29.3)
)

ui <- fluidPage(
  titlePanel("CẢI TIẾN CHẤT LƯỢNG NHÀ VỆ SINH - BV 30-4"),
  sidebarLayout(
    sidebarPanel(
      selectInput("noi_dung", "Chọn nội dung:", choices = c("Sàn nhà vệ sinh"))
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(data, aes(x = reorder(Tieu_chi, Ty_le), y = Ty_le)) +
      geom_col(fill = "#1a9641") +
      coord_flip() +
      geom_text(aes(label = paste0(Ty_le, "%")), hjust = 1.1, color = "white") +
      labs(x = "Tiêu chí", y = "Tỷ lệ đạt (%)", title = "Sàn nhà vệ sinh") +
      theme_minimal()
  })
}

shinyApp(ui, server)
