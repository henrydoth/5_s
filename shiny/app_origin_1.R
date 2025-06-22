# 📦 Tải thư viện
library(shiny)
library(ggplot2)
library(dplyr)
library(flextable)
library(DT)
library(here)

# 📂 Nạp dữ liệu & object phân tích
source(here("R", "packages.R"))
source(here("R", "00_setup.R"))
source(here("R", "01_load_data.R"))
source(here("R", "so_lan_thuc_hien_ve_sinh_24_h.R"))
source(here("R", "dieu_kien_chung.R"))
source(here("R", "khia_canh_san_nha_ve_sinh.R"))
source(here("R", "khia_canh_thiet_bi_ve_sinh.R"))
source(here("R", "khia_canh_vat_dung_trong_nha_ve_sinh.R"))
source(here("R", "khoa_phong_so_lan_thuc_hien_ve_sinh_24_h.R"))

# 🖼 UI
ui <- fluidPage(
  titlePanel("CẢI TIẾN CHẤT LƯỢNG NHÀ VỆ SINH - BV 30-4"),
  sidebarLayout(
    sidebarPanel(
      selectInput("view", "Chọn nội dung:",
                  choices = c(
                    "Số lần vệ sinh",
                    "Điều kiện chung",
                    "Sàn nhà vệ sinh",
                    "Thiết bị vệ sinh",
                    "Vật dụng trong nhà vệ sinh",
                    "Theo khoa phòng"
                  )
      )
    ),
    mainPanel(
      uiOutput("content_ui")
    )
  )
)

# 💡 Server logic
server <- function(input, output, session) {
  output$content_ui <- renderUI({
    switch(input$view,
           "Số lần vệ sinh" = tagList(
             h4("Bảng số lần vệ sinh 24h"),
             DTOutput("tbl_vs"),
             plotOutput("plot_vs")
           ),
           "Điều kiện chung" = tagList(
             DTOutput("tbl_dk"),
             plotOutput("plot_dk")
           ),
           "Sàn nhà vệ sinh" = tagList(
             DTOutput("tbl_san"),
             plotOutput("plot_san")
           ),
           "Thiết bị vệ sinh" = tagList(
             DTOutput("tbl_tb"),
             plotOutput("plot_tb")
           ),
           "Vật dụng trong nhà vệ sinh" = tagList(
             DTOutput("tbl_vat"),
             plotOutput("plot_vat")
           ),
           "Theo khoa phòng" = tagList(
             DTOutput("tbl_khoa")
           )
    )
  })
  
  # Bảng & biểu đồ tương ứng
  output$tbl_vs   <- renderDT(as.data.frame(summary_wide))
  output$plot_vs  <- renderPlot(plot_vs_24h)
  
  output$tbl_dk   <- renderDT(as.data.frame(tbl_dk_chung))
  output$plot_dk  <- renderPlot(plot_dk_chung)
  
  output$tbl_san  <- renderDT(as.data.frame(tbl_san_nvs))
  output$plot_san <- renderPlot(plot_san_nvs)
  
  output$tbl_tb   <- renderDT(as.data.frame(tbl_tbvs))
  output$plot_tb  <- renderPlot(plot_tbvs)
  
  output$tbl_vat  <- renderDT(as.data.frame(tbl_vat_dung))
  output$plot_vat <- renderPlot(plot_vat_dung)
  
  output$tbl_khoa <- renderDT(as.data.frame(tbl_vs_khoa))
}

# 🚀 Chạy app
shinyApp(ui, server)
