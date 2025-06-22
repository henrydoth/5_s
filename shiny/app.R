# 📦 Tải thư viện
library(shiny)
library(ggplot2)
library(dplyr)
library(flextable)
library(DT)

# 📂 Nạp dữ liệu & object phân tích (trỏ tới thư mục r/)
source("packages.R")
source("00_setup.R")
source("01_load_data.R")
source("so_lan_thuc_hien_ve_sinh_24_h.R")
source("dieu_kien_chung.R")
source("khia_canh_san_nha_ve_sinh.R")
source("khia_canh_thiet_bi_ve_sinh.R")
source("khia_canh_vat_dung_trong_nha_ve_sinh.R")
source("khoa_phong_so_lan_thuc_hien_ve_sinh_24_h.R")

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
             uiOutput("tbl_dk"),
             plotOutput("plot_dk")
           ),
           "Sàn nhà vệ sinh" = tagList(
             uiOutput("tbl_san"),
             plotOutput("plot_san")
           ),
           "Thiết bị vệ sinh" = tagList(
             uiOutput("tbl_tb"),
             plotOutput("plot_tb")
           ),
           "Vật dụng trong nhà vệ sinh" = tagList(
             uiOutput("tbl_vat"),
             plotOutput("plot_vat")
           ),
           "Theo khoa phòng" = tagList(
             uiOutput("tbl_khoa"),
             plotOutput("plot_khoa")
           )
    )
  })
  
  # ✅ Bảng sử dụng flextable → htmltools_value → renderUI
  output$tbl_dk <- renderUI({ htmltools_value(tbl_dk_chung) })
  output$tbl_san <- renderUI({ htmltools_value(tbl_san_nvs) })
  output$tbl_tb <- renderUI({ htmltools_value(tbl_tbvs) })
  output$tbl_vat <- renderUI({ htmltools_value(tbl_vat_dung) })
  output$tbl_khoa <- renderUI({ htmltools_value(tbl_vs_khoa) })
  
  # Biểu đồ tương ứng
  output$plot_vs  <- renderPlot(plot_vs_24h)
  output$plot_dk  <- renderPlot(plot_dk_chung)
  output$plot_san <- renderPlot(plot_san_nvs)
  output$plot_tb  <- renderPlot(plot_tbvs)
  output$plot_vat <- renderPlot(plot_vat_dung)
  output$plot_khoa <- renderPlot(plot_bang_huong_dan_rua_tay_high)
  
  # Riêng bảng số lần vệ sinh giữ lại DT::datatable vì đó là data.frame
  output$tbl_vs <- renderDT(as.data.frame(summary_wide))
}

# 🚀 Chạy app
shinyApp(ui, server)
