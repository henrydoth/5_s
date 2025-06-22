# 📦 Tải thư viện
library(shiny)
library(shinydashboard)
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

# 🖼 UI với shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "BV 30-4: Cải tiến vệ sinh"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Số lần vệ sinh", tabName = "vs", icon = icon("calendar-check")),
      menuItem("Điều kiện chung", tabName = "dk", icon = icon("clipboard-check")),
      menuItem("Sàn nhà vệ sinh", tabName = "san", icon = icon("border-all")),
      menuItem("Thiết bị vệ sinh", tabName = "tb", icon = icon("tools")),
      menuItem("Vật dụng trong nhà vệ sinh", tabName = "vat", icon = icon("soap")),
      menuItem("Theo khoa phòng", tabName = "khoa", icon = icon("hospital-user"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("vs",
              h3("Bảng số lần vệ sinh trong 24 giờ"),
              DTOutput("tbl_vs"),
              br(),
              plotOutput("plot_vs")
      ),
      tabItem("dk",
              h3("Điều kiện chung"),
              uiOutput("tbl_dk"),
              br(),
              plotOutput("plot_dk")
      ),
      tabItem("san",
              h3("Sàn nhà vệ sinh"),
              uiOutput("tbl_san"),
              br(),
              plotOutput("plot_san")
      ),
      tabItem("tb",
              h3("Thiết bị vệ sinh"),
              uiOutput("tbl_tb"),
              br(),
              plotOutput("plot_tb")
      ),
      tabItem("vat",
              h3("Vật dụng trong nhà vệ sinh"),
              uiOutput("tbl_vat"),
              br(),
              plotOutput("plot_vat")
      ),
      tabItem("khoa",
              h3("Theo khoa phòng"),
              uiOutput("tbl_khoa"),
              br(),
              plotOutput("plot_khoa")
      )
    )
  )
)

# 💡 Server logic
server <- function(input, output, session) {
  # Bảng sử dụng flextable → htmltools_value → renderUI
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
  
  # Bảng số lần vệ sinh (dùng datatable)
  output$tbl_vs <- renderDT(as.data.frame(summary_wide))
}

# 🚀 Chạy app
shinyApp(ui, server)
