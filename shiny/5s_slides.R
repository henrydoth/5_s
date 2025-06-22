# 📦 Nạp thư viện
library(officer)
library(flextable)
library(ggplot2)

# 📂 Template
my_pres <- read_pptx("source/template.pptx")

# 🧱 Hàm tiện ích tạo slide
add_flextable_slide <- function(pres, title, ft_obj) {
  pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
  pres <- ph_with(pres, value = title, location = ph_location_type(type = "title"))
  pres <- ph_with(pres, value = ft_obj, location = ph_location_type(type = "body"))
  return(pres)
}

add_plot_slide <- function(pres, title, plot_obj) {
  pres <- add_slide(pres, layout = "Title and Content", master = "Office Theme")
  pres <- ph_with(pres, value = title, location = ph_location_type(type = "title"))
  pres <- ph_with(pres, value = plot_obj, location = ph_location_type(type = "body"))
  return(pres)
}

# 🧭 Slide 1: Mục lục nội dung trình bày
ul <- unordered_list(
  level_list = rep(1, 5),
  str_list = c(
    "Số lần vệ sinh", 
    "Điều kiện vệ sinh chung", 
    "Sàn nhà vệ sinh", 
    "Thiết bị - Vật dụng", 
    "Khoa phòng"
  ),
  style = fp_text(font.size = 18)
)
my_pres <- add_slide(my_pres, layout = "Title and Content", master = "Office Theme")
my_pres <- ph_with(my_pres, "Nội dung trình bày", location = ph_location_type(type = "title"))
my_pres <- ph_with(my_pres, ul, location = ph_location_type(type = "body"))

# 🔹 1. Số lần vệ sinh
my_pres <- add_flextable_slide(my_pres, "Số lần vệ sinh 24h: Nhân viên vs Khách hàng", flextable_vs_24h)
my_pres <- add_plot_slide(my_pres, "Biểu đồ số lần vệ sinh 24h", plot_vs_24h)
my_pres <- add_plot_slide(my_pres, "Top 7 khoa phòng vệ sinh nhiều nhất", plot_top5_high)
my_pres <- add_plot_slide(my_pres, "Top 7 khoa phòng vệ sinh ít nhất", plot_top5_low)

# 🔹 2. Điều kiện vệ sinh chung
my_pres <- add_flextable_slide(my_pres, "Điều kiện vệ sinh chung: Nhân viên vs Khách hàng", tbl_dk_chung)
my_pres <- add_plot_slide(my_pres, "Biểu đồ điều kiện vệ sinh chung", plot_dk_chung)
my_pres <- add_plot_slide(my_pres, "Top 5 điều kiện đạt cao nhất", plot_top5_dieu_kien_high)
my_pres <- add_plot_slide(my_pres, "Top 5 điều kiện đạt thấp nhất", plot_top5_dieu_kien_low)
my_pres <- add_plot_slide(my_pres, "Khoa có tỷ lệ bảng hướng dẫn rửa tay cao", plot_bang_huong_dan_rua_tay_high)
my_pres <- add_plot_slide(my_pres, "Khoa có tỷ lệ bảng hướng dẫn rửa tay thấp", plot_bang_huong_dan_rua_tay_low)

# 🔹 3. Sàn nhà vệ sinh
my_pres <- add_flextable_slide(my_pres, "Khía cạnh sàn nhà vệ sinh", tbl_san_nvs)
my_pres <- add_plot_slide(my_pres, "Biểu đồ sàn nhà vệ sinh", plot_san_nvs)

# 🔹 4. Thiết bị và vật dụng
my_pres <- add_flextable_slide(my_pres, "Khía cạnh thiết bị vệ sinh", tbl_tbvs)
my_pres <- add_plot_slide(my_pres, "Biểu đồ thiết bị vệ sinh", plot_tbvs)
my_pres <- add_flextable_slide(my_pres, "Khía cạnh vật dụng trong nhà vệ sinh", tbl_vat_dung)
my_pres <- add_plot_slide(my_pres, "Biểu đồ vật dụng trong nhà vệ sinh", plot_vat_dung)

# 🔹 5. Khoa phòng
my_pres <- add_flextable_slide(my_pres, "Số lần vệ sinh theo khoa/phòng", tbl_vs_khoa)

# 💾 Lưu file
dir.create("output", showWarnings = FALSE)
print(my_pres, target = "5s_ket_qua_ve_sinh_slides.pptx")
