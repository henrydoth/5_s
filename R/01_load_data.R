# 📁 Đường dẫn file dữ liệu
file_path <- here::here("data", "khao_sat_nvs.xlsx")

# 🧾 Đọc sheet "Form Responses 1"
raw_data <- readxl::read_excel(file_path, sheet = "Form Responses 1")

# 🧹 Tiền xử lý dữ liệu

df <- raw_data %>%
  select(-c(`Column 1`, `Column 2`, `Column 3`, `Column 4`))

 