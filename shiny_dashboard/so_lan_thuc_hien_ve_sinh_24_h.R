# 📦 Nạp thư viện cần thiết
library(dplyr)
library(stringr)
library(janitor)
library(flextable)
library(ggplot2)
library(glue)

# 🔤 Bước 1: Mã hóa nhóm đối tượng sử dụng nhà vệ sinh và chuyển đổi biến số lần vệ sinh
df <- df %>%
  mutate(
    doi_tuong_sd = case_when(
      str_detect(`7. Nhà vệ sinh dành cho`, regex("nhân viên", ignore_case = TRUE)) ~ "Nhân viên",
      str_detect(`7. Nhà vệ sinh dành cho`, regex("khách|người bệnh|bệnh nhân", ignore_case = TRUE)) ~ "Khách hàng",
      TRUE ~ NA_character_
    ),
    so_lan_vs_24h = suppressWarnings(as.numeric(`14.  Số lần thực hiện vệ sinh trong 24h:`))
  ) %>%
  filter(!is.na(doi_tuong_sd), !is.na(so_lan_vs_24h))

# 📋 Bước 2: Tóm tắt mô tả số lần vệ sinh theo nhóm đối tượng
summary_vs_24h <- df %>%
  group_by(doi_tuong_sd) %>%
  summarise(
    n = n(),
    trung_binh = mean(so_lan_vs_24h, na.rm = TRUE),
    sd = sd(so_lan_vs_24h, na.rm = TRUE),
    median = median(so_lan_vs_24h, na.rm = TRUE),
    iqr = IQR(so_lan_vs_24h, na.rm = TRUE),
    min = min(so_lan_vs_24h, na.rm = TRUE),
    max = max(so_lan_vs_24h, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

# 📐 Bước 3: Kiểm định Mann-Whitney giữa hai nhóm
kq_wilcox <- wilcox.test(so_lan_vs_24h ~ doi_tuong_sd, data = df)

# 🧾 Bước 4: Tạo bảng theo hàng (chỉ số) và cột (2 nhóm + p-value)
summary_wide <- tibble(
  `Chỉ số` = c(
    "Số quan sát (n)",
    "Trung bình ± SD",
    "Trung vị (IQR)",
    "Min - Max"
  ),
  `Nhân viên` = c(
    summary_vs_24h$n[summary_vs_24h$doi_tuong_sd == "Nhân viên"],
    glue("{summary_vs_24h$trung_binh[summary_vs_24h$doi_tuong_sd == 'Nhân viên']} ± {summary_vs_24h$sd[summary_vs_24h$doi_tuong_sd == 'Nhân viên']}"),
    glue("{summary_vs_24h$median[summary_vs_24h$doi_tuong_sd == 'Nhân viên']} ({summary_vs_24h$iqr[summary_vs_24h$doi_tuong_sd == 'Nhân viên']})"),
    glue("{summary_vs_24h$min[summary_vs_24h$doi_tuong_sd == 'Nhân viên']} - {summary_vs_24h$max[summary_vs_24h$doi_tuong_sd == 'Nhân viên']}")
  ),
  `Khách hàng` = c(
    summary_vs_24h$n[summary_vs_24h$doi_tuong_sd == "Khách hàng"],
    glue("{summary_vs_24h$trung_binh[summary_vs_24h$doi_tuong_sd == 'Khách hàng']} ± {summary_vs_24h$sd[summary_vs_24h$doi_tuong_sd == 'Khách hàng']}"),
    glue("{summary_vs_24h$median[summary_vs_24h$doi_tuong_sd == 'Khách hàng']} ({summary_vs_24h$iqr[summary_vs_24h$doi_tuong_sd == 'Khách hàng']})"),
    glue("{summary_vs_24h$min[summary_vs_24h$doi_tuong_sd == 'Khách hàng']} - {summary_vs_24h$max[summary_vs_24h$doi_tuong_sd == 'Khách hàng']}")
  ),
  `p-value` = c(round(kq_wilcox$p.value, 3), rep("", 3))
)

flextable_vs_24h <- summary_wide %>%
  flextable() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  set_caption("Bảng: So sánh số lần vệ sinh trong 24 giờ giữa nhà vệ sinh Nhân viên và Khách hàng")

# 📈 Bước 5: Vẽ biểu đồ boxplot trực quan
plot_vs_24h <- ggplot(df, aes(x = doi_tuong_sd, y = so_lan_vs_24h, fill = doi_tuong_sd)) +
  geom_boxplot() +
  labs(
    title = "Biểu đồ boxplot số lần vệ sinh trong 24h",
    x = "Đối tượng sử dụng",
    y = "Số lần vệ sinh",
    fill = "Đối tượng sử dụng NVS"
  ) +
  theme_minimal(base_family = "Times New Roman")

# 🧾 Bước 6: Nhận xét inline mô tả bảng
nhan_xet_bang <- glue(
  "Trung bình số lần vệ sinh trong 24 giờ ở nhà vệ sinh dành cho {summary_vs_24h$doi_tuong_sd[1]} là {summary_vs_24h$trung_binh[1]} ± {summary_vs_24h$sd[1]}, " %||%
    "trong khi ở nhóm {summary_vs_24h$doi_tuong_sd[2]} là {summary_vs_24h$trung_binh[2]} ± {summary_vs_24h$sd[2]}."
)

# 🧠 Bước 7: Diễn giải thống kê kiểm định
nhan_xet_kt <- glue(
  "Kiểm định Mann-Whitney cho thấy sự khác biệt giữa hai nhóm không có ý nghĩa thống kê (p = {round(kq_wilcox$p.value, 3)})."
)

# 📌 Bước 8: Nội dung chèn vào phần bàn luận Quarto
nhan_xet_ban_luan <- glue(
  "Kết quả phân tích cho thấy {nhan_xet_bang} {nhan_xet_kt} Điều này cho thấy tần suất vệ sinh giữa hai nhóm nhà vệ sinh chưa có sự khác biệt rõ rệt, phản ánh mức độ kiểm soát vệ sinh tương đương giữa khu vực dành cho nhân viên và khu vực cho khách hàng."
)
