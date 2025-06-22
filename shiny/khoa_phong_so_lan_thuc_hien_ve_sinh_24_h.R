# 📦 Nạp thư viện cần thiết
library(dplyr)
library(stringr)
library(janitor)
library(flextable)
library(ggplot2)
library(glue)

# 📌 Bước 1: Làm sạch và chuẩn hóa biến số lần vệ sinh & khoa phòng
df_vs_khoa <- df %>%
  mutate(
    don_vi = `1. Đơn vị`,
    so_lan_vs_24h = suppressWarnings(as.numeric(`14.  Số lần thực hiện vệ sinh trong 24h:`))
  ) %>%
  filter(!is.na(don_vi), !is.na(so_lan_vs_24h))

# 📊 Bước 2: Thống kê mô tả theo khoa/phòng
summary_vs_khoa <- df_vs_khoa %>%
  group_by(don_vi) %>%
  summarise(
    n = n(),
    trung_binh = mean(so_lan_vs_24h),
    sd = sd(so_lan_vs_24h),
    median = median(so_lan_vs_24h),
    iqr = IQR(so_lan_vs_24h),
    min = min(so_lan_vs_24h),
    max = max(so_lan_vs_24h)
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~round(.x, 1)))

# 📋 Bước 3: Tạo bảng flextable (sắp xếp theo trung bình giảm dần)
tbl_vs_khoa <- summary_vs_khoa %>%
  arrange(desc(trung_binh)) %>%
  mutate(
    `Trung bình ± SD` = glue("{trung_binh} ± {sd}"),
    `Trung vị (IQR)` = glue("{median} ({iqr})"),
    `Min - Max` = glue("{min} - {max}")
  ) %>%
  select(
    `Khoa/Phòng` = don_vi,
    `Số quan sát` = n,
    `Trung bình ± SD`,
    `Trung vị (IQR)`,
    `Min - Max`
  ) %>%
  flextable() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  set_caption("Bảng: Mô tả số lần vệ sinh trong 24h theo từng khoa/phòng (sắp xếp giảm dần)")

# 📈 Bước 4: Biểu đồ cột tổng thể
plot_vs_khoa <- df_vs_khoa %>%
  group_by(don_vi) %>%
  summarise(trung_binh = mean(so_lan_vs_24h, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(don_vi, trung_binh), y = trung_binh)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  geom_text(aes(label = round(trung_binh, 1)), hjust = 1.1, color = "white", size = 3.5, family = "Times New Roman") +
  labs(
    title = "Trung bình số lần vệ sinh theo khoa/phòng",
    x = "Khoa/Phòng",
    y = "Trung bình số lần vệ sinh"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 13)

# 📈 Bước 5: Biểu đồ 7 khoa/phòng có số lần vệ sinh cao nhất
plot_top5_high <- summary_vs_khoa %>%
  arrange(desc(trung_binh)) %>%
  slice_head(n = 7) %>%
  ggplot(aes(x = reorder(don_vi, trung_binh), y = trung_binh)) +
  geom_col(fill = "#1a9641") +
  coord_flip() +
  geom_text(aes(label = trung_binh), hjust = 1.1, color = "white", size = 4) +
  labs(
    title = "Khoa/phòng số lần VS cao",
    x = "Khoa/Phòng",
    y = "Trung bình số lần vệ sinh"
  ) +
  theme_minimal(base_family = "Times New Roman")

# 📈 Bước 6: Biểu đồ 7 khoa/phòng có số lần vệ sinh thấp nhất
plot_top5_low <- summary_vs_khoa %>%
  arrange(trung_binh) %>%
  slice_head(n = 7) %>%
  ggplot(aes(x = reorder(don_vi, trung_binh), y = trung_binh)) +
  geom_col(fill = "#d7191c") +
  coord_flip() +
  geom_text(aes(label = trung_binh), hjust = 1.1, color = "white", size = 4) +
  labs(
    title = "Khoa/phòng số lần VS thấp",
    x = "Khoa/Phòng",
    y = "Trung bình số lần vệ sinh"
  ) +
  theme_minimal(base_family = "Times New Roman")

# ✍️ Bước 7: Nhận xét tự động
nhan_xet_top5_high <- glue(
  "Top 7 đơn vị có tần suất vệ sinh cao nhất bao gồm: {paste0(summary_vs_khoa %>% arrange(desc(trung_binh)) %>% slice_head(n = 7) %>% pull(don_vi), collapse = ", ")}"
)

nhan_xet_top5_low <- glue(
  "Top 7 đơn vị có tần suất vệ sinh thấp nhất bao gồm: {paste0(summary_vs_khoa %>% arrange(trung_binh) %>% slice_head(n = 7) %>% pull(don_vi), collapse = ", ")}"
)
