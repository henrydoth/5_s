# 📦 Tải gói cần thiết
pacman::p_load(dplyr, tidyr, stringr, ggplot2, flextable, glue)

# 🧾 Cột tương ứng với khía cạnh vật dụng trong nhà vệ sinh
cols_vat_dung <- c(
  "D. Khía cạnh vật dụng trong nhà vệ sinh [1. Giấy vệ sinh]",
  "D. Khía cạnh vật dụng trong nhà vệ sinh [2. Thùng đựng chất thải có nắp]",
  "D. Khía cạnh vật dụng trong nhà vệ sinh [3. Xà phòng hoặc dung dịch rửa tay]",
  "D. Khía cạnh vật dụng trong nhà vệ sinh [4. Gương soi]",
  "D. Khía cạnh vật dụng trong nhà vệ sinh [5. Móc treo quần áo]",
  "D. Khía cạnh vật dụng trong nhà vệ sinh [6. Giá để bệnh phẩm (phân, nước tiểu)]"
)

# 🧹 Chuẩn hóa dữ liệu long format
df_vat_dung_long <- df %>%
  select(all_of(cols_vat_dung)) %>%
  pivot_longer(cols = everything(),
               names_to = "Tieu_chi", values_to = "Danh_gia") %>%
  mutate(
    Tieu_chi = str_replace_all(Tieu_chi, "^D\\. Khía cạnh vật dụng trong nhà vệ sinh \\[|\\]$", ""),
    Danh_gia = case_when(
      str_detect(Danh_gia, regex("có|✓|yes", ignore_case = TRUE)) ~ "Có",
      str_detect(Danh_gia, regex("không|x|chưa", ignore_case = TRUE)) ~ "Không",
      TRUE ~ "Không rõ"
    )
  )

# 📊 Bảng thống kê tần suất và tỷ lệ
tbl_vat_dung <- df_vat_dung_long %>%
  count(Tieu_chi, Danh_gia) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Danh_gia,
    values_from = c(n, Ty_le),
    values_fill = 0
  ) %>%
  flextable() %>%
  set_header_labels(
    Tieu_chi = "Nội dung",
    `n_Không` = "Số 'Không'", `Ty_le_Không` = "Tỷ lệ 'Không' (%)",
    `n_Có` = "Số 'Có'", `Ty_le_Có` = "Tỷ lệ 'Có' (%)",
    `n_Không rõ` = "Số 'Không rõ'", `Ty_le_Không rõ` = "Tỷ lệ 'Không rõ' (%)"
  ) %>%
  set_table_properties(width = 1, layout = "autofit")

# 📈 Biểu đồ tỷ lệ 'Có'
plot_vat_dung <- df_vat_dung_long %>%
  count(Tieu_chi, Danh_gia) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(Danh_gia == "Có") %>%
  ggplot(aes(x = reorder(Tieu_chi, Ty_le), y = Ty_le)) +
  geom_col(fill = "#9467bd") +
  coord_flip() +
  geom_text(aes(label = paste0(Ty_le, "%")),
            hjust = -0.1, size = 4, family = "Times New Roman") +
  labs(
    x = "Nội dung",
    y = "Tỷ lệ 'Có' (%)",
    title = "Tỷ lệ đạt chuẩn vật dụng trong nhà vệ sinh"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 13) +
  ylim(0, 105)

# 📝 Nhận xét tự động
top_vat_dung <- df_vat_dung_long %>%
  count(Tieu_chi, Danh_gia) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(Danh_gia == "Có") %>%
  arrange(desc(Ty_le))

nhan_xet_vat_dung <- glue("
🔎 Trong các khía cạnh vật dụng trong nhà vệ sinh, tiêu chí **{top_vat_dung$Tieu_chi[1]}** có tỷ lệ 'Có' cao nhất ({top_vat_dung$Ty_le[1]}%),
tiếp theo là **{top_vat_dung$Tieu_chi[2]}** ({top_vat_dung$Ty_le[2]}%) và **{top_vat_dung$Tieu_chi[3]}** ({top_vat_dung$Ty_le[3]}%).")
