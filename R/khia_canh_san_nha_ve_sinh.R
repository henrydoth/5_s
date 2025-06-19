# 📦 Tải gói cần thiết
pacman::p_load(dplyr, tidyr, stringr, ggplot2, flextable, glue)

# 🧾 Cột tương ứng với Khía cạnh sàn nhà vệ sinh
cols_san_nvs <- c(
  "B. Khía cạnh sàn nhà vệ sinh [1. Khô ráo]",
  "B. Khía cạnh sàn nhà vệ sinh [2. Trơn trượt]",
  "B. Khía cạnh sàn nhà vệ sinh [3. Sạch, không vết bẩn]",
  "B. Khía cạnh sàn nhà vệ sinh [4. Rác rơi vãi]",
  "B. Khía cạnh sàn nhà vệ sinh [5. Bong tróc, nứt vỡ]"
)

# 🧹 Chuẩn hóa dữ liệu long format
df_san_nvs_long <- df %>%
  select(all_of(cols_san_nvs)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Tieu_chi",
    values_to = "Danh_gia"
  ) %>%
  mutate(
    Tieu_chi = str_replace_all(Tieu_chi, "^B\\. Khía cạnh sàn nhà vệ sinh \\[|\\]$", ""),
    Danh_gia = case_when(
      str_detect(Danh_gia, regex("có|✓|yes", ignore_case = TRUE)) ~ "Có",
      str_detect(Danh_gia, regex("không|x|chưa", ignore_case = TRUE)) ~ "Không",
      TRUE ~ "Không rõ"
    )
  )

# 📊 Tạo bảng thống kê
tbl_san_nvs <- df_san_nvs_long %>%
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

# 📈 Biểu đồ tỷ lệ Có
plot_san_nvs <- df_san_nvs_long %>%
  filter(Danh_gia == "Có") %>%
  count(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ggplot(aes(x = reorder(Tieu_chi, Ty_le), y = Ty_le)) +
  geom_col(fill = "#2ca02c") +
  coord_flip() +
  geom_text(aes(label = paste0(Ty_le, "%")), hjust = -0.1, size = 4, family = "Times New Roman") +
  labs(x = "Nội dung", y = "Tỷ lệ 'Có' (%)", title = "Tỷ lệ đạt về khía cạnh sàn nhà vệ sinh") +
  theme_minimal(base_family = "Times New Roman", base_size = 13) +
  ylim(0, 105)

# 📝 Nhận xét tự động
top_san_nvs <- df_san_nvs_long %>%
  filter(Danh_gia == "Có") %>%
  count(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  arrange(desc(Ty_le))

nhan_xet_san_nvs <- glue("
🔎 Trong các khía cạnh sàn nhà vệ sinh, tiêu chí **{top_san_nvs$Tieu_chi[1]}** có tỷ lệ 'Có' cao nhất ({top_san_nvs$Ty_le[1]}%),
tiếp theo là **{top_san_nvs$Tieu_chi[2]}** ({top_san_nvs$Ty_le[2]}%) và **{top_san_nvs$Tieu_chi[3]}** ({top_san_nvs$Ty_le[3]}%).")
