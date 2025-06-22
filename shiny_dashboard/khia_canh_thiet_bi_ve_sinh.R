# 📦 Tải gói cần thiết
pacman::p_load(dplyr, tidyr, stringr, ggplot2, flextable, glue)

# 🧾 Cột tương ứng với thiết bị vệ sinh
cols_tbvs <- c(
  "C. Khía cạnh thiết bị vệ sinh (lavabo, vòi xịt, vòi rửa tay,...) [1. Nứt vỡ]",
  "C. Khía cạnh thiết bị vệ sinh (lavabo, vòi xịt, vòi rửa tay,...) [2. Hỏng, tắc nghẽn]",
  "C. Khía cạnh thiết bị vệ sinh (lavabo, vòi xịt, vòi rửa tay,...) [3. Bồn cầu vệ sinh dính đọng phân, nước tiểu]",
  "C. Khía cạnh thiết bị vệ sinh (lavabo, vòi xịt, vòi rửa tay,...) [4. Vòi rửa tay tự động ngắt nước]"
)

# 🧹 Chuẩn hóa dữ liệu long format
df_tbvs_long <- df %>%
  select(all_of(cols_tbvs)) %>%
  pivot_longer(cols = everything(),
               names_to = "Tieu_chi", values_to = "Danh_gia") %>%
  mutate(
    Tieu_chi = str_replace_all(Tieu_chi, "^C\\. Khía cạnh thiết bị vệ sinh \\(lavabo, vòi xịt, vòi rửa tay,\\.\\.\\.\\) \\[|\\]$", ""),
    Danh_gia = case_when(
      str_detect(Danh_gia, regex("có|✓|yes", ignore_case = TRUE)) ~ "Có",
      str_detect(Danh_gia, regex("không|x|chưa", ignore_case = TRUE)) ~ "Không",
      TRUE ~ "Không rõ"
    )
  )

# 📊 Bảng tần suất và tỷ lệ
tbl_tbvs <- df_tbvs_long %>%
  count(Tieu_chi, Danh_gia) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  pivot_wider(names_from = Danh_gia,
              values_from = c(n, Ty_le),
              values_fill = 0) %>%
  flextable() %>%
  set_header_labels(
    Tieu_chi = "Nội dung",
    `n_Không` = "Số 'Không'", `Ty_le_Không` = "Tỷ lệ 'Không' (%)",
    `n_Có` = "Số 'Có'", `Ty_le_Có` = "Tỷ lệ 'Có' (%)",
    `n_Không rõ` = "Số 'Không rõ'", `Ty_le_Không rõ` = "Tỷ lệ 'Không rõ' (%)"
  ) %>%
    set_table_properties(width = 1, layout = "autofit")

# 📈 Biểu đồ tỷ lệ "Có" theo từng tiêu chí (khớp bảng)
plot_tbvs <- df_tbvs_long %>%
  count(Tieu_chi, Danh_gia) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(Danh_gia == "Có") %>%
  ggplot(aes(x = reorder(Tieu_chi, Ty_le), y = Ty_le)) +
  geom_col(fill = "#ff7f0e") +
  coord_flip() +
  geom_text(aes(label = paste0(Ty_le, "%")),
            hjust = -0.1, size = 4, family = "Times New Roman") +
  labs(
    x = "Nội dung",
    y = "Tỷ lệ 'Có' (%)",
    title = "Tỷ lệ đạt chuẩn thiết bị vệ sinh"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 13) +
  ylim(0, 105)

# 📝 Nhận xét tự động (inline)
top_tbvs <- df_tbvs_long %>%
  count(Tieu_chi, Danh_gia) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(Danh_gia == "Có") %>%
  arrange(desc(Ty_le))

nhan_xet_tbvs <- glue("
🔎 Trong các khía cạnh thiết bị vệ sinh, tiêu chí **{top_tbvs$Tieu_chi[1]}** có tỷ lệ 'Có' cao nhất ({top_tbvs$Ty_le[1]}%),
tiếp theo là **{top_tbvs$Tieu_chi[2]}** ({top_tbvs$Ty_le[2]}%) và **{top_tbvs$Tieu_chi[3]}** ({top_tbvs$Ty_le[3]}%).")
