# 📦 Tải gói
pacman::p_load(dplyr, tidyr, stringr, ggplot2, flextable, glue)

# 📌 Danh sách 12 điều kiện chung
cols_dk_chung <- c(
  "A. Điều kiện chung [1. Biển chỉ dẫn đến nhà vệ sinh]",
  "A. Điều kiện chung [2. Nhà vệ sinh được đánh số]",
  "A. Điều kiện chung [3. Quy định (hướng dẫn) sử dụng nhà vệ sinh]",
  "A. Điều kiện chung [4. Đảm bảo thông gió (quạt hút, cửa thông gió)]",
  "A. Điều kiện chung [5. Đảm bảo ánh sáng]",
  "A. Điều kiện chung [6. Mùi hôi]",
  "A. Điều kiện chung [7. Nước sạch để rửa tay, nước để dội bồn cầu]",
  "A. Điều kiện chung [8. Bảng hướng dẫn rửa tay]",
  "A. Điều kiện chung [9. Tay vịn trong nhà vệ sinh]",
  "A. Điều kiện chung [10. Biển cảnh báo té ngã]",
  "A. Điều kiện chung [11. Chuông bấm hoặc điện thoại khi gặp sự cố]",
  "A. Điều kiện chung [12. Bảng checklist thực hiện vệ sinh cho nhân viên vệ sinh]"
)

# 🧹 Pivot dữ liệu từ wide → long để xử lý
df_dk_chung_long <- df %>%
  select(all_of(cols_dk_chung)) %>%
  pivot_longer(cols = everything(),
               names_to = "Tieu_chi",
               values_to = "Dat") %>%
  mutate(
    Tieu_chi = str_replace_all(Tieu_chi, "^A\\. Điều kiện chung \\[|\\]$", ""),
    Dat = case_when(
      str_detect(Dat, regex("có|đạt|✓|yes", ignore_case = TRUE)) ~ "Đạt",
      str_detect(Dat, regex("không|chưa|x", ignore_case = TRUE)) ~ "Không đạt",
      TRUE ~ "Không rõ"
    )
  )

# 📊 Bảng tần suất theo tiêu chí
tbl_dk_chung <- df_dk_chung_long %>%
  count(Tieu_chi, Dat) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Dat,
    values_from = c(n, Ty_le),
    values_fill = 0
  ) %>%
  flextable() %>%
  set_header_labels(
    Tieu_chi = "Tiêu chí",
    `n_Đạt` = "Số đạt", `Ty_le_Đạt` = "Tỷ lệ đạt (%)",
    `n_Không đạt` = "Số không đạt", `Ty_le_Không đạt` = "Tỷ lệ không đạt (%)",
    `n_Không rõ` = "Số không rõ", `Ty_le_Không rõ` = "Tỷ lệ không rõ (%)"
  ) %>%
    set_table_properties(width = 1, layout = "autofit")

plot_dk_chung <- df_dk_chung_long %>%
  count(Tieu_chi, Dat) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(Dat == "Đạt") %>%
  ggplot(aes(x = reorder(Tieu_chi, Ty_le), y = Ty_le)) +
  geom_col(fill = "#1f77b4") +
  coord_flip() +
  geom_text(aes(label = paste0(Ty_le, "%")), 
            position = position_stack(vjust = 0.5),
            size = 4, color = "white", family = "Times New Roman") +
  labs(
    x = "Tiêu chí",
    y = "Tỷ lệ đạt (%)",
    title = "Tỷ lệ đạt các điều kiện chung"
  ) +
  theme_minimal(base_family = "Times New Roman", base_size = 13)

# 📝 Nhận xét tự động
top_dieu_kien <- df_dk_chung_long %>%
  count(Tieu_chi, Dat) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(Dat == "Đạt") %>%
  arrange(desc(Ty_le))

nhan_xet_dieu_kien <- glue("
🔎 Trong số các điều kiện chung, tiêu chí **{top_dieu_kien$Tieu_chi[1]}** có tỷ lệ đạt cao nhất ({top_dieu_kien$Ty_le[1]}%),
tiếp theo là **{top_dieu_kien$Tieu_chi[2]}** ({top_dieu_kien$Ty_le[2]}%) và **{top_dieu_kien$Tieu_chi[3]}** ({top_dieu_kien$Ty_le[3]}%).")
