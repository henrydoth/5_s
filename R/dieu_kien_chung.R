# 📦 Tải gói
pacman::p_load(dplyr, tidyr, stringr, ggplot2, flextable, glue)



## kết thúc chèn shiny

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
  select(`1. Đơn vị`, all_of(cols_dk_chung)) %>%
  pivot_longer(cols = all_of(cols_dk_chung),
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



# 📉 Tỷ lệ đạt các điều kiện
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

# 📜 Nhận xét tự động
top_dieu_kien <- df_dk_chung_long %>%
  count(Tieu_chi, Dat) %>%
  group_by(Tieu_chi) %>%
  mutate(Ty_le = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  filter(Dat == "Đạt") %>%
  arrange(desc(Ty_le))

nhan_xet_dieu_kien <- glue(
  "\U0001F50E Trong số các điều kiện chung, tiêu chí **{top_dieu_kien$Tieu_chi[1]}** có tỷ lệ đạt cao nhất ({top_dieu_kien$Ty_le[1]}%),\ntiếp theo là **{top_dieu_kien$Tieu_chi[2]}** ({top_dieu_kien$Ty_le[2]}%) và **{top_dieu_kien$Tieu_chi[3]}** ({top_dieu_kien$Ty_le[3]}%)."
)

# 📈 Biểu đồ top 5 điều kiện có tỷ lệ đạt cao nhất
plot_top5_dieu_kien_high <- top_dieu_kien %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(Tieu_chi, Ty_le), y = Ty_le)) +
  geom_col(fill = "#1a9641") +
  coord_flip() +
  geom_text(aes(label = paste0(Ty_le, "%")),
            hjust = 1.1, color = "white", size = 4) +
  labs(
    title = "5 tiêu chí đạt cao nhất",
    x = "Tiêu chí",
    y = "Tỷ lệ đạt (%)"
  ) +
  theme_minimal(base_family = "Times New Roman")

# 📉 Biểu đồ top 5 điều kiện có tỷ lệ đạt thấp nhất
plot_top5_dieu_kien_low <- top_dieu_kien %>%
  arrange(Ty_le) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = reorder(Tieu_chi, Ty_le), y = Ty_le)) +
  geom_col(fill = "#d7191c") +
  coord_flip() +
  geom_text(aes(label = paste0(Ty_le, "%")),
            hjust = 1.1, color = "white", size = 4) +
  labs(
    title = "5 tiêu chí đạt thấp nhất",
    x = "Tiêu chí",
    y = "Tỷ lệ đạt (%)"
  ) +
  theme_minimal(base_family = "Times New Roman")

nhan_xet_top_dieu_kien_high <- glue(
  "Các điều kiện chung có tỷ lệ đạt cao nhất bao gồm: {paste(top_dieu_kien$Tieu_chi[1:5], collapse = ", ")}"
)

nhan_xet_top_dieu_kien_low <- glue(
  "Ngược lại, những điều kiện có tỷ lệ đạt thấp nhất là: {paste(top_dieu_kien %>% arrange(Ty_le) %>% slice_head(n = 5) %>% pull(Tieu_chi), collapse = ", ")}"
)

# 📊 Top 7 khoa phòng có bảng hướng dẫn rửa tay cao/thấp nhất
df_bang_hd <- df %>%
  mutate(
    don_vi = `1. Đơn vị`,
    huong_dan = `A. Điều kiện chung [8. Bảng hướng dẫn rửa tay]`
  ) %>%
  filter(!is.na(don_vi)) %>%
  mutate(huong_dan = case_when(
    str_detect(huong_dan, regex("có|đạt|✓|yes", ignore_case = TRUE)) ~ 1,
    str_detect(huong_dan, regex("không|chưa|x", ignore_case = TRUE)) ~ 0,
    TRUE ~ NA_real_
  ))

bang_hd_by_khoa <- df_bang_hd %>%
  group_by(don_vi) %>%
  summarise(ty_le = round(mean(huong_dan, na.rm = TRUE) * 100, 1)) %>%
  filter(!is.na(ty_le))

plot_bang_huong_dan_rua_tay_high <- bang_hd_by_khoa %>%
  arrange(desc(ty_le)) %>%
  slice_head(n = 7) %>%
  ggplot(aes(x = reorder(don_vi, ty_le), y = ty_le)) +
  geom_col(fill = "#1a9641") +
  coord_flip() +
  geom_text(aes(label = paste0(ty_le, "%")), hjust = 1.1, color = "white", size = 4) +
  labs(
    title = "Bảng hướng dẫn rửa tay thấp",
    x = "Khoa/Phòng",
    y = "Tỷ lệ có bảng hướng dẫn (%)"
  ) +
  theme_minimal(base_family = "Times New Roman")

plot_bang_huong_dan_rua_tay_low <- bang_hd_by_khoa %>%
  arrange(ty_le) %>%
  slice_head(n = 7) %>%
  ggplot(aes(x = reorder(don_vi, ty_le), y = ty_le)) +
  geom_col(fill = "#d7191c") +
  coord_flip() +
  geom_text(aes(label = paste0(ty_le, "%")), hjust = 1.1, color = "white", size = 4) +
  labs(
    title = "Bảng hướng dẫn rửa tay thấp",
    x = "Khoa/Phòng",
    y = "Tỷ lệ có bảng hướng dẫn (%)"
  ) +
  theme_minimal(base_family = "Times New Roman")

nhan_xet_bang_hd <- glue(
  "\U0001F4CB Trong số các khoa/phòng, nhóm có tỷ lệ bảng hướng dẫn rửa tay cao nhất là: {paste(bang_hd_by_khoa %>% arrange(desc(ty_le)) %>% slice_head(n = 3) %>% pull(don_vi), collapse = ", ")}.\n"
)
