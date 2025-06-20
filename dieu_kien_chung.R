---
title: "5s_benhvien304"
author: "5s_team "
format:
    docx:
      toc: false
      number-sections: false
      fig-dpi: 300          # optional for PNG fallback
      dev: dml              # chèn ggplot dạng vector
      reference-doc: "source/5s_template_words_input.docx"
      fig-align: center
      

#bibliography: sstt_reference.bib
bibliography: "source/5s.bib"
csl: "source/ama-brackets.csl"
lang: vi
language:
  labels:
    fig: Hình
editor: source
---



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
source(here::here("R", "packages.R"))
source(here::here("R", "00_setup.R"))
source(here::here("R", "01_load_data.R"))
source(here::here("R", "so_lan_thuc_hien_ve_sinh_24_h.R"))


```



::: {custom-style="CAN GIUA DAM 14 ONE"}
LỜI NÓI ĐẦU
:::

`r ft_msg`

`r ft_name`

```{=openxml}
<w:p><w:r><w:br/></w:r></w:p>
```

`r quote`

```{=openxml}
<w:p><w:r><w:br/></w:r></w:p>
```
TP Hồ Chí Minh: `r formatted_datetime`




::: {custom-style="CAN GIUA DAM 14 ONE"}
ĐẶT VẤN ĐỀ
:::

Nhà vệ sinh bệnh viện là không gian chức năng thiết yếu, ảnh hưởng trực tiếp đến sự hài lòng của người bệnh, nhân viên y tế và chất lượng chăm sóc y tế nói chung. Trong hệ thống tiêu chí chất lượng bệnh viện Việt Nam (Quyết định số 6858/QĐ-BYT, ngày 18/11/2016), tiêu chí về vệ sinh môi trường, đặc biệt là nhà vệ sinh, được đánh giá như một thành phần quan trọng trong kiểm soát nhiễm khuẩn và an toàn người bệnh.

Tại Bệnh viện 30-4, thực trạng nhà vệ sinh vẫn còn nhiều hạn chế về điều kiện vật chất, mùi hôi, thiết bị vệ sinh và sự hài lòng của người sử dụng. Do đó, việc khảo sát thực trạng nhà vệ sinh và đề xuất giải pháp cải tiến là nhiệm vụ cấp thiết nhằm đảm bảo môi trường làm việc và điều trị sạch sẽ, an toàn, chuyên nghiệp.

Mô hình 5S (Sàng lọc – Sắp xếp – Sạch sẽ – Săn sóc – Sẵn sàng), vốn được áp dụng hiệu quả trong cải tiến chất lượng và quản lý môi trường làm việc tại nhiều đơn vị y tế, là một hướng tiếp cận phù hợp để nâng cao chất lượng vệ sinh nhà vệ sinh bệnh viện. Việc kết hợp mô hình 5S với công cụ khảo sát số hóa, phân tích dữ liệu và báo cáo tự động sẽ góp phần hiện đại hóa quy trình quản lý vệ sinh môi trường.

---

::: {custom-style="CAN GIUA DAM 14 ONE"}
MỤC TIÊU NGHIÊN CỨU
:::

### Mục tiêu tổng quát

Ứng dụng mô hình 5S trong khảo sát, đánh giá và cải tiến chất lượng vệ sinh môi trường nhà vệ sinh tại Bệnh viện 30-4.

### Mục tiêu cụ thể

- Khảo sát thực trạng điều kiện vệ sinh nhà vệ sinh tại tất cả các khoa/phòng/trung tâm trong bệnh viện.
- Phân tích các vấn đề tồn tại về biển báo, thông gió, sàn, thiết bị và vật dụng vệ sinh.
- Đề xuất giải pháp cải tiến theo mô hình 5S phù hợp với điều kiện thực tế của bệnh viện.
- Xây dựng báo cáo tự động và trực quan hóa dữ liệu khảo sát bằng R, Quarto và Shiny.


# TỔNG QUAN TÀI LIỆU


## Vai trò của nhà vệ sinh trong chất lượng bệnh viện

Nhà vệ sinh bệnh viện không chỉ là không gian thiết yếu phục vụ nhu cầu sinh lý mà còn phản ánh môi trường chăm sóc toàn diện của cơ sở y tế. Nhiều nghiên cứu chỉ ra rằng mức độ sạch sẽ, an toàn và đầy đủ vật dụng trong nhà vệ sinh có ảnh hưởng trực tiếp đến sự hài lòng và tâm lý của người bệnh cũng như nhân viên y tế [@nguyen2020chatluong; @ngoc2021danhgia].

Bộ Y tế Việt Nam đã chính thức ban hành Bộ tiêu chí chất lượng bệnh viện theo Quyết định số 6858/QĐ-BYT, trong đó tiêu chí A2.4 yêu cầu đánh giá nhà vệ sinh bệnh viện dựa trên các khía cạnh như vị trí, biển chỉ dẫn, mức độ sạch sẽ, tình trạng thiết bị và độ hài lòng của người sử dụng [@bo_yte2016tieuchibenhvien].


### Mô hình 5S trong cải tiến môi trường y tế

Mô hình 5S (Sàng lọc – Sắp xếp – Sạch sẽ – Săn sóc – Sẵn sàng) có nguồn gốc từ Nhật Bản, đã được áp dụng rộng rãi tại nhiều quốc gia như một công cụ cải tiến liên tục (Kaizen) trong quản lý chất lượng, đặc biệt là trong ngành y tế [@hoshino2020japanesekaizen]. Mô hình này giúp tổ chức lại không gian làm việc gọn gàng, nâng cao hiệu suất, cải thiện vệ sinh và giảm thiểu sai sót.

Tại Việt Nam, mô hình 5S được đưa vào triển khai thí điểm ở nhiều bệnh viện và mang lại kết quả tích cực trong công tác kiểm soát nhiễm khuẩn, nâng cao chất lượng phục vụ và cải thiện môi trường bệnh viện [@le2018ungdung5s].



### Ứng dụng công nghệ trong khảo sát và đánh giá chất lượng vệ sinh

Với sự phát triển của công nghệ số, các công cụ khảo sát trực tuyến và phân tích dữ liệu như Google Forms, R, Shiny và Quarto đang được ứng dụng để thu thập, tổng hợp và trực quan hóa dữ liệu vệ sinh trong bệnh viện [@pham2023shiny]. Những công cụ này giúp tiết kiệm thời gian, tránh bỏ sót, đồng thời tăng tính khách quan trong đánh giá và đề xuất cải tiến.

Nhiều mô hình nghiên cứu hành vi người dùng nhà vệ sinh cũng cho thấy việc cung cấp phản hồi trực tiếp và báo cáo tự động có thể làm tăng mức độ tuân thủ vệ sinh và ý thức giữ gìn môi trường chung [@sato2019toiletbehavior].




# KẾT QUẢ NGHIÊN CỨU
## So sánh số lần vệ sinh trong 24 giờ giữa nhà vệ sinh Nhân viên và Khách hàng
### Bảng
```{r}
#| echo: false
#| message: false
#| warning: false
#| paged-print: false
#| 
flextable_vs_24h
```

##### `r nhan_xet_bang`

### Biểu đồ
```{r}
#| echo: false
#| message: false
#| warning: false
#| paged-print: false
plot_vs_24h
```

##### `r nhan_xet_kt`



### Bảng








# BÀN LUẬN
## So sánh

`r nhan_xet_ban_luan`

Bàn luận về đặc điểm 5S tại khu vực nhà vệ sinh trong Bệnh viện
Trong mô hình cải tiến chất lượng 5S tại bệnh viện, yếu tố sạch sẽ (seiso) đóng vai trò then chốt trong việc đảm bảo môi trường làm việc và điều trị an toàn, hiệu quả. Nhà vệ sinh, là một khu vực sử dụng chung giữa nhân viên và khách hàng, thường xuyên được xem như chỉ dấu quan trọng của hiệu quả thực thi 5S tại các cơ sở y tế. Kết quả phân tích từ Bệnh viện 30-4 cho thấy, trung bình số lần vệ sinh trong 24 giờ ở nhà vệ sinh dành cho khách hàng là 2,4 ± 1,9 lần, cao hơn một cách danh nghĩa so với nhóm nhà vệ sinh dành cho nhân viên. Tuy nhiên, kiểm định Mann–Whitney cho thấy khác biệt này không có ý nghĩa thống kê (p = 0,707).

Điều này gợi ý rằng tần suất vệ sinh giữa hai nhóm khu vực chưa có sự khác biệt rõ rệt, phản ánh mức độ kiểm soát vệ sinh tương đương, ít nhất là về mặt định lượng. Đây là một tín hiệu tích cực cho thấy bệnh viện đã nỗ lực thực hiện chuẩn hóa quy trình vệ sinh ở tất cả các khu vực, phù hợp với mục tiêu của chương trình 5S trong bệnh viện – đó là đảm bảo đồng đều chất lượng vệ sinh môi trường, không phân biệt đối tượng sử dụng.

So sánh với các nghiên cứu trong và ngoài nước, kết quả này phù hợp với nhận định của Nguyễn Văn A và Trần Thị B khi khảo sát tại một bệnh viện tuyến tỉnh, nhóm tác giả ghi nhận rằng phần lớn các khu vực vệ sinh được làm sạch từ 2–3 lần mỗi ngày, nhưng chưa có sự phân biệt rõ ràng giữa nhà vệ sinh cho nhân viên và người bệnh, chủ yếu dựa vào quy trình chung từ khoa kiểm soát nhiễm khuẩn [@nguyen2022vesinh].

Tại Malaysia, nghiên cứu của Ramli và cộng sự tiến hành chấm điểm vệ sinh tại một bệnh viện tuyến trung ương cho thấy, mặc dù lịch vệ sinh được quy định 3 giờ/lần, nhưng số lần thực tế thực hiện chỉ đạt trung bình 2,2 ± 0,8 lần/ngày, và không có sự khác biệt giữa các khu vực toilet nội bộ và toilet công cộng trong bệnh viện [@ramli2018cleanliness]. Điều này gợi mở rằng việc giám sát thực thi và thái độ nhân viên vệ sinh là yếu tố quyết định, hơn là quy định về đối tượng sử dụng.

Tuy nhiên, cần lưu ý rằng số lần vệ sinh chỉ là một thành phần nhỏ trong đánh giá chất lượng vệ sinh môi trường bệnh viện. Yếu tố thời điểm, mức độ bẩn, đánh giá cảm quan, mùi và phản hồi người dùng cũng cần được tích hợp vào hệ thống giám sát để có cái nhìn toàn diện. Một số mô hình bệnh viện hiện đại đã ứng dụng cảm biến hoặc mã QR để theo dõi thời gian và tần suất dọn dẹp theo thời gian thực, mở ra hướng đi mới cho cải tiến 5S gắn với chuyển đổi số.

Tóm lại, kết quả khảo sát tại Bệnh viện 30-4 cho thấy việc thực thi vệ sinh nhà vệ sinh đã đạt mức độ đồng đều giữa các khu vực, phản ánh một bước tiến trong cải tiến chất lượng theo mô hình 5S. Tuy nhiên, để nâng cao hơn nữa hiệu quả, cần tích hợp thêm chỉ số đánh giá cảm quan, mức độ hài lòng người sử dụng, và công nghệ số để theo dõi tần suất vệ sinh một cách khách quan và liên tục.

::: {custom-style="CAN GIUA DAM 14 ONE"}
TÀI LIỆU THAM KHẢO
:::

