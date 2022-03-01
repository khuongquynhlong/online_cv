* Đổi đường dẫn để lưu kết quả và các bảng xuất ra từ Stata
* Thường để các file liên quan ở thư mục này
cd "D:\Dropbox\Long\Projects\Courses\Basic\Bai tap"

* Đọc dữ liệu
use "thieumau.dta", clear

* Phần 1:
*------------------------------ Tạo các biến số -------------------------------*

*--- -	Tuổi được tính bằng cách lấy 2019 trừ đi năm sinh 
gen tuoi = 2019 - namsinh
label variable tuoi "Tuoi (years)" // Gán nhãn biến

*--- Nhóm tuổi > 70 và < 70
gen nhomtuoi = 1 
replace nhomtuoi = 2 if tuoi > 70
lab def nhomtuoi 1"<=70 tuoi" 2">70 tuoi" // tạo nhãn giá trị, lab val = label value
lab val nhomtuoi nhomtuoi // gán nhãn cho biến nhomtuoi
lab var nhomtuoi "Nhom tuoi" // gán nhãn biến, giống như tuổi, lab var là viết tắ của label variable

*--- -	Thừa cân khi BMI ≥ 23 kg/m2
sum cannang chieucao // Chiều cao đang để đơn vị cm --> chuyển qua m

gen bmi = cannang / (chieucao/100)^2
lab var bmi "BMI (kg/m2)" 

* cách 1
gen thuacan = 0
replace thuacan = 1 if bmi >= 23
lab var thuacan "Thua can, BMI>= 23"

label define yesno 1"Co" 0"Khong" 
label value thuacan yesno 

/*
* cách 2 (chọn 1 trong 2 cách)
gen thuacan = bmi >= 23
lab var thuacan "Thua can, BMI>= 23"
lab def yesno 1"Co" 0"Khong" 
lab val thuacan yesno 
*/

/*
Mức độ đường huyết là biến thứ tự bao gồm:
      •	Bình thường khi < 6,2 mmol/l, 
      •	Tăng vừa khi từ 6,2 – 10,2 mmol/l
      •	Tăng cao khi > 10,2 mmol/l
*/

recode dhuyet (min/6.199999 = 1 "Binh thuong") (6.2/10.2 = 2 "Vua") (10.2/max = 3 "Cao"), gen(dhuyet_mucdo)
lab var dhuyet_mucdo "Muc do duong huyet"

/*
Mức độ kiểm soát đường huyết được chia làm 3 nhóm gồm:
      •	Kiểm soát đường huyết tốt khi chỉ số HbA1c < 7%, 
      •	Kiểm soát vừa khi 7 ≤ chỉ số HbA1c ≤ 8% 
      •	Kiểm soát kém khi chỉ số HbA1c > 8%.
*/
recode hba1c (min/6.9999 = 1 "Tot") (7/8 = 2 "Vua") (8.01/max = 3 "Kem"), gen(dhuyet_kiemsoat)
lab var dhuyet_kiemsoat "Kiem soat duong huyet"

/* 
Thiếu máu khi 
      •	Hb < 13 g/l đối với nam hoặc
      •	Hb < 12 g/l đối với nữ 
*/

tab gioi // 1 nam; 0 nu
gen thieumau = 0 if hb!= .
replace thieumau = 1 if (hb < 13 & gioi ==1) | (hb < 12 & gioi ==0)
lab val thieumau yesno // gán nhãn giá trị, dùng lại nhãn "yesno" đã khai báo ở trên (dòng 32)
label variable thieumau "Thieu mau, 1 = co, 0 = khong"


* Phần 2:
*-------------------------------- Phân tích  ----------------------------------*

* Bảng 1: Đặc điểm của người tham gia nghiên cứu (n = zzz)
count // Đếm số mẫu (370)

* Biến định tính
tab1 gioi nhomtuoi thuacan dhuyet_mucdo dhuyet_kiemsoat thieumau

* Biến định lượng
hist dhuyet, norm // Không chuẩn --> nên dùng trung vị (khoảng tứ phân vị)
hist hba1c, norm // Không chuẩn --> nên dùng trung vị (khoảng tứ phân vị)
hist hb, norm // Chuẩn --> nên dùng trung bình (độ lệch chuẩn)

sum dhuyet hba1c, detail
sum hb

* Bảng 2: Phân bố chỉ số Hb theo các đặc điểm của người tham gia nghiên cứu (n = zzz)
* Có rất nhiều cách để làm bảng này (ở đây tóm tắt 2 cách)

* Làm thủ công từng biến (cách 1)
bysort gioi: sum hb
bysort nhomtuoi: sum hb
bysort thuacan: sum hb
bysort dhuyet_mucdo: sum hb
bysort dhuyet_kiemsoat: sum hb

* Dùng vòng lặp (cách 2)

foreach var of varlist gioi nhomtuoi thuacan dhuyet_mucdo dhuyet_kiemsoat {
	bysort `var': sum hb
}

* kiểm định
* Theo giới tính
sdtest hb, by(gioi) // Phương sai không đồng nhất
ttest hb, by(gioi) unequal

* Theo nhóm tuổi
sdtest hb, by(nhomtuoi) // Phương sai đồng nhất
ttest hb, by(nhomtuoi)

* Theo nhóm thừa cân
sdtest hb, by(thuacan) // Phương sai đồng nhất
ttest hb, by(thuacan)

* Theo mức độ đường huyết
oneway hb dhuyet_mucdo, tab bon // thỏa tiêu chuẩn ANOVA

* Theo mức độ kiểm soát đường huyết
oneway hb dhuyet_kiemsoat, tab // Phương sai không đồng nhất --> không thỏa tiêu chuẩn ANOVA
kwallis hb, by(dhuyet_kiemsoat) // Kruskal wallis 

* Bảng 3. Mối liên quan giữa chỉ số Hb với các đặc tính của người tham gia nghiên cứu (n = zzz)
* Hồi quy tuyến tính
* Đơn biến
reg hb gioi
reg hb tuoi
reg hb thuacan
reg hb hba1c
reg hb i.dhuyet_mucdo
reg hb i.dhuyet_kiemsoat

* Đa biến (Vì ví dụ này khá ít biến, các biến cũng đều có thể liên quan tới hb --> có thể cho full các biến vào, trừ biến HBA1C vì đã có dhuyet_kiemsoat)
reg hb gioi tuoi thuacan i.dhuyet_mucdo i.dhuyet_kiemsoat


* Bảng 4. Mối liên quan giữa thiếu máu với các đặc điểm của người tham gia nghiên cứu (n = zzz) 

* Mô tả
tab gioi thieumau, exp ro chi
tab nhomtuoi thieumau, exp ro chi
tab thuacan thieumau, exp ro chi
tab dhuyet_mucdo thieumau, exp ro chi
tab dhuyet_kiemsoat thieumau, exp ro chi


* Có thể sử dụng loop cho nhanh
foreach var of varlist gioi nhomtuoi thuacan dhuyet_mucdo dhuyet_kiemsoat {
	tab `var' thieumau, exp ro chi
}

* Hồi quy logistic đơn biến
logistic thieumau gioi
logistic thieumau nhomtuoi
logistic thieumau thuacan
logistic thieumau i.dhuyet_mucdo
logistic thieumau i.dhuyet_kiemsoat

* Hồi quy logistic đa biến
* Giống như với hồi quy tuyến tính ở trên
* Vì ví dụ này khá ít biến, các biến cũng đều có thể liên quan tới thiếu máu --> có thể cho full các biến vào
logistic thieumau gioi nhomtuoi thuacan i.dhuyet_mucdo i.dhuyet_kiemsoat


********************************************************************************
* Khi làm thực tế, để cho nhanh thì mình có thể áp dụng các phương pháp tạo bảng tự động
* Cách dưới đây dùng để tạo các bảng tự động (Rất cần thiết khi nhiều biến số)
* Trước tiên cần cài đặt các câu lệnh cần thiết, càn phải có kết nối internet
* Lưu ý, chỉ cần cài 1 lần duy nhất, lần sau không cần cài lại (dòng 177-191)

* Cài lệnh "table1"
net from http://fmwww.bc.edu/RePEc/bocode/t
net install table1

* Cài lệnh "basetable"
net from http://fmwww.bc.edu/RePEc/bocode/b
net install basetable

* Cài lệnh "tabout"
net from http://fmwww.bc.edu/RePEc/bocode/t
net install tabout 

* Cài lệnh "eststo và esttab"
net from http://www.stata-journal.com/software/sj14-2
net install st0085_2



*----- Bảng 1: Đặc điểm của người tham gia nghiên cứu (n = zzz)
* Sử dụng Table 1 (lưu ý, dấu /// chỉ có chức năng xuống dòng cho dễ nhìn)

table1, vars(gioi cat\ nhomtuoi cat\ thuacan cat\ dhuyet conts \ dhuyet_mucdo cat\ /// 
		hba1c conts \ dhuyet_kiemsoat cat\ hb contn \ thieumau cat\) ///
		format(%9.1f) cformat(%9.1f) onecol ///
		saving("Table1.xls", sheet (Table1, replace))  // lưu thành file excel tên là "Table1", đường dẫn được đặt ở đầu bài


*----- Bảng 2. Phân bố chỉ số Hb theo các đặc điểm của người tham gia nghiên cứu (n = zzz)

foreach var of varlist gioi nhomtuoi thuacan dhuyet_mucdo dhuyet_kiemsoat { 
	tabout `var' ///
	using "Table2.xls", /// 
	cells(mean hb sd hb) sum ///  
	npos(lab) format (1 1) ///  
	append style(xls)
}


*----- Bảng 3. Mối liên quan giữa chỉ số Hb với các đặc tính của người tham gia nghiên cứu (n = zzz)
eststo: reg hb gioi tuoi thuacan  i.dhuyet_mucdo i.dhuyet_kiemsoat

esttab using "Table3.rtf", b(2) ci(2) replace /// 
wide label nogaps star( * 0.05 ** 0.01 *** 0.001) stats(r2 N) 
eststo clear 

*----- Bảng 4. Mối liên quan giữa thiếu máu với các đặc điểm của người tham gia nghiên cứu (n = zzz) 
* Mô tả

basetable 	thieumau gioi (r) nhomtuoi (r) thuacan (r) dhuyet_mucdo (r) dhuyet_kiemsoat(r), ///
			toxl("Table4.xls", table3, replace)
* Hồi quy logistic đơn biến
eststo: logistic thieumau gioi
eststo: logistic thieumau nhomtuoi
eststo: logistic thieumau thuacan
eststo: logistic thieumau i.dhuyet_mucdo
eststo: logistic thieumau i.dhuyet_kiemsoat

esttab using "Table4.rtf", b(2) ci(2) replace /// 
wide eform label nogaps star( * 0.05 ** 0.01 *** 0.001) stats(r2 N) 
eststo clear 

*----- Bảng 5: Các yếu tố liên quan đến thiếu máu bằng mô hình hồi qui logistic đa biến (n= )

* Hồi quy logistic

eststo: logistic thieumau gioi nhomtuoi thuacan i.dhuyet_mucdo i.dhuyet_kiemsoat

esttab using "Table5.rtf", b(2) ci(2) replace /// 
wide eform label nogaps star( * 0.05 ** 0.01 *** 0.001) stats(r2 N) 
eststo clear 







