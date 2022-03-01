********************************************************************************
*********************** Đại Học Y Tế Công Cộng Hà Nội***************************

*-------------------------  Phân tích thống kê cơ bản -------------------------*
*------------------------------ Do file tạo bảng ------------------------------*
********************************************************************************
* Set up
clear all
set more off

*----- Cài đặt các gói cần thiết (chỉ cần chạy 1 lần duy nhất)
ssc install table1

net from http://www.stata-journal.com/software/sj15-3
net install dm0042_2

net from http://www.stata-journal.com/software/sj14-2
net install st0085_2 

*----- Tạo đường dẫn đến các thư mục

global rawdata D:\Dropbox\Long\Projects\Courses\Writing\Stata\Rawdata
global result D:\Dropbox\Long\Projects\Courses\Writing\Stata\Results


*----- đọc dữ liệu (từ đường dẫn $rawdata)

use "$rawdata\Hypertension.dta", clear

*------ Table 1

* Mô tả hết các biến
* lưu file excel tên là "Desc.xls" và sheet tên là "Table1"
* Đường dẫn là $result (xem lại ở global)

table1, vars(age contn\ age conts\ age_gr cat\ sex cat\ occupation cat\ marital cat\ ///
			weight contn\ height contn\ bmi contn\ overweight cat\ health_status cat\ ///
			bmi_gr cat\ systolic_BP contn\ diastolic_BP contn\ hypertension cat) ///
			format(%9.1f) cformat(%9.1f) onecol ///
			saving("$result\Desc.xls", sheet (Table1, replace)) 

* Mô tả trong nhóm điều trị (treatment == 1)
* lưu tên sheet là "Table1b"

table1 if treatment == 1, vars(age contn\ age conts\ age_gr cat\ sex cat\ occupation cat\ marital cat\ ///
								weight contn\ height contn\ bmi contn\ overweight cat\ health_status cat\ ///
								bmi_gr cat\ systolic_BP contn\ diastolic_BP contn\ hypertension cat) ///
								format(%9.1f) cformat(%9.1f) onecol ///
								saving("$result\Desc.xls", sheet (Table1b, replace)) 
			

*------ Table 2
* Mô tả và so sánh giữa 2 nhóm Treatment vs. Control
* Vẫn lưu ở file excel "Desc.xls", tên sheet là "Table2"
table1, by(treatment) vars(age contn\ age conts\ age_gr cat\ sex cat\ occupation cat\ marital cat\ ///
							weight contn\ height contn\ bmi contn\ overweight cat\ health_status cat\ ///
							bmi_gr cat\ systolic_BP contn\ diastolic_BP contn\ hypertension cat) ///
							format(%9.1f) cformat(%9.1f) onecol ///
							saving("$result\result.xls", sheet (Table2, replace)) 

* Giả sử chỉ chọn nhóm bị Overweight
* Lưu ở sheet "Table2b"
table1 if overweight == 1, by(treatment) vars(age contn\ age conts\ age_gr cat\ sex cat\ ///
												occupation cat\ marital cat\ weight contn\ ///
												height contn\ bmi contn\ health_status cat\ ///
												bmi_gr cat\ systolic_BP contn\ diastolic_BP contn\ ///
												hypertension cat) ///
												format(%9.1f) cformat(%9.1f) onecol ///
												saving("$result\Desc.xls", sheet (Table2b, replace)) 

*------ Table 3 (Hồi quy đa biến)

* Trước tiên cần lưu mô hình {eststo: } (ở đây là mô hình logistic)

eststo: logistic hypertension age i.sex i.occupation i.marital bmi i.health_status i.treatment

* Xuất ra file MS word
* Lưu ở file tên là "Table3b"
esttab using "$result\Table3.rtf", b(2) ci(2) replace /// 
		wide label nogaps star( * 0.05 ** 0.01 *** 0.001) stats(r2 N) eform

* Xóa bỏ bộ nhớ đang lưu (tránh chồng chéo với nhau)
eststo clear


*------ Table 3 (Hồi quy đơn biến)
* "Gom" tất cả các biến cần phân tích vào object "indvar", dùng {global}

global indvar age age_gr sex occupation marital weight height bmi overweight health_status treatment

* Điều kiện nếu biến có > 10 giá trị hoặc là biến nhị phân ==> coi như biến liên tục (không cần i.)
* Nếu biến có <= 10 giá trị và khác biến nhị phân ==> coi là biến phân loại (cần i.)
* Thay đổi giá trị 10..., thay đổi mô hình... cho phù hợp với từng dữ liệu
* Lưu file tên là "Table3a"

*----- Hồi quy logistic
foreach var of varlist $indvar {
	
	distinct `var'
	local x = r(ndistinct)
	
	if `x' < 10 & `x' != 2 {
		eststo: logistic hypertension i.`var'
	}
	if `x' > 10 | `x' == 2 {
		eststo: logistic hypertension `var'
	}
}

* Hồi quy logistic, dùng "eform" để xuất ra OR
esttab using "$result\Table3a.rtf", b(2) ci(2) replace /// 
		wide label nogaps star( * 0.05 ** 0.01 *** 0.001) stats(r2 N) eform
eststo clear

*----- Hồi quy Tuyến tính

foreach var of varlist $indvar {
	
	distinct `var'
	local x = r(ndistinct)
	
	if `x' < 10 & `x' != 2 {
		eststo: reg systolic_BP i.`var'
	}
	if `x' > 10 | `x' == 2 {
		eststo: reg systolic_BP `var'
	}
}

* Hồi quy Tuyến tính, không dùng "eform" (lấy hệ số beta)
esttab using "$result\Table3c.rtf", b(2) ci(2) replace /// 
		wide label nogaps star( * 0.05 ** 0.01 *** 0.001) stats(r2 N)
eststo clear

********************************************************************************
*----- End
*  June 06-08, 2020
* @ Khương Quỳnh Long 

