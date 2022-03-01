des
tab death
* Death là biến phụ thuộc dạng nhị phân*
histogram v25d
* v25d là biến định lượng theo phân phối chuẩn*
* phân tích hồi quy logistic hai biến
logit death v25d
bayes: logit death v25d
* Chẩn đoán MCMC trước khi diễn giải kết quả
* vẽ trace plot cho v25d
bayesgraph trace {death: v25d}
* vữ cho intercept
bayesgraph trace {death: _cons}
* vẽ cho cả 2 tham số trong 1 hình
bayesgraph trace {death: _cons} {death: v25d}, byparm
* Vẽ tất cả các tham số
bayesgraph trace _all, byparm
* Vẽ tất cả các tham số kiểm tra Autocorreclation plot*
bayesgraph ac _all, byparm
* vẽ CUmulative sums plot*
bayesgraph cusum _all, byparm
*@@@@@@@@
* Vẽ phân phối hậu định
bayesgraph hist _all, byparm norm
* Vẽ 4 loại biểu đồ trong 1 hình
bayesgraph diagnostics {death: v25d}
* Kiểm tra Effect SSIZE
bayesstats ess
*DIỄN GIẢI KẾT QUẢ
bayesstats summary
* DIỄN GIẢI KẾT QUẢ
** Nồng độ vitamin D càng cao thì tử vong càng thấp. Coeff=-0.012. CrI95% (-0,21)- (-0.004)
* KIỂM ĐINH BỘ 3
* compVal
bayestest interval ({death: v25d}, lower(0))///
({death: v25d}, lower(0.1)) ///
({death: v25d}, lower(0.15))
* ROPE
bayestest interval {bodyfat: weight}, lower(0) upper(0.1)
* BAYES FACTOR
*???????????????