## 給与所得控除

deduct_income <- function(person) {
  
  income <- person$income_earned
  
  ## 控除の金額がジャンプする境界となる収入額
  border1 <- 162.5
  border2 <- 180
  border3 <- 360
  border4 <- 660
  border5 <- 850
  
  if (income <= border1) {
    d <- 55
  } else if (income > border1 & income <= border2) {
    d <- income * 0.4 - 10
  } else if (income > border2 & income <= border3) {
    d <- income * 0.3 + 8
  } else if (income > border3 & income <= border4) {
    d <- income * 0.2 + 44
  } else if (income > border4 & income <= border5) {
    d <- income * 0.1 + 110
  } else if (income > border5) {
    d <- 195
  }
  
  d
}