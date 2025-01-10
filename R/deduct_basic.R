## 基礎控除

deduct_basic <- function(person) {
  b1 <- 2400
  b2 <- 2450
  b3 <- 2500
  
  income_earned <- person$income_earned
  
  ## 国民民主党の案に従う場合の加算額
  add <- 0
  
  ## 所得税・住民税に応じて、引数で変えられるようにした

  ## 所得税の控除額を返す場合
  if (income_earned <= b1) {
    ded_income <- 48
  } else if (income_earned > b1 & income_earned <= b2) {
    ded_income <- 32
  } else if (income_earned > b2 & income_earned <= b3) {
    ded_income <- 16
  } else {
    ded_income <- 0
  }

  ## 住民税の控除額を返す場合
  if (income_earned <= b1) {
    ded_resid <- 43
  } else if (income_earned > b1 & income_earned <= b2) {
    ded_resid <- 29
  } else if (income_earned > b2 & income_earned <= b3) {
    ded_resid <- 15
  } else {
    ded_resid <- 0
  }
  
  ## 国民民主党の案を反映できるよう、add項を追加
  tibble(
    basic_ded_income = ded_income + add,
    basic_ded_resid = ded_resid + add
  )
}

#deduct_basic(ex1)
