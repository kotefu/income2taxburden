## 協会けんぽの保険料額データを加工する
## https://www.kyoukaikenpo.or.jp/g7/cat330/sb3150/r06/r6ryougakuhyou3gatukara/
## Excel [都道府県別]のファイル

library(here)
library(tidyverse)
library(readxl)

my_colname <- c(
  "標準報酬_等級", 
  "標準報酬_月額",
  "報酬月額_下端", 
  "報酬月額_上端", 
  "保険料_介護保険2号非該当_全額", 
  "保険料_介護保険2号非該当_折半額", 
  "保険料_介護保険2号該当_全額", 
  "保険料_介護保険2号該当_折半額",
  "厚生年金保険料_全額",
  "厚生年金保険料_折半額"
  )

file <- here("data/kyokai_kenpo/kyokai_kenpo_r6.xlsx")

## 全表で共通部分を抽出
monthly <- read_excel(
  file,
  skip = 10
  ) |> 
  select(2, 3, 5) |> 
  set_names("Monthly", "Lower", "Upper") |> 
  slice(1:50) |> 
  mutate(Class = 1:50)

## シート名から取得した都道府県名を列として加えるため
sheets <- readxl::excel_sheets(file)

## 全国健康保険協会管掌健康保険料のデータを取得する関数
get_premium <- function(file, sheet) { 
  data <- read_excel(
    file,
    skip = 10,
    sheet = sheet
    ) |> 
    select(`...6`, `...7`, `...8`, `...9`) |> 
    set_names(my_colname[5:8]) |> 
    slice(1:50) |> 
    mutate(
      Pref = sheet,
      Class = 1:50,
      .before = "保険料_介護保険2号非該当_全額"
      ) |> 
    left_join(monthly, by = join_by(Class))
  }

## データ取得作業
result <- map2(file, sheets, \(x, y) get_premium(x, y)) |>  
  list_rbind()

## 並べ替え・四捨五入と保存
result <- result |> 
  relocate(Pref, Class, Monthly, Lower, Upper)

result_rounded <- result |> 
  mutate(
    across(where(is.double), \(x) round(x))
  )

write_csv(result_rounded, here("data/kyokai_kenpo/premiums.csv"))
