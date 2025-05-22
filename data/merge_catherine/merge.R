
## Load packages
library(haven)
library(readxl)
library(writexl)
library(tidyverse)

## Load data
wave1 <- read_dta("data//full.dta")
network_full <- read_excel("data/network_miguel/network_characteristics_without_outliers.xlsx", sheet = "Sheet1")
key <- read_excel("data/merge_catherine/id_map.xlsx")

## Rename ID variables
wave1 <- wave1 |> rename(ID1 = A)
network_full <- network_full |> rename (ID2 = ID...1) |> select(-ID...100)
key <- key |> rename (ID1 = Catherine, ID2 = Miguel)

## Remove NAs (-99) from key
key_clean <- key |> 
  filter(ID1 != -99 & ID2 != -99)

## Join the key to each dataset 

# Join the key to wave1
wave1_with_key <- merge(wave1, key_clean, by.x = "ID1", by.y = "ID1", all.x = TRUE)

# Join the key to network_full
network_full_with_key <- merge(network_full, key_clean, by.x = "ID2", by.y = "ID2", all.x = TRUE)

# Filter out cases in datasets that do not have corresponding matching in the key

sum(is.na(wave1_with_key$ID2))
sum(is.na(network_full_with_key$ID1))

wave1_cleaned <- wave1_with_key |> 
  filter(!is.na(ID2))

network_full_cleaned <- network_full_with_key |> 
  filter(!is.na(ID1))

## Merge datasets by mapped ID

merged_data <- merge(wave1_cleaned, network_full_cleaned, by = "ID2", all = TRUE)

## Remove extra ID columns

merged_data <- merged_data |> select(-ID1.y) |> rename (ID1 = ID1.x)

# Export dataset
write_csv(merged_data, "data/merged_data.csv")
