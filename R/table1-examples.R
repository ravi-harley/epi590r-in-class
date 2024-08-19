library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


Yes

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(race_eth_cat, region_cat, starts_with("sleep"), income),
	label = list(
		race_eth_cat ~ "Race/Ethnicity",
		region_cat ~ "Region",
		sleep_wkdy ~ "Hours of Sleep (Weekday)",
		sleep_wknd ~ "Hours of Sleep (Weekend)",
		income ~ "Income"),
	statistic = list(
		income ~ "{p90}",
		income ~ "{p10}",
		starts_with("sleep") ~ "min = {min}",
		starts_with("sleep") ~ "max = {max}"),
	digits = list(
		income ~ (3),
		starts_with("sleep") ~ (1))) |>
		add_p(test = list(all_continuous() ~ "t.test",
											all_categorical() ~ "chisq.test")) |>
		add_overall(col_label = "**Total**") |>
		modify_table_styling(
			columns = label,
			rows = label == "Race/Ethnicity",
			footnote = "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
		)










