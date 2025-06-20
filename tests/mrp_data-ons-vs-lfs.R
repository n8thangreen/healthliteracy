# check that the independent and joint probability estimates are similar

load(here::here("data/mrp_data_ons.RData"))
mrp_data_ons <- mrp_data
load(here::here("data/mrp_data_lfs.RData"))
mrp_data_lfs <- mrp_data

xx <-
  merge(mrp_data_ons[[1]],
        mrp_data_lfs[[1]],
        by = c("imd", "workingstatus", "sex", "own_home", "age", "ethnicity", "gross_income",
               "uk_born", "english_lang", "qualification", "job_status")) |>
  mutate(p_diff = product_p.y - product_p.x)

summary(xx$p_diff)
hist(xx$p_diff, breaks = 1000, xlim = c(-0.001, 0.001))
