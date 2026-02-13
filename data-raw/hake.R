# Outputs from the hake stock assessment. Run code line-by-line and check plots.
# For hake SRG in 2026, just running the relevant lines by hand to be able to get the
# spawning biomass intervals calculated for the talk.

# hake-2024/hake_mcmc.rds contains a large tibble, with each row being an MCMC
# sample, and columns representing various things we might want to look
# at. Extracting some of the useful ones here to save as data objects.

# `mcmc_save()` could be used for the base model results as the full MCMC
# results were saved in the .rds for the base model. But this is not the case for the
# forecasts, which are done partway down.


# Change assess_yr each year if needed, the rest is automated, though save older
# versions like in pacea.

load_all()
library(dplyr)

assess_yr <- 2026       # Year of the hake assessment; update each year

# The hake-2024/*.rds files is built automatically from (Andy or Chris Grandin)
#  running `mcmc_save()` in the hake-assessment repo after having just built the document.
# mcmc_save() automatically creates the hake-<assess_yr> directory here if it doesn't exist,
#  and puts the file into it, all named with assess_yr.

hake_dir <- paste0(here::here(),
                   "/data-raw/hake-",
                   assess_yr,
                   "/")

# Full tibble of mcmc results
hake_mcmc <- readRDS(paste0(hake_dir,
                            "hake_mcmc.rds"))

# Check that we don't need to keep the Iteration column, can just use rownames
# if want to get back to the MCMC sample number

expect_equal(as.numeric(rownames(hake_mcmc)),
             dplyr::pull(hake_mcmc, "Iter"))

# These are identical
expect_equal(hake_mcmc["R_Virgin", ],
             hake_mcmc["R_Initial", ])

hake_mcmc

# This is everything that is saved:
names(hake_mcmc)

# Jump to the end for calculations for 2026 SRG talk.

# Recruitment
# Define objects similar to in pacea, but with _mcmc after

hake_recruitment_mcmc <- dplyr::select(hake_mcmc,
                                       "R_Virgin",
                                       "R_1966":"R_2024")

names(hake_recruitment_mcmc) <- gsub(pattern = "R_",
                                     replacement = "",
                                     x = names(hake_recruitment_mcmc))

hake_recruitment_mcmc <- as_tibble(hake_recruitment_mcmc/1e6)    # Convert from thousands
                                                                 # to billions of fish
usethis::use_data(hake_recruitment_mcmc,
                  overwrite = TRUE)

rec_2021 <- dplyr::pull(hake_recruitment_mcmc,
                        `2021`)

attr(rec_2021, "axis_name") <-
  "Recruitment (billions of age-0 fish)"

usethis::use_data(rec_2021,
                  overwrite = TRUE)

# class(hake_recruitment_new) <- c("pacea_recruitment",
#                                 class(hake_recruitment_new))


# Relative spawning biomass

hake_relative_biomass_mcmc <- dplyr::select(hake_mcmc,
                                            "Bratio_1967":"Bratio_2027")
                                            # There is no
                                            # Bratio_unfished, presumably since
                                            # should all be 1
hake_relative_biomass_mcmc                  # NOte we will add forecasts on later

names(hake_relative_biomass_mcmc) <- gsub(pattern = "Bratio_",
                                          replacement = "",
                                          x = names(hake_relative_biomass_mcmc))

quantile(hake_relative_biomass_mcmc$`2024`, c(0.05, 0.50, 0.95))   # These match
                                                                   # Table g first row

quantile(hake_relative_biomass_mcmc$`2027`, c(0.05, 0.50, 0.95))   # These don't
                                                                   # match anything, so remove them
hake_relative_biomass_mcmc <- dplyr::select(hake_relative_biomass_mcmc,
                                            -c("2025", "2026", "2027"))

# Now add in the forecasts.
# Full MCMC results are not already saved in an .rds for the
#  hake assessment build, so I had to extract
#  the relevant folders from the server. Just doing future catches of 350,000,
# which is close to the average of the last 10 years, as shown in
#  Fig j of the assessment. Doing this somewhat manually, based on
# `hake::load_forecasts()`.

source(paste0(here::here(),
              "/R-orig/create-hake-forecast-mcmc.R"))

# Relative spawning biomass, the function create_hake_forecast_mcmc() has a
# default catch alternative of "06-350000", corresponding the 350,000 t per year.
rel_2025 <- create_hake_forecast_mcmc(forecast_year_dir = 2025)
rel_2027 <- create_hake_forecast_mcmc(forecast_year_dir = 2027)
# expect_equal(rel_2025, rel_2027)   # As expected, fails because it's a new
#  MCMC simulation (though historic values match)

quants_2025 <- apply(rel_2025,
                     2,
                     quantile,
                     probs = c(0.05, 0.5, 0.95),
                     na.rm = TRUE)

# Using forecast-year-2025, this matches Table g for 2024, 2025, 2026, but not
# 2027. Note the use of 5th and 95th percentiles in Table g.

## t(quants_2025) %>% round(digits = 2)
##        5%  50%  95%
## 2024 0.51 0.99 2.01
## 2025 0.49 1.01 2.13
## 2026 0.42 0.96 2.17
## 2027 0.32 0.73 1.83

# However, this next one matches Table g, so use this as covers all years.
quants_2027 <- apply(rel_2027,
                     2,
                     quantile,
                     probs = c(0.05, 0.5, 0.95),
                     na.rm = TRUE)

## t(quants_2027) %>% round(digits = 2)
##        5%  50%  95%
## 2024 0.51 0.99 2.01
## 2025 0.49 1.01 2.13
## 2026 0.42 0.96 2.17
## 2027 0.35 0.88 2.20

expect_equal(rel_2027$`2024`,
             hake_relative_biomass_mcmc$`2024`)   # So no need to resave 2024

hake_relative_biomass_mcmc_forecast <- dplyr::select(rel_2027,
                                                     -c("2024"))

hake_relative_biomass_mcmc <- cbind(hake_relative_biomass_mcmc,
                                    hake_relative_biomass_mcmc_forecast) %>%
  tibble::as_tibble()

usethis::use_data(hake_relative_biomass_mcmc,
                  overwrite = TRUE)

# Female spawning biomass

# These are identical
expect_equal(hake_mcmc["B_Virgin", ],
             hake_mcmc["B_Initial", ])

hake_spawning_biomass_mcmc <- dplyr::select(hake_mcmc,
                                            "B_Virgin",
                                            "B_1966":"B_2024")
hake_spawning_biomass_mcmc

names(hake_spawning_biomass_mcmc) <- gsub(pattern = "B_",
                                          replacement = "",
                                          x = names(hake_spawning_biomass_mcmc))

# Convert from tonnes to millions of tonnes to have reasonable numbers:
hake_spawning_biomass_mcmc <- as_tibble(hake_spawning_biomass_mcmc/1e6)

quantile(hake_spawning_biomass_mcmc$`2024`, c(0.025, 0.50, 0.975))
  # These match 5th bullet of one-page summary (except 97.5% is off by 1 tonne).

quantile(hake_spawning_biomass_mcmc$`2023`, c(0.025, 0.50, 0.975))
  # These match 5th bullet of one-page summary that gives 2023 spawning biomass

usethis::use_data(hake_spawning_biomass_mcmc,
                  overwrite = TRUE)

# Trying to get experimental MCMC chains from server, as being used for Kelli's
# manuscript. Use exp for suffix for now.

hake_dir_exp <- paste0(here::here(),
                       "/data-raw/hake-",
                       assess_yr,
                       "/02-version-05-test-models/")

hake_14_long <- readRDS(paste0(hake_dir_exp,
                            "14-long-base.rds"))
# Contains all MCMC results and maybe more, as a list object.
# There's an hake.Rdata file in mcmc/ on the server but I don't think
#    that's useful, has things like number of chains etc, don't think it has results.

# Using some from hake-assessment-2024::mcmc_save()
dat <- as_tibble(hake_14_long$mcmc)

keep <- select(dat,
               "Iter":"SR_BH_steep",
               "SSB_Virgin":"Recr_2027",
               "Bratio_1967":"Recr_unfished",
               "B_MSY/SSB_unfished":"ForeCatch_2027",
               "Dyn_Bzero_Virg":"ln(SSB)_2023")

names(keep) = gsub(pattern = "SSB_",
                   replacement = "B_",
                   x = names(keep))

names(keep) = gsub(pattern = "Recr_",
                   replacement = "R_",
                   x = names(keep))

keep

# Copying from above to just have recruitment, need 14_long
hake_recruitment_mcmc_14_long <- dplyr::select(keep,
                                               "R_Virgin",
                                               "R_1966":"R_2024")

names(hake_recruitment_mcmc_14_long) <- gsub(pattern = "R_",
                                             replacement = "",
                                             x = names(hake_recruitment_mcmc_14_long))

hake_recruitment_mcmc_14_long <- as_tibble(hake_recruitment_mcmc_14_long/1e6)    # Convert from thousands
                                                                 # to billions of fish

usethis::use_data(hake_recruitment_mcmc_14_long,
                  overwrite = TRUE)

# 2026 SRG - jumping to here to do some calculations, not saving anything new in
# the package.

# Relative spawning biomass

hake_relative_biomass_mcmc <- dplyr::select(hake_mcmc,
                                            "Bratio_1967":"Bratio_2027")
                                            # There is no
                                            # Bratio_unfished, presumably since
                                            # should all be 1
hake_relative_biomass_mcmc                  # NOte we will add forecasts on later

names(hake_relative_biomass_mcmc) <- gsub(pattern = "Bratio_",
                                          replacement = "",
                                          x = names(hake_relative_biomass_mcmc))


create_intervals(hake_relative_biomass_mcmc$`2026`)$intervals %>% a()
# ETI match 2026 assessment bullet point

# 90%:
create_intervals(hake_relative_biomass_mcmc$`2026`,
                 credibility = 0.9)$intervals %>% a()
# ETI match 2026 assessment Table 3 first row


# Female spawning biomass

hake_spawning_biomass_mcmc <- dplyr::select(hake_mcmc,
                                            "B_Virgin",
                                            "B_1966":"B_2026")
hake_spawning_biomass_mcmc

names(hake_spawning_biomass_mcmc) <- gsub(pattern = "B_",
                                          replacement = "",
                                          x = names(hake_spawning_biomass_mcmc))

# Convert from tonnes to millions of tonnes to have reasonable numbers:
hake_spawning_biomass_mcmc <- as_tibble(hake_spawning_biomass_mcmc/1e6)

create_intervals(hake_spawning_biomass_mcmc$`2026`)$intervals %>% a()
# ETI match 2026 assessment bullet point

# 90%:
B_res_90 <- create_intervals(hake_spawning_biomass_mcmc$`2026`,
                 credibility = 0.9)

