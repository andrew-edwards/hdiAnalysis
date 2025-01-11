# Adapting from pacea/data-raw/herring/herring.R. Not saving as data object in
# package, just local rds file. Then saving what we want in report/results-herring.Rmd.

# Outputs from the Pacific Herring stock assessments. Run code line-by-line and check plots.
#  See ?herring for details. Combining parts of Matt's sample code, hake.R,  harbour-seals.R,
#  and pacea-save() from hake-assessment package.

# From DFO (2024) STOCK STATUS UPDATE WITH APPLICATION OF MANAGEMENT PROCEDURES
# FOR PACIFIC HERRING (CLUPEA PALLASII) IN BRITISH COLUMBIA: STATUS IN 2023 AND
# FORECAST FOR 2024. Science Response.

# Earlier longer email from Matt, shortened:

## These are the five *.RData files (one for each SAR) with iSCAM model output for
## the assessment that we used this year and for the last few years -- it’s not the
## new one that was reviewed in June 2023  which we will use next year.

## There are five major herring stock assessment regions (SARs): Haida Gwaii (HG),
## Prince Rupert District (PRD), Central Coast (CC), Strait of Georgia (SoG), and
## West Coast of Vancouver Island (WCVI). (There are also two minor SARs but we
## don’t do a full stock assessment for these ones.) I just added these to the
## names of the *.RData files (usually they’re just called “aaa_gfiscam.RData” and
## stored in their respective folders). For herring we use median and
## 90% CI. Note that spawning biomass goes up to 2024, but 2024 is a projection so you might want to omit it.
## There’s a LOT of other stuff in there.. just ask if you want to know more.

# Order (e.g. Figure 8) is HG, PRD, CC, SOG, WCVI.

# load_all()
source("create-herring-object-hdi.R")

assess_yr <- 2023       # Year of the assessment (status in that year)
regions_all <- c("HG", "PRD", "CC", "SOG", "WCVI")

# call create_herring_object_hdi() once for each region, and make into a list
#  for recruitment

herring_mcmc <- list()

for(i in 1:length(regions_all)){
  results_region <- create_herring_object_hdi(assess_yr,
                                              regions_all[i])

  herring_mcmc[[i]] <- list(region = regions_all[i],
                            recruitment = results_region$recruitment,
                            spawning_biomass = results_region$spawning_biomass,
                            unfished_spawning_biomass =
                              results_region$unfished_spawning_biomass)
}

herring_mcmc

saveRDS(herring_mcmc,
        file = "herring_mcmc.rds")
