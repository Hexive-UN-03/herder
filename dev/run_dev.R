# remove all environmental variables to avoid carryover allowing things to work when they shouldn't
rm(list = ls())
setwd("~/mccuem/herder_update/herder")

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app(dataset = "../../herder/ALL_WGS_HORSES_test.csv", vcf_path = "../../herder/joint_call.goldenPath.20240827.vep.vcf.gz", gtf_path = "../../herder/Equus_caballus.EquCab3.0.109.gtf.gz")

