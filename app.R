# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE, 
        gargle_oauth_email = TRUE,
        gargle_oauth_cache = "AsthmaWorkshop/.secrets")
AsthmaWorkshop::run_app() # add parameters here (if any)
