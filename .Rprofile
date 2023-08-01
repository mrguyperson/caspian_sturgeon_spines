# letting the user know that a custom .Rprofile being used
print("Using custom .Rprofile ...")

# setup for httpgd to print figures correctly
if (interactive() && Sys.getenv("RSTUDIO") == "") { 
    Sys.setenv(TERM_PROGRAM = "vscode") 
    if ("httpgd" %in% .packages(all.available = TRUE)) { 
        options(vsc.plot = FALSE) 
        options(device = function(...) { 
            httpgd::hgd(silent = TRUE) 
            .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = "Beside") 
            })
    } 
    source(file.path(Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"), ".vscode-R", "init.R"))} 

# change default directory so here() doesn't get borked
# folder <- "FHAST"
# working_dir <- file.path(getwd(), folder)
# setwd(working_dir)
# print(paste0("Setting working directory to ", working_dir, "."))
# rm(working_dir)

# set max print, otherwise the console prints for ages...
max_lines <- 1e3
options(max.print = max_lines)
print(paste0("Setting max print lines to ", max_lines, "."))
rm(max_lines)
