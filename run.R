source("libraries.R")
for (f in list.files("src/")) {source(paste0("src/", f))}

main <- make_main_data()
