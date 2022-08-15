library(UpSetR)
library(data.table)

d <- as.data.table(readRDS("objects/main_data.rds"))[AnyCancerPhe == 1][, surgery := surgery_dsb]

setnames(d, old = "imt", new = "immunotherapy")

upset_plot <- upset(d, sets = c("radiation", "chemo", "surgery", "immunotherapy"), sets.bar.color = "#56B4E9",
                    order.by = "freq", empty.intersections = "on")

cairo_pdf(filename = "objects/cancer_treatment_upset_plot.pdf", width = 7, height = 5)
print(upset_plot)
dev.off()

