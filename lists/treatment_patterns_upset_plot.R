library(UpSetR)

d <- readRDS("objects/main_data.rds")[AnyCancerPhe == 1]

upset_plot <- upset(d, sets = c("radiation", "chemo", "surgery"), sets.bar.color = "#56B4E9",
                    order.by = "freq", empty.intersections = "on")

cairo_pdf(filename = "objects/cancer_treatment_upset_plot.pdf", width = 7, height = 5)
print(upset_plot)
dev.off()

