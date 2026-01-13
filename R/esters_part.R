library(dplyr)    
library(ggplot2)   

donnees$period <- NA
donnees$period[donnees$year <= 2007] <- "Pre-crisis"
donnees$period[donnees$year >= 2009] <- "Post-crisis"
donnees <- donnees[!is.na(donnees$period), ]
table(donnees$period)

table(donnees$year)
banks_pre <- unique(donnees$institution_name[donnees$period == "Pre-crisis"])
banks_post <- unique(donnees$institution_name[donnees$period == "Post-crisis"])
banks_common <- intersect(banks_pre, banks_post)
length(banks_common)
donnees_common <- donnees[donnees$institution_name %in% banks_common, ]
table(donnees_common$period)
table(donnees_common$year)

donnees_common$in_roe <- as.numeric(gsub(",", ".", donnees_common$in_roe))

roe_pre <- donnees_common$in_roe[donnees_common$period == "Pre-crisis"]
roe_post <- donnees_common$in_roe[donnees_common$period == "Post-crisis"]

roe_boxplot <- boxplot(roe_pre, roe_post,
        names = c("Pre", "Post"),
        main = "ROE before vs after crisis",
        ylab = "Return on Equity")

wilcox.test(roe_pre, roe_post)
median(roe_pre, na.rm=TRUE)
median(roe_post, na.rm=TRUE)




