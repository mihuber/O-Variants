library(tidyverse)
library(readr)
library(cowplot)

all_variants <- read_csv("~/Documents/Orienteering/Events/181013 ArgeAlp Flumserberg/Varianten/Staffellauf.Variations.txt", col_names = FALSE) %>%
        separate(X1, into = c("nr", "xx"), del = ".", extra = "merge") %>%
        separate(xx, into = c("lap", "var"), del = "\\:", extra = "merge") %>%
        mutate(var = gsub('[0-9]+', '', var)) %>%
        mutate(cat = ifelse(is.na(as.numeric(nr)), nr, NA))

for (i in 1:nrow(all_variants)) {
        if (is.na(all_variants$cat[i])) {
                all_variants$cat[i] = all_variants$cat[i-1]
        }
}

### select class and laps
classes = c("H14", "D14", "DE", "HE", "H18", "D18", "H35", "H45", "H55", "D35", "D45", "D55")
laps = c("1", "2")

for (c in classes) {
        vcl = all_variants %>%
                filter(cat == c) %>%
                filter(lap %in% laps) %>%
                group_by(lap) %>%
                add_count(var) %>%
                separate(var, into = c("f1", "f2", "f3"), sep = c(1, 2), fill = "right") %>%
                as.data.frame()
        
        perms = expand.grid(f1 = unique(vcl$f1), f2 = unique(vcl$f2), f3 = unique(vcl$f3)) %>%
                as.data.frame(.) %>%
                mutate(lap = 1, cat = c, nr = NA, n = NA)
        
        vcl = rbind(vcl, perms) %>%
                mutate(var = paste0(f1, f2, f3)) %>%
                select(-f1, -f2, -f3)
        
        p = vcl %>%
                #filter(!(is.na(nr))) %>%
                ggplot(aes(x = lap, y = var, group = nr, color = nr)) + 
                geom_line() +
                geom_point(size = 2) +
                geom_text(aes(label = n, hjust = 2)) +
                background_grid(major = "y", minor = "y") +
                ggtitle(c)
        p
        ggsave(paste0("~/Documents/Orienteering/Events/181013 ArgeAlp Flumserberg/Varianten/Plot_", c, ".pdf"), plot = p)
}



# vcl = as.data.frame(table(vcl)) %>%
#         mutate(Freq = Freq - 1) %>%
#         arrange(desc(Freq)) 
# 
# sink(paste0("~/Dropbox/Orienteering/Flumserberg_Arge_Alp_18/Variantenzuteilung/", c , "_", l, ".txt"))
# 
# cat("Kategorie", c, "L??ufer", l , "\n")
# cat("\n")
# cat("Verteilung der Varianten", "\n")
# Vars
# cat("\n")
# cat("Verteilung erste Gabelung")
# table(vcl$f1)
# cat("\n")
# cat("Verteilung zweite Gabelung")
# table(vcl$f2)
# cat("\n")
# cat("Verteilung dritte Gabelung")
# table(vcl$f3)
# 
# sink()
