library(tidyverse)
library(ggplot2)
library(cowplot)

forkings = c("ABC", "ABC", "AB") # 3*3*2
#forkings = c("ABC", "ABC", "ABC") # 3*3*3
#forkings = c("ABC", "ABC") # 3*3
#forkings = c("AB", "AB") # 2*2

perms <- function(v) {
    expand.grid(f1 = unlist(strsplit(v[1], "")),
                f2 = unlist(strsplit(v[2], "")),
                f3 = unlist(strsplit(v[3], ""))) %>%
        as.data.frame(.) %>%
        mutate(var = paste0(f1, f2, f3)) %>%
        mutate(var = gsub("NA", "", var)) %>%
        pull(var)
}

teams = 1:30
laps = 3
forks = 1:3
table = data.frame(team = NA, lap = NA, fork = NA, vars = NA)

f = 2
t = 4

for (l in laps) {
    for (f in forks) {
        for (t in teams) {
                
                # possible variants up to this fork
                pv = substr(perms(forkings), 1, f) %>%
                        unique()
                
                # existing variants up to this fork
                ev = table %>%
                        filter(lap == l) %>%
                        filter(fork == f) %>%
                        pull(vars)
                
                # variant at previous fork
                vpf = table %>%
                        filter(team == t) %>%
                        filter(lap == l) %>%
                        filter(fork == f-1) %>%
                        pull(vars)
                
                # allowed next variants
                if (f > 1) {
                        anv_log = mapply(function(x) { if (substr(x, 1, f-1) == vpf) return(TRUE) else return(FALSE) }, c(pv, ev))
                        anv = c(pv, ev)[anv_log == TRUE]
                } else {
                        anv = c(pv, ev)
                }
                nv = names(sort(table(anv)))[1]
                
            row = data.frame(team = t, lap = l, fork = f, vars = nv)
            table = rbind(table, row) %>%
                filter(!(is.na(vars)))
        }
    }
}


# SORT BY A SPECIIFC KEY (HERE THE LEAST USED VARIANT)
# x <- c("white","white","blue","green","red","blue","red")
# y <- c("red","white","blue","green")
# x[order(match(x, y))]
# # [1] "red"   "red"   "white" "white" "blue"  "blue"  "green"



final_table = table %>%
    spread(key = fork, value = vars) %>%
    mutate(var = paste0(`3`)) %>%
    select(team, lap, var) %>%
    group_by(lap) %>%
    add_count(var)
    
### plot
p = final_table %>%
    ggplot(aes(x = lap, y = var, group = team, color = team)) + 
    geom_line() +
    geom_point(size = 2) +
    geom_text(aes(label = n, hjust = 2)) +
    background_grid(major = "y", minor = "y")
p
