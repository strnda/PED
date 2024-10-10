########
lop <- c("data.table", "fst")

to_instal <- lop[which(x = !(lop %in% installed.packages()[,"Package"]))]

if(length(to_instal) != 0) {
  
  install.packages(to_instal)
}

temp <- lapply(X = lop, 
               FUN = library, 
               character.only = T)
rm(temp, lop, to_instal)

########

dta <- readRDS(file = "./data/camel_br.rds")

class(x = dta)

dta$date

dta[, "date"]
dta[c(5:15), 2]

dta

dta <- as.data.table(x = dta)

dta

dta_wide <- dcast(data = dta,
                  formula = date ~ id,
                  value.var = "streamflow_mm")
system.time({
  write.fst(x = dta,
            path = "./data/dta_long.fst")
})

system.time({
  write.fst(x = dta_wide,
            path = "./data/dta_wide.fst")
})

file.size("./data/dta_long.fst")
file.size("./data/dta_wide.fst")

f <- fst(path = "./data/dta_long.fst")
object.size(x = f)

dta_import <- f[1:100000, c("date", "streamflow_mm")]

dta <- read.fst(path = "./data/dta_long.fst",
                as.data.table = TRUE)

dta[i, j, by]

dta[between(x = as.numeric(x = year(x = date)),
            lower = 1988,
            upper = 2000), 
    .(mean = mean(x = streamflow_mm),
      sd = sd(x = streamflow_mm)),
    by = id]

dta[1:3]
dta[, 2]
dta$streamflow_mm
dta[, "id"]
dta[, id]
dta[, .(date, id)]
dta[, val := rnorm(n = .N)]
dta[val > 1, flag := TRUE]
dta[flag == TRUE,]
dta[, flag := NULL]


aux <- data.table(catchment = unique(x = dta$id),
                  something = sample(x = letters,
                                     replace = TRUE, 
                                     size = length(x = unique(x = dta$id))))
aux

dta_merge <- merge(x = dta,
                   y = aux,
                   by.x = "id",
                   by.y = "catchment")
dta_merge

setnames(x = aux,
         old = "catchment",
         new = "id")

aux

dta_merge <- merge(x = dta,
                   y = aux,
                   by = "id")
dta_merge

dta_merge[, mean(x = val), by = .(something, id)]

fwrite()

fread()



