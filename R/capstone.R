
setwd("/Users/Katie/Desktop/capstone")
files <- dir("./data", pattern=".csv$", full.names = TRUE, recursive= TRUE)

country <- gsub(".+/+|_.*.csv", "", files) 

roundcode <- gsub(".*_+(.*).csv$", "\\1", files)

firstup <- function(x) {                          # Capitalize first letter of Country code
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

fileinfo <- data.frame(filename = files,
                       country = firstup(country),
                       roundcode = roundcode,
                       listid = paste0(firstup(country), ", Round ", roundcode),
                       stringsAsFactors = FALSE)

all <- lapply(files, read.csv, as.is=TRUE,
              header=FALSE, encoding = "UTF-8")

names(all) <- fileinfo$listid[1:length(all)] 

sapply(all, dim)                                 # All different-sized files
                                                 # Figure our which row contains the variable names 
fileinfo$varrow <- NA
fileinfo$datarow <- NA
i<-1
for (i in 1:length(all)) {
  y <- all[[i]]
  j <- 1
  temp <- tolower(trimws(unlist(y[j,])))
  while (!all(c("country", "round", "site_id") %in% temp) && j < 10) {
    j <- j + 1
    temp <- tolower(trimws(unlist(y[j,])))
  }
  fileinfo$varrow[i] <- j
  while (sum(!is.na(as.numeric(unlist(y[j,])))) < 5 && j < 10) j <- j + 1
  fileinfo$datarow[i] <- j
}
fileinfo

for(i in 1:length(all)) {
  y <- all[[i]]
  varnames <- unlist(y[fileinfo$varrow[i],])
  varnames[is.na(varnames)] <- ""
  names(y) <- varnames
  y <- y[fileinfo$datarow[i]:nrow(y), varnames != ""]
  y <- y[y$country!="",]
  rownames(y) <- NULL
  names(y) <- tolower(trimws(names(y)))
  all[[i]] <- y
}

sapply(all, dim) # dimensions of each country


allvars <- unique(unlist(sapply(all, names)))

for (i in 1:length(all)) {
  y <- all[[i]]
  needthese <- allvars[!(allvars %in% names(y))]
  if (length(needthese)>0) {
    for (j in 1:length(needthese)) {
      for(n in 1:nrow(y)){               
        y[n, needthese[j]]<- NA
      }
    }
  }
  y <- y[,order(names(y))]
  all[[i]] <- y
}
sapply(all, dim)

z <- data.frame()
for (i in 1:length(all)) z <- rbind(z, all[[i]])
sapply(z, class)

for (i in 1:ncol(z)) {
  if (is.character(z[,i])) { 
    if (sum(!is.na(as.numeric(z[,i])))/(sum(!is.na(z[,i]))+1) > 0.9) {
      z[,i] <- as.numeric(z[,i])
      cat("column", i, "changed to numeric.\n")
    }
  }
}


write.csv(z, "./csv/original/newest_merge.csv", row.names=FALSE, fileEncoding="UTF-8")
stop("End of work for now.")


