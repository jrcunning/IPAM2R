import_ipam <- function(dir, ipam.pattern="*.csv", info.pattern="*ids.txt") {
  ipam <- list.files(path=dir, pattern=ipam.pattern, full.names=TRUE)
  basenames <- gsub("\\..*$", "", basename(ipam))
  if (!is.null(info.pattern)) {
    info <- list.files(path=dir, pattern=info.pattern, full.names=TRUE)
  } else {
    info <- NULL
  }



  # Read in each file
  for (i in 1:length(ipam)) {

    # Read in IPAM data file (should be a single row, ;-delimited)
    df <- read.delim(ipam[i], sep=";")
    # Get rid of trailing NA
    if (is.na(dplyr::last(df))) df <- df[,-(ncol(df))]

    # Make data long form and recast with rows for each AOI (based on column names)
    df2 <- reshape2::melt(df)
    df2$AOI <- stringr::str_extract(string=df2$variable, pattern="[0-9]+")
    df2$AOI <- as.numeric(as.character(df2$AOI))
    df2$var <- stringr::str_extract(string=df2$variable, pattern="[^0-9|\\.]+")
    head(df2)
    df3 <- reshape2::dcast(na.omit(df2), AOI ~ var)

    # Read in sample names from ids file
    if (!is.null(info)) {
      nm <- read.table(info[1])
      colnames(nm) <- "ID"
    }

    # Merge data with IDs and assign to global environment
    df4 <- data.frame(nm, df3)
    df4$file <- basenames[i]
    assign(basenames[i], df4)
  }

  # Merge all data frames together
  result <- do.call(rbind, mget(basenames))
  rownames(result) <- NULL  # gets rid of rownames
}

