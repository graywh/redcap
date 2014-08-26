library(redcap)

redcapFun <- function(APIKEY) {
    rcon <- redcapConnection('https://redcap.vanderbilt.edu/api/', APIKEY)
    gg <- exportMetaData(rcon)

    # reverse a string
    strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse='')
    # remove quotes
    rmq <- function(x) gsub("['\"]", '', x)
    clean <- function(x) { gsub('[\n]', ' ', x) }

    labels <- vector('list', nrow(gg))
    factors <- vector('list', nrow(gg))
    # remove quote characters, replace line breaks
    # gg$field_label <- gsub('[\x80-\x9f]', '', clean(gg$field_label), useBytes=TRUE)
    gg$field_label <- rmq(iconv(clean(gg$field_label), "WINDOWS-1252", "UTF-8"))
    gg$select_choices_or_calculations <- rmq(iconv(clean(gg$select_choices_or_calculations), "WINDOWS-1252", "UTF-8"))

    for(i in seq(nrow(gg))) {
        fld <- gg[i,]
        label <- NULL
        factor <- NULL
        if(fld$field_type == 'checkbox') {
            choices <- strsplit(fld$select_choices_or_calculations, ' *[|] *')[[1]]
            choices <- sub('^[ ]+(.*)$', '\\1', choices)
            choices <- sub('(.*)[ ]+$', '\\1', choices)
            nums <- sub('^([^,]+).*$', '\\1', choices)
            choices <- sub('^[^,]+[, ]+(.*)$', '\\1', choices)
            label <- sprintf('label(data$%s___%s) <- "%s (choice=%s)"', fld$field_name, nums, sub('[\n]', ' ', fld$field_label), rmq(choices))
            factor <- sprintf('data$%1$s___%2$s <- factor(data$%1$s___%2$s, levels=c("0", "1"), lables=c("Unchecked", "Checked"))', fld$field_name, nums)
        } else if(fld$field_type %in% c('radio', 'dropdown')) {
            label <- sprintf('label(data$%s) <- "%s"', fld$field_name, sub('[\n]', ' ', fld$field_label))
            factor <- sprintf('attr(data$%s, "redcapLevels") <- NULL', fld$field_name)
        } else if(fld$field_type == 'yesno') {
            label <- sprintf('label(data$%s) <- "%s"', fld$field_name, sub('[\n]', ' ', fld$field_label))
            factor <- sprintf('data$%1$s <- factor(data$%1$s, levels=c("1", "0"), labels=c("Yes", "No"))', fld$field_name)
        } else if(fld$field_type == 'truefalse') {
            label <- sprintf('label(data$%s) <- "%s"', fld$field_name, sub('[\n]', ' ', fld$field_label))
            factor <- sprintf('data$%1$s <- factor(data$%1$s, levels=c("1", "0"), labels=c("True", "False"))', fld$field_name)
        } else if(fld$field_type %in% c('notes', 'text', 'descriptive')) {
            label <- sprintf('label(data$%s) <- "%s"', fld$field_name, sub('[\n]', ' ', fld$field_label))
        } else {
            print(sprintf("I don't know field_type %s for field %s", fld$field_type, fld$field_name))
        }
        labels[[i]] <- label
        factors[[i]] <- factor
    }

    # <instrument>_complete
    complete <- sprintf('%s_complete', unique(gg$form_name))
    labels[[nrow(gg)+1]] <- sprintf('label(data$%s) <- "Complete?"', complete)
    factors[[nrow(gg)+1]] <- sprintf('data$%1$s <- factor(data$%1$s, levels=c("2", "1", "0"), labels=c("Complete", "Unverified", "Incomplete"))', complete)

    info <- c('library(Hmisc)', unlist(factors), unlist(labels))
    # write(info, 'data_labels.r')
    data <- exportRecords(rcon)
    # source('data_labels.r')
    eval(parse(text=info))
    # data has been read in, fix names to 30 characters
    # names(data) <- strReverse(abbreviate(strReverse(names(data)), 30))
    # data <- as.data.frame(lapply(data, ))
    return(data)
}
