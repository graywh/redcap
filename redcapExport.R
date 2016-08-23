redcapExportMeta <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/') {

    if (!require('RCurl')) {
        stop('RCurl is not installed')
    }

    meta_data <- read.csv(text=postForm(uri=URI, token=APIKEY, content='metadata',
                           format='csv',
                           # RCurl options
                           .opts=curlOptions(ssl.verifyhost=2)),
             stringsAsFactors=FALSE, na.strings='')
    subset(meta_data, field_type != 'field_type')
}

redcapExport <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/', labels=TRUE, checkboxLabels=FALSE, forms=NULL, fields=NULL, events=NULL) {

    if (!require('RCurl')) {
        stop('RCurl is not installed')
    }

    rmq <- function(x) gsub("['\"]", '', x)
    clean <- function(x) { gsub('[\n]', ' ', x) }

    # if Hmisc is available, apply labels
    Hmisc <- require('Hmisc')

    # Fetch metadata
    meta_data <- subset(redcapExportMeta(APIKEY, URI), field_type %in% c('text','notes','dropdown','radio','checkbox','calc','slider','yesno','truefalse'))

    form_field_names <- sprintf('%s_complete', unique(meta_data$form_name))
    if (!is.null(forms)) {
        forms <- intersect(forms, unique(meta_data$form_name))
        form_field_names <- sprintf('%s_complete', forms)
    }
    if (!is.null(fields)) {
        form_fields <- subset(meta_data, form_name %in% forms)$field_name                 # Select all the field (variable) names that are in the forms the user specified.
        if (!all(fields %in% form_fields)) {
            specific_fields <- intersect(fields, c(unique(meta_data$field_name), form_field_names))
            fields <- union(form_fields, specific_fields)
            if (!is.null(forms)) {
                fields <- c(fields, form_field_names)
                forms <- NULL
            }
            form_field_names <- intersect(form_field_names, fields)
        }
    }
    if (length(forms) > 0) {
        meta_data <- subset(meta_data, meta_data$form_name %in% forms)
    } else if (length(fields) > 0) {
        meta_data <- subset(meta_data, meta_data$field_name %in% fields)
    }

    # Fetch records
    data <- read.csv(text=postForm(uri=URI, token=APIKEY, content='record',
                                   format='csv', type='flat',
                                   # Redcap API options
                                   rawOrLabel=c('raw','label')[1 + labels], # real values or codes
                                   exportCheckboxLabel=c('false','true')[1 + checkboxLabels], # real values or checked/unchecked
                                   forms=paste(forms, collapse=','),
                                   events=paste(events, collapse=','),
                                   fields=paste(fields, collapse=','),
                                   # RCurl options
                                   .opts=curlOptions(ssl.verifyhost=2)),
                     stringsAsFactors=FALSE, na.strings='')

    for (i in seq_len(nrow(meta_data))) {
        fld <- as.list(meta_data[i,])
        choices <- redcapExtractChoices(fld$select_choices_or_calculations)
        nums <- choices$numbers
        choices <- choices$labels

        if (fld$field_type == 'checkbox') {

            for (j in seq(length(nums))) {
                checkbox_name <- sprintf('%s___%s', fld$field_name, nums[j])

                # advance to next field if not in dataset
                if (is.null(data[[checkbox_name]])) next

                if (labels) {
                    if (checkboxLabels) {
                        levels <- choices[j]
                    } else {
                        levels <- c('Unchecked', 'Checked')
                    }
                    data[[checkbox_name]] <- factor(data[[checkbox_name]], levels=levels)
                } else {
                    data[[checkbox_name]] <- factor(data[[checkbox_name]], levels=c('0','1'))
                }
                if (Hmisc) {
                    label(data[[checkbox_name]]) <- sprintf('%s (choice=%s)', clean(fld$field_label), rmq(choices[j]))
                }
            }

        } else {
            # advance to next field if not in dataset
            if (is.null(data[[fld$field_name]])) next

            if (fld$field_type %in% c('radio','dropdown','yesno','truefalse')) {
                if (labels) {
                    if (fld$field_type == 'yesno') {
                        levels <- c('No','Yes')
                    } else if (fld$field_type == 'truefalse') {
                        levels <- c('False','True')
                    } else {
                        levels <- choices
                    }
                    data[[fld$field_name]] <- factor(data[[fld$field_name]], levels=levels)
                } else {
                    if (fld$field_type == 'yesno') {
                        levels <- c('0','1')
                    } else if (fld$field_type == 'truefalse') {
                        levels <- c('0','1')
                    } else {
                        levels <- choices
                    }
                    data[[fld$field_name]] <- factor(data[[fld$field_name]], levels=levels)
                }

            } else if ((!is.na(fld$text_validation_type_or_show_slider_number) &&
                        fld$text_validation_type_or_show_slider_number %in% c('float','int') ) ||
                       fld$field_type %in% c('calc') ) {
                suppressWarnings(data[[fld$field_name]] <- as.numeric(data[[fld$field_name]]))

            }

            if (Hmisc) {
                label(data[[fld$field_name]]) <- fld$field_label
            }
        }
    }

    for (form_field_name in form_field_names) {
        data[[form_field_name]] <- factor(data[[form_field_name]], levels=c('Incomplete','Unverified','Complete'))
        if (Hmisc) {
            label(data[[form_field_name]]) <- 'Complete?'
        }
    }

    data
}

redcapExtractChoices <- function(choices) {
    # extract checkbox choices to identify sub-variables in data
    choices <- strsplit(choices, ' *[|] *')[[1]]
    choices <- sub('^[ ]+(.*)$', '\\1', choices)
    choices <- sub('(.*)[ ]+$', '\\1', choices)
    nums <- sub('^([^,]+).*$', '\\1', choices)
    choices <- sub('^[^,]+[, ]+(.*)$', '\\1', choices)
    list(numbers=nums, labels=choices)
}
