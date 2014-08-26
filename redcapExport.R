redcapExport <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/', factors=TRUE, labels=TRUE, checkboxLabels=FALSE) {

    if (!require('RCurl')) {
        stop('RCurl is not installed')
    }

    rmq <- function(x) gsub("['\"]", '', x)
    clean <- function(x) { gsub('[\n]', ' ', x) }

    # if Hmisc is available, apply labels
    Hmisc <- require('Hmisc')

    # Fetch metadata
    meta_data <- read.csv(text=postForm(uri=URI, token=APIKEY, content='metadata',
                                        format='csv',
                                        # RCurl options
                                        .opts=curlOptions(ssl.verifyhost=TRUE)),
                          stringsAsFactors=FALSE, na.strings='')

    # Fetch records
    data <- read.csv(text=postForm(uri=URI, token=APIKEY, content='record',
                                   format='csv', type='flat',
                                   # Redcap API options
                                   rawOrLabel=c('raw','label')[1 + labels], # real values or codes
                                   exportCheckboxLabel=c('false','true')[1 + checkboxLabels], # real values or checked/unchecked
                                   # RCurl options
                                   .opts=curlOptions(ssl.verifyhost=TRUE)),
                     stringsAsFactors=FALSE, na.strings='')

    for (i in seq(nrow(meta_data))) {
        fld <- as.list(meta_data[i,])

        if (fld$field_type == 'checkbox') {
            # extract checkbox choices to identify sub-variables in data
            choices <- strsplit(fld$select_choices_or_calculations, ' *[|] *')[[1]]
            choices <- sub('^[ ]+(.*)$', '\\1', choices)
            choices <- sub('(.*)[ ]+$', '\\1', choices)
            nums <- sub('^([^,]+).*$', '\\1', choices)
            choices <- sub('^[^,]+[, ]+(.*)$', '\\1', choices)

            for (i in seq(length(nums))) {
                checkbox_name <- sprintf('%s___%s', fld$field_name, nums[i])
                if (factors) {
                    data[[checkbox_name]] <- factor(data[[checkbox_name]])
                }
                if (Hmisc) {
                    label(data[[checkbox_name]]) <- sprintf('%s (choice=%s)', clean(fld$field_label), rmq(choices[i]))
                }
            }

        } else {
            if (fld$field_type %in% c('select','radio','dropdown','yesno','truefalse')) {
                if (factors) {
                    data[[fld$field_name]] <- factor(data[[fld$field_name]])
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

    form_names <- sprintf('%s_complete', unique(meta_data$form_name))
    for (form_name in form_names) {
        if (factors) {
            data[[form_name]] <- factor(data[[form_name]])
        }
        if (Hmisc) {
            label(data[[form_name]]) <- 'Complete?'
        }
    }

    data
}
