library('RCurl')

redcapExport <- function(APIKEY, URI='https://redcap.vanderbilt.edu/api/', factors=TRUE, labels=TRUE, headerLabels=FALSE, checkboxLabels=TRUE, surveyFields=FALSE, dataAccessGroups=FALSE) {

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
                                   rawOrLabel=c('raw','label')[2-labels],
                                   rawOrLabelHeaders=c('raw','label')[2-headerLabels],
                                   exportCheckboxLabel=c('false','true')[2-checkboxLabels],
                                   exportSurveyFields=c('false','true')[2-surveyFields],
                                   exportDataAccessGroups=c('false','true')[2-dataAccessGroups],
                                   # RCurl options
                                   .opts=curlOptions(ssl.verifyhost=TRUE)),
                     stringsAsFactors=FALSE, na.strings='')

}
