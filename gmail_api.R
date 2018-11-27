# justin.thomson
# demonstration of r gmail api

# install.packages("devtools")
#devtools::install_github("jimhester/gmailr")


library("gmailr")


getEmailFrom= function(id)
{
  from(message(id, format = 'full'))
}

getEmailBody = function(id)
{
  body(message(id, format="full"))
}

getEmailDate = function(id)
{
  date(message(id, format="full"))
}

getEmailSubject= function(id)
{
  subject(message(id, format="full"))
}


getDomainAnnotatedEmail = function(priorDaysToInclude=7, includeBody=F)
{
  if (priorDaysToInclude < 1)
  {
    print("Error: priorDaysToInclude must be greater than 0!")
    return
  }
  
  start_date = Sys.Date()-priorDaysToInclude
  
  gmail_search_query = paste0("received after:", start_date)
  
  # Retrieve message ids using the search query
  outdf = as.data.frame(id(messages(search = gmail_search_query)), stringsAsFactors = F)
  names(outdf) = "id"
  
  outdf$date= lapply(outdf$id, getEmailDate)
  
  outdf$from_address = lapply(outdf$id, getEmailFrom)
  
  outdf$subject = lapply(outdf$id, getEmailSubject)
  
  # get email domains and index by domain)
  outdf$from_domain=gsub(".*@|*>", "", outdf$from_address)
  outdf$from_domain_firstword=gsub(".*@|\\..*", "", outdf$from_address)
  
  outdf$body=lapply(outdf$id, getEmailBody)
  outdf$hasUnsubscribe = grepl("unsubscribe", outdf$body, ignore.case = T)

  if (!includeBody)
  {
    outdf$body =  NULL
  }  
  outdf
}

