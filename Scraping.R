library(rvest)

#Scraping the urls of each month's archive from the general archive page---- 
archivepage <- "#URL removed" %>%
  read_html %>%
  html_nodes("div.archive-content li") %>% #Finding what we want by div and content type
  html_nodes("a") %>% html_attr("href")
monthurls <- archivepage[1:(length(archivepage)-1)] # -1 because the last link did not point to a month archive page

#Scraping post content from each post in each month----  
library(devtools)
library(Rfacebook)  
fb_token <- "yourfb_tokenHere"

# Creating data frame variables to be used in loops going forward -- poorly named, I know
dz <- data.frame("PostContent")
names(dz) <- c("PostContent")
titleholder <- data.frame("Title")
titleholder <- c("Title")
DTD <- data.frame("Date")
names(DTD) <- c("Date")
dl <- data.frame(0)
names(dl) <- c("FB Shares and Likes")

for (i in monthurls) {
  urlsofposts <- i %>% #Select the url of each post from the month page
    read_html %>% #Go to that url
    html_nodes("h3") %>% #Get the title of the post from the post page
    html_nodes("a") %>% html_attr("href") #Get the url from that title
  
# 2nd for loop (nested) - b is the variable for each post url (poor naming, again)
  # I suspect that reading the html from each url three times to get data from
  # different nodes is inefficient. Spent time looking into a way to get the three 
  # parts of the data (title, content, date) from one read_html, but was taking more time to find than
  # just running this code did. With a larger data set, obviously it would be worth reducing the time
  # complexity, considering efficiency, etc.
  # b is the variable for the url in the list of posts (urlsofposts). Again, variable naming could be better.
  
  #Getting the title of the post
  for (b in c(urlsofposts)) {
    ptitle <- b %>%
      read_html %>%
      #Use html_node instead of html_nodes to get the first title on the page, i.e. the post's title
      html_node("h3.entry-header") 
    temptitle <- data.frame(html_text(ptitle))
    names(temptitle) <- c("Title")
    titleholder <- rbind(titleholder,temptitle)

    #Getting the content of the post
    blogpostt <- b %>%
      read_html %>%
      html_nodes("div.entry-content p")
    dg <- data.frame(paste(html_text(blogpostt), collapse=' '))
    names(dg) <- c("PostContent") 
    dz <- rbind(dz, dg)
    
    #Getting the date of the post
    footer <- b %>%
      read_html %>%
      html_nodes("p.entry-footer-info")
    DTE <- data.frame(paste(html_text(footer), collapse=' '))
    names(DTE) <- c("Date")
    DTD <- rbind(DTD, DTE)
    
  #Getting the # of FB shares on each post, using the graph API
    start <- paste0("https://graph.facebook.com/?fields=og_object{likes.summary(true).limit(0)},share&id=", b) 
    hi <- callAPI(start, fb_token)
    dc <- data.frame(hi$share$share_count)
    names(dc) <- c("FB Shares and Likes")  
    dl <- rbind(dl, dc)
  } 
}

# Combining the title, content, date, and # of FB shares into one data frame
titleholder <- cbind(titleholder, dz, DTD, dl)

#Cleaning the raw data----
library(stringr)
titleholder$Title <- gsub("^\r\n\t\t\t\t", "", titleholder$Title)
titleholder$Title <- gsub("\r\n\t\t\t", "", titleholder$Title)
titleholder$Date <- gsub("\r\n\t\t\t\t\r\n\t\t\t\r\n\t\t\t\t", "", titleholder$Date)
#This line removed
titleholder$Date <- gsub("\r\n\t\t\t\r\n\t\t\t\t", "", titleholder$Date)
titleholder$PostContent <- gsub("â€™", "\'", titleholder$PostContent)
titleholder$PostContent <- gsub("â€", "\"", titleholder$PostContent)
titleholder$PostContent <- gsub("â€œ", "\"", titleholder$PostContent)
titleholder$PostContent <- gsub("â€", "\"", titleholder$PostContent) 
titleholder <- titleholder[-1,,drop=F]