library(rvest)
library(xml2)
library(stringr)
options(warn=-1)
###### current criteria: subnational and contains 'population', format csv,xls,xlsx

start_time = Sys.time()
#initializing
pagenumber=1
page_empty=0
all_desc=c()
all_address=c()
all_location=c()
all_date=c()
what=0

while (page_empty==0){
  # for each page in main website
  prefix="https://data.humdata.org/dataset?ext_subnational=1&res_format=CSV&res_format=XLS&res_format=XLSX&tags=population&page="
  
  # read current page as html format
  main=read_html(paste0(prefix,pagenumber))
  # find its nodes (subpages)
  scrape0 <- html_nodes(main,xpath=".//*[contains(@class,'dataset-heading')]//a")
  
  # store the url connecting the subpage
  address0=html_attr(scrape0,'href')
  
  # determine whether we have reached the last page
  page_empty=1-sign(length(address0))
  pagenumber=pagenumber+1
  
  # break if empty
  if (page_empty==1){break}
  
  
  # extract details like location and year, for all subpages of current page

  for (i in 1:length(address0)){
    
    # read html of subpage
    subpage <- read_html(paste0("https://data.humdata.org",address0[i]))
    
    # scrape the website for description, data, date and location
    desc = html_nodes(subpage,xpath=".//*[contains(@class,'itemTitle')]")
    data = html_nodes(subpage,xpath=".//*[contains(@class,'resource-icon-btn')]")
    loca = html_nodes(subpage,xpath=".//*[contains(@class,'mx-country')]//a")
    year = html_nodes(subpage,xpath=".//tr[(((count(preceding-sibling::*) + 1) = 3) and parent::*)]//*[contains(@class,'dataset-details')]")
    
    # there is one case where date is missing!
    # https://data.humdata.org/dataset/proyecciones-de-poblacion-municipalmente
    # there are also multiple cases where location is not unique, we ignore this
    # https://data.humdata.org/dataset/lcb-displaced
    if (length(html_text(desc))>0 && length(html_text(loca))==1){
      
      address=html_attr(data,'href')
      
      # extend url link to full address
      addr=paste0('https://data.humdata.org',address)
      
      # extract the file style if '.' exists
      if (grepl('[.]',address)){
        style=tail(strsplit(address,'[.]')[[1]],1)
        # examine whether style is csv,xls,xlsx (prefix criterion is not strict)
        if (style=='xls' || style=='xlsx' || style=='csv'){
          all_desc = c(all_desc,html_text(desc))
          all_location = c(all_location,ccn(html_text(loca)))
          all_date = c(all_date,html_text(year))
          all_address=c(all_address,paste0("https://data.humdata.org",address0[i]))
          download.file(addr,destfile = paste0("E:/File/Pennsylvania/UNFPA/data/",ccn(html_text(loca)),'_',html_text(year),'.',style))
        }
      }
    }
  }
}
print(paste0('COD data processing takes',Sys.time()-start_time,'minutes'))




ccn=function(string){
  # define a function that 'clean country name'(ccn), e.g. ccn("Côte d'Ivoire")
  
  # remove non-alphabetic
  string=str_replace_all(string, "[[:punct:]]", " ")
  # remove Latin letters
  return(iconv(string,from="UTF-8", to='ASCII//TRANSLIT'))}



directory=data.frame(country=all_location,year=all_date,decription=all_desc,link=all_address)
