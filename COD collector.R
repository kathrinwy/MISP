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
          loc=ccn(html_text(loca))
          # examine whether location is in country list
          if (!loc %in% country_list){
            print(loc)
          } else{
            # there are two unusual cases: no date or has a period instead of single date
            date=html_text(year)
            # first examine non-empty date
            if (nchar(date)>=12){
              clean_date=str_sub(date,-12)
              all_date = c(all_date,clean_date)
              all_location = c(all_location,loc)
              all_desc = c(all_desc,html_text(desc))
              all_address=c(all_address,paste0("https://data.humdata.org",address0[i]))
              download.file(addr,quiet = 1,destfile = paste0("E:/File/Pennsylvania/UNFPA/data/",loc,'_',clean_date,'.',style))
            }
          }
        }
      }
    }
  }
}
print(paste0('COD data processing takes ',round(Sys.time()-start_time,2),' minutes'))



# define a function that 'clean country name'(ccn), e.g. ccn("Côte d'Ivoire")
ccn=function(string){
  # remove non-alphabetic
  string=str_replace_all(string, "[[:punct:]]", " ")
  # remove Latin letters
  return(iconv(string,from="UTF-8", to='ASCII//TRANSLIT'))}



directory=data.frame(country=all_location,year=all_date,decription=all_desc,link=all_address,stringsAsFactors=FALSE)



# build a country list from https://www.unfpa.org/worldwide
country_list=c('Angola','Botswana','Burundi','Comoros','Democratic Republic of the Congo',
               'Eritrea','Eswatini','Ethiopia','Kenya','Lesotho','Madagascar','Malawi','Mozambique',
               'Namibia','Rwanda','Seychelles','South Africa','South Sudan',
               'United Republic of Tanzania','Uganda','Zambia','Zimbabwe','Benin','Burkina Faso',
               'Republic of Cameroon','Cabo Verde','Central African Republic','Chad',
               'Republic of Congo','Cote d Ivoire','Equatorial Guinea','Gabon','Gambia','Ghana',
               'Guinea','Guinea-Bissau','Liberia','Mali','Mauritania','Niger','Nigeria',
               'Sao Tome and Principe','Senegal','Sierra Leone','Togo','Algeria','Djibouti',
               'Egypt','Kingdom of Bahrain','Kingdom of Saudi Arabia','State of Kuwait',
               'State of Qatar','United Arab Emirates','Sultanate of Oman','Iraq','Jordan','Lebanon',
               'Libyan Arab Jamahiriya','Morocco','State of Palestine','Somalia','Sudan',
               'Syrian Arab Republic','Tunisia','Yemen','Afghanistan','Bangladesh','Bhutan',
               'Cambodia','China','Democratic Peoples Republic of Korea','India','Indonesia',
               'Islamic Republic of Iran','Lao People s Democratic Republic','Malaysia','Maldives',
               'Mongolia','Myanmar','Nepal','Cook Islands','Federated States of Micronesia',
               'Fiji','Kiribati','Marshall Islands','Nauru','Niue','Palau','Samoa',
               'Solomon Islands','Tokelau','Tonga','Tuvalu and Vanuatu','Pakistan',
               'Papua New Guinea','Philippines','Sri Lanka','Thailand','Timor-Leste','Viet Nam',
               'Albania','Armenia','Azerbaijan','Belarus','Bosnia & Herzegovina','Georgia',
               'Kazakhstan','Kosovo','Kyrgyzstan','Macedonia','Moldova','Serbia','Tajikistan',
               'Turkey','Turkmenistan','Ukraine','Uzbekistan','Argentina','Bolivia','Brazil',
               ' Belize','Guyana','Saint Lucia','Jamaica','Suriname','Trinidad and Tobago',
               'Anguilla','Antigua and Barbuda','Aruba','Bahamas','Barbados','Bermuda',
               'British Virgin Islands','Cayman Islands','Dominica','Grenada','Montserrat',
               'Netherlands Antilles','Saint Kitts and Nevis','Saint Vincent and the Grenadines',
               'Turks and Caicos Islands','Chile','Colombia','Costa Rica','Cuba',
               'Dominican Republic','Ecuador','El Salvador','Guatemala','Haiti','Honduras','Mexico',
               'Nicaragua','Panama','Paraguay','Peru','Uruguay','Venezuela')


