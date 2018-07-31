library(rvest)
library(xml2)
install.packages("stringr", type="source")
library(stringr)
library(textreadr) 
options(warn=-1)

# where to save the downloads
working_directory="C:/Users/weny/Google Drive/2018/Humanitarian/MISP/HDX data/"

# define a function that 'clean country name'(ccn), e.g. ccn("Côte d'Ivoire")
ccn=function(string){
  # remove non-alphabetic
  string=str_replace_all(string, "[[:punct:]]", " ")
  # remove Latin letters
  return(iconv(string,from="UTF-8", to='ASCII//TRANSLIT'))}

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


###### current criteria: subnational and contains 'population', format (csv),xls,xlsx

start_time = Sys.time()
#initializing
pagenumber=1
page_empty=0
all_desc=c()
all_address=c()
all_location=c()
all_date=c()
all_local_address=c()
what=0

while (page_empty==0){
  # for each page in main website
  prefix="https://data.humdata.org/dataset?ext_subnational=1&res_format=XLS&res_format=XLSX&tags=population&page="
  #################
  # &res_format=CSV [NOT SUPPORTED YET BECAUSE OF JUDGE_EACH_DT]
  #################
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
        # examine whether style is (csv),xls,xlsx (prefix criterion is not accurate)
        if (style=='xls' || style=='xlsx'){
          loc=ccn(html_text(loca))
          # examine whether location is in country list
          if (!loc %in% country_list){
            print(loc)
          } else{
            # there are two unusual cases: no date or has a period instead of single date
            date=html_text(year)
            
            if (nchar(date)>=12){
              clean_date=str_sub(date,-12)
              all_date = c(all_date,clean_date)
              all_location = c(all_location,loc)
              all_desc = c(all_desc,html_text(desc))
              all_address=c(all_address,paste0("https://data.humdata.org",address0[i]))
              address_curr=paste0(working_directory,'data/',loc,'_',clean_date,'.',style)
              all_local_address=c(all_local_address,address_curr)
              download.file(addr,quiet = 1,destfile = address_curr,mode = "wb")
            }
          }
        }
      }
    }
  }
}
print(paste0('COD data processing takes ',round(Sys.time()-start_time,2),' minutes'))






directory=data.frame(country=all_location,year=all_date,decription=all_desc,link=all_address,location=all_local_address,stringsAsFactors=FALSE)

############ find country unique to most recent date
as_date=function(string){
  # we assume the date is like Sep 01,2011
  temp=strsplit(string,',| ')[[1]]
  year=temp[4]
  day=temp[2]
  month=match(temp[1],c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  return(as.POSIXct(paste0(year,'/',month,'/',day)))
}

unique_row=c()
unique_country=c()
unique_date=c()
for (i in 1:dim(directory)[1]){
  if (!directory[i,'country'] %in% unique_country){
    unique_row=c(unique_row,i)
    unique_country=c(unique_country,directory[i,'country'])
    unique_date=c(unique_date,as.character(as_date(directory[i,'year'])))
    print(as_date(directory[i,'year']))
  } else{
    # compare which date is more recent, replace if necessary
    temp_index=match(directory[i,'country'],unique_country)
    included_date=unique_date[temp_index]
    to_compare_date=as.character(as_date(directory[i,'year']))
    if (to_compare_date>included_date){
      unique_row[temp_index]=i
      unique_country[temp_index]=directory[i,'country']
      unique_date[temp_index]=to_compare_date
    }
  }
}

directory_unique=directory[unique_row,]

###################
library(readxl)

# define a function that return the index in a list where all strings in first statement can be found
find_index=function(str_vec,list_to_look){
  for (j in 1:length(list_to_look)){
    all_str_found=1
    for (i in 1:length(str_vec)){
      all_str_found=grepl(str_vec[i],list_to_look[j],ignore.case = TRUE)
      if(all_str_found==0){break}
    }
    if (all_str_found==1){return(j)}
  }
  return(0)}




extract_level2=function(data2,contain_2){
  # create an abbreviation
  col2=colnames(data2)
  
  if (find_index('male',col2)!=0 & find_index('female',col2)!=0){
    
    # if a data has disaggregated data and admin level 2, it has two rows that should not read as data
    # my thought is to combine two rows
    for (i in 1:length(col2)){
      col2[i]=paste0(col2[i],'____',data2[1,i])
    }
    colnames(data2)=col2
    data2=data2[2:dim(data2)[1],]
    
    admin1_name=find_index(c('Admin1','name'),col2)
    admin2_name=find_index(c('Admin2','name'),col2)
    male=find_index(c('Male'),col2)
    female=find_index(c('Female'),col2)
    age0_4=find_index(c('0','4'),col2)
    age5_9=find_index(c('5','9'),col2)
    age10_14=find_index(c('10','14'),col2)
    age15_19=find_index(c('15','19'),col2)
    age20_24=find_index(c('20','24'),col2)
    age25_29=find_index(c('25','29'),col2)
    age30_34=find_index(c('30','34'),col2)
    age35_39=find_index(c('35','39'),col2)
    age40_44=find_index(c('40','44'),col2)
    age45_49=find_index(c('45','49'),col2)
    age50_54=find_index(c('50','54'),col2)
    age55_59=find_index(c('55','59'),col2)
    age60_64=find_index(c('60','64'),col2)
    age65_69=find_index(c('65','69'),col2)
    age70_74=find_index(c('70','74'),col2)
    age75_79=find_index(c('75','79'),col2)
    age80_=find_index(c('80'),col2)
    
    extracted=data2[,c(admin1_name,admin2_name,male,female,age0_4,age5_9,age10_14,age15_19,age20_24,
                       age25_29,age30_34,age35_39,age40_44,age45_49,age50_54,age55_59,age60_64,age65_69,
                       age70_74,age75_79,age80_)]
    colnames(extracted)=c('Admin1','Admin2','Male','Female','0-4','5-9','10-14','15-19','20-24','25-29','30-34',
                          '35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+')
    
    return(list(extracted,contain_2))
  }else{
    contain_2=0
    print('This data has level 2 data but not about sex')
    return(list(data2,contain_2))
  }}

extract_level1=function(data1,contain_1){
  # create an abbreviation
  col1=colnames(data1)
  
  if (find_index('male',col1)!=0 & find_index('female',col1)!=0){
    
    
    admin1_name=find_index(c('Admin1','name'),col1)
    male=find_index(c('Male'),col1)
    female=find_index(c('Female'),col1)
    age0_4=find_index(c('4'),col1)
    age5_9=find_index(c('5','9'),col1)
    age10_14=find_index(c('10','14'),col1)
    age15_19=find_index(c('15','19'),col1)
    age20_24=find_index(c('20','24'),col1)
    age25_29=find_index(c('25','29'),col1)
    age30_34=find_index(c('30','34'),col1)
    age35_39=find_index(c('35','39'),col1)
    age40_44=find_index(c('40','44'),col1)
    age45_49=find_index(c('45','49'),col1)
    age50_54=find_index(c('50','54'),col1)
    age55_59=find_index(c('55','59'),col1)
    age60_64=find_index(c('60','64'),col1)
    age65_69=find_index(c('65','69'),col1)
    age70_74=find_index(c('70','74'),col1)
    age75_79=find_index(c('75','79'),col1)
    age80_84=find_index(c('80','84'),col1)
    age85_89=find_index(c('85','89'),col1)
    age90_94=find_index(c('90','94'),col1)
    age95_99=find_index(c('95','99'),col1)
    age100_=find_index(c('100'),col1)
    
    extracted=data1[,c(admin1_name,male,female,age0_4,age5_9,age10_14,age15_19,age20_24,
                       age25_29,age30_34,age35_39,age40_44,age45_49,age50_54,age55_59,age60_64,age65_69,
                       age70_74,age75_79,age80_84,age85_89,age90_94,age95_99,age100_)]
    max_length_str=c('Admin1','Male','Female','0-4','5-9','10-14','15-19','20-24',
                     '25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69',
                     '70-74','75-79','80+','85+','90+','95+','100+')
    colnames(extracted)=max_length_str[1:dim(extracted)[2]]
    
    return(list(extracted,contain_1))
  }else{
    contain_1=0
    print('This data has level 1 data but not about sex')
    return(list(data1,contain_1))
  }}


judge_each_DT=function(address){
  sheet_names=excel_sheets(address)
  
  contain_1=0
  contain_2=0
  
  contain_1=find_index(c('a','min','1'),sheet_names)
  if (contain_1!=0){data1=read_excel(address,sheet=contain_1)
  }else{data1=0;print('This country doesnt have level 1 data')}
  
  contain_2=find_index(c('a','min','2'),sheet_names)
  if (contain_2!=0){data2=read_excel(address,sheet=contain_2)
  }else{data2=0;print('This country doesnt have level 2 data')}
  
  if (contain_2!=0){
    answer2=extract_level2(data2,contain_2)
    contain_2=answer2[[2]]
    extracted=answer2[[1]]
  }
  
  if(contain_2!=0){
    print('This data has level 2 sex/age data')
    return(extracted)}
  
  ###
  if (contain_1!=0){
    answer1=extract_level1(data1,contain_1)
    contain_1=answer1[[2]]
    extracted=answer1[[1]]
  }
  
  if(contain_1!=0){
    print('This data has level 1 sex/age data')
    return(extracted)}
  
  return('This data has no disaggregated data')
}



############ save all uniformly-format data
for (i in 1:dim(directory_unique)[1]){
  print(paste0('processing',directory_unique[i,'location']))
  answer=judge_each_DT(directory_unique[i,'location'])
  if (class(answer)!='character'){
  write.csv(answer,paste0(working_directory,'uniform/',directory_unique[i,'country'],
                          '_',directory_unique[i,'year'],'.csv'))
  }
}



