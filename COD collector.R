library(rvest)
library(xml2)
library(stringr)
options(warn=-1)

# where to save the downloads, this is the only thing to set up
working_directory="E:/File/Pennsylvania/UNFPA/"

# define a function that 'clean country name'(ccn), e.g. ccn("Côte d'Ivoire")='Cote d Ivoire'
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
              #download.file(addr,quiet = 1,destfile = address_curr,mode = "wb")
            }
          }
        }
      }
    }
  }
}
print(paste0('COD data processing takes ',round(Sys.time()-start_time,2),' minutes'))


# the details of all data downloaded are recorded in a dataframe; 
# this directory has repeated country names; 
# refer to directory variable 'directory_unique' to see which country is done
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

# dataframe records which country is under consideration for info extraction
directory_unique=directory[unique_row,]

################### start to extract disaggregation from highest admin level
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



##### evaluate level 2, if not qualified, go to level 1
extract_level2=function(data2,contain_2){
  # create an abbreviation
  col2=colnames(data2)
  
  if (find_index(c('male',4),col2)!=0 & find_index(c('female',4),col2)!=0){
    # c('male','4') finds a column name that contains male and 4-yr old, i.e. both sex/age disaggregation
    
    # extract each column
    admin1_name=find_index(c('Admin1','name'),col2)
    admin2_name=find_index(c('Admin2','name'),col2)
    male=find_index(c('Male'),col2)
    female=find_index(c('Female'),col2)
    m_age0_4=find_index(c('male','4'),col2)
    m_age5_9=find_index(c('male','5','9'),col2)
    m_age10_14=find_index(c('male','10','14'),col2)
    m_age15_19=find_index(c('male','15','19'),col2)
    m_age20_24=find_index(c('male','20','24'),col2)
    m_age25_29=find_index(c('male','25','29'),col2)
    m_age30_34=find_index(c('male','30','34'),col2)
    m_age35_39=find_index(c('male','35','39'),col2)
    m_age40_44=find_index(c('male','40','44'),col2)
    m_age45_49=find_index(c('male','45','49'),col2)
    m_age50_54=find_index(c('male','50','54'),col2)
    m_age55_59=find_index(c('male','55','59'),col2)
    m_age60_64=find_index(c('male','60','64'),col2)
    m_age65_69=find_index(c('male','65','69'),col2)
    m_age70_74=find_index(c('male','70','74'),col2)
    m_age75_79=find_index(c('male','75','79'),col2)
    m_age80_=find_index(c('male','80'),col2)
    
    f_age0_4=find_index(c('female','4'),col2)
    f_age5_9=find_index(c('female','5','9'),col2)
    f_age10_14=find_index(c('female','10','14'),col2)
    f_age15_19=find_index(c('female','15','19'),col2)
    f_age20_24=find_index(c('female','20','24'),col2)
    f_age25_29=find_index(c('female','25','29'),col2)
    f_age30_34=find_index(c('female','30','34'),col2)
    f_age35_39=find_index(c('female','35','39'),col2)
    f_age40_44=find_index(c('female','40','44'),col2)
    f_age45_49=find_index(c('female','45','49'),col2)
    f_age50_54=find_index(c('female','50','54'),col2)
    f_age55_59=find_index(c('female','55','59'),col2)
    f_age60_64=find_index(c('female','60','64'),col2)
    f_age65_69=find_index(c('female','65','69'),col2)
    f_age70_74=find_index(c('female','70','74'),col2)
    f_age75_79=find_index(c('female','75','79'),col2)
    f_age80_=find_index(c('female','80'),col2)
    
    # compile into a new dataframe
    extracted=data2[,c(admin1_name,admin2_name,male,female,m_age0_4,m_age5_9,m_age10_14,m_age15_19,
                       m_age20_24,m_age25_29,m_age30_34,m_age35_39,m_age40_44,m_age45_49,m_age50_54,
                       m_age55_59,m_age60_64,m_age65_69,m_age70_74,m_age75_79,m_age80_,
                       f_age0_4,f_age5_9,f_age10_14,f_age15_19,f_age20_24,f_age25_29,f_age30_34,
                       f_age35_39,f_age40_44,f_age45_49,f_age50_54,f_age55_59,f_age60_64,
                       f_age65_69,f_age70_74,f_age75_79,f_age80_)]
    colnames(extracted)=c('Admin1','Admin2','Male','Female','male 0-4','male 5-9','male 10-14',
                          'male 15-19','male 20-24','male 25-29','male 30-34','male 35-39',
                          'male 40-44','male 45-49','male 50-54','male 55-59','male 60-64',
                          'male 65-69','male 70-74','male 75-79','male 80+','female 0-4',
                          'female 5-9','female 10-14','female 15-19','female 20-24',
                          'female 25-29','female 30-34','female 35-39','female 40-44',
                          'female 45-49','female 50-54','female 55-59','female 60-64',
                          'female 65-69','female 70-74','female 75-79','female 80+')
    
    return(list(extracted,contain_2))
  }else{
    # if admin level 2 does not qualify, indicate this by making contain_2=0
    contain_2=0
    print('This data has level 2 data but not about sex AND age disaggregation')
    return(list(data2,contain_2))
  }}

##### evaluate level 1
extract_level1=function(data1,contain_1){
  # create an abbreviation
  col1=colnames(data1)
  
  if (find_index(c('male',4),col1)!=0 & find_index(c('female',4),col1)!=0){
    
    admin1_name=find_index(c('Admin1','name'),col1)
    male=find_index(c('Male'),col1)
    female=find_index(c('Female'),col1)
    m_age0_4=find_index(c('male','4'),col1)
    m_age5_9=find_index(c('male','5','9'),col1)
    m_age10_14=find_index(c('male','10','14'),col1)
    m_age15_19=find_index(c('male','15','19'),col1)
    m_age20_24=find_index(c('male','20','24'),col1)
    m_age25_29=find_index(c('male','25','29'),col1)
    m_age30_34=find_index(c('male','30','34'),col1)
    m_age35_39=find_index(c('male','35','39'),col1)
    m_age40_44=find_index(c('male','40','44'),col1)
    m_age45_49=find_index(c('male','45','49'),col1)
    m_age50_54=find_index(c('male','50','54'),col1)
    m_age55_59=find_index(c('male','55','59'),col1)
    m_age60_64=find_index(c('male','60','64'),col1)
    m_age65_69=find_index(c('male','65','69'),col1)
    m_age70_74=find_index(c('male','70','74'),col1)
    m_age75_79=find_index(c('male','75','79'),col1)
    m_age80_84=find_index(c('male','80','84'),col1)
    m_age85_89=find_index(c('male','85','89'),col1)
    m_age90_94=find_index(c('male','90','94'),col1)
    m_age95_99=find_index(c('male','95','99'),col1)
    m_age100_=find_index(c('male','100'),col1)
    f_age0_4=find_index(c('female','4'),col1)
    f_age5_9=find_index(c('female','5','9'),col1)
    f_age10_14=find_index(c('female','10','14'),col1)
    f_age15_19=find_index(c('female','15','19'),col1)
    f_age20_24=find_index(c('female','20','24'),col1)
    f_age25_29=find_index(c('female','25','29'),col1)
    f_age30_34=find_index(c('female','30','34'),col1)
    f_age35_39=find_index(c('female','35','39'),col1)
    f_age40_44=find_index(c('female','40','44'),col1)
    f_age45_49=find_index(c('female','45','49'),col1)
    f_age50_54=find_index(c('female','50','54'),col1)
    f_age55_59=find_index(c('female','55','59'),col1)
    f_age60_64=find_index(c('female','60','64'),col1)
    f_age65_69=find_index(c('female','65','69'),col1)
    f_age70_74=find_index(c('female','70','74'),col1)
    f_age75_79=find_index(c('female','75','79'),col1)
    f_age80_84=find_index(c('female','80','84'),col1)
    f_age85_89=find_index(c('female','85','89'),col1)
    f_age90_94=find_index(c('female','90','94'),col1)
    f_age95_99=find_index(c('female','95','99'),col1)
    f_age100_=find_index(c('female','100'),col1)
    
    
    extracted=data1[,c(admin1_name,male,female,m_age0_4,m_age5_9,m_age10_14,m_age15_19,m_age20_24,
                       m_age25_29,m_age30_34,m_age35_39,m_age40_44,m_age45_49,m_age50_54,
                       m_age55_59,m_age60_64,m_age65_69,m_age70_74,m_age75_79,m_age80_84,
                       m_age85_89,m_age90_94,m_age95_99,m_age100_,f_age0_4,f_age5_9,
                       f_age10_14,f_age15_19,f_age20_24,f_age25_29,f_age30_34,f_age35_39,
                       f_age40_44,f_age45_49,f_age50_54,f_age55_59,f_age60_64,f_age65_69,
                       f_age70_74,f_age75_79,f_age80_84,f_age85_89,f_age90_94,f_age95_99,f_age100_)]
    max_length_str=c('Admin1','Male','Female','male 0-4','male 5-9','male 10-14','male 15-19',
                     'male 20-24','male 25-29','male 30-34','male 35-39','male 40-44',
                     'male 45-49','male 50-54','male 55-59','male 60-64','male 65-69',
                     'male 70-74','male 75-79','male 80-84','male 85-89','male 90-94',
                     'male 95-99','male 100+','female 0-4','female 5-9','female 10-14',
                     'female 15-19','female 20-24','female 25-29','female 30-34',
                     'female 35-39','female 40-44','female 45-49','female 50-54',
                     'female 55-59','female 60-64','female 65-69','female 70-74',
                     'female 75-79','female 80-84','female 85-89','female 90-94',
                     'female 95-99','female 100+')
    colnames(extracted)=max_length_str[1:dim(extracted)[2]]
    
    return(list(extracted,contain_1))
  }else{
    contain_1=0
    print('This data has level 1 data but not about sex')
    return(list(data1,contain_1))
  }}

# follow the decision tree in 'summary of HDX collector.pptx'
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
    print('This data HAS LEVEL 2 sex/age data')
    return(extracted)}
  
  ###
  if (contain_1!=0){
    answer1=extract_level1(data1,contain_1)
    contain_1=answer1[[2]]
    extracted=answer1[[1]]
  }
  
  if(contain_1!=0){
    print('This data HAS LEVEL 1 sex/age data')
    return(extracted)}
  
  return('This data has no disaggregated data')
}



############ save all uniformly-format data in a folder 'uniform'
# create a list that record all countries finished at current stage
final_list=c()
for (i in 1:dim(directory_unique)[1]){
  print(paste0('processing',directory_unique[i,'location']))
  answer=judge_each_DT(directory_unique[i,'location'])
  if (class(answer)!='character'){
  write.csv(answer,paste0(working_directory,'uniform/',directory_unique[i,'country'],
                          '_',directory_unique[i,'year'],'.csv'))
  # record all true valid country in a dataframe
  final_list=c(final_list,directory_unique[i,'country'])
  }
}


