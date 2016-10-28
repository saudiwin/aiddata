# Analyze aid data for Tunisia

require(magrittr)
require(dplyr)
require(tidyr)
require(tibble)
require(ggplot2)
require(bit64)
require(scales)
require(fuzzyjoin)
require(tidytext)
require(stringdist)

full_data <- data.table::fread('AidDataCoreThin_ResearchRelease_Level1_v3.0.csv',
                               integer64 = 'double') %>% as_data_frame %>% filter(!grepl(x = recipient,pattern='unspecified|Regional')) %>% 
  filter(year %in% c('2011','2012','2013')) %>% 
  mutate(recipient=ifelse(recipient=='Viet Nam','Vietnam',recipient))

#all_data <- data.table::fread('AidDataCoreFull_ResearchRelease_Level1_v3.0.csv',
#                              integer64 = 'double',check.names=FALSE,data.table=FALSE) %>% as_data_frame 

wb_gdp <- data.table::fread('7ec37f9b-975a-4fd9-bc84-68f3a117cb55_Data.csv',data.table=FALSE,encoding='UTF-8') %>% 
  as_data_frame %>% select(`Country Name`,`Country Code`,matches('YR'))

wb_gdp <- wb_gdp %>%  gather(year,GDP,matches('YR')) %>% mutate(year=substr(year,1,4),
                                                                 GDP=as.numeric(GDP))

# To do the join, we need to match each world bank token independently using regular expressions
data('stop_words')
unique_recip <- unique(full_data$recipient)

match_wb <- function(x) {
  outvar <- vector(length=length(x))
  for(i in 1:length(x)) {
    outvar[i] <- if(length(which(grepl(paste0('\\b',x[i]),unique_recip,ignore.case=TRUE)))==0) {
      ""
    } else {
      unique_recip[which(grepl(paste0('\\b',x[i]),unique_recip,ignore.case=TRUE))]
    }
  }
  return(outvar)
}

wb_text <- wb_gdp %>% unnest_tokens(country_token,`Country Name`) %>% select(`Country Code`,country_token) %>% 
  anti_join(stop_words,by=c('country_token'='word')) %>% filter(nchar(country_token)>3,!(country_token %in% c('republic',
                                                                                                              'united'))) %>% 
                                                                  mutate(match_code=match_wb(country_token)) %>% 
  mutate(match_code=if_else(`Country Code`=='WBG','Palestinian Adm. Areas',match_code)) %>% 
  mutate(match_code=if_else(`Country Code`=='DMA','Dominica',match_code)) %>% 
  mutate(match_code=if_else(`Country Code`=='KOR','Korea',match_code)) %>%
  mutate(match_code=if_else(`Country Code`=='PRK','Korea, Democratic Republic of',match_code)) %>%
  distinct(country_token,match_code,
                    .keep_all=TRUE)
wb_text %<>% distinct(`Country Code`,match_code,.keep_all=TRUE) %>% filter(match_code!='')
full_data <- full_data %>% left_join(wb_text,by=c('recipient'='match_code')) %>% left_join(wb_gdp) 

# Collapse by country

summarized <- full_data %>% group_by(recipient,year) %>% summarize(ratio_aid=sum(commitment_amount_usd_constant)/GDP[1],GDP=GDP[1]) %>% 
  filter(GDP>10000000000)

summarized %>% ungroup %>% 
  mutate(recip_label=ifelse(recipient=='Tunisia',recipient,NA)) %>% 
           ggplot(aes(x=as.factor(year),y=ratio_aid,group=recipient,colour=recip_label)) + geom_path() + guides(colour=FALSE) +
  theme_minimal()

summarized <- full_data %>% group_by(recipient,year) %>%   filter(GDP>10000000000) %>% summarize(ratio_aid=sum(commitment_amount_usd_constant)/GDP[1])
summarized <- summarized %>% group_by(recipient) %>% summarize(ratio_aid=mean(ratio_aid))

summarized %>% ungroup %>% 
  mutate(recip_label=ifelse(recipient=='Tunisia',recipient,NA),
         some_recips=ifelse(recipient %in% c('Afghanistan','Kuwait',
                                             'Jordan','Iraq','Egypt','Tunisia'),recipient,NA)) %>% 
  ggplot(aes(x=reorder(recipient,ratio_aid),y=ratio_aid,colour=recip_label)) + geom_bar(stat='identity',fill='lightskyblue3') + theme_minimal() + coord_flip() + 
  geom_text(aes(label=some_recips),colour='grey16',vjust='inward',hjust='inward') +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) + ylab("Percent of GDP as Aid, 2011-2013\nSource: Aiddata.org and World Bank") + scale_colour_brewer(palette='Set1') +
  guides(colour=FALSE) + xlab("")

full_data %>% count(recipient) %>% arrange(-n) %>% 
  mutate(recip_label=ifelse(recipient=='Tunisia',NA,recipient),
           recipient=factor(recipient,recipient),recip_color=ifelse(recipient=='Tunisia','color','no_color'),
         recip_name=ifelse(recipient=='Tunisia','Tunisia',NA)) %>% 
  ggplot(aes(x=recipient,y=n,fill=recip_color)) + geom_bar(stat='identity',alpha=0.5) + 
    stat_identity(geom='text',aes(y=n,label=recip_label),colour='black',hjust='inward',vjust='inward',check_overlap=TRUE) +  
  stat_identity(geom='text',aes(y=n,label=recip_name,colour=recip_color),hjust=-2,check_overlap=TRUE) +
  coord_flip()  + theme_minimal() +  theme(axis.ticks.y=element_blank(),
                         axis.text.y=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +  
  xlab('') + ylab('Number of aid projects, 2011-2013 from AidData') + 
  scale_colour_brewer(guide=FALSE,palette='Set1') + scale_fill_brewer(guide=FALSE,palette='Set1')

  png(filename='tunisia_aid_revenue.png',width=400,height=600,res=100)
  full_data %>% group_by(recipient) %>% 
    summarize(aid_money=sum(commitment_amount_usd_constant/1000)) %>% ungroup %>% arrange(-aid_money) %>% 
    mutate(recip_label=ifelse(recipient=='Tunisia',NA,recipient),
           recipient=factor(recipient,recipient),recip_color=ifelse(recipient=='Tunisia','color','no_color'),
           recip_name=ifelse(recipient=='Tunisia','Tunisia',NA),
           recip_label=ifelse(recip_label %in% sample(recipient,75),NA,recip_label)) %>% 
    ggplot(aes(x=recipient,y=aid_money,fill=recip_color)) + geom_bar(stat='identity',alpha=0.5) + 
    stat_identity(geom='text',aes(y=aid_money,label=recip_label),colour='black',hjust='inward',vjust='inward',check_overlap=TRUE) +  
    stat_identity(geom='text',aes(y=aid_money,label=recip_name,colour=recip_color),hjust=-3,check_overlap=TRUE) +
    coord_flip()  + theme_minimal() +  theme(axis.ticks.y=element_blank(),
                                             axis.text.y=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +  
    xlab('') + ylab('Amount of Aid Revenue in Millions USD\n2011-2013 from AidData') + 
    scale_colour_brewer(guide=FALSE,palette='Set1') + scale_fill_brewer(guide=FALSE,palette='Set1') + scale_y_continuous(labels=comma)
  dev.off()

