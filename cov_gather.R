


# PDO from NOAA ---------------------------------------------------------------------

pdo_1854_present<-read.table("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",  header=TRUE, skip=1, fill=TRUE)

#Note, the last year of data gets imported with -99 as the values missing appended to the previous month's value
#Makes for eg. November a character
#best to exclude Year 2022 first or if just doing summer and have full data for summer then can ignore


pdo_1854_present<-pdo_1854_present %>% filter(Year!= 2022) %>% as_tibble() %>% 
                  mutate(Jan = as.numeric(Jan)) %>% 
                  mutate(PDO.summer.av= rowMeans(select(.,May, Jun, Jul, Aug, Sep)))  %>% 
                  mutate(cov_PDO_pacific_year = rowMeans(select(.,Jan, Feb, Mar, Apr, May, Jun, Jul, Sep, Oct, Nov, Dec)))

pdo_simple<- pdo_1854_present %>% select(Year, PDO.summer.av, cov_PDO_pacific_year) %>% rename(year = Year, cov_PDO_pacific_summer = PDO.summer.av)
pdo_simple


