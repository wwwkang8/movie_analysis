```{r}
#라이브러리 함수 모음
#install.packages("dplyr")
#install.packages("magrittr")
#install.packages("reshape2")
#install_github("plgrmr/readAny", force = TRUE)
#install.packages("Rcpp")
library("xlsx")
library(dplyr)
library(magrittr)
library(xlsx)
library(ggplot2)
library(MASS)
library(devtools)
library(reshape2)
library(lattice)
library(wordcloud2)
library(readAny)
```
#주제선정 : 영화 흥행에 미치는 요인 상관관계
다른 산업에 비해 고위험, 고수익인 영화 산업은 콘텐츠 제작 후 단시간에 성공 유무가 판가름 나는 편이다. 개봉 전까지 몇 달동안 막대한 마케팅 비용을 쓰고도 개봉 1~2주 만에 실패가 결정되기도 하는 것이다. 그렇다 보니 영화 흥행의 답을 알아내고자 하는 산업 관계자들이 많았다. 그러나 여전히 이에 대한 답은 아무도 모른다. 개봉 스크린수, 배우 및 감독의 영향력, 배급사의 영향력, 제작 국가, 평점, 댓글 등 영화 흥행에 영향을 미치는 요소는 다양하다. 그리고 개봉 시기의 정치, 언론, 사회 환경도 영향을 미친다. 이러한 영화 흥행 요소에 대한 연구는 1970년대부터 진행되어 왔다. 선행연구들을 토대로 영화는 크게 제작자 측면에 해당되는 내재적 변수와 소비자 요인에서 접근하는 정보 원천 변수에 가장 영향을 받는다. 제작자 측면은 감독, 유명 배우, 스크린 수, 개봉 시즌, 관람등급, 배급사 및 장르 등이며, 소비자 측면은 단순한 평점부터 온라인 구전 점수, 구전 크기 등이다. 이처럼 영화 흥행에 영향을 줄 수 있는 요소는 무척 다양하며, 각 요소의 영향력도 일관적이지 않아 다양한 요소들이 동시에 복합적으로 영향을 끼친다고 결론을 내리고 있다. 

#과제 목표
영화의 흥행에 미치는 요인으로 예산, 댓글 수, 평점, 스크린 수, 배급사를 요인으로 정하여
영화의 수익과 비교하여 흥행을 분석한다. 영화 흥행을 분석하기 위한 데이터로는 국내 IPTV, 국내 Box Office 통계, 해외 Box office 통계이다.


#1. 2018년 국내 박스오피스
##국내에서 개봉한 2150개의 영화의 매출액, 매출액점유율, 관객수, 스크린수, 상영횟수 분석

#데이터 전처리
boxoffice2018=read.csv("2018_박스오피스.csv",sep="\t",header=T)
colnames(boxoffice2018)<-c("순위","영화명","개봉일","매출액","매출액점유율","관객수","스크린수","상영횟수","대표국적","국적","배급사")
boxoffice_2018<-boxoffice2018
top100Distributor<-head(boxoffice_2018,100)
#스크린수와 관객수의 상관관계를 확인
#스크린수가 1000개일 때 까지는 영화 관객수의 차이가 크지 않다.
#하지만 스크린 수가 1000개가 넘어가면 관객수가 많이 증가한다. 스크린 수에 따라 관객수가 비례하여 증가한다. 이는 그만큼 영화관에 투자하여 스크린을 많이 확보하여 관객들도 그만큼 더 많이 확보하는 것으로 보인다.
screen_audience<-ggplot(top100Distributor,aes(top100Distributor$스크린수,top100Distributor$관객수))
screen_audience<-screen_audience+geom_point();screen_audience
screen_audience<-screen_audience+geom_smooth(method="loess")

#스크린 횟수와 상영횟수 사이에 상관관계
#스크린 수가 많을수록 상영횟수가 많다. 둘의 관계는 정비례 관계로 확인된다.
screen_play<-ggplot(top100Distributor,aes(top100Distributor$스크린수,top100Distributor$상영횟수))
screen_play<-screen_play+geom_point(); screen_play
screen_play<-screen_play+geom_smooth(method="loess")

#스크린 수와 매출액 사이의 관계
#스크린 수와 매출액 사이의 편차는 크지만 대체적으로 스크린 수와 매출액 사이에는 정비례 관계가 존재한다. 스크린 수가 많을수록 매출액도 증가한다.
screen_profit<-ggplot(top100Distributor,aes(top100Distributor$스크린수,top100Distributor$매출액))
screen_profit<-screen_profit+geom_point(); screen_profit
screen_profit<-screen_profit+geom_smooth(method="loess")

#2. 해외 박스오피스의 영화 예산 통계와 평점 사이트 분석
financial_sum=read.csv("financial_summary.csv",header=T)
review_score=read.csv("review_score.csv",header=T)
#두 데이터 통합 전처리: 평점과 영화의 수익 관계를 확인하기 위해 두 데이터 통합
movie_info<-financial_sum
movie_info$IMDB<-review_score[,2]
movie_info$Metacritic<-review_score[,3]
movie_info$RottenTomatoes<-review_score[,4]
movie_info$RogerEbert<-review_score[,5]

#영화 평점사이트의 점수별 평점분포
#IMDB 사이트가 평균적인 평점이 제일 높고 Metacritics가 평균적으로 제일 낮은 평점을 주는 것으로 확인되었다.
IMDB_histogram<-qplot(movie_info$IMDB,data=movie_info, geom="histogram")
Metacritic_histogram<-qplot(movie_info$Metacritic,data=movie_info, geom="histogram")
RottenTomatoes_histogram<-qplot(movie_info$RottenTomatoes,data=movie_info, geom="histogram")
RogerEbert_histogram<-qplot(movie_info$RogerEbert,data=movie_info, geom="histogram")


#영화의 수입을 흥행 척도로 정하고 변수들을 지정하여 상관관계를 분석한다
# 1. 수입과 미국 국내 박스오피스의의 관계
#Box office는 매표소라는 뜻으로 영화산업에서 "흥행"의 척도로 사용된다. 그래서 Boxoffice가 높을수록 흥행에 성공한 영화이고 그로 인해 매출액도 높은 수치를 보이게 된다. 즉 두 요소는 정비례 관계를 가진다.
profit_boxoffice<-ggplot(data=movie_info, aes(x=movie_info$NetProfit,y=movie_info$DomesticBoxOffice))
profit_boxoffice+geom_point(colour="orange",size=6)

# 2. 총매출액과 예산의 관계
# 영화의 예산이 높다고 해서 총 매출액이 높아지는 것이 아님을 확인할 수 있다. 즉 영화의 흥행에 예산이 조건적으로 영향을 미칠 수 있지만 절대적이지는 않는다는 것을 보여준다
profit_budget<-ggplot(data=movie_info, aes(x=movie_info$NetProfit,y=movie_info$Budget))
profit_budget+geom_point(colour="orange",size=6)

# 3. 수입과 평점 사이의 관계
#수입과 평점사이의 관계를 확인하기 전에 4개의 평점사이트들의 평균 점수를 통일하여 환산하였다.
#IMDB는 10점 만점, Metacritic는 100점 만점, RottenTomatoes는 100점 만점, RogerEbert는 5점 만점이다. 4개의 평점 사이트의 평점을 10점 만점으로 환산하여 평균을 구했다
IMDB_score<-movie_info$IMDB
Metacritic_score<-movie_info$Metacritic/10
RottenTomatoes_score<-movie_info$RottenTomatoes/10
RogerEbert_score<-movie_info$RogerEbert*2

#4개의 평점사이트 평점들을 벡터 더하기 연산을 한다
tot_score<-IMDB_score+Metacritic_score+RottenTomatoes_score+RogerEbert_score

#수입과 평점 관의 관계 분석
#수입이 평점에는 크게 영향을 미치지 않는 것으로 나타났다. 수입이 중간일 때 평점이 높은 영화들이 많았다. 오히려 수입이 증가할 때마다 평점이 낮아지는 영화들이 보인다. 
profit_score<-ggplot(data=movie_info, aes(x=movie_info$NetProfit,y=tot_score))
profit_score+geom_point(colour="orange",size=6)

#예산과 평점간의 관계분석 : 영화예산이 많아지면 평점이 높아질까??
# 그래프를 보면 원형 점들이 양 극단으로 치우쳐 있는 것을 알 수 있다. 예산이 높다고 해서 무조건 평점이 상승하는 것이 아닌 것을 확인할 수 있다. 저예산 영화들 중에서도 평점이 높은 영화들이 많다. 
budget_score<-ggplot(data=movie_info, aes(x=movie_info$Budget,y=tot_score))
budget_score+geom_point(colour="orange",size=6)

#3. 2018년 2월 IPTV 영화 시청횟수 분석
IPTV는 인터넷 프로토콜 TV이다. 즉 인터넷 TV 생각하면 된다.
TV와 리모콘을 사용하여 인터넷을 하면 기존에 방영되었던 모든 방송들을 다시 시청할 수 있는 TV이다.주로 방영되었던 영화, 예능 프로그램들이 일정 금액을 받고 IPTV 방송리스트로 올라간다.
##IPTV Data 컬럼명
#2018년 2월 IPTV 영화 시청순위 1~200위에 대한 정보이다
iptv_stat=read.csv("2018_02_IPTV.csv",sep="\t",stringsAsFactors = FALSE)
iptv<-iptv_stat
#제작국가별 시청 횟수 순위
iptv$제작국가<-as.factor(iptv$제작국가)#제작국가를 factor로 변환한다
levels(iptv$제작국가)
iptv[21,"제작국가"]<-"미국" #국가가 섞여있는 경우 맨 앞에있는 국가이름으로 대체
iptv[63,"제작국가"]<-"독일"
iptv[69,"제작국가"]<-"영국"
iptv[90,"제작국가"]<-"미국"
iptv[101,"제작국가"]<-"영국"
iptv[159,"제작국가"]<-"미국"
iptv[160,"제작국가"]<-"프랑스"
iptv[163,"제작국가"]<-"영국"
iptv[171,"제작국가"]<-"미국"
iptv[144,"제작국가"]<-"아일랜드"
iptv$제작국가<-trimws(iptv$제작국가) #선행 후행 공백 제거
iptv$제작국가<-as.factor(iptv$제작국가)


#제작국가 그래프를 확인할 수 있다
#제작 국가의 경우 미국이 가장 많았고 그 다음 한국에서 제작한 영화가 많았다. 시청자들이 주로 보는 영화의 제작국가는 미국, 한국으로 편중되어 있다.
country<-ggplot(data=iptv,aes(x=factor(제작국가),fill=factor(제작국가)))
country<-country+geom_bar(width=.5)

#한국인이 선호하는 배우 워드클라우드
library(wordcloud2)
actor<-iptv$배우
actor1<-actor
actor1<-strsplit(actor1,",")
actor1<-unlist(actor1)
actor1<-trimws(actor1)
freq_actor<-table(actor1)
head(sort(freq_actor, decreasing = T), 20)
wordcloud2(data=head(sort(freq_actor, decreasing = T), 50),minSize=5)

#barchart로 영화 배우 인기도 표현
library(lattice)
barchart(tail(sort(freq_actor),15,col="lightblue",xlab="Actor Frequency", main="popular movie star"))

#한국인이 선호하는 영화 감독 워드클라우드 및 바차트
director<-iptv$감독
director<-strsplit(director,",")
director<-unlist(director)
director<-trimws(director)
freq_director<-table(director)
wordcloud2(data=freq_director, minSize=10)

barchart(tail(sort(freq_director),15,col="lightblue",xlab="Director Frequency", main="popular movie director"))

#ggplot layer
#1~5위의 영화 시청횟수가 나머지 195개의 영화 시청횟수에 비해 월등히 높다
#이것을 확인해 보았을 때, IPTV에서는 이미 흥행에 성공한 영화들을 주로 소비자들이 보는 것을 알 수 있다. 상영관에서 내린 흥행작들을 위주로 소비자들이 시청을 하기 때문에 다양한 장르의 영화를 시청하는 소비자들이 적은 것으로 해석할 수 있다.
top20<-iptv[iptv$순위<=20,]
moviestat<-ggplot(iptv,aes(순위,X2018년.총.이용건수))
moviestat<-moviestat+geom_point();moviestat
head(iptv, 5) #상위 5개의 영화


























