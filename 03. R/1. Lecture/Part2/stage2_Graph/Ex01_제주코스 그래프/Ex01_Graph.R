# [빅데이터를 활용한 스마트 데이터 전문가 양성과정]
# 2019-06-12-(수)2 / 135page

#--------------------------------------------------------------#
#--------------------- Part 2 Visualization -------------------#
#------------------- Section 1 : Wordcloud1 -------------------#
#--------------------------------------------------------------#

# 참고자료 : https://kuduz.tistory.com/1090 : 최대한 친절하게 쓴 R로 낱말구름, 의미연결망 그리기
#            https://junhewk.github.io/text/2017/06/18/single-word-analysis-korean-poem/
#            R에서 해보는 한국 일제강점기 시의 단어 분석

# tidyverse 패키지는 아직 한글을 지원하지 않는것으로 보임.. 다만 영문을 분석할 때는 유용한 툴
# 쓰고자 할때는 구글 번역 api를 통해 한글을 번역

install.packages('KoNLP')
install.packages('wordcloud')
install.packages('stringr')
library(KoNLP)
library(wordcloud)
library(stringr)
library(ggplot2)
library(plotrix)  # 3d 파이차트
library(RColorBrewer)
###
rm(list = ls())
###-----------------------------------------------------


useSejongDic() # 세종사전 호출 
mergeUserDic(data.frame(readLines("data/제주도여행지.txt"), "ncn"))  # 사용자가 원하는 단어를 추가 *** 중요
# buidDictionary() 곧 이 함수를 사용해야함
# stringr 패키지를 이용한 전처리 -----------------------

txt <- readLines("jeju.txt")  # 텍스트파일을 벡터형태로 받음
place   <- sapply(txt,extractNoun,USE.NAMES=F) # extracNoun은 한글 문장에서 명사만 출력 | USE.NAME 옵션은 벡터의 원래 이름을 나타낸다
head(unlist(place),30)
cdata   <- unlist(place)

# 정규표현식 참고자료 : https://m.blog.naver.com/PostView.nhn?blogId=easternsun&logNo=220201798414&proxyReferer=https%3A%2F%2Fwww.google.com%2F

place   <- str_replace_all(cdata,"[^[:alpha:]]","") # --한글, 영어 외는 삭제.
place   <- gsub(" ","",place)
txt     <- readLines("제주도여행코스gsub.txt")

for(i in 1:length(txt)){
  place <- gsub((txt[i]),"",place)
}

place <- Filter(function(x){nchar(x) >= 2},place)
write(unlist(place),"jeju_2.txt")
rev   <- read.table("jeju_2.txt")
nrow(rev)
wordcount <- table(rev)
head(sort(wordcount, decreasing=T),30)
top10 <- head(sort(wordcount,decreasing = T),10)

wordcloud2(top10_wordcount, size=0.5, col="random-light", backgroundColor="black", fontFamily='나눔바른고딕',
           minRotation=0, maxRotation=0)

# pie graph 그리기 =======================================================

pct <- round(top10/sum(top10)*100,1)

lab <- paste(names(top10),"\n",pct,"%")
lab <- paste(names(top10),"\n",pct,"%",sep='') # sep 는 빈칸 없애기.

palete <- brewer.pal(10,"Set3")
palete2 <- brewer.pal(7,"Reds")
windowsFonts(baedal=windowsFont("배달의민족 도현"))
  
pie(top10,
    col=palete,
    radius=0.9,
    main="제주도 추천 여행 코스 TOP10",
    cex=1.5,
    labels=lab,
    family="baedal")

# 3D pie graph 그리기 =======================================================

library(plotrix)

th_pct    <- round(top10/sum(top10)*100,1)
th_names  <- names(top10) # 변수 이름만 가져오기.
th_labels <- paste0(th_names,"\n","(",th_pct,")")

pie3D(top10,
      main="제주도 추천 여행 코스 Top 10",
      col=palete,
      cex=0.3,
      labels=th_labels,
      explode=0.05,
      family="baedal")


# barchart 그리기(1) 세로 =========================================================

bp <- barplot(top10,
        main="제주도 추천 여행 코스 TOP 10",
        col=palete,
        cex.names=0.7,
        las=2,
        ylim=c(0,25),
        family="baedal")
text(x = bp, y = top10*1.05, labels =paste("(",pct,"%",")"), col="black",cex=0.7)
text(x = bp, y = top10*0.95, labels =paste(top10,"건"), col="black",cex=0.7)


# barchart 그리기(2) 가로 =========================================================

bp <- barplot(top10,
              main="제주도 추천 여행 코스 TOP 10",
              col=palete,
              cex.names=0.7,
              las=2,
              xlim=c(0,25),
              family="baedal",
              horiz=T)
text(y = bp, x = top10*0.9, labels =paste0(top10,"건"), col="black",cex=0.7)
text(y = bp, x = top10*1.10, labels =paste0("(",pct,"%",")"), col="black",cex=0.7)



### ============================================================================
### ggplot으로 bar, pie graph 그리기. ==========================================
### ============================================================================

str(top10)
df_top10 <- as.data.frame(top10)

ggplot(df_top10,aes(x='',y=Freq,fill=rev,labes=lab)) + 
  geom_bar(width=1, stat='identity') 

ggplot(df_top10,aes(x=rev,y=Freq,fill=rev)) + 
  geom_bar(width=1, stat='identity') +
  coord_polar("y",start=0) 


library(dplyr)
options(digits = 2) # 알 프로그램 전체에 영향. 소수점하고 그 다음숫자 포함 2개만 쓰겠다. 0.0

df_top10 <- df_top10 %>% 
#  mutate(pct= Freq / sum(Freq)*100) %>% 
  mutate(pct= round(Freq / sum(Freq)*100,1)) %>% 
  mutate(ylabel = paste0(rev,pct, '%')) %>%  # paste에 0d이 붙으면 sep=''가 필요없다.
#  mutate(ylabel = paste(sprintf("%4.1f", pct), '%', sep='')) %>%
  arrange(desc(rev)) %>% # 그래프는 1에서부터 밑에서부터 깔아지기 때문에 레이블의 위치를 설정하기 위해 순서 변경
  mutate(ypos = cumsum(pct) - 0.5*pct)
df_top10

ggplot(df_top10,aes(x='',y=Freq,fill=rev)) + 
  geom_bar(width=1, stat='identity') +
  geom_text(aes(y=ypos, label=ylabel), color='black')

ggplot(df_top10,aes(x='',y=Freq,fill=rev, family='baedal')) + 
  geom_bar(width=1, stat='identity') +
  geom_text(aes(y=ypos, label=ylabel), color='black') +
  coord_polar("y",start=0) +
  ggtitle('제주도 추천 여행 코스 TOP10') +
  theme_bw(base_family="baedal",base_size = 10) +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5, 
                                  size = 20, 
                                  color = "darkblue"))




