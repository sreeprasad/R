#Twitter Sentiment analysis
library(twitteR)
delta.tweets = searchTwitter('@delta', n=1500)
#length(delta.tweets)
#class(delta.tweets)
tweet = delta.tweets[[1]]
#class(tweet)
#tweet$getScreenName()
#tweet$getText()
#install.packages("plyr")
#require(plyr)
#?laply
delta.text = laply(delta.tweets, function(t) t$getText() )
head(delta.text, 5)
hu.liu.pos = scan('/Users/msgovindankuttykutty/sentimentAnalysis/data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('/Users/msgovindankuttykutty/sentimentAnalysis/data/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical')
sample = c("You're awesome and I love you", "I hate and hate and hate. So angry. Die!","Impressed and amazed: you are peerless in your achievement of unparalleled mediocrity.")
result = score.sentiment(sample, pos.words, neg.words)
#class(result)
#result$score
delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')
grep('Kudos*',delta.text)
delta.text[[270]]<-"Kudos to Delta for leaving my luggage on the other side of the country but for hand delivering it to my house within the same day"
delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')
delta.scores$airline = 'Delta'
delta.scores$code = 'DL'
hist(delta.scores$score)
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
americanairline.tweets = searchTwitter('@americanair', n=1500)
americanairlinetweet = americanairline.tweets[[1]]
americanairline.text = laply(americanairline.tweets, function(t) t$getText() )
americanairline.scores = score.sentiment(americanairline.text, pos.words,neg.words, .progress='text')
grep('macnmomorsels',americanairline.text)
americanairline.text[[1179]] <-"NeVaehSkyy macnmomorsels americanair jeffimff SALUTE"
americanairline.scores = score.sentiment(americanairline.text, pos.words,neg.words, .progress='text')
americanairline.scores$airline = 'American'
americanairline.scores$code = 'AA'
hist(americanairline.scores$score)
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
continentalairline.tweets = searchTwitter('@continental', n=1500)
continentalairlinetweet = continentalairline.tweets[[1]]
continentalairline.text = laply(continentalairline.tweets, function(t) t$getText() )
continentalairline.scores = score.sentiment(continentalairline.text, pos.words,neg.words, .progress='text')
continentalairline.scores$airline = 'Continental'
continentalairline.scores$code = 'CO'
hist(continentalairline.scores$score)
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
jetblueairline.tweets = searchTwitter('@jetblue', n=1500)
jetblueairlinetweet = jetblueairline.tweets[[1]]
jetblueairline.text = laply(jetblueairline.tweets, function(t) t$getText() )
jetblueairline.scores = score.sentiment(jetblueairline.text, pos.words,neg.words, .progress='text')
grep('My booking',jetblueairline.text)
jetblueairline.text[[827]]
jetblueairline.text[[827]]<-"My booking my ticket @JetBlue when I get home"
jetblueairline.scores = score.sentiment(jetblueairline.text, pos.words,neg.words, .progress='text')
jetblueairline.scores$airline = 'JetBlue'
jetblueairline.scores$code = 'JB'
hist(jetblueairline.scores$score)
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
southwestairline.tweets = searchTwitter('@SOUTHWESTAIR', n=1500)		
southwestairlinetweet = southwestairline.tweets[[1]]
southwestairline.text = laply(southwestairline.tweets, function(t) t$getText() )
southwestairline.scores = score.sentiment(southwestairline.text, pos.words,neg.words, .progress='text')
grep('lmaooo*',southwestairline.text)
southwestairline.text[[118]]<-"@carol_ash: My @SouthwestAir flight attendant just said what happens in Vegas... is on Facebook."
grep('*SouthwestAir next week*',southwestairline.text)
southwestairline.text[[664]]<-"I'll be flying @SouthwestAir next week, and hopefully they'll have wifi on board! I need to be A-List status to get it free though."
grep('RitaB*',southwestairline.text)
southwestairline.text[[709]]<-"RitaB southwestair phlairport Awwwww thats mess up"
grep('Hopefully',southwestairline.text)
southwestairline.text[872]<-"Hopefully southwestair has their stuff together tonight because they surely stole my first day in Disney from me not happy"
grep('SouthwestAir allows*',southwestairline.text)
southwestairline.text[1220]<-"SouthwestAir allows  checked bags for free but AllegiantTravel is now charging  for your carryon WTF NOT thee business"
southwestairline.scores = score.sentiment(southwestairline.text, pos.words,neg.words, .progress='text')
southwestairline.scores$airline = 'SouthWest'
southwestairline.scores$code = 'SW'
hist(southwestairline.scores$score)
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
unitedairline.tweets = searchTwitter('@unitedairlines', n=1500)	
unitedairlinetweet = unitedairline.tweets[[1]]	
unitedairline.text = laply(unitedairline.tweets, function(t) t$getText() )
unitedairline.scores = score.sentiment(unitedairline.text, pos.words,neg.words, .progress='text')
grep('unitedairlines  if you*',unitedairline.text)
unitedairline.text[[190]] <-"unitedairlines  if you want to keep your customers Buy some new planes Had to make an unscheduled landing today at PBI  disc "
unitedairline.scores = score.sentiment(unitedairline.text, pos.words,neg.words, .progress='text')
unitedairline.scores$airline = 'United'
unitedairline.scores$code = 'UA'
hist(unitedairline.scores$score)
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
usairways.tweets = searchTwitter('@usairways', n=1500)	
usairwaystweet = usairways.tweets[[1]]
usairways.text = laply(usairways.tweets, function(t) t$getText() )
usairways.scores = score.sentiment(usairways.text, pos.words,neg.words, .progress='text')
usairways.scores$airline = 'USAirways'
usairways.scores$code = 'US'
hist(usairways.scores$score)	
all.scores = rbind( americanairline.scores, continentalairline.scores, delta.scores, jetblueairline.scores, southwestairline.scores, unitedairline.scores, usairways.scores )
install.packages('ggplot2')
library('ggplot2')
ggplot(data=all.scores) + geom_bar(mapping=aes(x=score, fill=airline), binwidth=1) + facet_grid(airline~.) +  theme_bw() + scale_fill_brewer() 
all.scores$very.pos = as.numeric( all.scores$score >= 2 )
all.scores$very.neg = as.numeric( all.scores$score <= -2 )
twitter.df = ddply(all.scores, c('airline', 'code'), summarise, pos.count = sum( very.pos ), neg.count = sum( very.neg ) )
twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count
twitter.df$score = round( 100 * twitter.df$pos.count /twitter.df$all.count )
orderBy(~-score, twitter.df)
install.packages('doBy')
library('doBy')
orderBy(~-score, twitter.df)
install.packages('XML')
library('XML')
acsi.url = 'http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'
acsi.df = readHTMLTable(acsi.url, header=T, which=1, stringsAsFactors=F)
acsi.df = acsi.df[,c(1,18)]
head(acsi.df,1)
colnames(acsi.df) = c('airline', 'score')
acsi.df$code = c('WN', NA, 'CO', NA, 'AA', 'DL',
'US', 'NW', 'UA')
acsi.df$score = as.numeric(acsi.df$score)
compare.df = merge(twitter.df, acsi.df, by='code', suffixes=c('.twitter', '.acsi'))
compare.df = subset(compare.df, all.count > 100)
ggplot( compare.df ) + geom_point(aes(x=score.twitter, y=score.acsi, color=airline.twitter), size=5) + geom_smooth(aes(x=score.twitter, y=score.acsi, group=1), se=F, method="lm") + theme_bw() + opts(legend.position=c(0.2, 0.85))