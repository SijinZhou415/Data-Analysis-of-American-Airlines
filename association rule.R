#association rule

library(arules)
library(arulesViz)

ARframe<-dfFactor
View(ARframe)

ARframeX<-as(ARframe,"transactions")
inspect(ARframeX)
itemFrequency(ARframeX)
ruleset<-apriori(ARframeX, parameter=list(support=0.005,confidence=0.5),appearance = list(default="lhs", rhs=("PromotionStance=Promoter")))
inspect(ruleset)

inspectDT(ruleset)
plot(ruleset)

ruleset1<-apriori(ARframeX, parameter=list(support=0.005,confidence=0.5),appearance = list(default="lhs", rhs=("PromotionStance=Detractor")))
inspectDT(ruleset1)
