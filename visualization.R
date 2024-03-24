# 필요한 패키지를 불러옵니다.
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(showtext)

# showtext를 사용하여 한글 폰트 문제를 해결합니다.
showtext_auto(enable = TRUE)
font_add_google("Nanum Gothic", "nanumgothic")

data <- read.csv("donotlabeling.csv")



# 파스텔 톤 색상 팔레트 설정
colors <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", 
            "#ffffcc", "#e5d8bd", "#fddaec", "#f2f2f2", "#b3e2cd", 
            "#fdcdac", "#1f78b4", "#a9a9a9")




View(data)
# 알록달록하고 이쁜 그래프를 그립니다.
p1 <- ggplot(data, aes(x=브랜드, fill=브랜드)) + 
  geom_bar() + 
  scale_fill_manual(values=colors) + 
  theme_minimal() + 
  labs(title="브랜드별 빈도", x="브랜드", y="빈도수") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family="nanumgothic"), 
        text = element_text(family="nanumgothic"), 
        plot.title = element_text(hjust = 0.5))
p1

# 알록달록하고 이쁜 그래프를 그립니다.
p2 <- ggplot(data, aes(x=색상, fill=색상)) + 
  geom_bar() + 
  scale_fill_manual(values=colors) + 
  theme_minimal() + 
  labs(title="색상별 빈도", x="색상", y="빈도수") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family="nanumgothic"), 
        text = element_text(family="nanumgothic"), 
        plot.title = element_text(hjust = 0.5))
p2

p3 <- ggplot(data, aes(x=변속기, fill=변속기)) + 
  geom_bar() + 
  scale_fill_manual(values=colors) + 
  theme_minimal() + 
  labs(title="변속기별 빈도", x="변속기", y="빈도수") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family="nanumgothic"), 
        text = element_text(family="nanumgothic"), 
        plot.title = element_text(hjust = 0.5))
p3

p4 <- ggplot(data, aes(x=연료, fill=연료)) + 
  geom_bar() + 
  scale_fill_manual(values=colors) + 
  theme_minimal() + 
  labs(title="연료별 빈도", x="연료", y="빈도수") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family="nanumgothic"), 
        text = element_text(family="nanumgothic"), 
        plot.title = element_text(hjust = 0.5))
p4

grid.arrange(p1, p2, p3, p4, nrow = 2)
