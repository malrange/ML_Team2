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

unique(data$색상)

# 파스텔 톤 색상 팔레트 설정
colors <- c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", 
            "#ffffcc", "#e5d8bd", "#fddaec", "#f2f2f2", "#b3e2cd", 
            "#fdcdac", "#1f78b4", "#a9a9a9")

colors_mapped <- c("회"="#a9a9a9",   # 회색
                   "흰"="#e9f9f9",   # 흰색
                   "검정"="black", # 검정색 (파란색 계열로 표현)
                   "파란"="#b3cde3", # 파란색
                   "노란"="#ffffcc", # 노란색
                   "갈"="#e5d8bd",   # 갈색
                   "주황"="#fdcdac", # 주황색
                   "빨간"="#fbb4ae", # 빨간색
                   "자주"="#decbe4", # 자주색
                   "분홍"="#fddaec", # 분홍색
                   "기타"="#b1b9f9", # 기타 (노란색 계열로 표현)
                   "녹"="#b3e2cd")  # 녹색


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
  scale_fill_manual(values=colors_mapped) + 
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
View(data)
# 주행거리에 대한 히스토그램
p5 <- ggplot(data, aes(x=주행거리)) +
  geom_histogram(binwidth=10000, fill="skyblue", color="black") +
  theme_minimal() +
  labs(title="주행거리 분포", x="주행거리 (km)", y="빈도") +
  scale_x_continuous(labels = scales::comma)

# 배기량에 대한 히스토그램
p6 <- ggplot(data, aes(x=배기량)) +
  geom_histogram(binwidth=100, fill="pink", color="black") +
  theme_minimal() +
  labs(title="배기량 분포", x="배기량 (cc)", y="빈도") +
  scale_x_continuous(labels = scales::comma)

p7 <- ggplot(data, aes(x=가격)) +
  geom_histogram(binwidth=100, fill="pink", color="black") +
  theme_minimal() +
  labs(title="가격 분포", x="가격(만원)", y="빈도") +
  scale_x_continuous(labels = scales::comma)
p7
# 세 개의 그래프를 한 페이지에 배열
grid.arrange(p5, p6, nrow = 2)


# 필요한 라이브러리를 로드합니다.
#install.packages("corrplot")
#install.packages("RColorBrewer")

library(corrplot)
library(RColorBrewer)


# 데이터의 연속형 변수들만 선택합니다. 
# 여기서는 예시이므로 실제 데이터에 맞게 변수를 선택해야 합니다.
data_continuous <- data[, c("가격", "배기량", "주행거리","")]
data$소유자.이전.횟수
# 상관계수 행렬을 계산합니다.
cor_matrix <- cor(data_continuous, use="complete.obs")  # NA 값이 있는 경우를 처리

# 상관계수 히트맵을 그립니다.
corrplot(cor_matrix, method="color", type="upper", order="hclust",
         tl.col="black", tl.srt=45,  # 텍스트 색상과 각도
         addCoef.col = "black", # 상관계수 값의 색상
         # 다음 옵션은 필요에 따라 조정 가능
         col=brewer.pal(n=8, name="RdBu"), # 색상 팔레트
         diag=FALSE) # 대각선(자기 자신과의 상관계수) 제거

cor_matrix <- cor(data_continuous, use = "complete.obs")  # NA 값이 있는 경우를 처리
corrplot(cor_matrix, method = "color", type = "full", order = "hclust",
         tl.col = "black", tl.srt = 45,  # 텍스트 색상과 각도
         addCoef.col = "black",  # 상관계수 값의 색상
         col = brewer.pal(n = 8, name = "RdBu")) # 색상 팔레트
  



