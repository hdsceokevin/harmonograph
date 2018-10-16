
# Harmonograph 함수 생성
harmonograph <- function(seed) {
  
  # 시드 설정
  set.seed(seed = seed)
  
  # 임의의 수 생성 (축)
  d1 <- runif(n = 1, min = 0, max = 1e-02)
  d2 <- runif(n = 1, min = 0, max = 1e-02)
  d3 <- runif(n = 1, min = 0, max = 1e-02)
  d4 <- runif(n = 1, min = 0, max = 1e-02)
  
  # 임의의 수 생성 (사인곡선의 x축 폭 설정)
  f1 <- jitter(x = sample(x = c(2, 3), size = 1))
  f2 <- jitter(x = sample(x = c(2, 3), size = 1))
  f3 <- jitter(x = sample(x = c(2, 3), size = 1))
  f4 <- jitter(x = sample(x = c(2, 3), size = 1))
  
  # 임의의 수 생성 (사인곡선의 y축 이동)
  p1 <- runif(n = 1, min = 0, max = pi)
  p2 <- runif(n = 1, min = 0, max = pi)
  p3 <- runif(n = 1, min = 0, max = pi)
  p4 <- runif(n = 1, min = 0, max = pi)
  
  # x, y값 생성 함수 
  xt <- function(t) exp(x = -d1*t) * sin(x = f1*t + p1) + exp(x = -d2*t) * sin(x = f2*t + p2)
  yt <- function(t) exp(x = -d3*t) * sin(x = f3*t + p3) + exp(x = -d4*t) * sin(x = f4*t + p4)
  
  # x축 및 y축 변화
  t <- seq(from = 1, to = 100, by = 0.001)
  
  # 데이터 저장 
  dat <- data.frame(t = t, x = xt(t), y = yt(t))
  
  # 마진 제거 
  par(mar = c(0, 0, 0, 0))
  
  # 그림 그리기 
  with(data = dat, 
       expr = plot(x = x, 
                   y = y, 
                   type = 'l', 
                   xlim = c(-2, 2), 
                   ylim = c(-2, 2), 
                   xlab = '', 
                   ylab = '', 
                   xaxt = 'n', 
                   yaxt = 'n'))
  
  # 시드 출력
  text(x = 1.5, y = -2, labels = paste('Seed No : ', seed))
}


# 비디오 아트 흉내 : 2초마다 새로운 harmonograph가 그려짐
for (i in 1:1000) {
  harmonograph(seed = i)
  Sys.sleep(time = 2)
}

