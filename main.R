library(ggplot2)
library(viridis)

grades <- function(data){ # oceny z pierwszego i drugiego semestru
  grade <- seq(0,20)
  count1 <- rep(0,1+20) #1st semester
  count2 <- rep(0,1+20) #2st semester
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32])){
      count1[data[i,31]] = 1 + count1[data[i,31]]
      count2[data[i,32]] = 1 + count2[data[i,32]]
    }
  }
  d <- data.frame(grade, count1, count2)
  p <- ggplot(data = d) + geom_point(aes(x = grade, y = count1, color = "1. semestr")) + geom_point(aes(x = grade, y = count2, color = "2. semestr"))
  print(p + labs(x = "ocena", y = "iloœæ"))
}

grades_fit_sem1 <- function(data){ # dopasowanie rozk³adu normalnego do ocen z 1. semestru
  grade <- seq(0,20)
  count <- rep(0,1+20)
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31])){
      count[data[i,31]] = 1 + count[data[i,31]]
    }
  }
  df <- c()
  for(i in 1:length(count)){
    for(j in 1:count[i]){
      df <- append(df, i)
    }
  }
  fit <- fitdistr(df, "normal")
  f <- fit$estimate
  p <- ggplot(data = data.frame(df), aes(x = df)) +
    geom_histogram(bins = 21, color = "black", fill = "red", aes(y = stat(count/sum(count)))) +
    stat_function(fun = dnorm, args = list(mean = f[1], sd = f[2]))
  print(p + labs(x = "ocena", y = "iloœæ"))
}

grades_fit_sem2 <- function(data){ # dopasowanie rozk³adu normalnego do ocen z 2. semestru
  grade <- seq(0,20)
  count <- rep(0,1+20)
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,32])){
      count[data[i,32]] = 1 + count[data[i,32]]
    }
  }
  df <- c()
  for(i in 1:length(count)){
    for(j in 1:count[i]){
      df <- append(df, i)
    }
  }
  fit <- fitdistr(df, "normal")
  f <- fit$estimate
  p <- ggplot(data = data.frame(df), aes(x = df)) +
    geom_histogram(bins = 21, color = "black", fill = "red", aes(y = stat(count/sum(count)))) +
    stat_function(fun = dnorm, args = list(mean = f[1], sd = f[2]))
  print(p + labs(x = "ocena", y = "iloœæ"))
}

travel_time <- function(data){ # czas podró¿y do szko³y
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,13])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,13])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "czas podró¿y", y = "ocena"))
}

study_time <- function(data){ # czas nauki i œrednia 2 pierwszych grup
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,14])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,14])
    }
  }
  s1 <- 0
  c1 <- 0
  s2 <- 0
  c2 <- 0
  for(i in 1:length(xs)){
    if(xs[i] == 1){
      c1 <- c1 + 1
      s1 <- s1 + ys[i]
    }
    else if(xs[i] == 2){
      c2 <- c2 + 1
      s2 <- s2 + ys[i]
    }
  }
  m1 <- s1 / c1
  m2 <- s2 / c2
  print(m1)
  print(m2)
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "czas nauki", y = "ocena"))
}

famrel <- function(data){ # jakoœæ rodzinnych relacji
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,24])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,24])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "jakoœæ relacji", y = "ocena"))
}

health <- function(data){ # stan zdrowia
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,29])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,29])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "jakoœæ zdrowia", y = "ocena"))
}

absences <- function(data){ # iloœæ nieobecnoœci
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,30])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,30])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 20) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "iloœæ nieobecnoœci", y = "ocena"))
}

workday_alcohol <- function(data){ # spo¿ycie alkoholu w tygodniu roboczym
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,27])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,27])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "iloœæ alkoholu", y = "ocena"))
}

weekend_alcohol <- function(data){ # spo¿ycie alkoholu w weekend
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,28])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,28])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "iloœæ alkoholu", y = "ocena"))
}

goout <- function(data){ # iloœæ wyjœæ ze znajomymi
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,26])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,26])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "czêstoœæ wyjœæ", y = "ocena"))
}

freetime <- function(data){
  xs <- c()
  ys <- c()
  for(i in 1:nrow(data[1])){
    if(!is.na(data[i,31]) && !is.na(data[i,32]) && !is.na(data[i,25])){
      ys <- append(ys, data[i,32] + data[i,33])
      xs <- append(xs, data[i,25])
    }
  }
  p <- ggplot(data = data.frame(xs, ys), aes(x = xs, y = ys)) +
    geom_bin2d(bins = 30) + scale_fill_continuous(type = "viridis") + theme_bw() +
    geom_smooth(method = "lm", se = FALSE)
  print(p + labs(x = "iloœæ czasu wolnego", y = "ocena"))
}

main <- function(){ # g³ówna funkcja (launcher)
  path = "C:\\Users\\Szymon\\Desktop\\statystyka_projekt\\por_data.csv" # lokalizacja danych (tutaj u mnie)
  data = read.csv(file = path) # parsing
  
  # aby narysowaæ wykres nale¿y poni¿ej odkomentowaæ odpowiedni¹ funkcjê
  
  #grades(data)
  #grades_fit_sem1(data)
  #grades_fit_sem2(data)
  
  #travel_time(data)
  #study_time(data)
  #famrel(data)
  #health(data)
  #absences(data)
  #workday_alcohol(data)
  #weekend_alcohol(data)
  #goout(data)
  #freetime(data)
}

main()