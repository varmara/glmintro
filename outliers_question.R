y_mix <- c(runif(5, 100, 115), runif(5, 110, 125), runif(5, 120, 135))

y_mix1 <- c(y_mix, 42)
y_mix2 <- c(y_mix, 155)
y_mix3 <- c(46, y_mix)
y_mix4 <- c(149, y_mix)


x_wo <- seq(1,15,1)
x_12 <- c(x_wo, 21)
x_34 <- c(0, x_wo)


frame_wo <- data.frame(y_mix, x_wo)
frame_w <- data.frame(y_mix1, y_mix2, y_mix3, y_mix4, x_12, x_34) 

library(ggplot2)

gg_wo <- ggplot(frame_wo, aes(x = x_wo)) + geom_point() + aes(y = y_mix) + scale_y_continuous(limits=c(40,180)) + scale_x_continuous(limits=c(-1,21))

gg_w1 <- ggplot(frame_w, aes(x = x_12)) + geom_point() + aes(y = y_mix1) + scale_y_continuous(limits=c(40,180)) + scale_x_continuous(limits=c(-1,21))

gg_w2 <- ggplot(frame_w, aes(x = x_12)) + geom_point() + aes(y = y_mix2) + scale_y_continuous(limits=c(40,180)) + scale_x_continuous(limits=c(-1,21))

gg_w3 <- ggplot(frame_w, aes(x = x_34)) + geom_point() + aes(y = y_mix3) + scale_y_continuous(limits=c(40,180)) + scale_x_continuous(limits=c(-1,21))

gg_w4 <- ggplot(frame_w, aes(x = x_34)) + geom_point() + aes(y = y_mix4) + scale_y_continuous(limits=c(40,180)) + scale_x_continuous(limits=c(-1,21))

library(gridExtra)
grid.arrange(gg_w1, gg_w2, gg_w3, gg_w4, ncol=2)

