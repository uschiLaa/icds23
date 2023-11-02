library(ggpubr)
library(tidyverse)
library(mulgar)
pal <- RColorBrewer::brewer.pal(6, "Dark2")
col <- pal[as.numeric(sketches_train$word)]
set.seed(12062020)
sketch1 <- sketches_train %>%
  group_by(word) %>%
  sample_n(5) %>%
  ungroup() %>%
  add_column(n = rep(1:5, 6)) %>%
  pivot_longer(cols = contains("V"), names_to = "pixel", values_to = "grey") %>%
  mutate(pixel = as.numeric(sub("V", "", pixel))) %>%
  mutate(x=(pixel-1)%%28+1, y = -(floor((pixel-1)/28)+1))

lvls <- levels(sketches_train$word)


p11 <- ggplot(filter(sketch1, word==lvls[1], n==1), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[1])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p12 <- ggplot(filter(sketch1, word==lvls[1], n==2), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[1])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p13 <- ggplot(filter(sketch1, word==lvls[1], n==3), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[1])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p14 <- ggplot(filter(sketch1, word==lvls[1], n==4), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[1])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p15 <- ggplot(filter(sketch1, word==lvls[1], n==5), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[1])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p21 <- ggplot(filter(sketch1, word==lvls[2], n==1), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[2])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p22 <- ggplot(filter(sketch1, word==lvls[2], n==2), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[2])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p23 <- ggplot(filter(sketch1, word==lvls[2], n==3), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[2])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p24 <- ggplot(filter(sketch1, word==lvls[2], n==4), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[2])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p25 <- ggplot(filter(sketch1, word==lvls[2], n==5), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[2])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")

p31 <- ggplot(filter(sketch1, word==lvls[3], n==1), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[3])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p32 <- ggplot(filter(sketch1, word==lvls[3], n==2), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[3])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p33 <- ggplot(filter(sketch1, word==lvls[3], n==3), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[3])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p34 <- ggplot(filter(sketch1, word==lvls[3], n==4), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[3])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p35 <- ggplot(filter(sketch1, word==lvls[3], n==5), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[3])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")

p41 <- ggplot(filter(sketch1, word==lvls[4], n==1), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[4])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p42 <- ggplot(filter(sketch1, word==lvls[4], n==2), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[4])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p43 <- ggplot(filter(sketch1, word==lvls[4], n==3), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[4])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p44 <- ggplot(filter(sketch1, word==lvls[4], n==4), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[4])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p45 <- ggplot(filter(sketch1, word==lvls[4], n==5), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[4])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")

p51 <- ggplot(filter(sketch1, word==lvls[5], n==1), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[5])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p52 <- ggplot(filter(sketch1, word==lvls[5], n==2), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[5])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p53 <- ggplot(filter(sketch1, word==lvls[5], n==3), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[5])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p54 <- ggplot(filter(sketch1, word==lvls[5], n==4), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[5])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p55 <- ggplot(filter(sketch1, word==lvls[5], n==5), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[5])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")

p61 <- ggplot(filter(sketch1, word==lvls[6], n==1), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[6])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p62 <- ggplot(filter(sketch1, word==lvls[6], n==2), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[6])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p63 <- ggplot(filter(sketch1, word==lvls[6], n==3), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[6])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p64 <- ggplot(filter(sketch1, word==lvls[6], n==4), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[6])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
p65 <- ggplot(filter(sketch1, word==lvls[6], n==5), aes(x=x, y=y, fill=grey)) + geom_tile() +
  scale_fill_gradient(low="white", high = pal[6])+
  theme_void() +
  theme(aspect.ratio=1, legend.position="none")
t1 <- text_grob(lvls[1], face="bold", size=15, color=pal[1])
t2 <- text_grob(lvls[2], face="bold", size=15, color=pal[2])
t3 <- text_grob(lvls[3], face="bold", size=15, color=pal[3])
t4 <- text_grob(lvls[4], face="bold", size=15, color=pal[4])
t5 <- text_grob(lvls[5], face="bold", size=15, color=pal[5])
t6 <- text_grob(lvls[6], face="bold", size=15, color=pal[6])


gridExtra::grid.arrange(t1, t2, t3, t4, t5, t6,
                        p11, p21, p31, p41, p51, p61,
                        p12, p22, p32, p42, p52, p62,
                        p13, p23, p33, p43, p53, p63,
                        p14, p24, p34, p44, p54, p64,
                        p15, p25, p35, p45, p55, p65,
                        ncol=6, heights=c(0.2,rep(1, 5)))



