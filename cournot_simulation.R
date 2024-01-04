library(tidyverse)

#parameters
c_1 <- 1
c_2 <- 2
c_3 <- 3

result_df <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(result_df) = c("a", "q_1", "q_2", "q_3")

for (a in 1:100) {
  A <- matrix(c(2 * c_1 + 2, 1, 1, 1, 2 * c_2 + 2, 1, 1, 1, 2 * c_3 + 2), 3, 3)
  B <- matrix(c(a, a, a), 3, 1)
  s <- solve(A, B)
  result_df[nrow(result_df) + 1, ] = c(a, s[1], s[2], s[3])
}



result_df2 <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(result_df2) = c("a", "qa_1", "qa_2")

for (a in 1:100) {
  A <- matrix(c(2 * c_1 + 2, 1, 1, 2 * c_2 + 2), 2, 2)
  B <- matrix(c(a, a), 2, 1)
  s <- solve(A, B)
  result_df2[nrow(result_df2) + 1, ] = c(a, s[1], s[2])
}

result_df2

result_ba_df <- result_df %>% 
  left_join(result_df2, by = "a") %>% 
  mutate(
    diff1 = qa_1 - q_1,
    diff2 = qa_2 - q_2
  )

ggplot(data = result_ba_df) +
  geom_line(aes(x = a, y = diff1), color = "red") +
  geom_line(aes(x = a, y = diff2), color = "blue") 

ggplot(data = result_df) +
  geom_line(aes(x = a, y = q_1)) +
  geom_line(aes(x = a, y = q_2)) +
  geom_line(aes(x = a, y = q_3))


####Difference####
a <- 10
m <- 1

result_df <- data.frame(matrix(nrow = 0, ncol = 9))

colnames(result_df) = c("a", "k", "l", "m", "q_1b", "q_2b", "q_3b", "q_1a", "q_2a")


for (k in 0:10) {
  for (l in 0:10) {
    q_1_after <- (2*a*l + a)/(4*k*l + 4*k + 4*l + 3)
    q_1_before <- (4*a*l*m + 2*a*l + 2*a*m + a)/(8*k*l*m + 8*k*l + 8*k*m + 6*k + 8*l*m + 6*l + 6*m + 4)
    
    q_2_after <- (2*a*k + a)/(4*k*l + 4*k + 4*l + 3)
    q_2_before <- (4*a*k*m + 2*a*k + 2*a*m + a)/(8*k*l*m + 8*k*l + 8*k*m + 6*k + 8*l*m + 6*l + 6*m + 4)
    
    q_3_before <- (4*a*k*l + 2*a*k + 2*a*l + a)/(8*k*l*m + 8*k*l + 8*k*m + 6*k + 8*l*m + 6*l + 6*m + 4)
    
    result_df[nrow(result_df) + 1, ] = c(a, k, l, m, q_1_before, q_2_before, q_3_before, q_1_after, q_2_after)
  }
}

result_df <- result_df %>% 
  mutate(
    q_1_diff = q_1a - q_1b,
    q_2_diff = q_2a - q_2b,
    diff_in_diffs = q_2_diff - q_1_diff,
    diff_relation = case_when(
      diff_in_diffs > 0 ~ "Diff1 < Diff2",
      diff_in_diffs == 0 ~ "Diff1 = Diff 2",
      diff_in_diffs < 0 ~ "Diff1 > Diff2"
    )
  )

ggplot(data = result_df, aes(x = k, y = l, color = diff_in_diffs, shape = diff_relation)) +
  geom_point(size = 2) +
  xlim(0, 10) +
  ylim(0, 10) +
  scale_color_gradient(low = "green", high = "red", na.value = NA) +
  labs(x = "c_1", y = "c_2", title = "Effect of One Firm Exit on the Other Firms' Supply") +
  theme_minimal()


p <- ggplot(data = result_df, aes(x = k, y = l, color = diff_in_diffs, shape = diff_relation)) +
  geom_point(size = 2) +
  xlim(0, 10) +
  ylim(0, 10) +
  scale_color_gradient(low = "green", high = "red", na.value = NA) +
  labs(x = "c_1", y = "c_2", title = "Effect of One Firm's Exit on the Other Firms' Supply") +
  theme_minimal()

ggsave(filename = "fig/cournot/trade_cost_exit.png", width = 10, height = 8, dpi = 150,  plot = p)


