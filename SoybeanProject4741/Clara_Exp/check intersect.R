data<- IntersectVarieties
data
str(data)
count = 1
check = data.frame(matrix(NA, nrow=15485, ncol=20))
str(check)

for (i in 1:5) {
  for (j in 1:5) {
   if (i!=j)
     {
      check[,count] <- intersect(data[,i], data[,j])
      count = count + 1
   }
  }
}

str(check)


write.csv(check, file = "CheckVarieties.csv")

########

data2 <- IntersectFamilies
data2
str(data2)


count2 = 1
check2 = data.frame(matrix(NA, nrow=1953, ncol=20))
str(check2)

for (i in 1:5) {
  for (j in 1:5) {
    if (i!=j)
    {
      temp <- data.frame(intersect(data2[,i], data2[,j]))
      temp2<-intersect(data2[,i], data2[,j])
        x <- nrow(temp)
        for (k in 1:x) {
        check2[k, count2] = temp2[k]  
        }
      
      count2 = count2 + 1
    }
  }
}
count2

check2
write.csv(check2, file = "CheckFamilies.csv")

