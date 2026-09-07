"Hello World!"
5 + 5
plot(1:10)

#esto es un comentario
#escrito en mas
#de una linea
print("Hello World!")

#### DATA TYPES ###
my_var <- 30 # my_var is type of numeric
my_var
my_var <- "Sally" # my_var is now of type character (aka string)

### NUMBERS ###
x <- 10.5
y <- 10.5
x+y
x*y

is.character(x)
is.character(y)
is.numeric(x)



### STRING ###
str <- "Hello World!"

nchar(str)

str <- "Hello World!"
grepl("H", str)
grepl("Hello", str)
grepl("X", str)

str1 <- "Hello"
str2 <- "World"

paste(str1, str2)

### BOOLEANS ###
10>9
10<9

1:100

### OPERATORS ###
my_var <- 3

my_var <<- 3

3 -> my_var

3 ->> my_var

my_var

### IF ELSE ###
a <- 33
b <- 33

if (b > a) {
  print("b is greater than a")
} else if (a == b) {
  print ("a and b are equal")
}

a <- 30
b <- 33

if (b > a) {
  print("b is greater than a")
} else if (a == b) {
  print ("a and b are equal")
}

### WHILE LOOP ###
i <- 1
while (i < 6) {
  print(paste("el numero es",i))
  i <- i + 1
  if (i == 4) {
    break
  }
}

### FOR LOOP ###
for (x in 1:10) {
  print(x)
}

rango <- 1:20
for (x in rango) {
  print(x)
}

fruits <- c("banana", "apple", "orange")
fruits

### VECTORS ###
fruits <- c("banana", "apple", "orange")
fruits

numbers <- c(1, 2, 3)
numbers

### LISTS ###
thislist <- list("apple", "banana", "cherry")

thislist[3]
length(thislist)
"apple" %in% thislist

### MATRICES ###
thismatrix <- matrix(c("apple", "banana", "cherry", "orange"), nrow = 2, ncol = 2)

thismatrix
thismatrix[2,]
thismatrix[, c(1,2)]
"apple" %in% thismatrix
dim(thismatrix)

### ARRAYS ###
thisarray <- c(1:24)
multiarray <- array(thisarray, dim = c(4, 3, 2))

multiarray[2, 3, 2]
2 %in% multiarray
length(multiarray)
for(x in multiarray){
  print(x)
}

### DATA FRAMES ###
Data_Frame <- data.frame (
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)

Data_Frame

summary(Data_Frame)
Data_Frame[1]

Data_Frame[["Training"]]

Data_Frame$Training
New_col_DF <- cbind(Data_Frame, Steps = c(1000, 6000, 2000))
New_col_DF
ncol(Data_Frame)
nrow(Data_Frame)

### FACTORES ###
music_genre <- factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"), levels = c("Classic", "Jazz", "Pop", "Rock", "Other"))

levels(music_genre)
length(music_genre)
music_genre[3] <- "Pop"

music_genre[3]