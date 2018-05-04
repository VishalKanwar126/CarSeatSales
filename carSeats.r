## we are going to use the Carseats dataset from the ISLR library

library(ISLR) # book's library
library(tree) # to fit decision trees
attach(Carseats) # dataset to use

head(Carseats)


#??????????????????????????????????????????????????????????????????????????????

High = ifelse(Sales>=8,"Yes","No")
Carseats = data.frame(Carseats,High)
Carseats = Carseats[,-1]




head(Carseats)
#??????????????????????????????????????????????????????????????????????????????



### Summarise the dataset
# dimension of dataset :
dim(Carseats)
#list type for each attribute:
sapply(Carseats, class)
#take a peak at the data:
head(Carseats)
#list the levels for the class
levels(Carseats$High)
#statistical summary
summary(Carseats)

###Visualize the dataset:

# split input and output
x <- Carseats[,1:10]
y <- Carseats[,11]


Carseats$ShelveLoc = NULL
Carseats = data.frame(Carseats, ShelveLoc)
Carseats$High = NULL
Carseats = data.frame(Carseats, High)
head(Carseats)

# boxplot for each attribute on one image
par(mar=c(1,1,1,1))
par(mfrow=c(1,10))
for(i in 1:10) {
  boxplot(x[,i], main=names(Carseats)[i])
}




## Fitting Classificatiion tree models


### Start Data Manipulation
range(Sales)# sales range from 0 to 16
#create a categorical variable on sales

#append High to Carseats dataset: Hence we get the final dataset to work upon
#Carseats = data.frame(Carseats,High)


### Splitting data into testing and training using:
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
training_data = Carseats[train,]
testing_data = Carseats[test,]
testing_High = High[test] # to compare the prediction value and actual sales

## fit the tree model using training data:
tree_model = tree(High~., training_data)
plot(tree_model)
text(tree_model, pretty=0) # to give the real values categorical variables

#check how the model is doing using the testing_data
tree_pred = predict(tree_model, testing_data, type="class")
mean(tree_pred != testing_High) # this is used in order to find the error in prediction which is=28.5%
# since the eroor is high therefore in order to reduce it we do pruning:


###prune the tree
##perform cross validation to check where to stop pruning
set.seed(3)
cv_tree = cv.tree(tree_model, FUN= prune.misclass)
names(cv_tree)

#plotting size v/s error (dev) rate:
plot(cv_tree$size,
     cv_tree$dev,
     type='b')
# since the minimum error rate is at tree_size=9 (approx) therefore prune at 9

### prune the tree 
pruned_model = prune.misclass(tree_model, best = 9)
plot(pruned_model)          
text(pruned_model, pretty = 0)

###checking how the model is doing:
tree_pred = predict(pruned_model, testing_data, type = "class")
mean(tree_pred != testing_High)  # Thus the pruned tree becomes better than the earlier tree ;)