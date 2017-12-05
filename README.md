# Machine Learning:

- Field of study that gives computers the ability to learn
 without being explicitly being programmed

- He says, a computer program is said to learn from experience E,
 with respect to some task T, and some performance measure P, if
 its performance on T as measured by P improves with experience E.

## Machine learning algorithms:
- Supervised learning
- Unsupervised learning
- Reinforcement learning
- Recommender systems

## Supervised Learning:
"Right Answers" given

Regression
- Predict continuous valued output
- Trying to map input variables to some continuous function
- Given a picture of a person, we have to predict their age on the basis of the given picture

Classification
- Discrete value output
- Trying to map input variables into discrete categories
- Given a patient with a tumor, we have to predict whether the tumor is malignant or benign.

## Unsupervised Learning:
Not given the "Right answer"

- Clustering
- Cocktail party problem

Octave (Matlab)

Cocktail party algorithm
```octave
[W,s,v] = svd((repmat(sum(x. *x,1),size(x,1),1). *x)*x');
```
svd - single value decomposition

## Model and cost function

Notation
- m = number of training examples
- x = input variable (feature)
- y = output variable (target)
- (x,y) = one training example
- (x^i,y^i) = i'th training example
- h = hypothesis function (output from learning algorithm with training set)
 h maps from x's to y's

h(x) = d + e(x) (d and e are variables like theta 0 and theta 1)
this is really just a different way to do slope intercept form
y = m(x) + b
## Cost function
1/2m (sum from 1 to m) (h(x^i) - y^i)^2
squared error cost function

## Linear Algebra Review

### Matricies
Dimension of matrix: number of rows x number of columns
```
[
  [1,2],
  [2,3],
  [3,2]
]
```
this matrix would be a 3x2 matrix.

Matrix elements: i,j in the i'th row and the j'th column
First number is row and second is columns

### Vector
Is just a matrix with 1 column.
```
[
  [1],
  [2],
  [3],
  [4]
]
```

## Multiple Features

More notation:
- n = number of Features
- x^i = input (features) of i'th training example
- x^ij = value of feature j in i'th training example

Feature scaling is good for when working with gradient decent and not needed for the normal equation.

Gradient Descent
- Need to choose learning rate
- Needs many iterations
- Works well when n (features) is large

Normal equation
- No need to choose learning rate
- Don't need to iterate
- Need to compute inverse of matrix (On^3)
- Slow if n (features) is large
- if n < 10k is slow, would use gradient descentt
