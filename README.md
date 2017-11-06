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
