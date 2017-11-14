function J = costFunction(X, y, theta)

% X is the "design matrix" containing our training exampltes
% y is the class lables

m = size(X, 1); % number of training examples
predictions = X * theta; % predictions of hypothesis on all m exampltes

sqrErrors = (predictions - y).^2; % squared errors

J = 1/(2 * m) * sum(sqrErrors)
