function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices.
%
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);

% You need to return the following variables correctly
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m
%
% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the
%               first time.
%
% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%

A1 = addOnes(X);

Z2 = A1 * Theta1';
A2 = addOnes(sigmoid(Z2));

Z3 = A2 * Theta2';
A3 = sigmoid(Z3);

yEncode = encode(y, num_labels);

inSum = -yEncode .* log(A3) - ((1 - yEncode) .* log(1 - A3));

JNoReg = (1 / m) * sum(sum(inSum));

Theta1NoFirst = Theta1(:,2:end);
Theta2NoFirst = Theta2(:,2:end);

J = JNoReg + (lambda / (2 * m) * (sum(sum(Theta1NoFirst .^ 2)) + sum(sum(Theta2NoFirst .^ 2))));


delta3 = A3 - yEncode;

delta2 = (delta3 * Theta2NoFirst) .* sigmoidGradient(Z2);

Delta2 = delta3' * A2;
Delta1 = delta2' * A1;

tmp1 = (lambda / m * Theta1);
tmp2 = (lambda / m * Theta2);
tmp1(:,1) = zeros(size(Theta1,1),1);
tmp2(:,1) = zeros(size(Theta2,1),1);


Theta1_grad = (Delta1 / m) + tmp1;
Theta2_grad = (Delta2 / m) + tmp2;

% -------------------------------------------------------------

% =========================================================================

% Unroll gradients
grad = [Theta1_grad(:) ; Theta2_grad(:)];

end

function out = addOnes(X)
  out = [ones(size(X,1),1), X];
end

function out = encode(y, labels)
  out = [1:labels] == y;
end

function out = decode(y)
  [_,out] = max(y, [], 2);
end

function o = printSize(A)
  [row, col] = size(A);
  disp(["Size of " inputname(1) " is " num2str(row) "x" num2str(col)]);
end
