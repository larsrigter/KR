%% Quantities
quantity('inflow').
quantity('outflow').
quantity('volume').

%% Possible derivaties
derivative_space('-').
derivative_space(0).
derivative_space('+').
derivative_space('?').

%% Possible quantity spaces
quantity_space('inflow', 0).
quantity_space('inflow', '+').

quantity_space('outflow', 0).
quantity_space('outflow', '+').
quantity_space('outflow','max').

quantity_space('volume', 0).
quantity_space('volume', '+').
quantity_space('volume', 'max').

%% Explanations
explain('?', 'This value is ambiguous').
explain('-', 'This value is decreasing').
explain('0', 'This value is constant').
explain('+', 'This value is increasing').
explain('max', 'This value is at its maximum').


transition('+').

%% Dependencies
i1('inflow', 'volume').
i0('outflow', 'volume').
p1('volume', 'outflow').
p0('something', 'somethingelse').
vc('volume', 'outflow', 'max').
vc('volume', 'outflow', 0).


%% General (model independent) rules
is_positively_influencing(X, Y):-
    i1(X, Y).

is_negatively_influencing(X, Y):-
    i0(X, Y).

is_positively_proportional_to(X, Y):-
    p1(X, Y).

% Not used in our problem
is_negatively_proportional_to(X, Y):- 
    p0(X, Y).

is_steady(X, Y, Magnitude):-
    vc(X, Y, Magnitude);
    vc(Y, X, Magnitude).

increasing_derivative('+').
increasing_derivative(0).
increasing_derivative('unknown').
is_increasing(Y, X, SubjectMagnitude, SubjectDerivative, ObjectDerivative):-
    is_positively_proportional_to(Y, X),
    SubjectDerivative == '+',
    increasing_derivative(ObjectDerivative);
    is_negatively_proportional_to(Y, X),
    SubjectDerivative == '-',
    increasing_derivative(ObjectDerivative);
    is_positively_influencing(Y, X),
    SubjectMagnitude == '+',
    increasing_derivative(ObjectDerivative);
    is_negatively_influencing(Y, X),
    SubjectMagnitude == '-',
    increasing_derivative(ObjectDerivative).

decreasing_derivative('-').
decreasing_derivative(0).
decreasing_derivative('unknown').
is_decreasing(Y, X, SubjectMagnitude, SubjectDerivative, ObjectDerivative):-
    is_positively_proportional_to(Y, X),
    SubjectDerivative == '-',
    decreasing_derivative(ObjectDerivative);
    is_negatively_proportional_to(Y, X),
    SubjectDerivative == '+',
    decreasing_derivative(ObjectDerivative);
    is_positively_influencing(Y, X),
    SubjectMagnitude == '-',
    decreasing_derivative(ObjectDerivative);
    is_negatively_influencing(Y, X),
    SubjectMagnitude == '+',
    decreasing_derivative(ObjectDerivative).

is_ambiguous(Y, X, SubjectMagnitude, SubjectDerivative, ObjectDerivative):-
    is_increasing(Y, X, SubjectMagnitude, SubjectDerivative, ObjectDerivative),
    ObjectDerivative == '-';
    is_decreasing(Y, X, SubjectMagnitude, SubjectDerivative, ObjectDerivative),
    ObjectDerivative == '+'.


% state(Inflow, Outflow, Volume)?
% state('+', Outflow, Volume)?
% -> possible states.
% state('+', '0', '0')?
% no.
% state('+', '0', '+')?
% yes.


calculus(Subject, Object, '+'):-
    Subject = [SubjectName, SubjectMagnitude, SubjectDerivative],
    quantity_space(SubjectName, SubjectMagnitude),
    derivative_space(SubjectDerivative),
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    quantity_space(ObjectName, ObjectMagnitude),
    derivative_space(ObjectDerivative),
    is_increasing(SubjectName, ObjectName, SubjectMagnitude, SubjectDerivative, ObjectDerivative).

calculus(Subject, Object, '-'):-
    Subject = [SubjectName, SubjectMagnitude, SubjectDerivative],
    quantity_space(SubjectName, SubjectMagnitude),
    derivative_space(SubjectDerivative),
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    quantity_space(ObjectName, ObjectMagnitude),
    derivative_space(ObjectDerivative),
    is_decreasing(SubjectName, ObjectName, SubjectMagnitude, SubjectDerivative, ObjectDerivative).

calculus(Subject, Object, '?'):-
    Subject = [SubjectName, SubjectMagnitude, SubjectDerivative],
    quantity_space(SubjectName, SubjectMagnitude),
    derivative_space(SubjectDerivative),
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    quantity_space(ObjectName, ObjectMagnitude),
    derivative_space(ObjectDerivative),
    is_ambiguous(SubjectName, ObjectName, SubjectMagnitude, SubjectDerivative, ObjectDerivative).

calculus(Subject, Object, 'unknown'):-
    Subject = [SubjectName, SubjectMagnitude, SubjectDerivative],
    quantity_space(SubjectName, SubjectMagnitude),
    derivative_space(SubjectDerivative),
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    quantity_space(ObjectName, ObjectMagnitude),
    derivative_space(ObjectDerivative),
    not(is_increasing(SubjectName, ObjectName, SubjectMagnitude, SubjectDerivative, ObjectDerivative)),
    not(is_decreasing(SubjectName, ObjectName, SubjectMagnitude, SubjectDerivative, ObjectDerivative)),
    not(is_ambiguous(SubjectName, ObjectName, SubjectMagnitude, SubjectDerivative, ObjectDerivative)).


resolution(_, [], []).
resolution(Subject, [Object|Rest], [ResolvedObject|ResolvedObjects]):-
    ResolvedObject = [_, _, ObjectDerivative],
    calculus(Subject, Object, ObjectDerivative),
    resolution(Subject, Rest, ResolvedObjects).

state(QuantityStates, ResultStates):-
    state(QuantityStates, QuantityStates, ResultStates).

state([QuantityState], States, ResultStates):-
    QuantityState = [Quantity, Magnitude, Derivative],
    quantity_space(Quantity, Magnitude),
    derivative_space(Derivative),
    resolution(QuantityState, States, ResultStates).

state(QuantityStates, States, ResultStates):-
    QuantityStates = [S|T],
    S = [Quantity, Magnitude, Derivative],
    quantity_space(Quantity, Magnitude),
    derivative_space(Derivative),
    resolution(S, States, NextStates),
    state(T, NextStates, ResultStates).





