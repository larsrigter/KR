%% Quantities
quantity('inflow').
quantity('outflow').
quantity('volume').

%% Possible derivaties
derivative('-').
derivative(0).
derivative('+').

%% Possible quantity spaces
space('inflow', 0).
space('inflow', '+').

space('outflow', 0).
space('outflow', '+').
space('outflow','max').

space('volume', 0).
space('volume', '+').
space('volume', 'max').

%% Explanations
explain('?', 'This value is ambiguous').
explain('-', 'This value is decreasing').
explain('0', 'This value is constant').
explain('+', 'This value is increasing').
explain('max', 'This value is at its maximum').


transition('+')

%% Dependencies
i1('inflow', 'volume').
i0('outflow', 'volume').
p1('volume', 'outflow').
vc('volume', 'outflow', 'max').
vc('volume', 'outflow', 0).


%% General (model independent) rules
is_positively_influencing(X, Y):-
    i1(X, Y).

is_negatively_influencing(X, Y):-
    i0(X, Y).

is_positively_proportional_to(X, Y):-
    p1(X, Y);
    p1(Y, X).

% Not used in our problem
is_negatively_proportional_to(X, Y):- 
    p0(X, Y);
    p0(Y, X).

has_value_correspondence(X, Y, Value):-
    vc(X, Y, Value).

is_increasing(X):-
    is_positively_influencing(Y, X);
    is_positively_proportional_to(Y, X),
    state(Y, '+').

is_decreasing(X):-
    is_negatively_influencing(Y, X);
    is_negatively_proportional_to(Y, X),
    state(Y, '+').

is_decreasing(X)
    is_positively_influencing(Y, X);
    is_positively_proportional_to(Y, X),
    state(Y, '-').

% state(Inflow, Outflow, Volume)?
% state('+', Outflow, Volume)?
% -> possible states.
% state('+', '0', '0')?
% no.
% state('+', '0', '+')?
% yes.

state(InflowMagnitude, OutflowMagnitude, VolumMagnitude):-
    space('inflow', InflowMagnitude),
    derivative(InflowDerivative),
    space('outflow', OutflowMagnitude),
    derivative(OutflowDerivative),
    space('volume', VolumeMagnitude),
    derivative(VolumeDerivative),
    valid_state(InflowMagnitude, InflowDerivative, OutflowMagnitude, 
        OutflowDerivative, VolumeMagnitude, VolumeDerivative).


valid_state(InflowMagnitude, InflowDerivative, OutflowMagnitude, 
    OutflowDerivative, VolumeMagnitude, VolumeDerivative):-  
    check_conflict()





