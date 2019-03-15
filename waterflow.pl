%% Quantities
quantity('inflow').
quantity('outflow').
quantity('volume').

%% Possible quantity spaces
space('inflow', 0).
space('inflow', '+').

space('outflow', 0).
space('outflow', '+').
space('outflow','max').

space('volume', 0).
space('volume', '+').
space('volume', 'max').

%% Dependencies
i1('inflow', 'volume').
i0('outflow', 'volume').
p1('volume', 'outflow').
vc('volume', 'outflow', 'max').
vc('volume', 'outflow', 0).

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
    vc(X, Y, Value);
    vc(Y, X, Value).

is_increasing(X):-
    is_positively_influencing(Y, X);
    is_positively_proportional_to(Y, X),
    state(Y, '+').

is_decreasing(X):-
    is_negatively_influencing(Y, X);
    is_negatively_proportional_to(Y, X),
    state(Y, '+').

% state(Inflow, Outflow, Volume)?
% state('+', Outflow, Volume)?
% -> possible states.
% state('+', '0', '0')?
% no.
% state('+', '0', '+')?
% yes.


state(Inflow, Outflow, Volume):-
    space('inflow', Inflow),
    space('outflow', Outflow),
    space('volume', Volume),
    valid_state(Inflow, Outflow, Volume).


valid_state(Inflow, Outflow, Volume):-  
    % checking everything





