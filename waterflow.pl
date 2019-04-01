% Quantities
quantity(inflow).
quantity(outflow).
quantity(volume).

exogenous(inflow).

% Possible derivatives
derivative_space(inflow, '-').
derivative_space(_, 0).
derivative_space(inflow, '+').

decreasing_derivative_previous('-').
decreasing_derivative_previous(0).

increasing_derivative_previous('+').
increasing_derivative_previous(0).

% Possible quantity spaces
quantity_space(_, 0).
quantity_space(_, '+').
quantity_space(volume, max).
quantity_space(outflow, max).

lowest_magnitude(_, 0).
highest_magnitude(inflow, +).
highest_magnitude(outflow, max).
highest_magnitude(volume, max).

% Explanations
explain('?', 'This value is ambiguous').
explain('-', 'This value is decreasing').
explain(0, 'This value is constant').
explain('+', 'This value is increasing').
explain(max, 'This value is at its maximum').

% Transitions
transition('-', 0).
transition(0, '+').
transition('+', max).
transition('+', '+').

% Dependencies
i1(inflow, volume).
i0(outflow, volume).
p1(volume, outflow).
p0(something, somethingelse).
vc(volume, outflow, max).
vc(volume, outflow, 0).

% General (model independent) rules
is_positively_influencing(X, Y):-
    i1(X, Y).

is_negatively_influencing(X, Y):-
    i0(X, Y).

is_positively_proportional_to(X, Y):-
    p1(X, Y).

% Not used in our problem
is_negatively_proportional_to(X, Y):-
    p0(X, Y).

has_value_correspondence(X, Y):-
    vc(X, Y, _);
    vc(Y, X, _).

is_magnitude_transition(X, X, _):-
    transition(X, X).

is_magnitude_transition(X, X, 0).

is_magnitude_transition(X, Y, Derivative):-
    transition(X, Y),
    Derivative == '+';
    transition(Y, X),
    Derivative == '-'.

is_derivative_transition(Name, _, _, _):-
    not(exogenous(Name)), !.

is_derivative_transition(_, _, X, X):- !.

is_derivative_transition(Name, NextMagnitude, _, 0):-
    highest_magnitude(Name, NextMagnitude), !;
    lowest_magnitude(Name, NextMagnitude).

is_derivative_transition(Name, _, X, Y):-
    exogenous(Name),
    transition(X, Y), !;
    exogenous(Name),
    transition(Y, X).

consistent_magnitude(Object, Subject):-
    Subject = [SubjectName, SubjectMagnitude, _],
    Object = [ObjectName, ObjectMagnitude, _],
    has_value_correspondence(SubjectName, ObjectName),
    ObjectMagnitude == SubjectMagnitude.

consistent_magnitude(Object, Subject):-
    Subject = [SubjectName, _, _],
    Object = [ObjectName, _, _],
    not(has_value_correspondence(SubjectName, ObjectName)).

inconsistent_derivative([Name, Magnitude, Derivative]):-
    Derivative == '-',
    lowest_magnitude(Name, Magnitude), !;
    Derivative == '+',
    highest_magnitude(Name, Magnitude).

is_increasing([SubjectName, SubjectMagnitude, SubjectDerivative], [ObjectName, _, _]):-
    is_positively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '+';
    is_negatively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '-';
    is_positively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == 'max';
    is_positively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == '+';
    is_negatively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == '-'.

is_decreasing([SubjectName, SubjectMagnitude, SubjectDerivative], [ObjectName, _, _]):-
    is_positively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '-';
    is_negatively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '+';
    is_negatively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == 'max';
    is_positively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == '-';
    is_negatively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == '+'.

is_ambiguous(Subject, Object):-
    Subject = [_, _, _],
    Object = [_, _, ObjectDerivative],
    is_decreasing(Subject, Object),
    ObjectDerivative == '+';
    Subject = [_, _, _],
    Object = [_, _, ObjectDerivative],
    is_increasing(Subject, Object),
    ObjectDerivative == '-'.

is_steady(Subject, Object):-
    not(is_decreasing(Subject, Object)),
    not(is_increasing(Subject, Object)),
    not(is_ambiguous(Subject, Object)).

inequality_resolution([_, _, _], [ObjectName, ObjectMagnitude, ObjectDerivative], '-'):-
    ObjectDerivative == '+',
    not(inconsistent_derivative([ObjectName, ObjectMagnitude, '-'])).
    % write("We assume the following quantity is more influential: "),
    % write(Quantity).

inequality_resolution([_, _, _], [ObjectName, ObjectMagnitude, ObjectDerivative], '-'):-
    ObjectDerivative == '-',
    not(inconsistent_derivative([ObjectName, ObjectMagnitude, '-'])).
    % write("We assume the following quantity is less influential: "),
    % write(Quantity).

inequality_resolution(_, _, 0).
    % write("We assume this quantity is just as influental as the other and they cancel each other out.").

inequality_resolution([_, _, _], [ObjectName, ObjectMagnitude, ObjectDerivative], '+'):-
    ObjectDerivative == '+',
    not(inconsistent_derivative([ObjectName, ObjectMagnitude, '+'])).
    % write("We assume the following quantity is less influential: "),
    % write(Quantity).

inequality_resolution([_, _, _], [ObjectName, ObjectMagnitude, ObjectDerivative], '+'):-
    ObjectDerivative == '-',
    not(inconsistent_derivative([ObjectName, ObjectMagnitude, '+'])).
    % write("We assume the following quantity is more influential: "),
    % write(Quantity).

derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, '+']):-
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    increasing_derivative_previous(ObjectDerivative),
    is_increasing(Subject, Object).

derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, '-']):-
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    decreasing_derivative_previous(ObjectDerivative),
    is_decreasing(Subject, Object).

derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, Inequality]):-
    Object = [ObjectName, ObjectMagnitude, _],
    is_ambiguous(Subject, Object), !,
    inequality_resolution(Subject, Object, Inequality).

derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, ObjectDerivative]):-
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    consistent_magnitude(Subject, Object),
    is_steady(Subject, Object), !.

resolution_single(_, [], []):- !.
resolution_single(Subject, [Object|Rest], [ResolvedObject|ResolvedObjects]):-
    consistent_magnitude(Subject, Object), !,
    derivative_calculus(Subject, Object, ResolvedObject),
    resolution_single(Subject, Rest, ResolvedObjects).

resolution([], State, State):- !.
resolution(SubjectState, ObjectState, ResultState):-
    SubjectState = [Q|T],
    resolution_single(Q, ObjectState, NextState),
    length(T, TailLength),
    length(ObjectState, StateLength),
    Pop = StateLength-TailLength,
    split(NextState, Pop, _, L2),
    resolution(L2, NextState, ResultState).

% ASSUMPTION: Derivative cannot be negative if magnitude is at lowest point!
% ASSUMPTION: Derivative cannot be positive if magnitude is at highest point!

% Pick a state according to the possible values each quantity can take on
pick_state([], []):- !.
pick_state([Quantity|T], [Quantity|PickedQuantities]):-
    Quantity = [Name, Magnitude, Derivative],
    quantity_space(Name, Magnitude),
    derivative_space(Name, Derivative),
    not(inconsistent_derivative(Quantity)),
    pick_state(T, PickedQuantities).

% Pick a state preserving the order of names to reduce total combinations
pick_name_state([], []):- !.
pick_name_state([[Name, _, _]|Quantities], [[Name, _, _]|NextQuantities]):-
    pick_name_state(Quantities, NextQuantities).

% Pick a state which complies to the transition rules
transition_state(State, TransitionState):-
    pick_name_state(State, PreservedNames),
    pick_state(PreservedNames, TransitionState),
    valid_transition(State, TransitionState).

next_states(State, NextStates):-
    setof(NextState, next_state(State, NextState), NextStates).

% Pick a next transition state subject to the transition rules
next_state(State, NextState):-
    transition_state(State, TransitionState),
    resolution(TransitionState, TransitionState, NextState).

% Check for each quantity if its next values are legit
valid_transition([], []):- !.
valid_transition([Quantity|Quantities], [NextQuantity|NextQuantities]):-
    Quantity = [Name, Magnitude, Derivative],
    NextQuantity = [_, NextMagnitude, NextDerivative],
    is_magnitude_transition(Magnitude, NextMagnitude, Derivative), !,
    is_derivative_transition(Name, NextMagnitude, Derivative, NextDerivative), !,
    valid_transition(Quantities, NextQuantities).

% Print function: print a nested list in a readable manner
print([]).
print([H|T]):-
    write(H),
    nl,
    print(T).

% Split a list into two sublist given the length of the first list
split(L, 0, [], L).
split([H|T], N, [H|X], L2) :-
        N > 0,
        M is N - 1,
        split(T, M, X, L2).

