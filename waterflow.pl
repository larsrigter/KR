% Quantities
quantity(inflow).
quantity(outflow).
quantity(volume).

% Possible derivaties
derivative_space('-').
derivative_space(0).
derivative_space('+').
derivative_space('?').

decreasing_derivative_previous('-').
decreasing_derivative_previous(0).

increasing_derivative_previous('+').
increasing_derivative_previous(0).

% Possible quantity spaces
quantity_space(inflow, 0).
quantity_space(inflow, '+').

quantity_space(outflow, 0).
quantity_space(outflow, '+').
quantity_space(outflow, max).

quantity_space(volume, 0).
quantity_space(volume, '+').
quantity_space(volume, max).

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

is_transition(X, Y):-
    transition(X, Y);
    transition(Y, X).

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

consistent_magnitude(Object, Subject):-
    Subject = [SubjectName, SubjectMagnitude, _],
    Object = [ObjectName, ObjectMagnitude, _],
    has_value_correspondence(SubjectName, ObjectName),
    ObjectMagnitude == SubjectMagnitude.

consistent_magnitude(Object, Subject):-
    Subject = [SubjectName, _, _],
    Object = [ObjectName, _, _],
    not(has_value_correspondence(SubjectName, ObjectName)).

is_increasing([SubjectName, SubjectMagnitude, SubjectDerivative], [ObjectName, _, _]):-
    is_positively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '+';
    is_negatively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '-';
    is_positively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == '+';
    is_negatively_influencing(SubjectName, ObjectName),
    SubjectMagnitude == '-'.

is_decreasing([SubjectName, SubjectMagnitude, SubjectDerivative], [ObjectName, _, _]):-
    is_positively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '-';
    is_negatively_proportional_to(SubjectName, ObjectName),
    SubjectDerivative == '+';
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
    ObjectDerivative == '-';
    Object = [_, _, ObjectDerivative],
    ObjectDerivative == '?'.

is_steady(Subject, Object):-
    Subject = [SubjectName, SubjectMagnitude, SubjectDerivative],
    quantity_space(SubjectName, SubjectMagnitude),
    derivative_space(SubjectDerivative),
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    quantity_space(ObjectName, ObjectMagnitude),
    derivative_space(ObjectDerivative),
    not(is_decreasing(Subject, Object)),
    not(is_increasing(Subject, Object)),
    not(is_ambiguous(Subject, Object)).



derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, '+']):-
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    increasing_derivative_previous(ObjectDerivative),
    is_increasing(Subject, Object).

derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, '-']):-
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    decreasing_derivative_previous(ObjectDerivative),
    is_decreasing(Subject, Object).

derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, '?']):-
    Object = [ObjectName, ObjectMagnitude, _],
    is_ambiguous(Subject, Object).

derivative_calculus(Subject, Object, [ObjectName, ObjectMagnitude, ObjectDerivative]):-
    Object = [ObjectName, ObjectMagnitude, ObjectDerivative],
    consistent_magnitude(Subject, Object),
    is_steady(Subject, Object).

resolution_single(Subject, [Object], [ResolvedObject]):-
    consistent_magnitude(Subject, Object),
    derivative_calculus(Subject, Object, ResolvedObject), !.

resolution_single(Subject, [Object|Rest], [ResolvedObject|ResolvedObjects]):-
    consistent_magnitude(Subject, Object),
    derivative_calculus(Subject, Object, ResolvedObject),
    resolution_single(Subject, Rest, ResolvedObjects).

resolution([SubjectState], ObjectStates, ResultStates):-
    resolution_single(SubjectState, ObjectStates, ResultStates), !.

resolution(SubjectStates, ObjectStates, ResultStates):-
    SubjectStates = [S|T],
    % S = [SubjectName, SubjectMagnitude, SubjectDerivative],
    % quantity_space(SubjectName, SubjectMagnitude),
    % derivative_space(SubjectDerivative),
    resolution_single(S, ObjectStates, NextStates),
    resolution(T, NextStates, ResultStates).



% state([['inflow', 0, ''+''], ['outflow', M1, D1], ['volume', M2, D2]])

% state([]).
% state(State):-
%     State = [Quantity, Magnitude, Derivative],
%     quantity_space(Quantity, Magnitude),
%     derivative_space(Derivative).

% state(States):-
%     States = [S|T],
%     S = [Quantity, Magnitude, Derivative],
%     quantity_space(Quantity, Magnitude),
%     derivative_space(Derivative),
%     state(T).


pick_states([[InitialName, _, _]], ObjectState):-
    ObjectState = [InitialName, PrevMagnitude, PrevDerivative],
    quantity_space(InitialName, PrevMagnitude),
    derivative_space(PrevDerivative).

pick_states([H|T], [ObjectState|ObjectStates]):-
    H = [InitialName, _, _],
    ObjectState = [InitialName, PrevMagnitude, PrevDerivative],
    quantity_space(InitialName, PrevMagnitude),
    derivative_space(PrevDerivative),
    pick_states(T, ObjectStates).

state(States):-
    % pick_states(States, PickedObjectStates),
    pick_states(States, PickedStates),
    resolution(PickedStates, PickedStates, States).

% state([SubjectState], ObjectStates, ResultStates):-
%     resolution(SubjectState, ObjectStates, ResultStates).

% state(SubjectStates, ObjectStates, ResultStates):-
%     SubjectStates = [S|T],
%     % S = [SubjectName, SubjectMagnitude, SubjectDerivative],
%     % quantity_space(SubjectName, SubjectMagnitude),
%     % derivative_space(SubjectDerivative),
%     resolution(S, ObjectStates, NextStates),
%     state(T, NextStates, ResultStates).





