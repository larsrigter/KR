:-include('qualitative_reasoner.pl').

% Quantities
quantity(inflow).
quantity(outflow).
quantity(volume).
quantity(pressure).
quantity(height).

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
quantity_space(pressure, max).
quantity_space(height, max).

lowest_magnitude(_, 0).
highest_magnitude(inflow, +).
highest_magnitude(outflow, max).
highest_magnitude(volume, max).
highest_magnitude(pressure, max).
highest_magnitude(height, max).

% Transitions
transition('-', 0).
transition(0, '+').
transition('+', max).
transition('+', '+').

% Dependencies
i1(inflow, volume).
i0(outflow, volume).
p1(pressure, outflow).
p1(volume, height).
p1(height, pressure).
p0(something, somethingelse).

vc(Q1, Q2, max):-
    quantity(Q1),
    quantity(Q2),
    Q1 \= inflow,
    Q2 \= inflow,
    Q1 \= Q2.

vc(Q1, Q2, 0):-
    quantity(Q1),
    quantity(Q2),
    Q1 \= inflow,
    Q2 \= inflow,
    Q1 \= Q2.

