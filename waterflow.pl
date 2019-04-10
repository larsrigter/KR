:-include('qualitative_reasoner.pl').

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

