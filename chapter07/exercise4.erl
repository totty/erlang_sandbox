-module(exercise4).
-export([perimeter/1, area/1]).
-include("exercise4.hrl").

perimeter(Draw) when is_record(Draw, circle) ->
    2 * math:pi() * Draw#circle.radius;
perimeter(Draw) when is_record(Draw, rectangle) ->
    2 * (Draw#rectangle.length + Draw#rectangle.width);
perimeter(Draw) ->
    {error, Draw}.

area(Draw) when is_record(Draw, circle) ->
    math:pow(Draw#circle.radius, 2) * math:pi();
area(Draw) when is_record(Draw, rectangle) ->
    Draw#rectangle.length * Draw#rectangle.width;
area(Draw) ->
    {error, Draw}.




