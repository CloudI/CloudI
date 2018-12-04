
-record(point,{
  point_type :: any(),
  x :: float(),
  y :: float(),
  z :: float() | undefined,
  m :: float() | undefined
  }).

-record(multi_point,{
  point_type :: any(),
  points :: [epgsql_ewkb:point(epgsql_ewkb:point_type())]
  }).

-record(line_string,{
  point_type :: any(),
  points :: [epgsql_ewkb:point(epgsql_ewkb:point_type())]
  }).

-record(multi_line_string,{
  point_type :: any(),
  line_strings :: [epgsql_ewkb:line_string(epgsql_ewkb:point_type())]
  }).

-record(circular_string,{
  point_type :: any(),
  points :: [epgsql_ewkb:point(epgsql_ewkb:point_type())]
  }).

-record(compound_curve,{
  point_type :: any(),
  lines :: [epgsql_ewkb:basic_string(epgsql_ewkb:point_type())]
  }).

-record(multi_curve,{
  point_type :: any(),
  curves :: [epgsql_ewkb:curve(epgsql_ewkb:point_type())]
  }).

-record(polygon,{
  point_type :: any(),
  rings :: [epgsql_ewkb:line_string(epgsql_ewkb:point_type())]
  }).

-record(multi_polygon,{
  point_type :: any(),
  polygons :: [epgsql_ewkb:polygon(epgsql_ewkb:point_type())]
  }).

-record(triangle,{
  point_type :: any(),
  rings :: [epgsql_ewkb:line_string(epgsql_ewkb:point_type())]
  }).

-record(curve_polygon,{
  point_type :: any(),
  rings :: [epgsql_ewkb:curve(epgsql_ewkb:point_type())]
  }).

-record(polyhedral_surface,{
  point_type :: any(),
  polygons :: [epgsql_ewkb:polygon(epgsql_ewkb:point_type())]
  }).

-record(multi_surface,{
  point_type :: any(),
  surfaces :: [epgsql_ewkb:surface(epgsql_ewkb:point_type())]
  }).

-record(tin,{
  point_type :: any(),
  triangles :: [epgsql_ewkb:triangle(epgsql_ewkb:point_type())]
  }).
