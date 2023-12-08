(*Montgomery curve parameters*)
ACoeff = 1;
BCoeff = 1;

(*Montgomery curve parametric functions*)
xMontgomery[t_] = t;
yMontgomery[t_] = Sqrt[(t^3 + ACoeff*t^2 + t)/BCoeff];

(*Map the Montgomery curve onto the torus*)
xMapped[pt_] := xTorus[uMap[pt[[1]]], vMap[pt[[2]]]];
yMapped[pt_] := yTorus[uMap[pt[[1]]], vMap[pt[[2]]]];
zMapped[pt_] := zTorus[uMap[pt[[1]]], vMap[pt[[2]]]];

(*Calculate Montgomery curve points mod p*)
montgomeryCurvePointsModP = {};
For[x = 0, x < p, x++,
  For[y = 0, y < p, y++,
    If[Mod[BCoeff*y^2 - x^3 - ACoeff*x^2 - x, p] == 0,
      AppendTo[montgomeryCurvePointsModP, {x, y}];
    ];
  ];
];

(*Map the Montgomery curve points onto the torus*)
mappedCurvePoints = Map[Function[pt, {xMapped[pt], yMapped[pt], zMapped[pt]}], montgomeryCurvePointsModP];

(*Plotting*)
Show[
  ParametricPlot3D[
    {xTorus[u, v], yTorus[u, v], zTorus[u, v]}, 
    {u, 0, 2 Pi}, 
    {v, -Pi, Pi}, 
    PlotStyle -> Directive[LightBlue, Opacity[0.2]], 
    Mesh -> None
  ], 
  Graphics3D[
    {
      Directive[Black], 
      PointSize[Small], 
      lines, 
      Blue, 
      PointSize[Large], 
      Point[mappedCurvePoints]
    }, 
    ImageSize -> Large
  ], 
  ParametricPlot3D[
    {xMapped[{u, yMontgomery[u]}], yMapped[{u, yMontgomery[u]}], zMapped[{u, yMontgomery[u]}]}, 
    {u, 0, p - 1}, 
    PlotStyle -> Directive[Blue, Thickness[0.003]]
  ],
    (*Line at x = 0*)
  ParametricPlot3D[
    {xTorus[Pi, v], yTorus[Pi, v], zTorus[Pi, v]}, 
    {v, -Pi, Pi}, 
    PlotStyle -> Directive[Purple, Opacity[0.5], Thickness[0.003]]
  ],
  (*Line at y = 0*)
  ParametricPlot3D[
    {xTorus[u, 0], yTorus[u, 0], zTorus[u, 0]}, 
    {u, 0, 2 Pi}, 
    PlotStyle -> Directive[Red, Opacity[0.5], Thickness[0.003]]
  ],
  Boxed -> False, 
  Axes -> False
]