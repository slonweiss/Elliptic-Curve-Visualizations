(*Hessian curve parameters*)
dCoeff = 1;

(*Hessian curve parametric functions*)
xHessian[t_] = t;
yHessian[t_] = ((dCoeff*t^2 - 1)^(1/3))/t;

(*Map the Hessian curve onto the torus*)
xMapped[pt_] := xTorus[uMap[pt[[1]]], vMap[pt[[2]]]];
yMapped[pt_] := yTorus[uMap[pt[[1]]], vMap[pt[[2]]]];
zMapped[pt_] := zTorus[uMap[pt[[1]]], vMap[pt[[2]]]];

(*Calculate Hessian curve points mod p*)
hessianCurvePointsModP = {};
For[x = 0, x < p, x++,
  For[y = 0, y < p, y++,
    If[Mod[x^3 + y^3 + 1 - dCoeff*x*y, p] == 0,
      AppendTo[hessianCurvePointsModP, {x, y}];
    ];
  ];
];

(*Map the Hessian curve points onto the torus*)
mappedCurvePoints = Map[Function[pt, {xMapped[pt], yMapped[pt], zMapped[pt]}], hessianCurvePointsModP];

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
    {xMapped[{u, yHessian[u]}], yMapped[{u, yHessian[u]}], zMapped[{u, yHessian[u]}]}, 
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