(*Define radii of torus*)c = 4;
a = 2;

(*Torus parametric functions*)
xTorus[u_, v_] = (c + a Cos[v]) Cos[u];
yTorus[u_, v_] = (c + a Cos[v]) Sin[u];
zTorus[u_, v_] = a Sin[v];

(*Edwards curve parameters*)
dCoeff = 0.5;

(*Edwards curve parametric functions*)
xElliptic[t_] = Cos[t]/(1 + dCoeff*Sin[t]^2);
yElliptic[t_] = (Cos[t]*Sin[t])/(1 + dCoeff*Sin[t]^2);

(*Map the Edwards curve onto the torus*)
xMapped[pt_] := xTorus[uMap[pt[[1]]], vMap[pt[[2]]]];
yMapped[pt_] := yTorus[uMap[pt[[1]]], vMap[pt[[2]]]];
zMapped[pt_] := zTorus[uMap[pt[[1]]], vMap[pt[[2]]]];

(*Finite Field and Mesh Points*)
p = 17; (*Modulus*)
fieldPoints = Tuples[Range[0, p - 1], 2];
uMap[x_] := 2 Pi x/(p - 1) + Pi;
vMap[y_] := Pi (2 y/(p - 1) - 1) + Pi;
meshPoints = 
  Map[Function[
    pt, {xTorus[uMap[pt[[1]]], vMap[pt[[2]]]], 
     yTorus[uMap[pt[[1]]], vMap[pt[[2]]]], 
     zTorus[uMap[pt[[1]]], vMap[pt[[2]]]]}], fieldPoints];

(*Calculate Edwards curve points mod p*)
edwardsCurvePointsModP = {};
For[x = 0, x < p, x++, 
  For[y = 0, y < p, y++, 
    If[Mod[x^2 + y^2 - 1 - dCoeff*x^2*y^2, p] == 0, 
      AppendTo[edwardsCurvePointsModP, {x, y}];];];];

(*Map the Edwards curve points onto the torus*)
mappedCurvePoints = 
  Map[Function[pt, {xMapped[pt], yMapped[pt], zMapped[pt]}], 
   edwardsCurvePointsModP];

(*Generate lines for the mesh grid with translucent lines*)
lines = {};
For[i = 1, i <= p, i++, 
  For[j = 1, j < p, j++, 
    AppendTo[
     lines, {Directive[Black, Opacity[0.3]], 
      Line[{meshPoints[[p (i - 1) + j]], 
        meshPoints[[p (i - 1) + j + 1]]}]}];
    AppendTo[
     lines, {Directive[Black, Opacity[0.3]], 
      Line[{meshPoints[[p (j - 1) + i]], 
        meshPoints[[p (j) + i]]}]}];];];

(*Plotting*)
Show[ParametricPlot3D[{xTorus[u, v], yTorus[u, v], zTorus[u, v]}, {u, 
   0, 2 Pi}, {v, -Pi, Pi}, 
  PlotStyle -> Directive[LightBlue, Opacity[0.2]], Mesh -> None], 
 Graphics3D[{Directive[Black], PointSize[Small], lines, Blue, 
   PointSize[Large], Point[mappedCurvePoints]}, ImageSize -> Large], 
 ParametricPlot3D[{xMapped[{u, xElliptic[u]}], 
   yMapped[{u, yElliptic[u]}], zMapped[{u, yElliptic[u]}]}, {u, 0, 
   p - 1}, PlotStyle -> Directive[Blue, Thickness[0.003]]], 
 ParametricPlot3D[{xMapped[{u, xElliptic[u]}], 
   yMapped[{u, yElliptic[u]}], zMapped[{u, yElliptic[u]}]}, {u, 0, 
   1*(p - 1)}, 
  PlotStyle -> Directive[Blue, Opacity[0.5], Thickness[0.003]]], 
 ParametricPlot3D[{xTorus[Pi, v], yTorus[Pi, v], 
   zTorus[Pi, v]}, {v, -Pi, Pi}, 
  PlotStyle -> Directive[Purple, Thickness[0.003]]], 
 ParametricPlot3D[{xTorus[u, 0], yTorus[u, 0], zTorus[u, 0]}, {u, 0, 
   2 Pi}, PlotStyle -> Directive[Red, Thickness[0.003]]], 
 Boxed -> False, Axes -> False]