(* ::Package:: *)

(* construct a boundary matrix from a set of (high)-simplices to a set of (low)-simplices *)
boundaryMatrix[low_, high_] := Module[
  {boundaries = Subsets[#, {Length@low[[1]]}] & /@ high},
  Table[
   Switch[Flatten@Position[boundaries[[j]], low[[i]]],
    {}, 0,
    {_?OddQ}, 1,
    {_?EvenQ}, -1
   ],
   {i, Length@low},
   {j, Length@high}
  ]
]
(* construct the set of boundary matrices for a simplicial complex *)
boundaryMatrices[complex_] := Table[
  boundaryMatrix[complex[[k]], complex[[k + 1]]],
  {k, Length@(complex /. {} -> Nothing) - 1}
]
(* calculate the betti numbers for simplical complex *)
bettiNumbers[complex_] := Module[{
   dims = Length /@ (complex /. {} -> Nothing),
   ranks = Join[{0}, MatrixRank /@ boundaryMatrices[complex], {0}]
  },
  Table[dims[[i]] - ranks[[i]] - ranks[[i + 1]], {i, Length@dims}]
]


(* construct a vietoris-rips complex from a set of given points for distance (d) up to dimension (dim) *)
vrComplex[points_, d_, dim_] :=
 Module[
  {addCofaces, closeTo, graph, length = Length[points], V = {}},
  graph = Table[
    Select[Range[i, length], EuclideanDistance @@ points[[{i, #}]] < d &],
    {i, length}
  ];
  addCofaces[graph_, els_, neighbors_] := Module[{},
    AppendTo[V, els];
    If[Length[els] <= dim,
     Do[
      addCofaces[graph, Append[els, vertex], Intersection[neighbors, graph[[vertex]]]],
      {vertex, Rest[neighbors]}
     ]
    ]
  ];
  Do[
   addCofaces[graph, {i}, graph[[i]]],
   {i, Range[Length[graph]]}
  ];
  PadRight[GatherBy[V, Length], dim + 1, {{}}]
]


(* demonstration *)
outer = Ellipsoid[{0, 0}, {6, 5}];
inner = Ellipsoid[{0, 0}, {2.5, 2}];
region = Region@RegionDifference[
  TransformedRegion[outer, RotationTransform[30 Degree]],
  TransformedRegion[inner, RotationTransform[10 Degree]]
];
bounds = RegionBounds[region];
outer = Rotate[outer, 30 Degree];
inner = Rotate[inner, 10 Degree];
points = RandomPoint[region, 20];
ClearAll[drawComplex];
drawComplex[points_, comp_, d_, opts : OptionsPattern[Graphics]] := Graphics[
  {
   FaceForm[Transparent], EdgeForm[Black], inner, outer,
   EdgeForm[Dashed], Opacity[0.2], Yellow, EdgeForm[Gray], Disk[#, d/2] & /@ points,
   Opacity[1], LightGray, GraphicsComplex[points, Polygon /@ comp[[3]]],
   Gray, MeshPrimitives[#, 2] & /@ (ConvexHullMesh[points[[#]]] & /@ comp[[4]]),
   Darker[Gray], MeshPrimitives[#, 2] & /@ (ConvexHullMesh[points[[#]]] & /@ comp[[5]]),
   Black, GraphicsComplex[points, Line /@ comp[[2]]],
   Point[points],
   Text[Style[
     StringRiffle[MapIndexed[
       StringReplace[" \!\(\*SubscriptBox[\(\[Beta]\), \(n\)]\): " ~~ ToString[#1], "n" -> ToString[#2[[1]] - 1]] &,
       bettiNumbers[comp]
       ], ",  "
      ], FontSize -> 12],
    {bounds[[1, 1]] - .9, bounds[[2, 2]] + .9}, {-1, 1},
    Background -> White
   ]
  },
  PlotRange -> bounds,
  PlotRangePadding -> 1, opts
]
Manipulate[
  drawComplex[points, vrComplex[points, d, 4], d],
  {d, 0.2, 6}
]
