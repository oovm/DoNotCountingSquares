(* ::Package:: *)

SetDirectory@NotebookDirectory[];
ClearAll["Globla`*"]
img = Import@"Task.PNG";
{w, h} = 40 * {4, 4}
img = Binarize@ImageResize[ColorNegate@img, {w, h}]


corners = ImageCorners[img, MaxFeatureDisplacement -> 0, Method -> {"StrengthFraction" -> 1}];
corners = Union@Round[corners, 20];
subset = Union@Round[ImageValuePositions[img, 1], 10];
collist = Catenate@Map[Partition[#, 2, 1]&, GatherBy[corners, First], {1}];
col = If[SubsetQ[subset, #], {First@#, Last@#}, Nothing]& /@ (Table[{#[[1, 1]], i}, {i, #[[1, 2]], #[[2, 2]], 10}]& /@ collist);
rowlist = Catenate@Map[Partition[#, 2, 1]&, GatherBy[corners, Last], {1}];
row = If[SubsetQ[subset, #], {First@#, Last@#}, Nothing]& /@ (Table[{i, #[[1, 2]]}, {i, #[[1, 1]], #[[2, 1]], 10}]& /@ rowlist);
Show[ListLinePlot[Join[col, row], PlotStyle -> {Gray}, AspectRatio -> w / h], Graphics[{PointSize[0.01], Red, Point[corners]}]]


squaresQ[list_] := Block[
	{l = list, sub},
	sub = EuclideanDistance @@@ Subsets[N@l, {2}];
	(SameQ @@ TakeSmallest[sub, 4]) && (SameQ @@ TakeLargest[sub, 2])
];
rec = Catenate@Outer[List, ##]& @@@ (Tuples[Subsets[Range[0, #, 20], {2}]& /@ {w, h}]);
nopts = Complement[Catenate@Table[{i, j}, {i, 0, w, 20}, {j, 0, h, 20}], corners];
squ = Select[rec, DisjointQ[#, nopts] && squaresQ[#]&];
squpts[l_] := Block[
	{list = l, x0, y0, step},
	{x0, y0} = list[[1]];
	step = Last[list[[2]] - list[[1]]];
	Catenate@Map[Partition[#, 2, 1]&, Join[Table[{#, i}, {i, y0, y0 + step, 20}]& /@ {x0, x0 + step}, Table[{i, #}, {i, x0, x0 + step, 20}]& /@ {y0, y0 + step}], 1]
];
test = Flatten[If[Norm[Subtract @@ #] > 20, Partition[Table[#[[1]] + -20 i Normalize[Subtract @@ #] , {i, 0, Norm[Subtract @@ #] / 20}], 2, 1], {#}]& /@ Join[col, row], 1];
Length[squs = Rectangle[#1, #4]& @@@ Select[squ, SubsetQ[test, squpts[#]]&]]


grouped = SortBy[Normal@GroupBy[squs, Area], Keys]
getSVG[a_ -> b_] := Block[
	{color, g},
	SeedRandom[a / 1600];
	color = RandomColor[];
	g = {
		Thick, Line /@ col, Line /@ row,
		EdgeForm[Directive[Thick, Red]], Opacity[0.2], color, #
	}&;
	Export[
		"Size-" <> ToString[a / 1600] <> ".SVG",
		Multicolumn[Flatten[Graphics[g[#]]& /@ b], {Automatic, 5}, Appearance -> "Horizontal"]
	]
];
getSVG /@ grouped


head = "\
# \|01f648 \:522b\:6570\:65b9\:5757

![](./Task.PNG)

- \:5171\:8ba1 **" <> ToString@Length@Flatten[Last /@ grouped] <> "** \:4e2a

";
str = StringTemplate["\
## Area = `1`

- \:603b\:5171 **`2`** \:4e2a

![](./Size-`1`.SVG)
\n"];
tail = StringJoin[str @@@ ({#1 / 1600, Length@#2}& @@@ grouped)];
Export["Readme.md", head <> tail, "Text"]
