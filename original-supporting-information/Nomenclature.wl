(* ::Package:: *)

BeginPackage["Nomenclature`"]


(*Commonly occuring variables/functions*)
\[CurlyRho][\[Rho]_,\[Rho]p_]
\!\(\*OverscriptBox[\(\[CurlyRho]\), \(_\)]\)[\[Rho]_,\[Rho]p_]
\[CapitalPhi][\[CurlyPhi]_,\[CurlyPhi]p_]
Z[z_,zp_]
\[Phi][\[CurlyPhi]_,\[CurlyPhi]p_]
k[\[Rho]_,\[Rho]p_,z_,zp_]
\!\(\*OverscriptBox[\(k\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_]
a[\[Rho]_,z_,zp_]
\!\(\*OverscriptBox[\(a\), \(_\)]\)[\[Rho]_,z_,zp_]
\[Kappa][\[Rho]_, \[Rho]p_]
L[\[Rho]_,z_,zp_]
R[\[Rho]_,\[Rho]p_,z_,zp_]
\!\(\*OverscriptBox[\(R\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_]
S[\[Rho]_,\[Rho]p_,z_,zp_]
\!\(\*OverscriptBox[\(S\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_]
T[\[Rho]_,\[Rho]p_,z_,zp_]
\!\(\*OverscriptBox[\(T\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_]
G[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_]
ns[\[CapitalPhi]p_]
qs[\[CapitalPhi]p_]
ts[\[CapitalPhi]p_]
zs[\[Rho]_,\[Rho]p_,z_,zp_]
ys[\[Rho]_,\[Rho]p_,z_,zp_]
\[Omega][\[CurlyPhi]_,\[CurlyPhi]p_]
v[\[Nu]_]
\!\(\*OverscriptBox[\(\[CapitalPhi]\), \(~\)]\)[\[CapitalPhi]p_]
\[CapitalUpsilon][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_]
(*transformed Legendre functions*)
EllipticFT[\[Phi]_,k_]
EllipticET[\[Phi]_,k_]
EllipticPiT[\[Alpha]_,\[Phi]_,k_]
EllipticDT[\[Phi]_,k_]
EllipticD[k_]
(*Canonical regularised beta summands*)
\[CapitalXi][\[Rho]_,\[Rho]p_,z_,zp_,\[Nu]_,p_]
\[Xi][\[Rho]_,\[Rho]p_,z_,zp_,\[Nu]_,p_]
\[Tau][\[Rho]_,\[Rho]p_,z_,zp_,\[Nu]_,p_]
(*Elementary functions*)
\[Alpha]1[\[Rho]_,\[Rho]p_,z_,zp_]
\[Alpha]2[\[Rho]_,\[Rho]p_,z_,zp_]
\[Alpha]3[\[Rho]_,\[Rho]p_,z_,zp_]
(*Single series*)
\[Beta]1[\[Rho]_,\[Rho]p_,z_,zp_,P_]
\[Beta]2[\[Rho]_,\[Rho]p_,z_,zp_,P_]
\[Beta]3[\[Rho]_,\[Rho]p_,z_,zp_,P_]
(*Double series*)
\[Gamma]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
\[Gamma]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
\[Gamma]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
(*Reduced form & computational series*)
\[Delta]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
\[Delta]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
\[Delta]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
\[Chi][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_]
\[Chi]\[Zeta][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_] 
\[Chi]\[Eta][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_] 
\[Chi]\[Iota][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_]
\[Zeta]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]   
\[Eta]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]   
\[Iota]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]   
\[Zeta]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]  
\[Eta]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]   
\[Iota]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]   
\[Zeta]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_] 
\[Eta]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]   
\[Iota]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_]
\[Delta]1\[Zeta]\[Eta]\[Iota][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_] 
\[Delta]2\[Zeta]\[Eta]\[Iota][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
\[Gamma]3\[Zeta]\[Eta]\[Iota][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]
(*Misc*)
s[\[CurlyPhi]_,\[CurlyPhi]p_]
\[Lambda][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_,p_]
\[CurlyTheta][\[Rho]_,\[Rho]p_,z_,zp_,P_] 
(*check if field point within volume*)
InsideVolume[\[Rho]p_,\[Rho]_,\[CurlyPhi]_,\[CurlyPhi]p_,zp_,z_]
InsideVolumeAxis[\[Rho]p_,zp_,z_]


Begin["`Private`"]


\[CurlyRho][\[Rho]_,\[Rho]p_] := (\[Rho]+\[Rho]p)
\!\(\*OverscriptBox[\(\[CurlyRho]\), \(_\)]\)[\[Rho]_,\[Rho]p_] := (\[Rho]-\[Rho]p)
\[CapitalPhi][\[CurlyPhi]_,\[CurlyPhi]p_] := (\[CurlyPhi]-\[CurlyPhi]p)
Z[z_,zp_] := (z-zp)
\[Phi][\[CurlyPhi]_,\[CurlyPhi]p_] := 1/2 (\[CurlyPhi]-\[CurlyPhi]p+\[Pi])
k[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[((4\[Rho] \[Rho]p)/(R[\[Rho],\[Rho]p,z,zp]^2)) ]
\!\(\*OverscriptBox[\(k\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[(4\[Rho] \[Rho]p)/\!\(\*OverscriptBox[\(R\), \(_\)]\)[\[Rho],\[Rho]p,z,zp]^2] 
a[\[Rho]_,z_,zp_] := Sqrt[(2\[Rho])/(\[Rho]+L[\[Rho],z,zp])]
\!\(\*OverscriptBox[\(a\), \(_\)]\)[\[Rho]_,z_,zp_] := Sqrt[(2\[Rho])/(\[Rho]-L[\[Rho],z,zp])]
\[Kappa][\[Rho]_,\[Rho]p_]:= Sqrt[(4\[Rho] \[Rho]p)/\[CurlyRho][\[Rho],\[Rho]p]^2]
L[\[Rho]_,z_,zp_] := Sqrt[Z[z,zp]^2+\[Rho]^2]
R[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[Z[z,zp]^2+\[CurlyRho][\[Rho],\[Rho]p]^2]
\!\(\*OverscriptBox[\(R\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[Z[z,zp]^2+\!\(\*OverscriptBox[\(\[CurlyRho]\), \(_\)]\)[\[Rho],\[Rho]p]^2]
S[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[L[\[Rho],z,zp]+\[Rho]p]
\!\(\*OverscriptBox[\(S\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[L[\[Rho],z,zp]-\[Rho]p]
T[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[L[\[Rho],z,zp]^2+\[Rho]p^2]
\!\(\*OverscriptBox[\(T\), \(_\)]\)[\[Rho]_,\[Rho]p_,z_,zp_] := Sqrt[L[\[Rho],z,zp]^2-\[Rho]p^2]
G[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_]:=1/Sqrt[T[\[Rho],\[Rho]p,z,zp]^2-2\[Rho] \[Rho]p Cos[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]]
ns[\[CapitalPhi]p_] := Floor[\!\(\*OverscriptBox[\(\[CapitalPhi]\), \(~\)]\)[\[CapitalPhi]p]]
qs[\[CapitalPhi]p_] := (-1)^Floor[2\!\(\*OverscriptBox[\(\[CapitalPhi]\), \(~\)]\)[\[CapitalPhi]p]+1]
ts[\[CapitalPhi]p_] := (1-2Abs[Round[\!\(\*OverscriptBox[\(\[CapitalPhi]\), \(~\)]\)[\[CapitalPhi]p]]-\!\(\*OverscriptBox[\(\[CapitalPhi]\), \(~\)]\)[\[CapitalPhi]p]])
zs[\[Rho]_,\[Rho]p_,z_,zp_]:= Sqrt[\[Rho]p^2/T[\[Rho],\[Rho]p,z,zp]^2]
ys[\[Rho]_,\[Rho]p_,z_,zp_]:= Sqrt[Z[z,zp]^2/T[\[Rho],\[Rho]p,z,zp]^2]
\[Omega][\[CurlyPhi]_,\[CurlyPhi]p_]:=(1+Round[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]/\[Pi]-1])
v[\[Nu]_] := (\[Nu]-2Floor[\[Nu]/2])
\!\(\*OverscriptBox[\(\[CapitalPhi]\), \(~\)]\)[\[CapitalPhi]p_] := \[CapitalPhi]p/(2\[Pi])
\[CapitalUpsilon][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_] := (Z[z,zp] (\[Rho] Cos[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]-\[Rho]p) )/(\[Rho] Sin[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]G[\[Rho],\[Rho]p,\[CurlyPhi],\[CurlyPhi]p,z,zp]^-1)
(*EllipticFT[\[Phi]_,k_]:= (Sign[2\[Phi]-\[Pi]]-1)EllipticK[k]+Sign[2\[Phi]-\[Pi]]Sign[Abs[2\[Phi]-\[Pi]]-\[Pi]]EllipticF[1/2 Abs[(Abs[2\[Phi]-\[Pi]]-\[Pi])],k]
EllipticET[\[Phi]_,k_]:= (Sign[2\[Phi]-\[Pi]]-1)EllipticE[k]+Sign[2\[Phi]-\[Pi]]Sign[Abs[2\[Phi]-\[Pi]]-\[Pi]]EllipticE[1/2 Abs[(Abs[2\[Phi]-\[Pi]]-\[Pi])],k]
EllipticPiT[\[Alpha]_,\[Phi]_,k_]:= (Sign[2\[Phi]-\[Pi]]-1)EllipticPi[\[Alpha],k]+Sign[2\[Phi]-\[Pi]]Sign[Abs[2\[Phi]-\[Pi]]-\[Pi]]EllipticPi[\[Alpha],1/2 Abs[(Abs[2\[Phi]-\[Pi]]-\[Pi])],k]*)
EllipticFT[\[Phi]_,k_]:= 2ns[2\[Phi]-\[Pi]] EllipticK[k]+qs[2\[Phi]-\[Pi]]EllipticF[\[Pi]/2 ts[2\[Phi]-\[Pi]],k]
EllipticET[\[Phi]_,k_]:= 2ns[2\[Phi]-\[Pi]]EllipticE[k]+qs[2\[Phi]-\[Pi]]EllipticE[\[Pi]/2 ts[2\[Phi]-\[Pi]],k]
EllipticPiT[\[Alpha]_,\[Phi]_,k_]:= 2ns[2\[Phi]-\[Pi]]EllipticPi[\[Alpha],k]+qs[2\[Phi]-\[Pi]]EllipticPi[\[Alpha],\[Pi]/2 ts[2\[Phi]-\[Pi]],k]
EllipticDT[\[Phi]_,k_]:= 1/k (EllipticFT[\[Phi],k]-EllipticET[\[Phi],k])
EllipticD[k_]:= 1/k (EllipticK[k]-EllipticE[k])
\[CapitalXi][\[Rho]_,\[Rho]p_,z_,zp_,\[Nu]_,p_]:= 1/(1+2 p+\[Nu]) (\[Rho]/(2 L[\[Rho],z,zp]))^(1+2 p+\[Nu]) BetaRegularized[zs[\[Rho],\[Rho]p,z,zp]^2,1+p+\[Nu]/2,1/2+p+\[Nu]/2] Binomial[1+2 p+\[Nu],p]
\[Xi][\[Rho]_,\[Rho]p_,z_,zp_,\[Nu]_,p_]:=((\[Rho] \[Rho]p)/(\[Rho]^2+\[Rho]p^2))^(2 p+\[Nu]+1) BetaRegularized[ys[\[Rho],\[Rho]p,z,zp]^2,1/2,2p+\[Nu]+1] 1/(\[Nu]+2p+1) Binomial[2p+\[Nu]+1,p]
\[Tau][\[Rho]_,\[Rho]p_,z_,zp_,\[Nu]_,p_]:= Pochhammer[5/2+p+\[Nu],p+1/2]  (\[Rho]p/\[Rho])^\[Nu] \[Xi][\[Rho]p,\[Rho],z,zp,\[Nu]+1+2p,0] 
\[Alpha]1[\[Rho]_,\[Rho]p_,z_,zp_] := ArcTanh[zs[\[Rho],\[Rho]p,z,zp]]
\[Alpha]2[\[Rho]_,\[Rho]p_,z_,zp_] := ArcTanh[ys[\[Rho],\[Rho]p,z,zp]]
\[Alpha]3[\[Rho]_,\[Rho]p_,z_,zp_]:= \[Rho] ArcTan[(Z[z,zp] \[Rho]p)/(\[Rho] T[\[Rho],\[Rho]p,z,zp])]-Z[z,zp] ArcTanh[\[Rho]p/T[\[Rho],\[Rho]p,z,zp]] - \[Rho]p Log[Z[z,zp]+T[\[Rho],\[Rho]p,z,zp]]
\[Beta]1[\[Rho]_,\[Rho]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\ \(\[CapitalXi][\[Rho], \[Rho]p, z, zp, \(-1\), p + 1]\)\)
\[Beta]2[\[Rho]_,\[Rho]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\ \(\[Xi][\[Rho], \[Rho]p, z, zp, \(-1\), p + 1]\)\)
\[Beta]3[\[Rho]_,\[Rho]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\(
\*FractionBox[\(\(\ \)\(Binomial[2 \((1 + p)\), 1 + p]\)\(\ \)\), \(
\*SuperscriptBox[\(4\), \(p\)] \(\((1 + p)\)!\)\)] \(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\[Tau][\[Rho], \[Rho]p, z, zp, \[Nu], p]\)\)\)
\[Gamma]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_] := 2\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\(Cos[\((\[Nu] + 1)\) \[CapitalPhi][\[CurlyPhi], \[CurlyPhi]p]] \(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\[CapitalXi][\[Rho], \[Rho]p, z, zp, \[Nu], p]\)\)\)
\[Gamma]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_] :=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\(Cos[\((\[Nu] + 1)\) \[CapitalPhi][\[CurlyPhi], \[CurlyPhi]p]] \(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\[Xi][\[Rho], \[Rho]p, z, zp, \[Nu], p]\)\)\)
\[Gamma]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_] := \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\(
\*FractionBox[\(\[Chi][\[CurlyPhi], \[CurlyPhi]p, p]\), \(Pochhammer[2 + 
\*FractionBox[\(p\), \(2\)], 1 + 
\*FractionBox[\(p\), \(2\)]]\)] \(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\[Tau][\[Rho], \[Rho]p, z, zp, \[Nu], 
\*FractionBox[\(p - 1\), \(2\)]]\)\)\)
\[Delta]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\ \(\[CapitalXi][\[Rho], \[Rho]p, z, zp, \[Nu], 0] \[Chi][\[CurlyPhi], \[CurlyPhi]p, \[Nu]]\)\)
\[Delta]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\ \(\[Xi][\[Rho], \[Rho]p, z, zp, \[Nu], 0] \[Chi][\[CurlyPhi], \[CurlyPhi]p, \[Nu]]\)\)
\[Delta]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\ \(\[CapitalXi][\[Rho], \[Rho]p, z, zp, \[Nu], 0] Sum[\[Lambda][\[CurlyPhi], \[CurlyPhi]p, \[Nu], p]\ Binomial[1 + \[Nu], p], {p, 0, Floor[\[Nu]/2]}]\)\)
\[Chi][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_]:=(2^\[Nu] (v[\[Nu]+1]Sign[Sin[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]]+ v[\[Nu]]s[\[CurlyPhi],\[CurlyPhi]p]) Beta[Sin[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]^2,1/2,1+\[Nu]/2]+v[\[Nu]](2^(\[Nu]+1) Beta[1/2,1+\[Nu]/2]\[Omega][\[CurlyPhi],\[CurlyPhi]p]-Binomial[1+\[Nu],(1+\[Nu])/2]\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]))
\[Chi]\[Zeta][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_] := 2^\[Nu] Sign[Sin[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]]Beta[Sin[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]^2,1/2,\[Nu]/2+1]
\[Chi]\[Eta][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_] := 2^\[Nu] s[\[CurlyPhi],\[CurlyPhi]p]Beta[Sin[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]^2,1/2,\[Nu]/2+1]
\[Chi]\[Iota][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_] := 2^(\[Nu]+1) Beta[1/2,\[Nu]/2+1]\[Omega][\[CurlyPhi],\[CurlyPhi]p]-Binomial[\[Nu]+1,\[Nu]/2+1/2]\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]
\[Zeta]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_] :=  \[CapitalXi][\[Rho],\[Rho]p,z,zp,2p,0]\[Chi]\[Zeta][\[CurlyPhi],\[CurlyPhi]p,2p]
\[Eta]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_] :=  \[CapitalXi][\[Rho],\[Rho]p,z,zp,2p+1,0]\[Chi]\[Eta][\[CurlyPhi],\[CurlyPhi]p,2p+1]
\[Iota]1[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_] :=  \[CapitalXi][\[Rho],\[Rho]p,z,zp,2p+1,0]\[Chi]\[Iota][\[CurlyPhi],\[CurlyPhi]p,2p+1]
\[Zeta]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_] :=  \[Xi][\[Rho],\[Rho]p,z,zp,2p,0]\[Chi]\[Zeta][\[CurlyPhi],\[CurlyPhi]p,2p]
\[Eta]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_] :=  \[Xi][\[Rho],\[Rho]p,z,zp,2p+1,0]\[Chi]\[Eta][\[CurlyPhi],\[CurlyPhi]p,2p+1]
\[Iota]2[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,p_] :=  \[Xi][\[Rho],\[Rho]p,z,zp,2p+1,0]\[Chi]\[Iota][\[CurlyPhi],\[CurlyPhi]p,2p+1]
\[Zeta]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,\[Nu]_,p_] := \[Tau][\[Rho],\[Rho]p,z,zp,\[Nu],p-1/2] \[Chi]\[Zeta][\[CurlyPhi],\[CurlyPhi]p,2p]/Pochhammer[2+p,1+p]
\[Eta]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,\[Nu]_,p_] := \[Tau][\[Rho],\[Rho]p,z,zp,\[Nu],p] \[Chi]\[Eta][\[CurlyPhi],\[CurlyPhi]p,2p+1]/Pochhammer[5/2+p,3/2+p]
\[Iota]3[\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,\[Nu]_,p_] := \[Tau][\[Rho],\[Rho]p,z,zp,\[Nu],p] \[Chi]\[Iota][\[CurlyPhi],\[CurlyPhi]p,2p+1]/Pochhammer[5/2+p,3/2+p]
\[Delta]1\[Zeta]\[Eta]\[Iota][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\((\[Zeta]1[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, p] + \[Eta]1[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, p] + \[Iota]1[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, p])\)\)
\[Delta]2\[Zeta]\[Eta]\[Iota][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\((\[Zeta]2[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, p] + \[Eta]2[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, p] + \[Iota]2[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, p])\)\)
\[Gamma]3\[Zeta]\[Eta]\[Iota][\[Rho]_,\[Rho]p_,\[CurlyPhi]_,\[CurlyPhi]p_,z_,zp_,P_]:= \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(P\)]\(
\*UnderoverscriptBox[\(\[Sum]\), \(p = 0\), \(P\)]\((\[Zeta]3[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, \[Nu], p] + \[Eta]3[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, \[Nu], p] + \[Iota]3[\[Rho], \[Rho]p, \[CurlyPhi], \[CurlyPhi]p, z, zp, \[Nu], p])\)\)\)
s[\[CurlyPhi]_,\[CurlyPhi]p_]:=If[Sin[2 \[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]==0,(-1)^Round[\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]/\[Pi]+1/2], Sign[Sin[2\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]]]
\[Lambda][\[CurlyPhi]_,\[CurlyPhi]p_,\[Nu]_,p_]:=If[\[Nu]-2p==1,\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p],Sin[(-1+\[Nu]-2p)\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]/(-1+\[Nu]-2p)]+(2 Sin[(1+\[Nu]-2p) \[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]])/(1+\[Nu]-2p)+Sin[(3+\[Nu]-2p)\[CapitalPhi][\[CurlyPhi],\[CurlyPhi]p]]/(3+\[Nu]-2p)
\[CurlyTheta][\[Rho]_,\[Rho]p_,z_,zp_,P_] :=\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(\[Nu] = 0\), \(Floor[
\*FractionBox[\(P - 1\), \(2\)]]\)]\ \(\[CapitalXi][\[Rho]p, \[Rho], z, zp, 2  \[Nu] + 1, 0]\ Binomial[2 + 2  \[Nu], \[Nu]]\)\)
InsideVolume[\[Rho]p_,\[Rho]_,\[CurlyPhi]p_,\[CurlyPhi]_,zp_,z_] :=  (\[Rho] <\[Rho]p[[2]]) && (\[Rho] > \[Rho]p[[1]]) && (z <zp[[2]]) && (z> zp[[1]]) && If[Mod[\[CurlyPhi]p[[2]]-\[CurlyPhi]p[[1]],2\[Pi]] < \[Pi],Mod[\[CurlyPhi]-\[CurlyPhi]p[[1]],2\[Pi]]<Mod[\[CurlyPhi]p[[2]]-\[CurlyPhi]p[[1]],2\[Pi]],Mod[\[CurlyPhi]p[[2]]-\[CurlyPhi]p[[1]],2\[Pi]]<Mod[\[CurlyPhi]-\[CurlyPhi]p[[1]],2\[Pi]]]
InsideVolumeAxis[\[Rho]p_,zp_,z_] := (0 == \[Rho]p[[1]]) && (z < zp[[2]]) && (z > zp[[1]]) 


End[];


EndPackage[];
