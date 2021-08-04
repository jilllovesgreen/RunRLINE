function Write_RLINE_input(x1, y1, z1, x2, y2, z2, emis, out_name)
%WRITE_RLINE_INPUT Summary of this function goes here
%   Detailed explanation goes here
len = length(x1);
ID = (1: len)';
X_begin = x1;
Y_begin = y1;
Z_begin = z1;
X_end = x2;
Y_end = y2;
Z_end = z2;
dCL = zeros(len , 1);
sigmaz0 = zeros(len , 1) + 2;
NumLanes = zeros(len , 1) + 1;
Emis_g_m_s = emis;
%ID	X_begin	Y_begin	Z_begin	X_end	Y_end	Z_end	dCL	sigmaz0	NumLanes	Emis_g_m_s	
%Hw1	dw1	Hw2	dw2	Depth	Wtop	Wbottom	KPH	FRC	LANES	TruckRoute	
%METERS	NetworkID	hist_rel_10	hist_rel_22	FFS

Hw1 = zeros(len , 1);
dw1	= zeros(len , 1);
Hw2	= zeros(len , 1);
dw2	= zeros(len , 1);
Depth = zeros(len , 1);
Wtop = zeros(len , 1);
Wbottom = zeros(len , 1);
KPH	= zeros(len , 1);
FRC	= zeros(len , 1);
LANES = zeros(len , 1);
TruckRoute = zeros(len , 1);
METERS = zeros(len , 1);
NetworkID = zeros(len , 1);
hist_rel_10 = zeros(len , 1);
hist_rel_22	= zeros(len , 1);
FFS = zeros(len , 1);

out_table = table(ID, X_begin, Y_begin, Z_begin, X_end, Y_end, Z_end, dCL, sigmaz0, NumLanes, Emis_g_m_s, ...
    Hw1, dw1, Hw2, dw2, Depth, Wtop, Wbottom, KPH, FRC, LANES, TruckRoute, ...
    METERS, NetworkID, hist_rel_10, hist_rel_22, FFS);

writetable(out_table, out_name)

end

