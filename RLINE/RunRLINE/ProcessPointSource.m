loc = 'MOD'; caseNo = 'case9';
addpath('../../Jerry_soot/' )
links = readtable(['pp_' loc '_' caseNo   '.csv']);

links(links.soot_g_s<0, :) = []; % remove negative emission rates

links.Emis_g_m_s = links.soot_g_s./links.METERS;
links(isnan(links.Emis_g_m_s), :) = []; % remove zero emission rates, 0/0 = NaN
link_length = height(links);
links = links(1:link_length(1), :);

pt_num = links.Emis_g_m_s == inf;  % positive number/0 = inf
pt_line = find(pt_num ==1);
% width = size(links, 2);
% temp = links.x_begin;
% links.x_begin = links.y_begin;
% links.y_begin = temp;
% 
% temp = links.x_end;
% links.x_end = links.y_end;
% links.y_end = temp;
line_source = links(links.Emis_g_m_s ~= inf,:);

clear pt_source
%pt_source = table(zeros(numel(pt_line), width));

if ~isempty(pt_line)
    for i1 = 1:numel(pt_line)
       pt_source(i1,:) = links(pt_line(i1)-1,:);  % replace point source XY as XY from last link
       pt_source.Emis_g_m_s(i1) = links.soot_g_s(pt_line(i1));   % keep the point source emis rate as original NOx_g_s
    end

    x1 = pt_source.X_begin;
    y1 = pt_source.Y_begin;
    x2 = pt_source.X_end;
    y2 = pt_source.Y_end;
    theta = atan2(y2-y1, x2-x1);
    x3 = x2 - cos(theta); % after cutting the 1 meter's new x1
    y3 = y2 - sin(theta); % after cutting the 1 meter's new y1
    figure, plot([x1, x2], [y1, y2], '*')
    hold on, plot(x3, y3, 'o')
    
    Write_RLINE_input(x3, y3, pt_source.Z_end, x2, y2, pt_source.Z_end, pt_source.Emis_g_m_s, ['linkAttribute_pt_' loc '_' caseNo '.csv']);
 
end
%ID	X_begin	Y_begin	Z_begin	X_end	Y_end	Z_end	dCL	sigmaz0	NumLanes	Emis_g_m_s	Hw1	dw1	Hw2	dw2	Depth	Wtop	Wbottom	KPH	FRC	LANES	TruckRoute	METERS	NetworkID	hist_rel_10	hist_rel_22	FFS
Write_RLINE_input(line_source.X_begin, line_source.Y_begin, line_source.Z_begin, ...
    line_source.X_end, line_source.Y_end, line_source.Z_end, line_source.Emis_g_m_s, ['linkAttribute_link_' loc '_' caseNo '.csv'])

