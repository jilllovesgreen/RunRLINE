fac_Z = zeros(length(fac.X), 1);
for i1 = 1:length(fac.X)
       dist = (link.X_begin - fac.X(i1)).^2 + (link.Y_begin - fac.Y(i1)).^2;
       ind = find(dist == min(dist));
       fac_Z(i1,1) = link.Z_begin(ind);
end