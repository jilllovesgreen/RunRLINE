function linkList = SearchLinkInRange(rec, recAttrib, link, searchRange)

recX = recAttrib.X(recAttrib.recID == rec, :);
recY = recAttrib.Y(recAttrib.recID == rec, :);
limit1 = ((link.X_begin - recX).^2 + (link.Y_begin - recY).^2)< searchRange^2; 
limit2 = ((link.X_end - recX).^2 + (link.Y_end - recY).^2)< searchRange^2; 
linkList = link.ID(limit1 | limit2);
end