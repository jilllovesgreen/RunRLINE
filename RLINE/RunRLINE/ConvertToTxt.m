function ConvertToTxt(inFileName,inArray)

fid = fopen(inFileName, 'w');

 for i=1:numel(inArray)
     
     rowOut=inArray{i};
    
     fprintf(fid, [rowOut '\n' ]);
     
 end
 
 fclose(fid);
 
end
 