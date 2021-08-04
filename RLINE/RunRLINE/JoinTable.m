function newTable = JoinTable(joinF1,tableA,joinF2,tableB)

% JoinTable join TableA (smaller or equal) to the TableB(bigger) to grab
% data from TableB. It could be a many-to-one join.
% joinF1 and joinF2 is numerical
% returns new table, only keep matched records
% newTable has same row number with tableA

sizeCheck=size(tableA);

if sizeCheck(1)==1 % turn row vector into column vector
   
    tableA=tableA';

end

joinField1Index=tableA(:,joinF1); % column vector

% the field to be joined on

joinField2Index=tableB(:,joinF2); % field to join in

count=0; 

for i1=1:numel(joinField1Index)
    
%     if mod(i1, 100)==1
%         
%     display(['processing ' num2str(i1)])    
%     end
    i2=joinField1Index(i1); % search index (ID) in target table one by one
    
    i3=find(joinField2Index==i2); % the index join
    
    if isempty(i3) % if true, i3 empty there is no match
        
        continue
        
    else
        
       count=count+1;
       
       newTable(count,:)=[tableA(i1,:) tableB(i3,:)];
        
       % newTable(count,:)=[tableA(count,:) tableB(i3,:)];
       
       % is a wrong statement, tableA(count,:) will not count the
       % unmatched IDs
           
    end
    
end
       
    % check if the id matches
    test=newTable(:,1)-newTable(:,2);
    
    if ~(sum(test)==0&& mean(test)==0)
        
        error ('index match fails')
        
    end
    
    newTable=newTable(:,2:end);

end

