% Run RLINE for the scenarios where local on-road emissions are sources
% and polygons (park, hospital, middle schools) and points are receptors
% RunRLINE only calculates 'norm concentration' when EF are 1. So no need
% to load emission factor
% can calculate multiple vehicle model year for each day/hour case

hr = 11; jday = 295; vehMY = [2007 2012]; lane = 4; loc = 'MOD' ; caseNo = 'case9';
%searchRange = 5000; % meter
k = 1609.344;  % mile to meter conversion factor
receptor = 'facility'; % receptor choose facility or block
metFname = [loc '_Jday' num2str(jday) 'Hr' num2str(hr) 'sfc.txt' ]; % SAC_Jday138Hr10sfc check must be put in RLINE dir
runFolder = '..\RLINE_PRO\';  % the folder with RLINE program
link=readtable(['linkAttribute_link_' loc '_' caseNo '.csv']); % ID3 (from 1 to n),x1,y1,z1, x2,y2,z2, dCL, sigmaz0,laneNum,emis, 

recH = 1; % receptor height, aka, general human nose height
sourceH = 2.7;  % source height, aka, truck exhaust pipe
addpath(runFolder)

recAttrib = readtable([receptor 'Attrib_' loc '.csv']); % FacID/BlockID, x,y,z,population

%--- link location, add offset to z level
link.Z_begin=link.Z_begin+sourceH;  link.Z_end=link.Z_end+sourceH;
uniRecID=recAttrib.recID;

%--- emission

pm_per_link = repmat(link.Emis_g_m_s,1, length(vehMY)); %zeros(len,length(vehMY)); emission rate already in g/meter/sec
nox_per_link = repmat(link.Emis_g_m_s,1, length(vehMY)); %zeros(len,length(vehMY));
linkAttrib=link{:,:};
linkAttrib=[linkAttrib(:,1:18), pm_per_link,  nox_per_link]; % caution g/meter/sec =  g/mile/veh * 1 veh/hour * conv_fac

%--- source file format
%Group X_b          Y_b   Z_b     X_e   Y_e    Z_e     dCL  sigmaz0 #lanes                
% NB   0(Column 1)  0(2)  0.5(3)  0(4)  300(5) 0.5(6)  0(7) 2(8)  1(9)
% Emis        Hw1    dw1   Hw2   dw2    Depth  Wtop  Wbottom
% 1.2e-6(10)  0(11)  0(12) 0(13) 0(14)  0(15)  0(16) 0(17)

%--- preallocate
outConcCell=cell(length(uniRecID),1); outConcDim=zeros(length(uniRecID),2);
sourceHeader= ones(3,18);
conc_receptor_pm = zeros(length(uniRecID), 1);
conc_receptor_nox = zeros(length(uniRecID), 1);
receptorHeader= ones(3,3);

%{'This file contains receptor locations';    
%                'X_coordinate  Y_Coordinate  Z_Coordinate';
%                '----------------------------------------------'};

%rec_max_conc_pm = zeros(numel(uniRecID),length(vehMY));
%rec_max_conc_nox = zeros(numel(uniRecID),length(vehMY));

recIDin = uniRecID; % uniRecID(i1);  % caution 

%-----receptor file
recLoc=JoinTable(1,recIDin,1,[recAttrib.recID, recAttrib.X, recAttrib.Y, recAttrib.Z]);  % update 20200910 
recFname=[ loc '_' receptor 'ID'  '.txt']; % caution, num2str(facIDin), if not keeping intermediate files do not add facIDin
dlmwrite([runFolder recFname],[receptorHeader; recLoc(:,2:3) recLoc(:,4)+recH]) % control receptor height, caution

%----- prepare for inputs
linkList = link.ID; %SearchLinkInRange(recIDin, recAttrib, link, searchRange); %
linkPara = linkList; %JoinTable(1,linkList,1,linkAttrib);
source = linkPara(:,1:18);  % caution
pm_i = repmat(linkPara(:,11),1, length(vehMY)).*linkPara(:,19:(19+length(vehMY)-1));% caution,could be multiple columns, link based emission rate in g/m/s
nox_i = repmat(linkPara(:,11),1, length(vehMY)).*linkPara(:,(19+length(vehMY)): end);% caution,could be multiple columns, link based emission rate in g/m/s

%-----source file
sourceFname=[ loc '_Source.txt']; % caution, num2str(facIDin), if not keeping intermediate files do not add facIDin
dlmwrite([runFolder sourceFname],[sourceHeader; source]) % to simplify write out matrix here, caution

%-----output file name
outFname=[  loc '_' receptor 'Jday' num2str(jday) 'Hr' num2str(hr) 'conc.csv']; % caution

%% -----input triger file
inputCell=textread('Line_Source_Inputs_Template.txt','%s'); % read the input template,
%replace the file names and write out to destination folder
inputCell{3,1}=[ '"' sourceFname '"'];  %or strcat( '''', sourceFname, '''');
% but here the double quote works too, so avoid use as extra functions
inputCell{7,1}=[ '"' recFname '"'];
inputCell{10,1}=[ '"' metFname '"'];  %fac_displ is at 50th row
inputCell{13,1}=[ '"' outFname '"']; 
inputCell{44,1}=strcat('''', 'Y', '''', [' ' num2str(lane)] ); % 
ConvertToTxt([runFolder 'Line_Source_Inputs.txt'],inputCell);
    
    %----- run RLINE
    
     %[~, ~]=dos([runFolder 'RLINEv1_2_g95.exe']);
     cd ../RLINE_PRO
     system('RLINEv1_2_g95.exe');  % will show RLINE run smsg in matlab
     cd ../RunRLINE
    
    %% ----- read output and save into matrix
    outCell=readtable(outFname);
    
    conc = outCell(:, 7: end-1);
    conc_sum = sum(table2array(conc), 2);
    % find reference line, the line of Jday, to determine where the
    % concentration vector starts in the csv output file
    %refLine=FindStr([num2str(jday) ','] , outCell); % find reference line, the line of Jday
    %concRow=refLine(1)+5; % jday, hour, recX, recY, recZ, concentration 1, 2, ... n    
    %outConcText=outCell(concRow:end);
    outConcText = outCell.SOURCEFILE{end}; % locate the last row of the first colume to find the concentrations
    outConcVec = str2num(outConcText);  % when emis rate is 1 g/m/s, this conc need to be multiplied with real PM/NOx emis rate
    outConcVec = outConcVec(7:end)';
    if length(outConcVec)~= length(linkList) % check if the extraction matches
        display(['Dimension of extracted vector are mismatched for Facility# ' num2str(recIDin)])
    end
    %convert into txt because there is ',' after every number, outdated
    %because of better conversion method available
    %%ConvertToTxt('Cell2Matrix.txt',outConcText); 
    %%outConcMat=dlmread('Cell2Matrix.txt'); % output concentration matrix of a certain facility/block
    %%outConcMat=outConcMat(:,1);
    %%outConcChar = cell2mat(outConcText);  % character array
    %%outConcMat = str2num(outConcChar(:,1:end-1));  % cannot use str2double
    %======
    
    %im2Fac=outConcVec*17/24* facLoc(5);%.* pm_i./nox_i ;  % caution! inhalation mass of each facility, 17 m3/day, im = C*t*breathrate*numberofpopulation
    
    %im_receptor_pm = pop_i*(breathRate)'*outConcVec.* pm_i;   %breathing rate: m3/hour update 20200910, inhalation (ug) = pop x br x con  
    
    %im_receptor_nox = pop_i*(breathRate)'*outConcVec.*nox_i;  %update 20200910  beforeupdate outConcVec*17/24* recLoc(pop_col).*nox_i
    
    conc_receptor_pm(i1,1) = sum(outConcVec);
    
    conc_receptor_nox(i1,1) = sum(outConcVec);
   % rec_max_conc_pm(i1,:) = max(outConcVec.* pm_i);
   % rec_max_conc_nox(i1,:) = max(outConcVec.* nox_i);
    
%     for i2=1:length(linkList)
%         
%     IM_pm(linkList(i2),:)=IM_pm(linkList(i2),:)+ im_receptor_pm(i2,:); % need to aggregate by link    
%     IM_nox(linkList(i2),:)=IM_nox(linkList(i2),:)+ im_receptor_nox(i2,:); 
%     end
    
    %end
    
    % check if the total inhaled mass is less than the total emissions, the
    % ratio should be no smaller than 1
    mass_per_link_pm = pm_per_link.*link.METERS/k*10^6; % ug = g/mile * meter/1609.3*10E6  
    ratio_pm = mass_per_link_pm./IM_pm; 
    IM_pm(ratio_pm<1) = mass_per_link_pm(ratio_pm<1); 
    
    mass_per_link_nox = nox_per_link.*link.METERS/k*10^6; % ug = g/mile * meter/1609.3*10E6  
    ratio_nox = mass_per_link_nox./IM_nox; 
    IM_nox(ratio_nox<1) = mass_per_link_nox(ratio_nox<1); 
    
    % group sum here by TomTom network ID
    network_id = link.NetworkID;
    IM_nwid = table(network_id, IM_pm, IM_nox); % IM by network ID, output "IM_pm" will be in the sequence of the vehicle model year
    stat = grpstats(IM_nwid, 'network_id', 'sum');
    writetable(stat, ['im_' receptor '_hr' num2str(hr) '.csv']); 
    
    sum(ratio_pm<1)
    sum(ratio_nox<1)
    emis_mass = table(network_id, mass_per_link_pm, mass_per_link_nox); % in ug
    stat_emis_mass = grpstats(emis_mass, 'network_id', 'sum');
    writetable(stat_emis_mass, ['emis_mass_hr' num2str(hr) '.csv']);
%end