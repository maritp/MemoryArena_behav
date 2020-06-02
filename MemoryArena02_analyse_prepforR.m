% ANALYSIS MEMORY ARENA

% analysis of sequence and placement distance & prep for R

clear
close all

home = 1;
dataset = 50; % 1 == memarena V1 (int70) | 50 == memarena V2 (int50)

if home 
    addpath(genpath('/Users/user/Documents/GitHub/MemoryArena_behav'))
else
    addpath(genpath('C:\Users\Petzkam\GitHub\MemoryArena_behav'))
end

%%

[paths subj] = getting_paths(home, dataset);

%%
cd(paths.dat)
load('memArena_dvs')

%% getting averages across participants for sequence and placement distance

dvs = struct;

for isub = 1:numel(subj)

    for iret = 1:2 % just first two retrieval sessions (without interference)
        
        % (1) sequence

        dvs.seq_(isub,iret) = mean(diff(seq_.ret{isub}(iret,:)) == 1) *100;
        
        %% (2) placement distance
        
        dvs.pldist(isub,iret) = mean(dist_.pldist{isub,iret});
        
        if iret == 1
           pldist(isub,:) = dist_.pldist{isub,iret}; % contains single objects
        end

    end
 
end


%% preparation for R

% reshape variables into long format
dvs.perf = perf_.ret(:,1:2)*100;
varOI = {'perf' 'seq_' 'pldist'};

memdat = [];
for i = 1:numel(varOI)
    
    memdat(:,i) = reshape(dvs.(varOI{i}),[],1);
    
end

time = vertcat(ones(numel(subj),1), ones(numel(subj),1)+1);
cond_ = [dist_.cond'; dist_.cond']; % con_ = 4 conditions: sleepnoINT, sleepINT, wakenoINT, wakeINT
del_ = cond_; % factor delay
del_(del_ == 1 | del_ == 2) = 0;
del_(del_ == 3 | del_ == 4) = 1;

int_ = cond_; % factor retrieval difficulty
int_(int_ == 1 | int_ == 3) = 0;
int_(int_ == 2 | int_ == 4) = 1;


% merge variables to dataframe
memdat = [memdat cond_ del_ int_ time];

%% save
cd(paths.dat)
if ~isfolder('prepR')
    mkdir 'prepR'
end

cd 'prepR'
dlmwrite('memdat.txt', memdat,'delimiter', '\t'); % save as a txt file (without header)

