% ANALYSIS MEMORY ARENA

% analysis of sequence and placement distance & prep for R

clear
close all

dataset = 70; % 70 == memarena V1 (int70) | 50 == memarena V2 (int50)

addpath(genpath('/Users/user/Documents/GitHub/MemoryArena_behav'))
datadir = '/Volumes/MEMTOSH/MemoryArena_behav_data';
%%

[paths, subj] = getting_paths(datadir, dataset);

%%
cd(paths.results)
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
        
        %% overlap & sequence (position)
        dvs.seqcorr_ret(isub,iret) = sum(seq_.ret{isub}(iret,:) - [1:20] == 0);
        dvs.pldistcorr_ret(isub,iret) = sum(dist_.retoverlap{isub, iret} >= 25); % 25 percent so that overlap was scored as correct

    end
    
    %% duration of enc+train & ntrianingrounds
    
    dvs.ntrain(isub) = dur_.ntrainrounds(isub);
    dvs.dur_enc(isub) = dur_.enc(isub,1);
    dvs.dur_train(isub) = sum(dur_.train(isub,:));
    

end


%% preparation for R

% reshape variables into long format
dvs.perf = perf_.ret(:,1:2)*100;
varOI = {'perf' 'seq_' 'pldist' 'dur_enc' 'dur_train' 'ntrain'};

memdat = [];
for i = 1:numel(varOI)
    
    if ismember(i,[1:3]) % 1-3 --> variable with ret 1 and 2 performance
        memdat(:,i) = reshape(dvs.(varOI{i}),[],1); 
    else % ret 2 doesnt exist -- var has to save twice to fit in the long format
        var_tmp = reshape(dvs.(varOI{i}),[],1);
        memdat(:,i) = [var_tmp; var_tmp];
    end
    
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
cd(paths.results)
if ~isfolder('prepR')
    mkdir 'prepR'
end

cd 'prepR'
dlmwrite('memdat.txt', memdat,'delimiter', '\t'); % save as a txt file (without header)

