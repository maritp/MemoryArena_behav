% ANALYSIS MEMORY ARENA

% extracting information about sequence, spatial error and
% overall performance

clear
close all

home = 1;
dataset = 50; % 1 == memarena training threshold 70 | 50 == memarena training threshold 50

if home 
    addpath(genpath('/Users/user/Documents/GitHub/MemoryArena_behav'))
else
    addpath(genpath('C:\Users\Petzkam\GitHub\MemoryArena_behav'))
end

%%
[paths, subj] = getting_paths(home, dataset); 

%% independent & dependent variables
condlabel = {'sleep' 'sleep_int' 'wake' 'wake_int'};
dvlabel = {'dist_' 'seq_' 'perf_'};
%%
for isub = 1:numel(subj) 
    
    cd([paths.rawdat, subj(isub).name, filesep, 'session01'])
    
    %--------------- getting the condition (using timestamp in filename of test 2)
    test = dir('*testtaskrepeat*');
    in = dir('*interference*');
    idx_h = strfind(test.name, 'h');
    t = test.name(idx_h-2:idx_h); %getting hour of timestamp
    
    if (strcmpi(t(1), '_') == 1 || strcmpi(t(1), '1') == 1) && isempty(in) % sleep NOint
        cond_(isub) = 1;
    elseif (strcmpi(t(1), '_') == 1 || strcmpi(t(1), '1') == 1) && ~isempty(in) % sleep INT
        cond_(isub) = 2;
    elseif strcmpi(t(1), '_') == 0 && isempty(in) %wake NOint
        cond_(isub) = 3;
    elseif strcmpi(t(1), '_') == 0 && ~isempty(in) % wake INT
        cond_(isub) = 4;
    end

    %% -------------------------------------- TRAINING --------------------------------------------
    trainfiles = dir('*_trainingtask_*');

    for itrain = 1:numel(trainfiles)
        
        traindat = load(fullfile(pwd,trainfiles(itrain).name));
        
        %% overall performance
        perf_.train(isub,itrain) = traindat.output.performance;
        
        %% sequence
        SubOrd = traindat.output.SubjectObjectOrder;
        
        if dataset == 50 && isub == 44 && itrain == 6  % last item (20) wasnt logged
            SubOrd(20) = 20;
        end
        
        seq_.train{isub}(itrain,:) = SubOrd;
        
        %% ------------ placement performance ------------------
        
        % preparation of subjective location (SubLoc) = correction for sequence error 
        SubLoc_orig = traindat.output.objectPositions; % position of placed objects (in subjective order!)
        
        if dataset == 50 && isub == 44 && itrain == 6 % last item (20) wasnt logged
            SubLoc_orig(20,:) = NaN; 
        end
        
        % correct location for sequence error
        SubLoc = nan(numel(SubOrd),2); % set objects where feedback about its location was given to NaN
        ObLoc = [traindat.output.correctPositionsX; traindat.output.correctPositionsY]'; % correct locations
        
        for iob = 1:numel(SubOrd)
            
            idx_last = find(SubOrd==iob, 1, 'last'); % get the location of the object when it was selected for the last time
            
            if ~isempty(idx_last)
               SubLoc(iob,:) = SubLoc_orig(idx_last,:); 
            end
            
        end
        
        dist_.trainLocOb{isub,itrain} = ObLoc;
        dist_.trainLocSub{isub,itrain} = SubLoc;
        dist_.trainLocSub_uncorr{isub,itrain} = SubLoc_orig; % uncorrected for sequence error
        
        %% placement distance
        
        for iob = 1:size(SubLoc,1)
            X = [];
            X = [SubLoc(iob,:); ObLoc(iob,:)];
            dist_.trainpldist{isub,itrain}(iob) = pdist(X,'euclidean'); 
        end
        
    end
    
    %% -------------------------------------- RETRIEVAL --------------------------------------------
    
    retfiles = dir('*_test*');
    
    if numel(retfiles) > 2 % resort to ret1, ret2, retInt 
        retfiles([1,2,3]) = retfiles([2,3,1]);
    end
    
    label_r = {'ret1' 'ret2' 'retInt'};
 
    for iret = 1:numel(retfiles)
        
        retdat = load(fullfile(pwd, retfiles(iret).name));
        
        %% overall performance
        perf_.ret(isub,iret) = retdat.output.performance;
        
        %% sequence
        SubOrd = retdat.output.SubjectObjectOrder;
        seq_.ret{isub}(iret,:) = SubOrd;
        
        %% ------------ placement performance ------------------
        
        % preparation of subjective location (SubLoc) = correction for sequence error 
        SubLoc_orig = retdat.output.objectPositions;
        
        % correct location for sequence error 
        [~, idx_] = ismember([1:20], SubOrd);
        SubLoc = SubLoc_orig(idx_,:);
        
        ObLoc = [retdat.output.correctPositionsX; retdat.output.correctPositionsY]';
        ObLoc_tmp = ObLoc(SubOrd,:);
        
        dist_.retLocOb{isub,iret} = ObLoc;
        dist_.retLocSub{isub,iret} = SubLoc;
        dist_.retLocSub_uncorr{isub,iret} = SubLoc_orig; % uncorrected for sequence error

        %% placement distance

        for iob = 1:size(SubLoc,1)
            X = [];
            X = [SubLoc(iob,:); ObLoc(iob,:)];
            dist_.pldist{isub,iret}(iob) = pdist(X,'euclidean'); 
        end

    end
     
end 

%% add additional information to dv struct
for idv = 1:numel(dvlabel)
    
    eval([dvlabel{idv} '.retlabel = label_r;'])
    eval([dvlabel{idv} '.cond = cond_;'])
    eval([dvlabel{idv} '.condlabel = condlabel;'])
    
end

%% save
cd(paths.maindir)

if ~isfolder(['results', num2str(dataset)])
    mkdir(['results', num2str(dataset)])
end

cd(['results', num2str(dataset)])
save('memArena_dvs', 'seq_', 'perf_', 'dist_')
