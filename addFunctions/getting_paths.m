% MEMORY ARENA.
% paths

function [x, y] = getting_paths(home, dataset)

if home 
    pre_ = '/Volumes/MEMTOSH/';
else
    pre_ = 'Z:\staresib-01\';
end


addpath(genpath([pre_, 'staresib-01', filesep, 'MemoryArena_behav_data']))

x.rawdat = [pre_, 'MemoryArena_behav_data', filesep, 'rawdata', num2str(dataset), filesep];
x.maindir = [pre_, 'MemoryArena_behav_data', filesep];
x.dat = [pre_, 'MemoryArena_behav_data', filesep, 'results', num2str(dataset), filesep];

cd(x.rawdat)

y = dir('s*');

