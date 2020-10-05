% MEMORY ARENA.
% paths

function [x, y] = getting_paths(a, b)

x.rawdat = [a, filesep, 'rawdata', num2str(b), filesep];
x.results = [a, filesep, 'results', num2str(b), filesep];

cd(x.rawdat)

y = dir('s*');

