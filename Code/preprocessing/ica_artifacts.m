%
% Frequency analysis with FieldTrip
%

% Do not run this on local machine -- too heavy

%% Fieldtrip
addpath /gpfs/hpchome/etais/hpc_kuz/Software/fieldtrip
ft_defaults


%% Load data

% training set
datapath = '../../Data/raw/train/Data*';
datasize = 13669360 - 80;

listing = dir(datapath);
allsignals = zeros(datasize, 59);
to = 0;
for fid = 1:length(listing)
    disp(['Processing ', listing(fid).name])
    signals = csvread([fileparts(datapath) '/' listing(fid).name], 1);
    fr = to + 1;  % take the next row after the previous data inject
    to = fr + size(signals, 1) - 1;
    allsignals(fr:to, :) = signals;
end

trainfbtimes = find(allsignals(:, 59) == 1);
trainsignals = allsignals(:, 2:57)';

% test set
datapath = '../../Data/raw/test/Data*';
datasize = 7913300 - 52;

listing = dir(datapath);
allsignals = zeros(datasize, 59);
to = 0;
for fid = 1:length(listing)
    disp(['Processing ', listing(fid).name])
    signals = csvread([fileparts(datapath) '/' listing(fid).name], 1);
    fr = to + 1;  % take the next row after the previous data inject
    to = fr + size(signals, 1) - 1;
    allsignals(fr:to, :) = signals;
end

testfbtimes = find(allsignals(:, 59) == 1);
testsignals = allsignals(:, 2:57)';


%% Prepare data

signal = csvread('../../Data/raw/train/Data_S02_Sess01.csv', 1);
signal = signal(:, 2:57)';
%signal = trainsignals;

% data structure
raw.label = {'Fp1', 'Fp2', 'AF7', 'AF3', 'AF4', 'AF8', 'F7', 'F5', 'F3', 'F1', 'Fz', ...
             'F2', 'F4', 'F6', 'F8', 'FT7', 'FC5', 'FC3', 'FC1', 'FCz', 'FC2', 'FC4', ...
             'FC6', 'FT8', 'T7', 'C5', 'C3', 'C1', 'Cz', 'C2', 'C4', 'C6', 'T8', ...
             'TP7', 'CP5', 'CP3', 'CP1', 'CPz', 'CP2', 'CP4', 'CP6', 'TP8', 'P7', ...
             'P5', 'P3', 'P1', 'Pz', 'P2', 'P4', 'P6', 'P8', 'PO7', 'POz', 'PO8', 'O1', 'O2'};
raw.fsample = 200;
raw.time  = {(1:size(signal, 2)) / 200};
raw.trial = {signal};

% preprocess
cfg = [];
cfg.detrend = 'no';
cfg.fsample = 200;
preprocessed = ft_preprocessing(cfg, raw);


%% ICA
cfg        = [];
cfg.method = 'runica';
comp = ft_componentanalysis(cfg, preprocessed);

% store comp for visualization on local machine
%save('../../Data/clean/comp.mat', 'comp', '-v7.3')


%% Plot
%figure('Visible', 'off')
figure
cfg = [];
cfg.component = [1:56];
cfg.layout    = 'kagglebci.lay';
cfg.comment   = 'no';
ft_topoplotIC(cfg, comp)
%saveas(gcf, '../../Figures/ica/components41to56.fig', 'fig')

% Open it with
% openfig('../../Figures/ica/components01to20.fig', 'new', 'visible');

% draw stuff
%for i = 1:5
%    openfig(['../../Figures/ica/trace', num2str(1), '.fig'], 'new', 'visible');
%end


%% Another plot
cfg = [];
cfg.layout = 'kagglebci.lay';
cfg.viewmode = 'component';
ft_databrowser(cfg, comp)






%% Remove