%
% Frequency analysis with FieldTrip
%


%% Load data
signal = csvread('../../Data/raw/train/Data_S02_Sess01.csv', 1);
signal = signal(:, 2:57)';


%% Prepare data

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


%% Plot
figure
cfg = [];
cfg.component = [41:56];       % specify the component(s) that should be plotted
cfg.layout    = 'kagglebci.lay';
cfg.comment   = 'no';
ft_topoplotIC(cfg, comp)


%% Another plot
cfg = [];
cfg.layout = 'kagglebci.lay';
cfg.viewmode = 'component';
ft_databrowser(cfg, comp)

%% Remove