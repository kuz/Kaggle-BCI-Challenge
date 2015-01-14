%
%  Pipeline for semi-manual eye artifact removal
%

%
% Training data
% -----------------------
% 'S02', 711010 - 5, [43]
% 'S06', 725210 - 5, [53]
% 'S07', 744410 - 5, [35]
% 'S11', 834810 - 5, [37]
% 'S12', 848210 - 5, [20]
% 'S13', 850610 - 5, [9]
% 'S14', 838610 - 5, [13]
% 'S16', 860210 - 5, [23]
% 'S17', 913210 - 5, [40]
% 'S18', 870410 - 5, [25]
% 'S20', 886610 - 5, [4]
% 'S21', 910010 - 5, [39]
% 'S22', 897210 - 5, [33]
% 'S23', 910010 - 5, [48]
% 'S24', 921210 - 5, [30]
% 'S26', 947610 - 5, [34]
%
% Test data
% -----------------------
% 'S01', 686010 - 5, []
% 'S03', 750010 - 5, []
% 'S04', 709010 - 5, []
% 'S05', 754410 - 5, []
% 'S08', 773410 - 5, []
% 'S09', 775810 - 5, []
% 'S10', 788810 - 5, []
% 'S15', 833810 - 5, []
% 'S19', 892810 - 5, []
% 'S25', 949210 - 5, []
%

subject = 'S01';
datasize = 686010;
dataset = 'test';


%% Load data
load(['../../Data/clean/icacomp/ica', subject, 'comp.mat'])


%% Show topomat and signal browser

% topomap
cfg = [];
cfg.component = [1:56];
cfg.layout    = 'kagglebci.lay';
cfg.comment   = 'no';
ft_topoplotIC(cfg, comp)
%openfig(['../../Figures/ica/ica', subject, 'topomaps.fig'], 'new', 'visible');

% signal browswer
cfg = [];
cfg.layout = 'kagglebci.lay';
cfg.viewmode = 'component';
ft_databrowser(cfg, comp)

% Your task here is to look at the pictures and decide which componenets
% you want to reject. Enlist than in the next code section in the
% cfg.component variable

%% Reject components with artifacts

% load original data
datapath = ['../../Data/raw/', dataset, '/Data_', subject, '*'];
listing = dir(datapath);
allsignals = zeros(datasize, 59);
sessions   = zeros(datasize, 1);
to = 0;
for fid = 1:length(listing)
    disp(['Reading ', listing(fid).name])
    signals = csvread([fileparts(datapath) '/' listing(fid).name], 1);
    fr = to + 1;  % take the next row after the previous data inject
    to = fr + size(signals, 1) - 1;
    allsignals(fr:to, :) = signals;
    sessions(fr:to, :) = ones(length(fr:to), 1) * fid;
end
fbtimes = find(allsignals(:, 59) == 1);
signal = allsignals(:, 2:57)';

% build FieldTrip data structure
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

% reject the components and store the update data
cfg = [];
cfg.component = [];  % to be removed component(s)
data = ft_rejectcomponent(cfg, comp, preprocessed);


%% Store as CSV file
% in the same format as the original data so that we can reuse all the
% extraction scripts we have
headers = {'Time','Fp1','Fp2','AF7','AF3','AF4','AF8','F7','F5','F3','F1','Fz','F2', ...
           'F4','F6','F8','FT7','FC5','FC3','FC1','FCz','FC2','FC4','FC6','FT8','T7', ...
           'C5','C3','C1','Cz','C2','C4','C6','T8','TP7','CP5','CP3','CP1','CPz','CP2', ...
           'CP4','CP6','TP8','P7','P5','P3','P1','Pz','P2','P4','P6','P8','PO7','POz', ...
           'P08','O1','O2','EOG','FeedBackEvent'};
cleaned = [allsignals(:, 1), data.trial{1}', allsignals(:, 58:59), sessions];
for s = unique(sessions)'
    disp(['Storing session ', num2str(s)])
    sessdata = cleaned(cleaned(:, 60) == s, 1:59);
    csvwrite_with_headers(...
        ['../../Data/clean/', dataset, '/Data_', subject, '_Sess0', num2str(s), '.csv'], ...
        sessdata, headers);
end










