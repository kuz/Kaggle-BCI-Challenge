%
% Produce ICA maps for each subject
%

% load Fieldtrip
addpath /gpfs/hpchome/etais/hpc_kuz/Software/fieldtrip
ft_defaults

% list of subjects
trainsubjects = {'S02', 'S06', 'S07', 'S11', 'S12', 'S13', 'S14', 'S16', ...
                 'S17', 'S18', 'S20', 'S21', 'S22', 'S23', 'S24', 'S26'};
trainsizes    = {711010 - 5, 725210 - 5, 744410 - 5, 834810 - 5, 848210 - 5, ...
                 850610 - 5, 838610 - 5, 860210 - 5, 913210 - 5, 870410 - 5, ...
                 886610 - 5, 910010 - 5, 897210 - 5, 910010 - 5, 921210 - 5, ...
                 947610 - 5};

testsubjects = {'S01', 'S03', 'S04', 'S05', 'S08', 'S09', 'S10', 'S15', 'S19', 'S25'};
testsizes    = {686010 - 5, 750010 - 5, 709010 - 5, 754410 - 5, 773410 - 5, ...
                775810 - 5, 788810 - 5, 833810 - 5, 892810 - 5, 949210 - 5};

% parameters
subjects = testsubjects;
sizes    = testsizes;
dataset  = 'test';

% loop over subject and produce topomap pictures of ICAs
for s = 1:length(subjects)
    
    % specify data source
    datapath = ['../../Data/raw/', dataset, '/Data_', subjects{s}, '*'];
    datasize = sizes{s};
    
    % read data
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
    
    signal = trainsignals;
    
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

    % ICA
    cfg        = [];
    cfg.method = 'runica';
    comp = ft_componentanalysis(cfg, preprocessed);
    save(['../../Data/clean/icacomp/ica', subjects{s}, 'comp.mat'], 'comp')
    
    % plot and store
    figure('Visible', 'off')
    cfg = [];
    cfg.component = [1:56];
    cfg.layout    = 'kagglebci.lay';
    cfg.comment   = 'no';
    ft_topoplotIC(cfg, comp)
    drawnow
    saveas(gcf, ['../../Figures/ica/ica', subjects{s}, 'topomaps.fig'], 'fig')
    
end

%
% See 'ica_manual_reject.m' for the next step
% 