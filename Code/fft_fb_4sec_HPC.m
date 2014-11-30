%
% Frequency analysis with FieldTrip, extract only data in timeframe
% [feedback + 4sec]
%

%% Add FieldTrip path
addpath /gpfs/hpchome/etais/hpc_kuz/Software/fieldtrip
ft_defaults

%% Directory listing
listing = dir('../Data/train/Data*');

% Prepare matrix to store data from all files, wc -l * will give out the
% total number of lines in every file, there are 13669360 - 80 lines
allsignals = zeros(13669360 - 80, 59);

% home version -- managable on a laptop
%allsignals = zeros(711010 - 5, 59); 


%% Load data
disp('Loading data ...')
to = 0;
for fid = 1:length(listing)
    disp(['Processing ', listing(fid).name])
    signals = csvread(['../Data/train/' listing(fid).name], 1);
    fr = to + 1;  % take the next row after the previous data inject
    to = fr + size(signals, 1) - 1;
    allsignals(fr:to, :) = signals;
end

fbtimes = find(allsignals(:, 59) == 1);
signals = allsignals(:, 2:57)';

labels = csvread('../Data/TrainLabels.csv', 1, 1);


%% FFT with FieldTrip
disp('Performing FFT ...')

% parameters
winsize = 1.0;
winstep = 1.0;

% data structure
clear raw
raw.fsample = 200;
raw.label = {};
raw.time = {};
raw.trial = {};
for fbt = fbtimes'
    signal = signals(:, fbt:(fbt + 799));
    raw.label = arrayfun(@(x) sprintf('%d',x), 1:size(signal, 1), 'uni', false);
    raw.time{end + 1}  = (1:size(signal, 2)) / 200;
    raw.trial{end + 1} = signal;
end

% preprocess
cfg = [];
cfg.detrend = 'no';
cfg.fsample = 200;
preprocessed = ft_preprocessing(cfg, raw);

% configure time-frequency transform
cfg              = [];
cfg.method       = 'mtmconvol';
cfg.taper        = 'hanning';
cfg.keeptrials   = 'yes';
cfg.foi          = 2:1:97;
cfg.t_ftimwin    = ones(length(cfg.foi), 1) .* winsize;
cfg.toi          = (winsize/2):winstep:(floor(length(preprocessed.time{1}) / preprocessed.fsample) - winsize/2);

% perform the transform
frequencies = ft_freqanalysis(cfg, preprocessed);


%% Glue into instances
disp('Composing dataset ...')
ntrials = size(frequencies.powspctrm, 1);
nchanls = size(frequencies.powspctrm, 2);
nfreqs  = size(frequencies.powspctrm, 3);
nchunks = size(frequencies.powspctrm, 4);

instances = zeros(ntrials * nchunks, nchanls * nfreqs + 1);
for t = 1:ntrials
    for c = 1:nchunks
        x = squeeze(frequencies.powspctrm(t, :, :, c));
        y = reshape(x', 1, nchanls * nfreqs);
        instances((t - 1) * nchunks + c, :) = [y labels(t)];
    end
end


%% Store the file
disp('Storing the dataset ...')
csvwrite('../Data/FFT Matlab/train_fft_win1_step1.csv', instances);

disp('All done.')
















