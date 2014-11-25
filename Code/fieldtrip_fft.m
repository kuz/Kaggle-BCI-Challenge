%
% Frequency analysis with FieldTrip
%


%% Load data
signal = csvread('../Data/train/Data_S02_Sess01.csv', 1);
signal = signal(:, 2:57)';


%% FFT with FieldTrip

% parameters
winsize = 1.0;
winstep = 1.0;

% data structure
raw.label = arrayfun(@(x) sprintf('%d',x), 1:size(signal, 1), 'uni', false);
raw.fsample = 200;
raw.time  = {(1:size(signal, 2)) / 200};
raw.trial = {signal};

% preprocess
cfg = [];
cfg.detrend = 'no';
cfg.fsample = 200;
preprocessed = ft_preprocessing(cfg, raw);

% configure time-frequency transform
cfg              = [];
cfg.method       = 'mtmconvol';
cfg.taper        = 'hanning';
cfg.foi          = 1:1:100;
cfg.t_ftimwin    = ones(length(cfg.foi), 1) .* winsize;
cfg.toi          = (winsize/2):winstep:(floor(length(preprocessed.time{1}) / preprocessed.fsample) - winsize/2);

% perform the transform
frequencies = ft_freqanalysis(cfg, preprocessed);


%% Glue into instances
instances = zeros(size(frequencies.powspctrm, 3), size(frequencies.powspctrm, 1) * size(frequencies.powspctrm, 2));
for s = 1:size(frequencies.powspctrm, 3)
    x = frequencies.powspctrm(:, :, s)';
    y = reshape(x, 1, 5600);
    y(find(y > 100)) = 0;
    instances(s, :) = y;
end

%% Store the file
csvwrite('../Data/FFT Matlab/Data_S02_Sess01_fft_win1_step1.csv', instances);





