%
% Frequency analysis with FieldTrip
%   - extract data in timeframe [feedback + 4sec]
%   - take is as one window for FFT
%   - run PCA on trianing data
%   - extract test data in the same way
%   - use previously obtained rotation matrix on test data
%


% The main function which produces FFT-PCA-transformed treainig and test
% datasets
%
% @param debug: set to true when debugging on you laptop
% @return: none
function fft_fb4sec_win4s(debug, winsize, winstep)
    if ~debug
        addpath /gpfs/hpchome/etais/hpc_kuz/Software/fieldtrip
        ft_defaults
    end

    % load data
    disp('Loading data ...')

    if debug
        % take only one test subject
        traindatapath = '../../Data/train/Data_S02_Sess01*';
        trainsize = 132002 - 1;
        %trainsize = 711010 - 5;
        testdatapath = '../../Data/test/Data_S01_Sess01*';
        testsize = 127402 - 1;
        %testsize = 686010 - 5;
    else
        % take all data
        traindatapath = '../../Data/train/Data*';
        trainsize = 13669360 - 80;
        testdatapath = '../../Data/test/Data*';
        testsize = 7913300 - 52;
    end
    
    % read signals
    [trainsignal, trainfbtimes] = readsignal(traindatapath, trainsize);
    [testsignal, testfbtimes]   = readsignal(testdatapath, testsize);
    
    % perform FFT
    trainfreqs = dofft(trainsignal, trainfbtimes, winsize, winstep);
    testfreqs  = dofft(testsignal, testfbtimes, winsize, winstep);
    
    % build dataset
    trainlabels = csvread('../../Data/TrainLabels.csv', 1, 1);
    traininstances = buildset(trainfreqs, trainlabels);
    testinstances = buildset(testfreqs, NaN);
        
    % store full-featured dataset
    disp('Storing full datasets ...')
    csvwrite(['../../Data/FFT Matlab/train_fft_fb4sec_win' num2str(winsize) ...
              '_step' num2str(winstep) '.csv'], traininstances);
    csvwrite(['../../Data/FFT Matlab/test_fft_fb4sec_win' num2str(winsize) ...
              '_step' num2str(winstep) '.csv'], testinstances);
    
    % perform PCA
    keepvar = 0.99;
    [trainpcaset, rotmat, keep] = dopca(traininstances, keepvar);
    testpcaset = applypca(testinstances, rotmat, keep);
        
    disp('Storing PCA datasets ...')
    csvwrite(['../../Data/FFT Matlab/train_fft_fb4sec_win' num2str(winsize) ...
              '_step' num2str(winstep) '_pca' num2str(keepvar * 100) '.csv'], trainpcaset);
    csvwrite(['../../Data/FFT Matlab/test_fft_fb4sec_win' num2str(winsize) ...
              '_step' num2str(winstep) '_pca' num2str(keepvar * 100) '.csv'], testpcaset);

    % all done
    disp('All Done.')
end


% Read all data from data files
%
% @param datapath: relative path to datafiles with a wildcard
% @param datasize: total number of rows in the files (TODO: automate)
% @return signals: huge matrix will add raw data in
% @return fbtimes: time moments of feedback events
function [signals, fbtimes] = readsignal(datapath, datasize)
    listing = dir(datapath);
    allsignals = zeros(datasize, 59);
    
    % Read in training data
    to = 0;
    for fid = 1:length(listing)
        disp(['Processing ', listing(fid).name])
        signals = csvread([fileparts(datapath) '/' listing(fid).name], 1);
        fr = to + 1;  % take the next row after the previous data inject
        to = fr + size(signals, 1) - 1;
        allsignals(fr:to, :) = signals;
    end

    fbtimes = find(allsignals(:, 59) == 1);
    signals = allsignals(:, 2:57)';
end


% Perform FFT using FieldTrip
%
% @param signals: matrix where rows are time moments and columns are
%                 channels
% @param fbtime: time moment when feedback event happened
% @param winsize: size of FFT window
% @param winstep: step of FFT window
% @return frequences: FieldTrip object with the results of FFT
function frequencies = dofft(signals, fbtimes, winsize, winstep)
    disp('Performing FFT ...')

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
    cfg.t_ftimwin    = ones(length(cfg.foi), 1) .* (winsize - 1/200);
    cfg.toi          = (winsize/2):winstep:(floor(length(preprocessed.time{1}) / preprocessed.fsample) - winsize/2);

    % perform the transform
    frequencies = ft_freqanalysis(cfg, preprocessed);
end


% Build a dataset from the processed data and list of labels
%
% @param frequencies: FieldTrip object with the result of FFT
% @param labels: vector with class labels
% @return instances: matrix where one rows are instance, columns
%                    are features and the last column is class
function instances = buildset(frequencies, labels)
    disp('Composing dataset ...')
    ntrials = size(frequencies.powspctrm, 1);
    nchanls = size(frequencies.powspctrm, 2);
    nfreqs  = size(frequencies.powspctrm, 3);

    if isnan(labels)
        instances = zeros(ntrials, nchanls * nfreqs);
    else
        instances = zeros(ntrials, nchanls * nfreqs + 1);
    end
    for t = 1:ntrials
        x = squeeze(frequencies.powspctrm(t, :, :));
        y = reshape(x', 1, nchanls * nfreqs);
        if isnan(labels)
            instances(t, :) = y;
        else
            instances(t, :) = [y labels(t)];
        end
    end
end


% Perform PCA on full-featured dataset
%
% @param instances: dataset to transform (with last column having labels)
% @param keepvar: % of variance to keep, something like 0.99
% @return pcainstances: the transformed dataset
% @return rotmat: rotation matrix to transform from the feature space
%                 to PCA space
% @return keep: number of PC to keep in resulting dataset
function [pcainstances, rotmat, keep] = dopca(instances, keepvar) 
    disp('Performing PCA ...')
    features = instances(:, 1:end-1);
    [rotmat, score, variance] = pca(features);

    cv = cumsum(variance / sum(variance));
    keep = min(find(cv >= keepvar));

    pcainstances = [score(:, 1:keep) instances(:, end)];
end


% Apply PCA given rotation matrix
%
% @param features: dataset to apply PCA to. Note this one is features only
% @param rotmat: the rotation matrix
% @param keep: number of component to keep
% @return pcainstances: 
function pcainstances = applypca(features, rotmat, keep)
    disp('Applying PCA ...')
    
    mu = mean(features);
    features = bsxfun(@minus, features, mu);
    pcainstances = features * rotmat;
    
    pcainstances = pcainstances(:, 1:keep);
end


% Small tutorial on PCA
% http://stackoverflow.com/questions/12688312/matlab-pca-analysis-and-reconstruction-of-multi-dimensional-data
%
% Define a dataset
% a = [1, 4, 3, 5, 2, 5; 1, 3, 7, 2, 5, 2; 2, 4, 1, 2, 6, 3; 7, 4, 3, 2, 2, 7;]
%
% Perform PCA
% [rotmat, score, variance] = pca(a)
%
% Apply PCA to a data
% mu = mean(a)
% a_hat = bsxfun(@minus, a, mu)
% a_hat * rotmat
% Note that this is the same as 
% score
% 
% Reconstruct demeaned data back from PC (to believe)
% a_hat_rec = score * rotmat'
% And compare it to
% a_hat
% Magic!
%








