%% read data 
chredlin = readtable('chredlin.csv');
head(chredlin)
%% drop the two influential observations
chredlin([6,24],:) = [];
%% fit a regression without `theft` and `age`
y = chredlin(:,6);
y = table2array(y);
X = chredlin(:,[2,3,7]);
X = table2array(X);
X(:,3) = log(X(:,3));
lm2 = fitlm(X,y)
% notice that now `race` is not significant anymore!