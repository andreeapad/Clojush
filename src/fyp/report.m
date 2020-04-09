%% Import data from text file.
%% Initialize variables.
file = '01-04-2020 generator_gan_seq6_150_d';
filename = strcat('C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\',file, '.csv');
delimiter = ',';
startRow = 2;

%% Format for each line of text:
formatSpec = '%*s%f%f%[^\n\r]';

%% Open the text file.
fileID = fopen(filename,'r');

%% Read columns of data according to the format.
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');

%% Close the text file.
fclose(fileID);

%% Create output variable
generatorlog1 = [dataArray{1:end-1}];

%% Clear temporary variables
clearvars filename delimiter startRow formatSpec fileID dataArray ans;

%%  Generate graph for pushgp run
populationSize = 1000;
[rows,cols] = size(generatorlog1)
generations = rows/populationSize;
X = 0: generations-1;
Y = ones(generations, 1);
Z = ones(generations, 1);
M = ones(generations, 1);
B = ones(generations, 1);
P = ones(generations, 1000);

% Calculate mean, std and min for population at each generation
for i=1:generations
    Y(i) = mean(generatorlog1((1+populationSize*(i-1)):(populationSize*i),2));
    Z(i) = std(generatorlog1((1+populationSize*(i-1)):(populationSize*i),2));
    B(i) = min(generatorlog1((1+populationSize*(i-1)):(populationSize*i),2));
end

%% Plot generational error statistics

%subplot(3,1,1);
subplot(2,1,1);
plot(X, Y,'DisplayName','mean')
hold on
plot(X, Z,'DisplayName','std')
legend
title('Discriminator average error and standard deviation')
xlabel('Generation')
ylabel('Error')

%subplot(3,1,2);
subplot(2,1,2);
plot(X, B,'DisplayName','best')
legend
ylim([0 20])
xlim([0 generations-1])
title('Discriminator best individual''s error')
xlabel('Generation')
ylabel('Error')

%% Plot entire population
%subplot(3,1,3);
%scatter(generatorlog1(:, 1), generatorlog1(:, 2), 0.6);
%boxplot(generatorlog1(:, 2), generatorlog1(:, 1), 'PlotStyle','compact');
%title('Generator population error')
%xlabel('Generation')
%ylabel('Error')

%% Save image
saveLocation = 'C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\graphs\';
fileName = strcat(saveLocation, file,'.png')
saveas(gcf, fileName);
