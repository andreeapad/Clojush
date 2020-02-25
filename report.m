%% Import data from text file.
%% Initialize variables.
filename = 'C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\25-02-2020 16-37 discr_sigmoid_300gen.csv';
delimiter = ',';
startRow = 2;

%% Format for each line of text:
%   column2: double (%f)
%	column3: double (%f)
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



% Calclulate mean error for population at each generation and store in Y
for i=1:generations
    disp(i)
    Y(i) = mean(generatorlog1((1+populationSize*(i-1)):(populationSize*i),2));
    Z(i) = std(generatorlog1((1+populationSize*(i-1)):(populationSize*i),2));
    M(i) = median(generatorlog1((1+populationSize*(i-1)):(populationSize*i),2));
    B(i) = min(generatorlog1((1+populationSize*(i-1)):(populationSize*i),2));

end

% Plot X = generation against Y = mean population error

%plot(X(1:50), Y(1:50));
%plot(X(:,10:end), Y(10:end, :))

subplot(2,1,1);
plot(X, Y,'DisplayName','mean')
hold on
plot(X, Z,'DisplayName','std')
legend
title('Discriminator average error and standard deviation')
xlabel('Generation')
ylabel('Error')

subplot(2,1,2);
plot(X, B,'DisplayName','best')
legend
title('Discriminator best individual''s error')
xlabel('Generation')
ylabel('Error')

saveLocation = 'C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\graphs\'
%fileName = strcat(saveLocation, datestr(now, 'dd-mmm-yy HH-MM'), ' discr_sigmoid.png')

fileName = strcat(saveLocation, '25-02-2020 16-37 discr_sigmoid_300gen.png')
saveas(gcf, fileName)


