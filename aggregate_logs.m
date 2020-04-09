%% Import data from text files
files = 6;

file = '13-03-2020 generator_gan_seq12_50x1';
filename = strcat('C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\',file, '.csv');
delimiter = ',';
startRow = 2;
formatSpec = '%*s%f%f%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
fclose(fileID);
exp1 = [dataArray{1:end-1}];

file = '17-03-2020 generator_gan_seq12_50x2';
filename = strcat('C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\',file, '.csv');
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
fclose(fileID);
exp2 = [dataArray{1:end-1}];
clearvars filename  fileID dataArray ans;

file = '17-03-2020 generator_gan_seq12_50x1_1';
filename = strcat('C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\',file, '.csv');
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
fclose(fileID);
exp3 = [dataArray{1:end-1}];
clearvars filename  fileID dataArray ans;

file = '18-03-2020 generator_gan_seq12_50x1_2';
filename = strcat('C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\',file, '.csv');
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
fclose(fileID);
exp4 = [dataArray{1:end-1}];
clearvars filename  fileID dataArray ans;

file = '26-03-2020 generator_gan_seq12_100';
filename = strcat('C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\',file, '.csv');
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
fclose(fileID);
exp5 = [dataArray{1:end-1}];


file = '27-03-2020 generator_gan_seq12_50';
filename = strcat('C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\',file, '.csv');
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string', 'EmptyValue', NaN, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
fclose(fileID);
exp6 = [dataArray{1:end-1}];

%% Clear temporary variables
clearvars filename delimiter startRow formatSpec fileID dataArray ans;

%%  
populationSize = 1000;
[rows,cols] = size(exp1)
generations = 50
X = 0: generations-1;
Y = zeros(generations, files);

% Calculate mean, std and min for population at each generation
for i=1:generations
    [r,c] = size(exp1);
    gen = r/populationSize;
    if gen >= i
        Y(i, 1) = min(exp1((1+populationSize*(i-1)):(populationSize*i),2)); 
    end
    
    [r,c] = size(exp2);
    gen = r/populationSize;
    if gen >= i
         Y(i, 2) = min(exp2((1+populationSize*(i-1)):(populationSize*i),2));
    end
    
    [r,c] = size(exp3);
    gen = r/populationSize;
    if gen >= i
        Y(i, 3) = min(exp3((1+populationSize*(i-1)):(populationSize*i),2)); 
    end
    
    [r,c] = size(exp4);
    gen = r/populationSize;
    if gen >= i
        Y(i, 4) = min(exp4((1+populationSize*(i-1)):(populationSize*i),2)); 
    end
    
    [r,c] = size(exp5);
    gen = r/populationSize;
    if gen >= i
        Y(i, 5) = min(exp5((1+populationSize*(i-1)):(populationSize*i),2)); 
    end
    
    [r,c] = size(exp6);
    gen = r/populationSize;
    if gen >= i
        Y(i, 6) = min(exp6((1+populationSize*(i-1)):(populationSize*i),2)); 
    end
end


%% Plot generational error statistics
%plot(X, max(Y'),'DisplayName','max', 'LineWidth', 1.5)
%hold on
%plot(X, mean(Y'),'DisplayName','mean', 'LineWidth',1.5)
%plot(X, min(Y'),'DisplayName','min', 'LineWidth', 1.5)
boxplot(Y', X)
%legend
ylim([-3000 25000])
title('Generator GAN best individual error')
xlabel('Generation')
ylabel('Error')


%% Save image
saveLocation = 'C:\Users\Andreea\OneDrive\University\third year\final year project\experiments\graphs\';
fileName = strcat(saveLocation, "boxplot_gan_generator_seq12_max500",'.png')
saveas(gcf, fileName);
