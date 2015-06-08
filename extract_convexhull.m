%% VARIABLES
%path = '/Users/dianabogantes/Documents/MATLAB/IMAGE_TESTS/BD-Vesale';
pathFeats = '/Users/dianabogantes/Documents/MATLAB/IMAGE_TESTS';
pathThumbs = '/Users/dianabogantes/Documents/MATLAB/IMAGE_TESTS/TranscriptionDataSet';


cHYProfilePath_30 = strcat(pathFeats,'/','feat-cv-y-profile_30.txt');
fileCHYProfile_30 = fopen(cHYProfilePath_30,'a');

% extract each tar.gz file, calculate descriptors and output result in text file
for i = 1:1
    %extract files
    %filename = strcat(path,'/',num2str(i),'.tar.gz');
    %images = untar(filename);
    %foldername = strcat(pathThumbs, '/', num2str(i));
    foldername = pathThumbs;
    allFiles = dir( foldername );
    allNames = { allFiles.name };
    %nelems = length(images);
    nelems = length(allNames);
    for j = 4 : nelems %%%% <=================== REVISAR
        % open image
        %image_path = images{1,j};
        image_path = strcat(foldername,'/',allNames{1,j});
        image = imread(image_path);
        % resize image
        image_30 = imresize(image, [30 NaN]);        
        
        %% extract convex hull Y-profile
        X30 = zeros(1,30,'uint32');
        
        cH = regionprops(image_30, 'ConvexHull');
        cvxH = cat(1, cH.ConvexHull);
        cvxH = fix(cvxH(:,2)); %get rows that have points
        for i = 1:size(cvxH,1)
            if cvxH(i,1)==0
                cvxH(i,1)=1;
            end
            X30(1,cvxH(i,1)) = X30(1,cvxH(i,1)) + 1;
        end
        
        x_data_30 = '';
        x_data_50 = '';
        for i1 = 1:29
            x_data_30 = strcat(x_data_30,num2str(X30(i1)));
            x_data_30 = strcat(x_data_30,',');
        end
        x_data_30 = strcat(x_data_30,num2str(X30(30)));
        
        expr1='img-';
        expr2='.png';
        expr = [expr1 '(.*?)' expr2];
        tok = regexp(allNames{1,j},expr,'tokens');
        imgID = cell2mat([tok{:}]);
        %imgID = strcat(x_data_30,',');
        
        fprintf(fileCHYProfile_30,'%s\t%s\n',imgID,x_data_30);
        

    end
end
fclose(fileCHYProfile_30);
disp('Finished execution');
