%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       1-Dimensional 234Th Model
%--------------------------------------------------------------------------
%                  Perrin Davidson | University of Chicago
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Importing Program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Import 234Th data ------------------------------------------------------
% Setup the import options and import the data:
opts = spreadsheetImportOptions("NumVariables", 5);

% Specify sheet and range:
opts.Sheet = "Sheet1";
opts.DataRange = "A2:E36347";

% Specify column names and types:
opts.VariableNames = ["Longitude", "Latitude", "Depth", "Th234", "Th234_1"];
opts.VariableTypes = ["double", "double", "double", "double", "double"];

% Import the data
th234_2 = readtable("/Users/perrindavidson/RESEARCH/whoi/data_wrangling/data/output/krige/kriged/2.8_resolution/th234_kriged_2.8.xlsx", opts, "UseExcel", false);

% Convert to output type
th234 = table2array(th234_2);

% Clear temporary variables
clear opts th234_2

% Import 238U data --------------------------------------------------------
% Setup the Import Options and import the data:
opts = spreadsheetImportOptions("NumVariables", 5);

% Specify sheet and range:
opts.Sheet = "Sheet1";
opts.DataRange = "A2:E36347";

% Specify column names and types:
opts.VariableNames = ["Longitude", "Latitude", "Depth", "U238", "U238_1"];
opts.VariableTypes = ["double", "double", "double", "double", "double"];

% Import the data:
u238_2 = readtable("/Users/perrindavidson/RESEARCH/whoi/data_wrangling/data/output/krige/kriged/2.8_resolution/u238_kriged_2.8.xlsx", opts, "UseExcel", false);

% Convert to output type:
u238 = table2array(u238_2);

% Clear temporary variables:
clear opts u238_2

% Import POC/234Th data ---------------------------------------------------
% Setup the Import Options and import the data
opts = spreadsheetImportOptions("NumVariables", 5);

% Specify sheet and range
opts.Sheet = "Sheet1";
opts.DataRange = "A2:E36347";

% Specify column names and types
opts.VariableNames = ["Longitude", "Latitude", "Depth", "Ratio", "Ratio_1"];
opts.VariableTypes = ["double", "double", "double", "double", "double"];

% Import the data
ratio2 = readtable("/Users/perrindavidson/RESEARCH/whoi/data_wrangling/data/output/krige/kriged/2.8_resolution/ratio_kriged_2.8.xlsx", opts, "UseExcel", false);

% Convert to output type
ratio = table2array(ratio2);

% Clear temporary variables
clear opts ratio2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                End Program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%