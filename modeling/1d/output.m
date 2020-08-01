%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       1-Dimensional 234Th Model
%--------------------------------------------------------------------------
%                  Perrin Davidson | University of Chicago
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            Printing Program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set fluxes and output --------------------------------------------------
% Set flux:
flux_plot = flux(flux(:,3) == plotting_depth,:);

% Save as excel:
filename1 = 'flux';
filename2 = num2str(plotting_depth);
path = 'modeling/1d/output/';
filename = strcat(path, filename1,'_',filename2, 'm.xlsx');
writematrix(flux_plot,filename)

% Print out all fluxes:
if print_all == 1
    for i = 1:1:34
        flux_plot = flux(flux(:,3) == depth(i),:);
        filename2 = num2str(depth(i));
        filename3 = strcat(filename1,'_',filename2, 'm');
        filename4 = strcat(path,'poc_fluxes_all.xlsx');
        writematrix(flux_plot,filename4,'Sheet',filename3,'WriteMode','append')
    end
    clear filename3 filename4
end

% Clean up:
clear filename filename1 filename2 path flux_plot plotting_depth depth print_all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                End Program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%