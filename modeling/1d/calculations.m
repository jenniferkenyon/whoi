%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                       1-Dimensional 234Th Model
%--------------------------------------------------------------------------
%                  Perrin Davidson | University of Chicago
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Calculations Program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Perform calculations ---------------------------------------------------
% Make depth bins:
for i = 1:1:array_end
    if depth(i) <= 300
        dz(i) = 10;
    elseif depth(i) > 300
        dz(i) = 50;
    end
end
dz = transpose(dz);

% Calculate disquilibrium:
disequilibrium = (u238(:,4) - th234(:,4)).*dz;

% Calculate sums:
for i = 1:1:array_end
    z = depth(i);
    if z <= 300
        if z == 10
            flux(i) = disequilibrium(i)*lambda;
        end
        ind = (z/10) - 1;
        sum = 0;
        for j = i:-1:(i-ind)
            sum = sum + disequilibrium(j);
        end
        flux(i) = sum*lambda;
    elseif z > 300
        ind = (z - 300)/50;
        sum = 0;
        for j = i:-1:(i-ind)
            sum = sum + disequilibrium(j);
        end
        flux(i) = sum*lambda;
    end
end
flux = transpose(flux);

% Clear unneeded variables:
clear ind sum z t_half lambda i j array_end dz disequilibrium

% Calculate POC flux:
flux = flux.*ratio(:,4);

% QC, as negative implies we have passed the PZZ and 234Th > 238U:
flux(flux < 0) = NaN;

% Assign coordinates:
flux = [th234(:,1), th234(:,2), th234(:,3), flux];

% The units for above are: 1/day*dpm/L*m*umol/dpm. The units I want are 
% mg C m-2 day-1. The unit conversion then follow as:
%
% umol*m/L*1/day = umol * (1 mol/1000000 umol) * (28.8 g/1 mol) * (1000
% mg C/1 g) * (1000 L/1 m3) * (1/day) =  28.8*1000*1000/1000000 = 28.8
%
% Converting:
factor = 28.8;
flux(:,4) = flux(:,4).*factor;

clear factor

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                End Program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%