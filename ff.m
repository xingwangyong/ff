function ff(varargin)
% Find Filenames and Foldernames that contains the input keywords.
% By default search in pwd and names are case insensitive. The order of
% keywords does not affect the result.
%
% Usage
%   ff [option1] [option2] [option2_arg] keyword1 [keyword2] [keywordN]
%       options include
%           -d folder
%               Search in a given folder, folder can be 
%                 (1) relative path, e.g. .., ../..
%                 (2) absolute path, e.g. C:\Program Files\
%                 (3) expression surrounded by ``, e.g. `matlabroot`
%           --dir
%               Only show directory
%           -e
%               Regexp is used to match keyword
%           -f
%               Show full path. Files have same path will be displayed in 
%               same color (black, orange, blue). It is recommended that
%               you use this option along with -n 1, i.e. ff -f -n 1, or
%               the output won't be ideal.
%               This option is only useful when combine with option '-r',
%               otherwise, the search is performed in one folder only, 
%               where all the files have the same path.
%           --file
%               Only show file
%           -i
%               Interactive mode. When in interactive mode, you run 
%               "ff -i [..] keyword" as usual, then matched files are
%               shown, some with indices. You can input index in command 
%               window to open a file. The function op() is a wrapper of
%               ff -i.
%           -n num_cols, -n<num_cols>
%               Set number of columns of the output. E.g. -n4, -n 4
%           --od
%               Also make folder openable via click, open means cd()
%           -r
%               Search recursively, only valid for R2016b and later
%           -s
%               Case sensitive
%           -t level, -t<level>
%               Show full path, level is an integer >= 0. This option is 
%               only useful when combine with option '-r'. Files have same 
%               path will be displayed in same color (black, orange, blue).
%               Here, same path is defined in a top-down fashion, level 
%               specifies how much we go down. Level is relative to where 
%               ff is executed (by default it is pwd), level=0 means pwd, 
%               level=1 means child dir of pwd, and so on
%               For example, two files 
%                   C:\dir1\child_dir\sub_dir1\a.txt
%                   C:\dir1\child_dir\sub_dir2\b.txt
%               When you run ff at C:\dir1
%                   The two files have same path under level 0:
%                       C:\dir1\
%                   The two files have same path under level 1: 
%                       C:\dir1\child_dir\
%                   The two files do not have same path under level 2:
%                       C:\dir1\child_dir\sub_dir1\
%                       C:\dir1\child_dir\sub_dir2\
%               When you run ff at C:\dir1\child_dir
%                   The two files have same path under level 0:
%                       C:\dir1\child_dir\
%                   The two files do not have same path under level 1:
%                       C:\dir1\child_dir\sub_dir1\
%                       C:\dir1\child_dir\sub_dir2\
%           -v keyword
%               Inverse match, file does not contain this keyword is 
%               matchted. Several inverse match can be concatenated, e.g. 
%               -v a -v b, this will match file does not contain 'a' or 'b'
%       short options that without arguments can be concatenated
%           -rs
%       you can use '--' to indicate the end of options, the rest are 
%       keywords. ff -i -r -- -s, here -s is treated as keyword, instead of
%       option
% 
% Example
%   ff my             % list all files whose name contain 'my'
%   ff my simulation  % list all files whose name contain 'my' and 'simulation'
%   ff -r my          % list all files recursively whose name contain 'my'
%   ff -- -r          % list all files whose name contain '-r'
%   ff -re \.m$       % list all files recursively whose extension is .m 
%   ff -r -s my simu  
%   ff -rs my
%   ff -i m           % turn on interactive mode, and search files whose name contain 'm'
%   ff -v test        % list files whose name not contain 'test'
%   ff -v test m      % list files whose name contain 'm' but not contain 'test'
%   ff -v a -v b      % list files whose name does not contaion 'a' or 'b'
%   ff -n 2 my        % list all files whose name contain 'my', show results in two columns
%   ff -f -n 1 m      % list all files (with fullpath) whose name contain 'm', show results in one column
%   ff -t 1 -n 1 m    % list all files (with fullpath) whose name contain 'm', show results in one column, top-down match for path
%   ff -d .. my       % list files whose name contains 'my' in parent folder
%   ff -d `matlabroot` m
% 
% Limitations
%   repeated keywords are matched only once
% 
% Reference and Credits
%   For the option table desgin, grep: a pedestrian, very fast grep utility, https://www.mathworks.com/matlabcentral/fileexchange/9647-grep-a-pedestrian-very-fast-grep-utility
%   For display text in the color of orange, export_fig: Publication-quality export of Matlab figures and axes to various vector & bitmap formats, https://www.mathworks.com/matlabcentral/fileexchange/23629-export_fig
% 
% See also
%   op


% Xingwang Yong, 20220428, add hyperlink to output
% Xingwang Yong, 20220502, enable multi column output
% Xingwang Yong, 20220527, add option -r, -s, -e 
% Xingwang Yong, 20220617, add option -i
% Xingwang Yong, 20220812, add option -v, remove option -e
% Xingwang Yong, 20220821, add option -f -t
% Xingwang Yong, 20220906, add option --od


% TODO, take cell array input for -v, is this really necessary?

%% user preference
% ncols = 4; % number of columns
hor_distance = 4; % horizontal distance between outputs
openable_file_suffixs = '\.(m|txt|c|cpp|h)$';  % '\.(m|txt|c|cpp)$'


%% parse input
[isRecursive,isCaseSensitive,isRegexp,isInteractive,args,input_opts,isInverseMatch,inverseMatchArgs,ncols,isShowFullpath,isFullpathTopdown,level,target_dir,isDirOpenable,isOnlyShowDir,isOnlyshowFile] = parse_input(varargin{:});
if isempty(args) && isempty(inverseMatchArgs)
    help(mfilename);
    disp('<strong>ff needs at least one keyword</strong>');
    return;
end


%% search
if isRecursive
    if verLessThan('matlab','9.1')
        warning('Recursive search is valid for R2016b and later');
        list = dir();
    else
        list = dir(fullfile(target_dir,'**','*'));
    end
else
    list = dir(target_dir);
end
excludeRange = ismember({list.name}, {'.','..'});  %remove the "." and ".." folder
list(excludeRange) = [];
if isOnlyShowDir
    list(~[list.isdir]) = [];
end
if isOnlyshowFile
    list([list.isdir]) = [];
end
fnames = {list.name}.';
pathes = {list.folder}.';
isdirs = {list.isdir}.';
if isRegexp && isCaseSensitive % this would be more readable if switch-case can take array input ... 
    searchFun = @regexp;
end
if isRegexp && ~isCaseSensitive
    searchFun = @regexpi;
end
if ~isRegexp && isCaseSensitive
    searchFun = @strfind;
end
if ~isRegexp && ~isCaseSensitive
    searchFun = @(str, pattern)strfind(lower(str), lower(pattern));
end

ind = true(size(fnames));
for k = 1:numel(args)
    startIndex = searchFun(fnames, args{k});
    ind = ind & ~cellfun(@isempty, startIndex);
end
if isInverseMatch
%     ind2 = true(size(fnames));
    ind2 = false(size(fnames));
    for k = 1:numel(inverseMatchArgs)
        startIndex = searchFun(fnames, inverseMatchArgs{k});
%         ind2 = ind2 & ~cellfun(@isempty, startIndex);
        ind2 = ind2 | ~cellfun(@isempty, startIndex);
    end
    ind = ind & ~ind2;
end
matches_name = fnames(ind);
matches_fullpath = fullfile(pathes(ind), fnames(ind));
isopenables = ~cellfun(@isempty, regexp(matches_name,openable_file_suffixs), 'UniformOutput',true );
if isDirOpenable
    matches_isdir = cell2mat(isdirs(ind));
    isopenables = isopenables | matches_isdir;
end
if isempty(matches_name)
    fprintf('No match for pattern\n');
    fprintf('Include\n');
    disp(args);
    fprintf('Exclude\n');
    disp(inverseMatchArgs);
    return
end


if isInteractive
    num_openalbe_files = sum(isopenables);
    num_digits = numel(num2str(num_openalbe_files));
    link_indices_text = cell(numel(matches_fullpath), 1);
    link_indices = zeros(numel(matches_fullpath), 1);
    counter = 1;
    for k = 1:numel(matches_fullpath)
        if ~isopenables(k)
            link_indices_text{k} = repmat(' ',1,num_digits);
        else
            link_indices_text{k} = sprintf('%-*d',num_digits,counter);
            link_indices(k) = counter;
            counter = counter + 1;
        end
    end
else
    link_indices_text = cell(numel(matches_fullpath), 1);
    link_indices_text(:) = {''};   
end

%% disp
% cmdSize = matlab.desktop.commandwindow.size;
% cmdWidth = cmdSize(1);
% lens = cellfun(@numel, matches_name);
% max_len = max(lens); % global max_len is unnecessary
% ncols = floor( (cmdWidth+numWhiteSpace)/(max_len+numWhiteSpace) );

% sort the result alphabetically into columns, 1st column is minimal, 2nd 
% larger than 1st column, the {'a','b',...,'o'} would be sorted like
%       a   e   i   m
%       b   f   j   n
%       c   g   k   o
%       d   h   l
nele = numel(matches_name);
ncols = min([nele,ncols]);
nrows = ceil( numel(matches_name)/ncols );
num_empty_cols = nrows*ncols - nele;
position_indices = zeros(nrows, ncols);
row_ind = 1;
col_ind = 1;
k = 1;
while k <= nele
    if row_ind==nrows && col_ind+num_empty_cols>ncols
        k = k - 1; % counteract k=k+1
    else
        position_indices(row_ind, col_ind) = k;
    end
    
    k = k + 1;
    row_ind = row_ind + 1;
    if row_ind > nrows
        row_ind = 1;
        col_ind = col_ind + 1;
    end
end



if isShowFullpath
    pathes = cellfun(@fileparts, matches_fullpath, 'UniformOutput',false);
end
if isFullpathTopdown
    matches_path_only = cellfun(@fileparts, matches_fullpath, 'UniformOutput',false);
    pathes_split = regexp(matches_path_only,filesep,'split');
    target_dir_split = regexp(target_dir,filesep,'split');
    concat_wrapper = @(path_split) concat_path(path_split, level+numel(target_dir_split));
    pathes = cellfun(concat_wrapper, pathes_split, 'UniformOutput',false);
end
isExtraStyles = false(numel(matches_fullpath), 1);
if isShowFullpath || isFullpathTopdown
    matches_name = matches_fullpath;
    
    uni_pathes = unique(pathes, 'stable');    
    for k = 1:numel(pathes)
        ind = find(ismember(uni_pathes, pathes{k}));
        % the first n files have same path has isExtraStyles as true, the subsequent m files have same path has isExtraStyles as false, the subsequent p files have same path has isExtraStyles as true, and so on
        if mod(ind,2)
            isExtraStyles(k) = true;
        else
            isExtraStyles(k) = false;
        end
    end
end

% get the width of each column
max_lens = ones(1,ncols);
for k = 1:ncols
    ind = position_indices(:,k);
    ind(ind==0) = [];
    lens = cellfun(@numel, matches_name(ind));
    max_lens(k) = max(lens); 
end

% print the result
for k = 1:nrows
    ind = position_indices(k,:);
    ind(ind==0) = [];
    
    link_ind = link_indices_text(ind);
    s = format_a_row(matches_fullpath(ind), matches_name(ind), hor_distance, max_lens, isopenables(ind), link_ind, isExtraStyles(ind));
    fprintf('%s\n',s);
end


%%
if isInteractive
    x = input('Please input an index: ', 's');
    if isempty(x) || isequal(0, x)
       return 
    end    
    
    if ismember(str2double(x), link_indices)
        fname = matches_fullpath( ismember(link_indices,str2double(x)) );
        if isfolder(fname)
            cd(fname{1});
        else
            matlab.desktop.editor.openDocument(fname);
        end
        
    else
        % quit quitely
    end
end

end

function [isRecursive,isCaseSensitive,isRegexp,isInteractive,args,input_opts,isInverseMatch,inverseMatchArgs,ncols,isShowFullpath,isFullpathTopdown,level,target_dir,isDirOpenable,isOnlyShowDir,isOnlyshowFile] = parse_input(varargin)
% copied from grep.m 
% grep: a pedestrian, very fast grep utility, https://www.mathworks.com/matlabcentral/fileexchange/9647-grep-a-pedestrian-very-fast-grep-utility
% 
% copied from https://stackoverflow.com/questions/56054625/getopts-style-arguments-for-compiled-matlab-programs

nargs = numel(varargin);
opts_tbl = {
  % flag    inival      npar    defpar  description
  %---------------------------------------------
  '-d'      false       1       pwd       'run in a given folder'
  '-e'      false       0        []       'use regexp'
  '-f'      false       0        []       'show full path'
  '-i'      false       0        []       'interactive mode'
  '-n'      false       1        4        'number of columns'  
  '-r'      false       0        []       'search recursively'
  '-s'      false       0        []       'case sensitive'
  '-t'      false       1        2        'show full path, but same path is matched in a top-down fashion'
  '-v'      false       1   cell(nargs,1) 'inverse match'
  '--dir'   false       0        []       'only show directory'
  '--file'  false       0        []       'only show file'  
  '--od'    false       0        []       'make folder openable via click'
  };
col_ind_flag        = 1;
col_ind_inival      = 2;
col_ind_npar        = 3;
col_ind_defpar      = 4;
% col_ind_description = 5;
delete_hyphen = @(str) strip(str,'left','-');
opts_flag = cellfun(delete_hyphen, opts_tbl(:,col_ind_flag), 'UniformOutput', false);
logi_ind_opts_with_arg = cell2mat(opts_tbl(:,col_ind_npar))>0;
logi_ind_opts_without_arg = cell2mat(opts_tbl(:,col_ind_npar))==0;
opts_flag_with_arg = opts_flag(logi_ind_opts_with_arg);
opts_flag_without_arg = opts_flag(logi_ind_opts_without_arg);
logi_ind_long_flag = cell2mat(  cellfun(@numel, opts_flag, 'UniformOutput', false)  )  >  1;
long_opts_flag = opts_flag(logi_ind_long_flag);
get_row_ind_by_flag = @(flag) ismember(opts_tbl(:,col_ind_flag),flag);
isnumeric_and_isnotempty = @(x) isnumeric(x) & (~isempty(x));
logi_ind_opt_with_numeric_arg = cell2mat( cellfun(isnumeric_and_isnotempty, opts_tbl(:,col_ind_defpar), 'UniformOutput', false) );
opts_flag_with_numeric_arg = opts_flag(logi_ind_opt_with_numeric_arg);


% Parse input argument list:
nopts = size(opts_tbl,1);
maxiter = nargs;%min([nargs, nopts]); % this is problematic if user input repeated options, e.g. -r -r -r -r -r keyword. Don't want to fix it now.
inputIndex = 1;
opts_counter = 0;
inverse_match_counter = 0;
opt_indices = zeros(1,nargs);
while (inputIndex <= maxiter)
    if strcmp(varargin{inputIndex}, '--')
        if inputIndex+1 <= maxiter
            inputIndex = inputIndex + 1;                   
        end
        break;
    end
    
    opt = varargin{inputIndex};
    if ~strcmp(opt(1), '-') % end of options, the rest are keywords. options are at first
        break;
    end
    
    if numel(opt)>1 && strcmp(opt(1:2), '--') % might be long option
        opt = opt(3:end);%opt = strip(opt, 'left', '-'); % user input might be -------r, strip would give false result
        if any( ismember(long_opts_flag,opt) )
            isOptWithoutArg = ismember(opt, opts_flag_without_arg);
            if isOptWithoutArg
                [~,optIndex1] = ismember(opt, opts_flag);
                opts_tbl(optIndex1, col_ind_inival) = cellfun(@(c)or(c,true), opts_tbl(optIndex1, col_ind_inival),'UniformOutput',false); % or() ensure that each option is set only one time
                inputIndex = inputIndex+1;
            else
                % TODO, for long options with arguments
            end
        else % invalid options are treated as input keyword
            break;
        end
    else % might be short option
        opt = opt(2:end);% opt = strip(opt, 'left', '-'); % user input might be -------r, strip would give false result
        optSplitted = cellstr(opt(:));
        isOptWithoutArg = ismember(optSplitted, opts_flag_without_arg);
        if all(isOptWithoutArg) % short options can be concatenated
            [~,optIndex1] = ismember(optSplitted, opts_flag);
            opts_tbl(optIndex1, col_ind_inival) = cellfun(@(c)or(c,true), opts_tbl(optIndex1, col_ind_inival),'UniformOutput',false); % or() ensure that each option is set only one time
            opts_counter = opts_counter + 1;
            opt_indices(opts_counter) = inputIndex;
            inputIndex = inputIndex+1;
        elseif numel(opt)>1 && ismember( optSplitted(1),opts_flag_with_numeric_arg )
            optArg = opt(2:end);
            switch optSplitted{1}
                case 'n'
                    flg = '-n';                    
                    ncols = str2double(optArg);
                    try
                        validateattributes(ncols,'double',{'integer','>=',1});
                    catch
                        warning('Trying to interprete %s as the option %s and its arguments, but failed, now treating it as a keyword', varargin{inputIndex}, flg);
                        break; % invalid options are treated as input keyword
                    end
                    opts_tbl{get_row_ind_by_flag(flg), col_ind_inival} = true;
                    opts_tbl{get_row_ind_by_flag(flg), col_ind_defpar} = ncols;
                    inputIndex = inputIndex+1;
                case 't'
                    flg = '-t';
                    level = str2double(varargin{inputIndex+1});
                    try
                        validateattributes(level,'double',{'integer','>=',0});
                    catch 
                        warning('Trying to interprete %s as the option %s and its arguments, but failed, now treating it as a keyword', varargin{inputIndex}, flg);
                        break; % invalid options are treated as input keyword
                    end
                    opts_tbl{get_row_ind_by_flag(flg), col_ind_inival} = true;
                    opts_tbl{get_row_ind_by_flag(flg), col_ind_defpar} = level;
                    inputIndex = inputIndex+1;
                otherwise
                    error('Unkown option with numeric argument: %s', opt(1));
            end                                
        elseif numel(opt)==1 && ismember(opt, opts_flag_with_arg)
            if strcmp('v',opt) % if input option is -v
                flg = '-v';
                if inputIndex+1 > maxiter
                    error('No arguments for option: %s', flg);
                end
                opts_tbl{get_row_ind_by_flag(flg), col_ind_inival} = true;
                inverse_match_counter = inverse_match_counter + 1;
                opts_tbl{get_row_ind_by_flag(flg), col_ind_defpar}{inverse_match_counter,1} = varargin{inputIndex+1};
                inputIndex = inputIndex+2;
            elseif strcmp('n',opt) % if input option is -n
                flg = '-n';
                if inputIndex+1 > maxiter
                    error('No arguments for option: %s', flg);
                end
                opts_tbl{get_row_ind_by_flag(flg), col_ind_inival} = true;
                ncols = str2double(varargin{inputIndex+1});
                try
                    validateattributes(ncols,'double',{'integer','>=',1});
                catch ME
                    msg = sprintf('Expected integer, now it is: %s', varargin{inputIndex+1});
                    causeException = MException('ff:WrongInputType',msg);
                    ME = addCause(ME,causeException);
                    rethrow(ME);
                end
                opts_tbl{get_row_ind_by_flag(flg), col_ind_defpar} = ncols;
                inputIndex = inputIndex+2;
            elseif strcmp('t',opt) % if input option is -t
                flg = '-t';
                if inputIndex+1 > maxiter
                    error('No arguments for option: %s', flg);
                end
                opts_tbl{get_row_ind_by_flag(flg), col_ind_inival} = true;
                level = str2double(varargin{inputIndex+1});
                try
                    validateattributes(level,'double',{'integer','>=',0});
                catch ME
                    msg = sprintf('Expected non-negative integer, now it is: %s', varargin{inputIndex+1});
                    causeException = MException('ff:WrongInputType',msg);
                    ME = addCause(ME,causeException);
                    rethrow(ME);
                end
                opts_tbl{get_row_ind_by_flag(flg), col_ind_defpar} = level;
                inputIndex = inputIndex+2;
            elseif strcmp('d',opt) % if input option is -d
                flg = '-d';
                if inputIndex+1 > maxiter
                    error('No arguments for option: %s', flg);
                end
                opts_tbl{get_row_ind_by_flag(flg), col_ind_inival} = true;
                inputPathCmd = varargin{inputIndex+1};
                if strcmp(inputPathCmd(1), '`') && strcmp(inputPathCmd(end), '`')
                    expr = inputPathCmd(2:end-1);
                    try
                        inputPath = eval(expr);
                    catch
                        error('Failed to parse the expression: %s', expr);
                    end
                else
                    inputPath = inputPathCmd;
                end
                if ~exist(inputPath, 'dir')
                    error('No such folder: %s', inputPath);
                end
                opts_tbl{get_row_ind_by_flag(flg), col_ind_defpar} = rel2abs(inputPath);
                inputIndex = inputIndex+2;
            else % invalid options are treated as input keyword
                break;
            end
        else % invalid options are treated as input keyword
            break;
        end
    end
end


isRegexp          = opts_tbl{get_row_ind_by_flag('-e'), col_ind_inival};
isShowFullpath    = opts_tbl{get_row_ind_by_flag('-f'), col_ind_inival};
isInteractive     = opts_tbl{get_row_ind_by_flag('-i'), col_ind_inival};
isRecursive       = opts_tbl{get_row_ind_by_flag('-r'), col_ind_inival};
isCaseSensitive   = opts_tbl{get_row_ind_by_flag('-s'), col_ind_inival};
isFullpathTopdown = opts_tbl{get_row_ind_by_flag('-t'), col_ind_inival};
isInverseMatch    = opts_tbl{get_row_ind_by_flag('-v'), col_ind_inival};
isOnlyShowDir     = opts_tbl{get_row_ind_by_flag('--dir'), col_ind_inival};
isOnlyshowFile    = opts_tbl{get_row_ind_by_flag('--file'), col_ind_inival};
isDirOpenable     = opts_tbl{get_row_ind_by_flag('--od'), col_ind_inival};


args = varargin(inputIndex:end);  
opt_indices(opt_indices==0) = [];
input_opts = varargin(opt_indices);   
target_dir       = opts_tbl{get_row_ind_by_flag('-d'), col_ind_defpar};
ncols            = opts_tbl{get_row_ind_by_flag('-n'), col_ind_defpar};
level            = opts_tbl{get_row_ind_by_flag('-t'), col_ind_defpar};
inverseMatchArgs = opts_tbl{get_row_ind_by_flag('-v'), col_ind_defpar};
inverseMatchArgs = inverseMatchArgs(~cellfun('isempty',inverseMatchArgs));
end

function s = format_a_row(filepaths, texts, numWhiteSpace, max_lens, isopenables, link_ind,isExtraStyles)
s = [];
whiteSpace = 32*ones(1,numWhiteSpace); % char(32)==white space
for k = 1:numel(texts)
    if isopenables(k)
        if isExtraStyles(k)
            s =  [s, whiteSpace, link_ind{k}, orangetext(gen_link(filepaths{k}, texts{k}, max_lens(k)))];            
        else
            s =  [s, whiteSpace, link_ind{k}, gen_link(filepaths{k}, texts{k}, max_lens(k))];            
        end
    else
        if isExtraStyles(k)
            s =  [s, whiteSpace, link_ind{k}, orangetext(texts{k}), 32*ones(1,max_lens(k)-numel(texts{k}))];
        else
            s =  [s, whiteSpace, link_ind{k}, texts{k}, 32*ones(1,max_lens(k)-numel(texts{k}))];
        end
    end
end
s(1:numWhiteSpace) = [];  % delete the leading white space, may error here if input are empty
end

function s = gen_link(filepath, text, max_len)
% generate string that will be displayed as a hyperlink, which will open
% a document if clicked
% 
% Input
%   filepath - fullpath of file  
%   text     - the display text of the hyperlink
%   max_len  - pad the output string to this length
% 
% Output
%   s        - string that contains hyperlink
% 
% Example
%   s = gen_link(which('ismember'), 'click to open ismember()', 30);
%   disp(s);

% Xingwang Yong, 20220502

if exist(filepath, 'dir')
    s = sprintf('<a href="matlab:cd(''%s'');">%s</a>%s', filepath, text, 32*ones(1,max_len-numel(text)));
else
    s = sprintf('<a href="matlab:matlab.desktop.editor.openDocument(''%s'');">%s</a>%s', filepath, text, 32*ones(1,max_len-numel(text)));
end

end



function omsg = orangetext(msg)
% disp(omsg) shows orange text
% 
% modified from displayPromoMsg() in Yair Altman's export_fig.m

omsg = ['[' 8 msg ']' 8];
end

function p = concat_path(path_split, level)
endInd = level;
if endInd > numel(path_split)
    endInd = numel(path_split);
end
p = strjoin(path_split(1:endInd),filesep);
end

function F = rel2abs(F)
% https://stackoverflow.com/a/36204160/14366360
if ~java.io.File(F).isAbsolute
    F = fullfile(pwd,F);
end
end