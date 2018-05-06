unit uOptions;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
  //{$modeswitch typehelpers}
{$ENDIF}

interface

uses
  {$IFDEF DARWIN}
    {$IFDEF UseCThreads}
  cthreads,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN}
  cwstring,
  {$ENDIF}
  Classes;

type
  TOptions = record
    StdIn: Boolean;
    StdOut: Boolean;
    InputFile: string;
    OutputFile: string;
    EmbedCSS: Boolean;
    EmbedJavaScript: Boolean;
    EmbedImages: Boolean;
    ShowHelp: Boolean;
  end;

var
  Opts: TOptions;
  Params: TStringList;

  function GetOptions: Boolean;
  procedure PrintUsage;


implementation

uses
  SysUtils,
  uUtil;

// Help message
procedure PrintUsage;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  with sl do
  begin
    Add(PRODUCT_INFO);
    Add('');
    Add(PRODUCT_SHORT_NAME + ' tries to embed all external resources like CSS, JavaScript and');
    Add('image files into an HTML document. It reads the content of those files and');
    Add('modifies the corresponding tags (link, style and img) accordingly.');
    Add('Reading of the source document is done with an XML parser, so this may fail');
    Add('completely (although ' + PRODUCT_SHORT_NAME + ' tries to fix broken HTML to some extent).');
    Add('Also the output document might contain slightly different whitespace.');
    Add('The resulting document is in most cases "portable", that is, it can be');
    Add('copied/moved to other locations without breaking the layout. Please note');
    Add('that currently embedding of font files isn''t supported.');
    Add('');
    Add('Usage:  ' + PRODUCT_SHORT_NAME + ' -i <infile> -o <outfile> [options]');
    Add('');
    Add('  <infile>          Use <infile> for input.');
    Add('                    If -i <infile> is ommitted, input is read from stdin. In');
    Add('                    this case a document root path must be supplied via');
    Add('                    -docroot, otherwise loading of resources referenced in the');
    Add('                    document is impossible.');
    Add('');
    Add('  <outfile>         Use <outfile> for output. If -o <outfile> is ommitted,');
    Add('                    output goes to stdout.');
    Add('');
    Add('Available options (others are silently ignored):');
    Add('');
    Add('  -h, --help        Show this help message and exit.');
    Add('');
    Add('  -docroot <path>   A path which is used to resolve relative filenames.');
    Add('                    Relative paths and environment variables can be used.');
    Add('');
    Add('  -css              Embed CSS files.');
    Add('');
    Add('  -js               Embed JavaScript files.');
    Add('');
    Add('  -img              Embed image files.');
    Add('');
    Add('If none of -css, -js and -img is used, all resources will be embedded.');
    Add('');
    Add('Return values:');
    Add('  0  Success.');
    Add('  1  Invalid/incomplete options or -h/--help was used.');
    Add('  2  An error occured.');
    Add('');
    Add('Warning or error messages during processing are written to stderr.');
    WriteLn(Text);
    Free;
  end;
end;


// Parameter available?
function HasOption(opt: string; out value: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  value := '';
  for i := 0 to Params.Count-1 do
    if (Params[i] = opt)  then
    begin
      Result := True;
      if (i < Params.Count-1) then
        value := Params[i+1];
      break;
    end;
end;

// Switch available?
function HasSwitch(opt: string): Boolean;
var
  dummy: string;
begin
  Result := HasOption(opt, dummy);
end;

function GetOptions: Boolean;
var
  i: Integer;
  Option: string;
  OutDirectory: string;
begin
  // Read options
  for i := 1 to ParamCount do
    Params.Add(ParamStr(i));
  if (HasSwitch('-h') or HasSwitch('--help')) then
  begin
    PrintUsage;
    Exit(False);
  end;
  Opts.StdIn := False;
  Opts.StdOut := False;
  // Minimum requirement
  if not(HasOption('-i', Option)) then
  begin
    Opts.StdIn := True;
    if not(HasOption('-docroot', Option)) then
    begin
      WriteLn('If input is read from stdin a directory must be given with -docroot.');
      Exit(False);
    end;
  end
  else
  begin
    Opts.InputFile := ExpandFileName(ExpandEnvironmentStrings(Option));
    if not(FileExists(Opts.InputFile)) then
    begin
      WriteLn('Input file ' + Opts.InputFile + ' doesn''t exist.');
      Exit(False);
    end;
  end;
  if not(HasOption('-o', Option)) then
    Opts.StdOut := True
  else
  begin
    Opts.OutputFile := ExpandFileName(ExpandEnvironmentStrings(Option));
    OutDirectory := ExtractFilePath(Opts.OutputFile);
    if not(DirectoryExists(OutDirectory)) then
    begin
      WriteLn('Output directory ' + OutDirectory + ' doesn''t exist.');
      Exit(False);
    end;
  end;
  Opts.EmbedCSS := HasSwitch('-css');
  Opts.EmbedJavaScript := HasSwitch('-js');
  Opts.EmbedImages := HasSwitch('-img');
  if (not(Opts.EmbedCSS) and not(Opts.EmbedJavaScript) and not(Opts.EmbedImages)) then
  begin
    Opts.EmbedCSS := True;
    Opts.EmbedJavaScript := True;
    Opts.EmbedImages := True;
  end;
  Result := True;
end;


initialization
  Params := TStringList.Create;

finalization
  Params.Free;

end.
