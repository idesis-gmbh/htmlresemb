program htmlresemb;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
  //{$modeswitch typehelpers}
{$ENDIF}

uses
  {$IFDEF DARWIN}
    {$IFDEF UseCThreads}
  cthreads,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF DARWIN}
  cwstring,
  {$ENDIF}
  sysutils,
  uUtil,
  uOptions,
  uMain;

var
  {%H-}TaskStart: TDateTime;

{$R *.res}

begin
  TaskStart := Now;
  ExitCode := ERR_OPTIONS;
  if GetOptions then
    ExitCode := ProcessFile;
  //WriteLn(Format('%sTook: %s', [NL, GetDurationFmt(TaskStart)]));
end.

