unit uMain;

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
  sysutils,
  Classes,
  uUtil,
  uOptions,
  uHTMLResEmb;

function ProcessFile: Integer;


implementation

function ProcessFile: Integer;
var
  ProcessingMessage: string;
  DocPath: string;
  sl: TStringList;
  msStdIn: TMemoryStream;
  HTMLProcessor: THTMLProcessor;
begin
  ProcessingMessage := '';
  DocPath := ExtractFilePath(Opts.InputFile);
  sl := TStringList.Create;
  HTMLProcessor := nil;
  msStdIn := nil;
  try
    try
      if (Opts.StdIn) then
      begin
        msStdin := StdInToMemoryStream;
        msStdIn.Position := 0; // !!
        sl.LoadFromStream(msStdIn);
      end
      else
        sl.LoadFromFile(Opts.InputFile);
      HTMLProcessor := THTMLProcessor.Create(DocPath, Opts.EmbedCSS, Opts.EmbedJavaScript, Opts.EmbedImages);
      if not(HTMLProcessor.ProcessHTML(sl, ProcessingMessage)) then
        Result := ERR_PROCESS
      else
      begin
        if (Opts.StdOut) then
          Write(sl.Text)
        else
          sl.SaveToFile(Opts.OutputFile);
        Result := SUCCESS;
      end;
      if (Result = ERR_PROCESS) then
        WriteLn(StdErr, 'Processing failed.');
      if (ProcessingMessage <> '') then
        WriteLn(StdErr, '(Warning) messages from HTML processing:', NL, ProcessingMessage);
    except
      Result := ERR_PROCESS;
      WriteLn(StdErr, 'Processing error: ', NL, Exception(ExceptObject).Message);
    end;
  finally
    msStdIn.Free;
    HTMLProcessor.Free;
    sl.Free;
  end;
end;

end.

