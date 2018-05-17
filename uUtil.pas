unit uUtil;

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
  SysUtils,
  Classes,
  fgl,
  DOM;

type
  TNodeVisitor = function(var ANode: TDOMNode): Boolean of object;
  TDOMNodes = specialize TFPGObjectList<TDOMNode>;

  { TXMLDocumentHelper }
  TXMLDocumentHelper = class helper for TXMLDocument
  type
    TDOMEntityReferenceHelper = class(TDOMEntityReference);
  public
    function CreateNumericEntity(AName: DOMString): TDOMEntityReference;
  end;

 { TDOMNodeHelper }
  TDOMNodeHelper = class helper for TDOMNode
  public
    function AppendSibling(ANewSibling: TDOMNode): TDOMNode;
  end;


const
  // Product
  PRODUCT_NAME = 'idesis HTML Resource Embedder';
  PRODUCT_VERSION = '0.9.2 (' + {$I %Date%} + ')';
  PRODUCT_SHORT_NAME = 'htmlresemb';
  PRODUCT_INFO = PRODUCT_NAME + ' (' + PRODUCT_SHORT_NAME + '), v' + PRODUCT_VERSION;

  // Processing result
  SUCCESS = 0;
  ERR_OPTIONS = 1;
  ERR_PROCESS = 2;

  // Standard strings
  WHITESPACE                    = [#0..' '];
  NL                            = LineEnding;

  // Utility functions
  function GetDurationFmt(AStartTime: TDateTime): string;
  function StdInToMemoryStream: TMemoryStream;
  function ExpandEnvironmentStrings(const s: string; IgnoreCase: Boolean = False): string;
  function GetAbsolutePath(ADirectory, ABaseDirectory: string): string;
  function GetFileContentAsString(AFileName: string): string;
  function GetImageAsBase64String(AFileName: string; out LoadSuccess: Boolean): DOMString;
  function WideStringReplace(const S, OldPattern, NewPattern: Widestring; Flags: TReplaceFlags): WideString;

  // XML
  procedure ParseXMLDocument(AStream: TStream; out AXMLDocument: TXMLDocument; ANodeVisitor: TNodeVisitor; APreserveCDATASections: Boolean = True);
  procedure ParseXMLDocument(AText: WideString; out AXMLDocument: TXMLDocument; ANodeVisitor: TNodeVisitor; APreserveCDATASections: Boolean = True);
  function RepairBrokenHTML(AHTML: WideString; out ExceptionMessage: string): WideString;


implementation

uses
  base64,
  iostream,
  FileUtil,
  XMLRead,
  simplehtmltreeparser;


{ ---------------------------------------------------------------------
    TXMLDocumentHelper
  --------------------------------------------------------------------- }
function TXMLDocumentHelper.CreateNumericEntity(AName: DOMString): TDOMEntityReference;
begin
  TDOMNode(Result) := Alloc(TDOMEntityReference);
  Result.Create(Self);
  TDOMEntityReferenceHelper(Result).FName := '#' + AName;
end;


{ ---------------------------------------------------------------------
    TXMLDocumentHelper
  --------------------------------------------------------------------- }
function TDOMNodeHelper.AppendSibling(ANewSibling: TDOMNode): TDOMNode;
begin
  if ((ANewSibling <> nil) and (ParentNode <> nil)) then
    if (NextSibling = nil) then
      ParentNode.AppendChild(ANewSibling)
    else
      ParentNode.InsertBefore(ANewSibling, NextSibling);
  Result := Self;
end;


// Task duration
function GetDurationFmt(AStartTime: TDateTime): string;
var
  st: TSystemTime;
begin
  with st do
  begin
    DecodeTime(Now-AStartTime, Hour, Minute, Second, MilliSecond);
    result := Format('%dh %dm %d%s%.3ds', [Hour, Minute, Second, FormatSettings.DecimalSeparator, MilliSecond]);
  end;
end;

// Read stdin into a MemoryStream
function StdInToMemoryStream: TMemoryStream;
const
{$IFDEF DARWIN}
BUFSIZE = 16384;
{$ELSE}
BUFSIZE = 1024;
{$ENDIF}
var
  stdin: TIOStream;
  buf: string;
  count: Integer;
begin
  Result := TMemoryStream.Create;
  stdin := TIOStream.Create(iosInput);
  try
    SetLength(buf, BUFSIZE);
    repeat
      count := stdin.Read(buf[1], Length(buf));
      if (count > 0) then
        Result.Write(buf[1], count);
    until count = 0;
  finally
    stdin.Free;
  end;
end;

// Expand environment variables
// http://www.lazarusforum.de/viewtopic.php?f=16&t=4729
function ExpandEnvironmentStrings(const s: string; IgnoreCase: Boolean = False): string;
{$IFDEF DARWIN}
const
  ENV_PREFIX ='$';
  ENV_POSTFIX = '';
{$ELSE}
const
  ENV_PREFIX = '%';
  ENV_POSTFIX = '%';
{$ENDIF}
var
  envs: TStringList;
  aEnvValue, aEnvName, aEnvString: string;
  flags: TReplaceFlags;
  i: Integer;
begin
  {$IFDEF DARWIN}
  if ((Length(s) > 1) and (s[1..2] = '~/')) then
    Result := AppendPathDelim(GetEnvironmentVariableUTF8('HOME')) + Copy(s, 3, MaxInt)
  else
    Result := s;
  {$ELSE}
  Result := s;
  {$ENDIF}
  if Pos(ENV_PREFIX, Result) = 0 then exit;
  if IgnoreCase then
    flags := [rfReplaceAll, rfIgnoreCase]
  else
    flags := [rfReplaceAll];
  envs := TStringList.Create;
  envs.Sorted := ENV_POSTFIX = '';
  for i := GetEnvironmentVariableCount downto 1 do
  begin
    AEnvString := GetEnvironmentString(i);
    envs.Add(aEnvString);
  end;
  for i := envs.Count-1 downto 0 do
  begin
    envs.GetNameValue(i, aEnvName, aEnvValue);
    Result := WideStringReplace(Result, ENV_PREFIX+aEnvName+ENV_POSTFIX, aEnvValue, flags);
    if Pos(ENV_PREFIX, Result) = 0 then Break;
  end;
  envs.Free;
end;

function GetAbsolutePath(ADirectory, ABaseDirectory: string): string;
begin
  Result := CreateAbsolutePath(ExpandEnvironmentStrings(ADirectory), ExpandEnvironmentStrings(ABaseDirectory));
  {$IFDEF WINDOWS}
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  {$ENDIF}
end;

function GetFileContentAsString(AFileName: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function GetImageAsBase64String(AFileName: string; out LoadSuccess: Boolean): DOMString;
var
  ImageFile: TFileStream;
  ResultStream: TStringStream;
  Base64Encoder: TBase64EncodingStream;
  FileExt: string;
  MimeType: string;
begin
  Result := Trim(AFileName);
  if not(FileExists(AFileName)) then
  begin
    LoadSuccess := False;
    Exit;
  end;
  FileExt := LowerCase(ExtractFileExt(AFileName));
  if (FileExt = '.png') then
    MimeType := 'image/png'
  else if ((FileExt = '.jpg') or (FileExt = '.jpeg')) then
    MimeType := 'image/jpeg'
  else if (FileExt = '.gif') then
    MimeType := 'image/gif'
  else if (FileExt = '.svg') then
    MimeType := 'image/svg+xml'
  else
    MimeType := '';
  ResultStream := TStringStream.Create('');
  Base64Encoder := TBase64EncodingStream.Create(ResultStream);
  try
    try
      ImageFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
      Base64Encoder.CopyFrom(ImageFile, 0);
      Base64Encoder.Flush; // !!! Without flushing the result can be incorrect!
      Result := 'data:' + MimeType + ';base64,' + ResultStream.DataString;
      LoadSuccess := True;
    except
      LoadSuccess := False;
    end;
  finally
    ImageFile.Free;
    Base64Encoder.Free;
    ResultStream.Free;
  end;
end;

// Like StringReplace but with changed types (string to WideString)
function WideStringReplace(const S, OldPattern, NewPattern: Widestring; Flags: TReplaceFlags): WideString;
var
  Srch,OldP,RemS: WideString; // Srch and Oldp can contain uppercase versions of S,OldPattern
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  if rfIgnoreCase in Flags then
    begin
    Srch:=WideUpperCase(Srch); //AnsiUpperCase
    OldP:=WideUpperCase(OldP); //AnsiUpperCase
    end;
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch); // AnsiPos (Unicode, not Ansi)
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;




procedure WriteStringToStream(AString: string; AStream: TStream);
begin
  AStream.WriteBuffer(Pointer(AString)^, Length(AString));
end;

procedure WriteStringToFile(AString: string; AFileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    WriteStringToStream(AString, fs);
  finally
    fs.Free;
  end;
end;



{ ---------------------------------------------------------------------
    XML
  --------------------------------------------------------------------- }
procedure InternalParseXMLDocument(AXMLInput: TXMLInputSource; out AXMLDocument: TXMLDocument; ANodeVisitor: TNodeVisitor; APreserveCDATASections: Boolean = True);
var
  Parser: TDOMParser;

  procedure VisitNodes(SourceNode: TDOMNode);
  begin
    if (SourceNode = nil) then
      Exit;
    if not(ANodeVisitor(SourceNode)) then
      Exit;
    SourceNode := SourceNode.FirstChild;
    while (SourceNode <> nil) do
    begin
      VisitNodes(SourceNode);
      SourceNode := SourceNode.NextSibling;
    end;
  end;

begin
  if (AXMLInput = nil) then
    Exit;
  Parser := TDOMParser.Create;
  Parser.Options.PreserveWhitespace := True;
  Parser.Options.CDSectionsAsText := not(APreserveCDATASections);
  Parser.Options.ExpandEntities:=True;
  try
    Parser.Parse(AXMLInput, AXMLDocument);
  finally
    AXMLInput.Free;
    Parser.Free;
  end;
  if (ANodeVisitor <> nil) then
    VisitNodes(AXMLDocument.DocumentElement);
end;

procedure ParseXMLDocument(AStream: TStream; out AXMLDocument: TXMLDocument; ANodeVisitor: TNodeVisitor; APreserveCDATASections: Boolean = True);
begin
  InternalParseXMLDocument(TXMLInputSource.Create(AStream), AXMLDocument, ANodeVisitor, APreserveCDATASections);
end;

procedure ParseXMLDocument(AText: WideString; out AXMLDocument: TXMLDocument; ANodeVisitor: TNodeVisitor; APreserveCDATASections: Boolean = True);
begin
  InternalParseXMLDocument(TXMLInputSource.Create(AText), AXMLDocument, ANodeVisitor, APreserveCDATASections);
end;

function RepairBrokenHTML(AHTML: WideString; out ExceptionMessage: string): WideString;
var
  TreeParser: TTreeParser;
begin
  TreeParser := TTreeParser.Create;
  with TreeParser do
  begin
    parsingModel := TParsingModel.pmHTML;
    repairMissingStartTags := True;
    repairMissingEndTags := True;
    readComments := True;
    readProcessingInstructions := True;
    try
      parseTree(AHTML);
      Result := getLastTree.outerXML();
    except
      ExceptionMessage := Exception(ExceptObject).Message;
    end;
    Free;
  end;
end;

end.
