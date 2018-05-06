unit uHTMLResEmb;

{$IFDEF FPC}
  {$mode objfpc}
  {$H+}
  //{$modeswitch typehelpers}
{$ENDIF}

interface

uses
  {$IFDEF DARWIN}
  cwstring,
  {$ENDIF}
  Classes,
  DOM,
  uUtil;


type
  { THTMLProcessor }
  THTMLProcessor = class(TObject)
  private
    FDocPath: string;
    FHeader: string;
    FHTMLBroken: Boolean;
    FEmbedCSS: Boolean;
    FEmbedJavaScript: Boolean;
    FEmbedImages: Boolean;
    FXMLSourceDocument: TXMLDocument;
    FDeleteList: TDOMNodes;
    procedure DeleteNodes;
    function StoreAndRemoveHeader(AInput: TStringList): string;
    procedure RestoreHeader(AInput: TStringList);
    function NumericEntity(AEntity: DOMString): TDOMNode;
    // Node-Handling by type
    procedure StyleNode(ANode: TDOMNode);
    procedure LinkNode(ANode: TDOMNode);
    procedure ScriptNode(ANode: TDOMNode);
    procedure ImageNode(ANode: TDOMNode);
    // Node loop
    procedure SelectNode(var ANode: TDOMNode);
    procedure ProcessAllNodes;
  public
    constructor Create(ADocPath: string; AEmbedCSS, AEmbedJavaScript, AEmbedImages: Boolean);
    destructor Destroy; override;
    function ProcessHTML(AInput: TStringList; out ProcessingMessage: string): Boolean;
  end;


implementation

uses
  sysutils,
  uXMLWriter;


{ ---------------------------------------------------------------------
    THTMLProcessor
  ---------------------------------------------------------------------}
// private
procedure THTMLProcessor.DeleteNodes;
var
  i: Integer;
begin
  for i := 0 to FDeleteList.Count-1 do
    if (FDeleteList[i].ParentNode <> nil) then
      FDeleteList[i].ParentNode.RemoveChild(FDeleteList[i]);
end;

function THTMLProcessor.StoreAndRemoveHeader(AInput: TStringList): string;
var
  Document: string;
  XMLDeclStart, DocTypeStart, p: Integer;
begin
  Document := AInput.Text;
  XMLDeclStart := Pos('<?xml', Document);
  DocTypeStart := Pos('<!DOCTYPE', Document);
  // Is an error in the document but handled nevertheless
  if (XMLDeclStart > DocTypeStart) then
  begin
    p := XMLDeclStart + 6;
    while p < Length(Document) do
      if (Document[p..p+1] = '?>') then
      begin
        FHeader := Copy(Document, 1, p+1);
        Delete(Document, 1, p+1);
        Break;
      end
      else
        Inc(p);
  end
  // No XML declaration, only DOCTYPE
  else if (DocTypeStart > XMLDeclStart) then
  begin
    p := DocTypeStart + 9;
    while p < Length(Document) do
      if (Document[p] = '>') then
      begin
        FHeader := Copy(Document, 1, p);
        Delete(Document, 1, p);
        Break;
      end
      else
        Inc(p);
  end;
  // No XML and DOCTYPE declarations
  Result := Document;
end;

procedure THTMLProcessor.RestoreHeader(AInput: TStringList);
begin
  // AInput will always contain these lines at the beginning (see ProcessHTML):
  // <?xml version="1.0" encoding="utf-8"?>
  // <!DOCTYPE html SYSTEM " ">
  // The first line will be deleted and the second (now first)
  // line will be replaced with the original header content (if available).
  AInput.Delete(0);
  if (FHeader <> '') then
    AInput[0] := FHeader
  else
    AInput.Delete(0);
end;

function THTMLProcessor.NumericEntity(AEntity: DOMString): TDOMNode;
begin
  Result := FXMLSourceDocument.CreateNumericEntity(AEntity);
end;

procedure THTMLProcessor.StyleNode(ANode: TDOMNode);
var
  StyleNodeContent: String;
begin
  // Wrap node content in CDATA section (enables special characters like '>' in CSS)
  // If RepairBrokenHTML was called, the special characters of already existing CDATA section
  // have been escaped, so wrapping them again (ANodeTextContent) can create nested CDATA
  // sections but thats not a problem for the browser/document.
  // This is only done with broken HTML (see SelectNode()), so the original code isn't touched.
  StyleNodeContent :=
    '*/' +
    NL +
    ANode.TextContent +
    NL +
    '/* ';
  while (ANode.ChildNodes.Count > 0) do
    ANode.RemoveChild(ANode.ChildNodes[0]);
  with ANode do
  begin
    AppendChild(FXMLSourceDocument.CreateTextNode(NL));
    AppendChild(FXMLSourceDocument.CreateTextNode('/*'));
    AppendChild(FXMLSourceDocument.CreateCDATASection(StyleNodeContent));
    AppendChild(FXMLSourceDocument.CreateTextNode('*/'));
    AppendChild(FXMLSourceDocument.CreateTextNode(NL));
  end;
end;

procedure THTMLProcessor.LinkNode(ANode: TDOMNode);
var
  Href, Rel: DOMString;
  StylesheetFileName: string;
  FileContent, StylesheetTagContent: String;
  NewStyleNode: TDOMElement;
begin
  Href := TDOMElement(ANode).GetAttribute('href');
  if (HRef = '') then
    Exit;
  Rel := TDOMElement(ANode).GetAttribute('rel');
  if ((Rel = '') or (Rel <> 'stylesheet')) then
    Exit;
  if Href[1..7] = 'file://' then
  begin
    {$IFDEF WINDOWS} // 'file:///c:/'
    StylesheetFileName := Href[9..MaxInt];
    {$ELSE}
    StylesheetFileName := Href[8..MaxInt];
    {$ENDIF}
  end
  else
    StylesheetFileName := GetAbsolutePath(Href, FDocPath);
  try
    FileContent := GetFileContentAsString(StylesheetFileName);
    StylesheetTagContent :=
      '*/' +
      NL +
      FileContent +
      NL +
      '/* ';
  except
    WriteLn(StdErr, 'Warning: couldn''t embed ' + StylesheetFileName);
    Exit;
  end;
  NewStyleNode := FXMLSourceDocument.CreateElement('style');
  with NewStyleNode do
  begin
    SetAttribute('type', 'text/css');
    AppendChild(FXMLSourceDocument.CreateTextNode(NL));
    AppendChild(FXMLSourceDocument.CreateTextNode('/*'));
    AppendChild(FXMLSourceDocument.CreateCDATASection(StylesheetTagContent));
    AppendChild(FXMLSourceDocument.CreateTextNode('*/'));
    AppendChild(FXMLSourceDocument.CreateTextNode(NL));
  end;
  with ANode do
  begin
    AppendSibling(FXMLSourceDocument.CreateTextNode(NL));
    AppendSibling(NewStyleNode);
    AppendSibling(FXMLSourceDocument.CreateTextNode(NL));
  end;
  FDeleteList.Add(ANode);
end;

procedure THTMLProcessor.ScriptNode(ANode: TDOMNode);
var
  Src: DOMString;
  ScriptFileName: string;
  FileContent, ScriptNodeContent: String;
begin
  Src := TDOMElement(ANode).GetAttribute('src');
  // Wrap node content in CDATA section (enables special characters like '>' in JavaScript)
  // If RepairBrokenHTML was called, the special characters of already existing CDATA section
  // have been escaped, so wrapping them again (ANodeTextContent) can create nested CDATA
  // sections but thats not a problem for the browser/document.
  if (Src = '') then
  begin
    // This is only done with broken HTML (see SelectNode()), so the original code isn't touched.
    if (FHTMLBroken) then
    begin
      ScriptNodeContent :=
        NL +
        '/*<!--*/' +
        NL +
        ANode.TextContent +
        NL +
        '/*-->*/' +
        NL +
        '//';
      while (ANode.ChildNodes.Count > 0) do
        ANode.RemoveChild(ANode.ChildNodes[0]);
      with ANode do
      begin
        AppendChild(FXMLSourceDocument.CreateTextNode(NL));
        AppendChild(FXMLSourceDocument.CreateTextNode('/*'));
        AppendChild(FXMLSourceDocument.CreateCDATASection(ScriptNodeContent));
        AppendChild(FXMLSourceDocument.CreateTextNode('*/'));
        AppendChild(FXMLSourceDocument.CreateTextNode(NL));
      end;
    end;
    Exit;
  end;
  if Src[1..7] = 'file://' then
  begin
    {$IFDEF WINDOWS} // 'file:///c:/'
    ScriptFileName := Src[9..MaxInt];
    {$ELSE}
    ScriptFileName := Src[8..MaxInt];
    {$ENDIF}
  end
  else
    ScriptFileName := GetAbsolutePath(Src, FDocPath);
  try
    FileContent := GetFileContentAsString(ScriptFileName);
    ScriptNodeContent :=
      NL +
      '/*<!--*/' +
      NL +
      WideStringReplace(FileContent, '</script>', '<\/script>', [rfIgnoreCase, rfReplaceAll]) +
      NL +
      '/*-->*/' +
      NL +
      '//';
  except
    WriteLn(StdErr, 'Warning: couldn''t embed ' + ScriptFileName);
    Exit;
  end;
  ANode.Attributes.RemoveNamedItem('src');
  ANode.AppendChild(FXMLSourceDocument.CreateTextNode('//'));
  ANode.AppendChild(FXMLSourceDocument.CreateCDATASection(ScriptNodeContent));
  ANode.AppendChild(FXMLSourceDocument.CreateTextNode(NL));
end;

procedure THTMLProcessor.ImageNode(ANode: TDOMNode);
var
  Src: DOMString;
  ImageFileName: string;
  Base64Success: Boolean;
  Base64Image: DOMString;
begin
  Src := TDOMElement(ANode).GetAttribute('src');
  if (Src = '') then
    Exit;
  if Src[1..7] = 'file://' then
  begin
    {$IFDEF WINDOWS} // 'file:///c:/'
    ImageFileName := Src[9..MaxInt];
    {$ELSE}
    ImageFileName := Src[8..MaxInt];
    {$ENDIF}
  end
  else
    ImageFileName := GetAbsolutePath(Src, FDocPath);
  Base64Success := False;
  Base64Image := GetImageAsBase64String(ImageFileName, Base64Success);
  if not(Base64Success) then
    WriteLn(StdErr, 'Warning: couldn''t embed ' + ImageFileName)
  else
    TDOMElement(ANode).SetAttribute('src', Base64Image);
end;

// Node selection by tag name
procedure THTMLProcessor.SelectNode(var ANode: TDOMNode);
var
  NodeName: DOMString;
begin
  NodeName := WideLowerCase(ANode.NodeName);
  if ((NodeName = 'style') and FHTMLBroken) then
    StyleNode(ANode)
  else if (FEmbedCSS and (NodeName = 'link')) then
    LinkNode(ANode)
  else if (FEmbedJavaScript and (NodeName = 'script')) then
    ScriptNode(ANode)
  else if (FEmbedImages and (NodeName = 'img')) then
    ImageNode(ANode);
end;

procedure THTMLProcessor.ProcessAllNodes;

  procedure VisitNodes(SourceNode: TDOMNode);
  begin
    if (SourceNode = nil) then
      Exit;
    SelectNode(SourceNode);
    SourceNode := SourceNode.FirstChild;
    while (SourceNode <> nil) do
    begin
      VisitNodes(SourceNode);
      SourceNode := SourceNode.NextSibling;
    end;
  end;

begin
  VisitNodes(FXMLSourceDocument.DocumentElement);
end;

constructor THTMLProcessor.Create(ADocPath: string; AEmbedCSS, AEmbedJavaScript, AEmbedImages: Boolean);
begin
  FDocPath := ADocPath;
  FHTMLBroken := False;
  FEmbedCSS := AEmbedCSS;
  FEmbedJavaScript := AEmbedJavaScript;
  FEmbedImages := AEmbedImages;
  FDeleteList := TDOMNodes.Create(False);
end;

// public
destructor THTMLProcessor.Destroy;
begin
  FDeleteList.Free;
  inherited Destroy;
end;

function THTMLProcessor.ProcessHTML(AInput: TStringList; out ProcessingMessage: string): Boolean;
var
  Buffer: TStringStream;
  DocumentStringContent: WideString;
  FirstErrMsg, SecondErrMsg: string;
  HeadElement, Title: TDOMNode;
begin
  Result := False;
  if (AInput.Count = 0) then
    Exit;
  FirstErrMsg := '';
  SecondErrMsg := '';
  ProcessingMessage := '';
  DocumentStringContent := StoreAndRemoveHeader(AInput);
  Buffer := TStringStream.Create('');
  FXMLSourceDocument := nil;
  try
    // Initial parsing
    try
      // Hack: this incomplete DOCTYPE is sufficient to make the FPC XML parser accept any HTML entities like &nbsp;
      ParseXMLDocument('<!DOCTYPE html SYSTEM " " >' + NL + DocumentStringContent, FXMLSourceDocument, nil, True);
    except
      FirstErrMsg := Exception(ExceptObject).Message;
      FHTMLBroken := False;
      // Retry and auto repair broken HTML. Unfortunately this removes CDATA sections
      // but they will be restored in StyleNode() and ScriptNode().
      DocumentStringContent := RepairBrokenHTML(DocumentStringContent, SecondErrMsg);
      // Try again
      ParseXMLDocument('<!DOCTYPE html SYSTEM " " >' + NL + DocumentStringContent, FXMLSourceDocument, nil, True);
    end;
    if (FirstErrMsg <> '') then
      ProcessingMessage := FirstErrMsg;
    HeadElement := FXMLSourceDocument.DocumentElement.FindNode('head');
    // Fix self closing title element
    if (HeadElement <> nil) then
    begin
      Title := HeadElement.FindNode('title');
      if ((Title <> nil) and (Title.TextContent = '')) then
        Title.AppendChild(NumericEntity(ENTITY_8203));
    end;
    ProcessAllNodes;
    DeleteNodes;
    // Write final result
    WriteXMLFile(FXMLSourceDocument, Buffer);
    Buffer.Position := 0;
    AInput.Clear;
    AInput.LoadFromStream(Buffer);
    RestoreHeader(AInput);
    Result := True;
  except
    Buffer.Free;
    if (FXMLSourceDocument <> nil) then
      FXMLSourceDocument.Free;
    if (SecondErrMsg <> '') then
      ProcessingMessage := FirstErrMsg + NL + SecondErrMsg + NL + Exception(ExceptObject).Message
    else
      ProcessingMessage := FirstErrMsg + NL + Exception(ExceptObject).Message;
    raise Exception.Create(ProcessingMessage);
  end;
end;


end.

