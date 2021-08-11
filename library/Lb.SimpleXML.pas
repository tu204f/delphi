unit Lb.SimpleXML;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Contnrs,
  System.Classes,
  System.Types,
  System.Generics.Defaults,
  System.Generics.Collections;

type
  TSimpleXML = class;
  TXMLNode = class;
  TXMLAttribute = class;

  TSimpleXML = class
  private
    FDescription: string;
    FEncoding: string;
    FRoot: TXMLNode;
    FDisableDescription: boolean;
    FIndent: integer;
    function GetXML: string;
    procedure SetXML(const Value: string);
    procedure SetDescription(const Value: string);
    function GetIndentString(Sender: TXMLNode): string;
  public
    property Indent: integer read FIndent write FIndent default 2;
    property xml: string read GetXML write SetXML;
    property Description: string read FDescription write SetDescription;
    property Root: TXMLNode read FRoot;
    function CreateNewRoot(const name: string): TXMLNode;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property DisableDescription: boolean read FDisableDescription write FDisableDescription;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;

  TXMLNode = class
  private
    FOwner: TSimpleXML;
    FName: string;
    FAttributes: TObjectList{$IFNDEF VER150}<TXMLAttribute>{$ENDIF};
    FChildNodes: TObjectList{$IFNDEF VER150}<TXMLNode>{$ENDIF};
    FText: string;
    FParent: TXMLNode;
    FIsAsCDATA: boolean;
    function GetAttribute(const name: string): string;
    function GetOuterXML: string;
    procedure SetAttribute(const name, Value: string);
    procedure SetText(const Value: string);
    function GetInnerXML: string;
    function GetText: string;
    function PackText(const str: string): string;
  public
    function FindAttribute(const name: string): TXMLAttribute;
    property Owner: TSimpleXML read FOwner;
    property Parent: TXMLNode read FParent;
    property OuterXML: string read GetOuterXML;
    property InnerXML: string read GetInnerXML;
    property name: string read FName write FName;
    property Text: string read GetText write SetText;
    property IsAsCDATA: boolean read FIsAsCDATA write FIsAsCDATA;
    property ChildNodes: TObjectList{$IFNDEF VER150}<TXMLNode>{$ENDIF} read FChildNodes;
    property Attributes: TObjectList{$IFNDEF VER150}<TXMLAttribute>{$ENDIF} read FAttributes;
    property Attribute[const name: string]: string read GetAttribute write SetAttribute;
    function HasAttribute(const name: string): boolean;
    function AddChildNode(const name: string): TXMLNode;
    function FindChildNode(const name: string): TXMLNode;
    constructor Create(aOwner: TSimpleXML);
    destructor Destroy; override;
    function ToString: string; {$IFNDEF VER150}override; {$ENDIF}
    function Copy(AParent: TXMLNode): TXMLNode;
  end;

  TXMLAttribute = class
  private
    FName: string;
    FValue: string;
    procedure SetValue(const aValue: string);
    function GetValue: string;
  public
    property &Name: string read FName write FName;
    property Value: string read GetValue write SetValue;
    function ToString: string; {$IFNDEF VER150}override; {$ENDIF}
  end;

implementation

uses
  System.StrUtils;

const
{$IFNDEF VER150}
  DEFAULT_DESCRIPTION = '<?xml version="1.0" encoding="UTF-8"?>';
{$ELSE}
  DEFAULT_DESCRIPTION = '<?xml version="1.0" encoding="Windows-1251"?>';
{$ENDIF}

/// <summary>
/// Поиск в буфере значения. Возвращается индекс найденного значения или -1 если не найдено
/// </summary>
function MemScan(var buffer; const Count: Integer; const b: Word): Integer; overload;
{$IFNDEF CPUX86}
var
  p: PWORD;
begin
  p := PWORD(@buffer);
  Result := 0;
  while (Result < Count) and (p[Result] <> b) do
    inc(Result);
  if Result = Count then
    Result := -1;
end;
{$ELSE}
asm
  push edi
  push ebx
  mov ebx, edx
  mov edi, EAX
  mov ax, cx
  mov ecx, edx
  cld
  repnz scasw
  jz @@found
  xor ebx,ebx
@@found:
  dec ebx
  mov eax,ebx
  sub eax,ecx
  pop ebx
  pop edi
end;
{$ENDIF}

/// <summary>
/// Поиск в буфере значения. Возвращается индекс найденного значения или -1 если не найдено
/// </summary>
function MemScan(var buffer; const Count: Integer; const b: DWORD): Integer; overload;
{$IFNDEF CPUX86}
var
  p: PDWORD;
begin
  p := PDWORD(@buffer);
  Result := 0;
  while (Result < Count) and (p[Result] <> b) do
    inc(Result);
  if Result = Count then
    Result := -1;
end;
{$ELSE}
asm
  push edi
  push ebx
  mov ebx, edx
  mov edi, EAX
  mov eax, ecx
  mov ecx, edx
  cld
  repnz scasd
  jz @@found
  xor ebx,ebx
@@found:
  dec ebx
  mov eax,ebx
  sub eax,ecx
  pop ebx
  pop edi
end;
{$ENDIF}



{TSimpleXML}

procedure TSimpleXML.Clear;
begin
  if assigned(FRoot) then
    FreeAndNil(FRoot);
  SetDescription(DEFAULT_DESCRIPTION);
end;

constructor TSimpleXML.Create;
begin
  FIndent := 2;
  SetDescription(DEFAULT_DESCRIPTION);
end;

function TSimpleXML.CreateNewRoot(const name: string): TXMLNode;
begin
  Clear;
  Result := TXMLNode.Create(self);
  Result.FName := name;
  FRoot := Result;
end;

destructor TSimpleXML.Destroy;
begin
  if assigned(FRoot) then
    FreeAndNil(FRoot);
  inherited;
end;

function TSimpleXML.GetIndentString(Sender: TXMLNode): string;
var
  i: integer;
begin
  if FIndent > 0 then begin
    i := 0;
    while (Sender <> nil) and (Sender <> FRoot) do begin
      inc(i, FIndent);
      Sender := Sender.Parent;
    end;
    if FRoot.name = '' then
      dec(i, FIndent);
    if i > 0 then
      Result := StringOfChar(' ', i);
  end;
end;

function TSimpleXML.GetXML: string;
var
  sb: TStringBuilder;
begin
  if assigned(FRoot) then begin
    sb := TStringBuilder.Create;
    try
      if FRoot.name <> '' then begin
        if not FDisableDescription then
          sb.Append(FDescription);
        if FIndent > 0 then
          sb.Append(sLineBreak);
        sb.Append(FRoot.OuterXML);
      end
      else
        sb.Append(FRoot.InnerXML);
      Result := sb.ToString;
    finally
      sb.Free;
    end;
  end
  else
    Result := '';
end;

procedure TSimpleXML.LoadFromFile(const FileName: string);
var
{$IFNDEF VER150}
  ec: TEncoding;
  buf: TArray<Byte>;
  size: integer;
{$ELSE}
  buf: TByteDynArray;
  s: string;
  ws: WideString;
  i: integer;
  enc: integer;
{$ENDIF}
  fs: TFileStream;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('Файла "%s" не существует.', [FileName]);
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(buf, fs.size);
    fs.Read(buf[0], fs.size);
{$IFNDEF VER150}
    ec := TEncoding.GetEncoding('UTF8');
    size := TEncoding.GetBufferEncoding(buf, ec, TEncoding.UTF8);
    SetXML(ec.GetString(buf, size, Length(buf) - size));
{$ELSE}
    enc := 0;
    //проверить на utf8
    if (fs.size > 3) and (buf[0] = $EF) and (buf[1] = $BB) and (buf[2] = $BF) then begin
      SetString(s, pchar(@buf[3]), fs.size - 3);
      s := Utf8ToAnsi(s);
      enc := 1;
      //проверить на unicode
    end
    else if (fs.size > 2) and (buf[0] = $FF) and (buf[1] = $FE) then begin
      SetString(ws, pchar(@buf[2]), fs.size - 2);
      s := ws;
    end
    else begin
      SetString(s, pchar(@buf[0]), fs.size);
      i := pos('?>', s);
      if i > 0 then
        SetDescription(LeftStr(s, i));
      if SameText(FEncoding, 'utf8') or SameText(FEncoding, 'utf-8') then begin
        s := Utf8ToAnsi(s);
        enc := 1;
      end;
    end;
    SetXML(s);
    case enc of
      0:
      FEncoding := 'Windows-1251';
      1:
      FEncoding := 'UTF8';
    end;
{$ENDIF}
  finally
{$IFNDEF VER150}
    if assigned(ec) and not TEncoding.IsStandardEncoding(ec) then
      ec.Free;
{$ENDIF}
    fs.Free;
    buf := nil;
  end;
end;

procedure TSimpleXML.SaveToFile(const FileName: string);
var
{$IFNDEF VER150}
  ec: TEncoding;
  buf: TArray<Byte>;
{$ELSE}
  buf: pchar;
  s: string;
{$ENDIF}
  size: integer;
  fs: TFileStream;
begin
  if FileExists(FileName) then
    if not System.SysUtils.DeleteFile(FileName) then
      raise Exception.CreateFmt('Не могу перезаписать файл "%s".', [FileName]);
{$IFNDEF VER150}
  if FEncoding <> '' then
    ec := TEncoding.GetEncoding(FEncoding)
  else
    ec := TEncoding.Unicode;
{$ENDIF}
  fs := TFileStream.Create(FileName, fmCreate);
  try
{$IFNDEF VER150}
    buf := ec.GetBytes(GetXML);
    size := Length(buf);
{$ELSE}
    s := GetXML;
    if SameText(FEncoding, 'utf8') then
      s := AnsiToUtf8(s);
    buf := pchar(s);
    size := Length(s);
{$ENDIF}
    if size > 0 then
      fs.Write(buf[0], size);
  finally
    fs.Free;
{$IFNDEF VER150}
    ec.Free;
{$ENDIF}
    buf := nil;
  end;
end;

procedure TSimpleXML.SetDescription(const Value: string);
var
  v: string;
  i: integer;
begin
  if Value <> '' then
    v := Trim(Value)
  else
    v := DEFAULT_DESCRIPTION;
  if FDescription <> v then begin
    FDescription := v;
    FEncoding := '';
    v := AnsiLowerCase(v);
    i := pos('encoding="', v);
    if i > 0 then begin
      FEncoding := midstr(v, i + 10, posEx('"', v, i + 10) - i - 10);
    end;
//    if FEncoding = '' then
//      SetDescription(DEFAULT_DESCRIPTION);
  end;
end;

procedure TSimpleXML.SetXML(const Value: string);
var
  s: string;
  ps, tmp: pchar;
  r, n: TXMLNode;
  v: string;

  function _unsafe(const str: string): string;
  type
    TWDRec = record
      t: string;
      c: Char;
    end;
  var
    sb: TStringBuilder;
    i, p: integer;
    upd: boolean;

  const
    cWds: array [0 .. 3] of TWDRec = (//
      (t: 'quot;'; c: '"'), //
      (t: 'lt;'; c: '<'), //
      (t: 'gt;'; c: '>'), //
      (t: 'amp;'; c: '>')//
      );

    function _checkWd: boolean;
    var
      j: integer;
    begin
      for j := 0 to high(cWds) do begin
        Result := StrLIComp(pchar(v) + i, pchar(cWds[j].t), Length(cWds[j].t)) = 0;
        if Result then begin
          sb.Append(cWds[j].c);
          inc(i, 1 + Length(cWds[j].t));
          p := i;
          Exit;
        end;
      end;
      Result := false;
    end;

    function _checkNumCh: boolean;
    var
      n: integer;
      tmp: integer;
    begin
      //&#число;
      Result := v[i + 1] = '#';
      if Result then begin
        tmp := i;
        inc(i, 2);
        n := 0;
        while i <= Length(v) do begin
          case v[i] of
            '0' .. '9': begin
              n := 10 * n + Ord(v[i]) - Ord('0');
            end;
            ';': begin
              sb.Append(Char(n));
              inc(i);
              p := i;
              Result := True;
              Exit;
            end;
            else
            Break;
          end;
          inc(i);
        end;
        i := tmp;
      end;
    end;

  begin
    //поменять псевдосимволы на символы
    sb := TStringBuilder.Create;
    try
      v := str;
      repeat
        i := 1;
        p := i;
        upd := false;
        while i <= Length(v) do begin
          if v[i] = '&' then begin
            sb.Append(v, p - 1, i - p);
            if _checkWd or _checkNumCh then begin
              upd := True;
              Continue;
            end;
          end;
          inc(i);
        end;
        if p < i then
          sb.Append(v, p - 1, i - p);
        v := sb.ToString;
        sb.Clear;
      until not upd;
      Result := v;
    finally
      sb.Free;
    end;
  end;

  procedure _toSpace(var word: string);
  var
    b: pchar;
  begin
    b := ps;
    while not(AnsiChar(ps^) in [#0, ' ', #13, #10, #9, '/', '>']) do
      inc(ps);
    SetString(word, b, ps - b);
  end;

  function _newNode(aParent: TXMLNode): TXMLNode;
  var
    word: string;
    name, Value: string;
    t: integer;
    a: TXMLAttribute;
    n: TXMLNode;
    pt: integer;
    pb: integer;
    s: string;
  label
    _freeAndExit;
  begin
    Result := nil;
    inc(ps);
    _toSpace(word);
    if (word = '') or (ps^ = #0) then
      Exit;
    Result := TXMLNode.Create(self);
    Result.FParent := aParent;
    Result.name := word;
    if ps^ = ' ' then begin //атрибуты?
      repeat
        while ps^ <= ' ' do
          if ps^ <> #0 then
            inc(ps)
          else
            goto _freeAndExit;
        if (ps^ <> '>') and (ps^ <> '/') then
        begin
          t := MemScan(ps^, tmp - ps,
{$IFDEF UNICODE}
            system.Word(Char('='))
{$ELSE}
            system.Byte(Char('='))
{$ENDIF}
            );
          if t < 0 then
            goto _freeAndExit;
          SetString(name, ps, t);
          ps := ps + t + 1;
          t := MemScan(ps^, tmp - ps,
{$IFDEF UNICODE}
            system.Word(Char('"'))
{$ELSE}
            system.Byte(Char('"'))
{$ENDIF}
            );
          if t < 0 then
            goto _freeAndExit;
          ps := ps + t + 1;
          t := MemScan(ps^, tmp - ps,
{$IFDEF UNICODE}
            system.Word(Char('"'))
{$ELSE}
            system.Byte(Char('"'))
{$ENDIF}
            );
          if t < 0 then
            goto _freeAndExit;
          SetString(Value, ps, t);
          ps := ps + t + 1;
          a := TXMLAttribute.Create;
          a.FName := name;
          a.Value := _unsafe(Value);
          Result.FAttributes.Add(a);
        end;
      until (ps^ = '>') or (ps^ = '/');
    end;
    if ps^ = '>' then begin //дети или текст?
      inc(ps);
      repeat
        t := MemScan(ps^, tmp - ps,
{$IFDEF UNICODE}
          system.Word(Char('<'))
{$ELSE}
          system.Byte(Char('<'))
{$ENDIF}
          );
        if t < 0 then //глотаем ошибку не закрытого нода
          Exit;
        SetString(word, ps, t);
        ps := ps + t;
        word := Trim(_unsafe(word));
        if (word <> '') then begin
          pt := 1;
          repeat
            pb := posEx(word, #10, pt);
            if pb = 0 then
              pb := Length(word) + 1;
            s := Trim(Copy(word, pt, pb - pt));
            if s <> '' then begin
              if Result.FText <> '' then
                Result.FText := Result.FText + ' ';
              Result.FText := Result.FText + s;
            end;
            pt := pb + 1;
          until pt > Length(word);
        end;
        if {$IFDEF UNICODE}pdword(@ps[1])^ = $2D0021{$ELSE}pword(@ps[1])^ = $2D21{$ENDIF} then begin
          //<!-- коментарий проглатываем
          t := strpos(ps, '-->') - ps;
          if t < 0 then
            Exit;
          ps := ps + t + 3;
        end
        else if StrLComp(ps, '<![CDATA[', 9) = 0 then begin
          //<![CDATA[ - обрабатываем
          ps := ps + 9;
          t := strpos(ps, ']]>') - ps;
          SetString(word, ps, t);
          if (word <> '') then begin
            Result.FText := Result.FText + word;
          end;
          ps := ps + t + 3;
          Result.FIsAsCDATA := True;
        end
        else if ps[1] <> '/' then begin //subNode
          n := _newNode(Result);
          if n <> nil then
            Result.ChildNodes.Add(n);
        end
        else begin
          //close node?
          word := Format('</%s>', [Result.FName]);
          if AnsiStrLIComp(ps, pchar(word), Length(word)) = 0 then
            ps := ps + Length(word)
          else
            inc(ps);
          Break;
        end;
      until false;
    end
    else if
{$IFDEF UNICODE}pdword(ps)^ = $003E002F{$ELSE}pword(@ps[1])^ = $3E2F{$ENDIF}{/>} then begin
      inc(ps, 2);
    end
    else
    _freeAndExit:
      FreeAndNil(Result);
  end;

begin
  Clear;
  s := Trim(Value);
  ps := pchar(s);
  //парсинг входящей строки
  //может быть, а может и не быть заголовка xml
  if AnsiStrLIComp(ps, '<?xml ', 6) = 0 then begin
    tmp := AnsiStrPos(ps, '?>');
    if tmp = nil then
      Exit;
    SetDescription(Copy(s, 1, tmp + 2 - ps));
    ps := tmp + 2;
  end;
  tmp := pchar(s) + Length(s);
  r := TXMLNode.Create(self);
  try
   //может быть 1 рутовый нод, а может быть и множество
    while ps <> nil do begin
      ps := AnsiStrPos(ps, '<');
      if ps <> nil then begin
        n := _newNode(nil);
        if n <> nil then
          r.ChildNodes.Add(n)
        else
          Break;
      end;
    end;
    case r.ChildNodes.Count of
      1: begin
        FRoot := r.ChildNodes[0];
        r.ChildNodes.OwnsObjects := false;
        try
          r.ChildNodes.Clear;
        finally
          r.ChildNodes.OwnsObjects := True;
        end;
      end;
      else begin
        FRoot := r;
        r := nil;
      end;
    end;
  finally
    if assigned(r) then
      FreeAndNil(r);
  end;
end;

{TXMLAttribute}

function TXMLAttribute.GetValue: string;
begin
  Result := FValue;
end;

procedure TXMLAttribute.SetValue(const aValue: string);
begin
  FValue := aValue;
end;

function TXMLAttribute.ToString: string;
  function _safe(const str: string): string;
  begin
    Result := StringReplace(StringReplace(str, '&', '&amp;', [rfReplaceAll]), '"', '&quot;', [rfReplaceAll]);
  end;

begin
  Result := Format('%s="%s"', [FName, _safe(Value)]);
end;

{TXMLNode}

function TXMLNode.AddChildNode(const name: string): TXMLNode;
begin
  Result := TXMLNode.Create(FOwner);
  Result.FName := name;
  Result.FParent := self;
  FChildNodes.Add(Result);
end;

function TXMLNode.Copy(AParent: TXMLNode): TXMLNode;
var
  i: integer;
  a: TXMLAttribute;
  n: TXMLNode;
begin
  Result := TXMLNode.Create(AParent.Owner);
  Result.FParent := AParent;
  Result.FName := FName;
  Result.FText := FText;
  Result.FIsAsCDATA := FIsAsCDATA;
  for i := 0 to FAttributes.Count - 1 do begin
    a := TXMLAttribute.Create;
    a.FName := TXMLAttribute(FAttributes[i]).FName;
    a.FValue := TXMLAttribute(FAttributes[i]).FValue;
    Result.FAttributes.Add(a);
  end;
  for i := 0 to FChildNodes.Count - 1 do begin
    n := TXMLNode(FChildNodes[i]).Copy(Result);
    Result.ChildNodes.Add(n);
  end;
end;

constructor TXMLNode.Create(aOwner: TSimpleXML);
begin
  FOwner := aOwner;
  FChildNodes := TObjectList{$IFNDEF VER150}<TXMLNode>{$ENDIF}.Create;
  FAttributes := TObjectList{$IFNDEF VER150}<TXMLAttribute>{$ENDIF}.Create;
end;

destructor TXMLNode.Destroy;
begin
  FreeAndNil(FChildNodes);
  FreeAndNil(FAttributes);
  inherited;
end;

function TXMLNode.FindAttribute(const name: string): TXMLAttribute;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FAttributes.Count - 1 do
    if SameText(TXMLAttribute(FAttributes[i]).FName, name) then begin
      Result := (TXMLAttribute(FAttributes[i]));
      Exit;
    end;
end;

function TXMLNode.FindChildNode(const name: string): TXMLNode;
var
  i: integer;
begin
  for i := 0 to ChildNodes.Count - 1 do begin
    Result := ChildNodes[i];
    if SameStr(Result.name, name) then
      Exit;
  end;
  Result := nil;
end;

function TXMLNode.GetAttribute(const name: string): string;
var
  a: TXMLAttribute;
begin
  a := FindAttribute(name);
  if a <> nil then
    Result := a.Value
  else
    Result := '';
end;

function TXMLNode.GetInnerXML: string;
var
  sb: TStringBuilder;
  i: integer;
begin
  if FChildNodes.Count > 0 then begin
    sb := TStringBuilder.Create;
    try
      for i := 0 to FChildNodes.Count - 1 do begin
        sb.Append(TXMLNode(FChildNodes[i]).OuterXML);
      end;
      Result := sb.ToString;
    finally
      sb.Free;
    end;
  end
  else
    Result := '';
end;

function TXMLNode.GetOuterXML: string;
var
  i: integer;
  sb: TStringBuilder;
  notClosed: boolean;
  withIndent: boolean;
  Indent: string;
begin
  withIndent := FOwner.FIndent > 0;
  Indent := FOwner.GetIndentString(self);
  sb := TStringBuilder.Create;
  try
    if withIndent then
      sb.Append(Indent);
    sb.Append(string('<'));
    sb.Append(FName);
    notClosed := True;
    if FAttributes.Count > 0 then begin
      for i := 0 to FAttributes.Count - 1 do begin
        sb.Append(string(' '));
        sb.Append(TXMLAttribute(FAttributes[i]).ToString);
      end;
    end;
    if FChildNodes.Count > 0 then begin
      if notClosed then begin
        sb.Append(string('>'));
        if withIndent then
          sb.Append(sLineBreak);
      end;
      notClosed := false;
      for i := 0 to FChildNodes.Count - 1 do
        sb.Append(TXMLNode(FChildNodes[i]).OuterXML);
    end;
    if FText <> '' then begin
      if notClosed then begin
        sb.Append(string('>'));
        if withIndent then
          sb.Append(sLineBreak);
      end;
      notClosed := false;
      if withIndent then
        sb.Append(Indent);
      sb.Append(PackText(FText));
      if withIndent then
        sb.Append(sLineBreak);
    end;
    if notClosed then begin
      sb.Append(' />');
    end
    else begin
      if withIndent then
        sb.Append(Indent);
      sb.Append('</');
      sb.Append(FName);
      sb.Append(string('>'));
    end;
    if withIndent then
      sb.Append(sLineBreak);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TXMLNode.GetText: string;
begin
  Result := FText;
end;

function TXMLNode.HasAttribute(const name: string): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to FAttributes.Count - 1 do
    if CompareStr(TXMLAttribute(FAttributes[i]).FName, name) = 0 then
      Exit;
  Result := false;
end;

function TXMLNode.PackText(const str: string): string;
var
  sr: TStringDynArray;
  i: integer;

  function _safe(const s: string): string;
  begin
    Result := StringReplace(StringReplace(s, '&', '&amp;', [rfReplaceAll]), '<', '&lt;', [rfReplaceAll]);
  end;

begin
  if IsAsCDATA then begin
    Result := FOwner.GetIndentString(self) + '<![CDATA[' + StringReplace(str, ']]>', ']]]]><![CDATA[>', [rfReplaceAll]) + ']]>';
  end
  else if FOwner.FIndent > 0 then begin
    sr := SplitString(str, sLineBreak);
    with TStringBuilder.Create do
      try
        for i := 0 to high(sr) do
          Append(Trim(_safe(sr[i]))).Append(' ');
        if Length > 0 then
          Length := Length - 1;
        Result := ToString;
      finally
        Free;
      end;
  end
  else begin
    Result := _safe(str);
  end;
end;

procedure TXMLNode.SetAttribute(const name, Value: string);
var
  a: TXMLAttribute;
begin
  a := FindAttribute(name);
  if a = nil then begin
    a := TXMLAttribute.Create;
    FAttributes.Add(a);
    a.FName := name;
  end;
  a.SetValue(Value);
end;

procedure TXMLNode.SetText(const Value: string);
begin
  FText := Value;
end;

function TXMLNode.ToString: string;
begin
  Result := GetOuterXML;
end;

end.
