unit Quik.SysUtils;

interface

{$I quik_connect.inc}

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

const
  _TDT_TABLE = 16;
  _TDT_FLOAT = 1;
  _TDT_STRING = 2;
  _TDT_BOOL = 3;
  _TDT_ERROR = 4;
  _TDT_BLANK = 5;
  _TDT_INTEGER = 6;
  _TDT_SKIP = 7;
  _TDT_NULL = -1;

type
  TTypeData = (
    tdtTable   = _TDT_TABLE,
    tdtFloat   = _TDT_FLOAT,
    tdtString  = _TDT_STRING,
    tdtBool    = _TDT_BOOL,
    tdtError   = _TDT_ERROR,
    tdtBlank   = _TDT_BLANK,
    tdtInteger = _TDT_INTEGER,
    tdtSkip    = _TDT_SKIP,
    tdtNull    = _TDT_NULL
  );

type
  TBlock = packed record
    TypeBlock: Word;
    SizeBlock: Word;
  end;

  TValueBlock = record
    Data: TBytes;
    Size: Integer;
    NameTable: String;
    Row1, Col1, Row2, Col2: Integer;
  end;
  TValueBlockList = TList<TValueBlock>;

  TValue = class(TObject)
    TypeValue: TTypeData;
  private
    FValue: Variant;
    function GetAsBoolean: Boolean;
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
  public
    constructor Create; overload;
    constructor Create(ATypeData: TTypeData; AValue: Variant); overload;
    constructor Create(AValue: Double); overload;
    constructor Create(AValue: String); overload;
    constructor Create(AValue: Boolean); overload;
    constructor Create(AValue: Integer); overload;
    procedure Clear;
    procedure Copy(const AValue: TValue);
    property Data: Variant read FValue write FValue;
  public
    property AsDouble: Double read GetAsDouble;
    property AsString: String read GetAsString;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsInteger: Integer read GetAsInteger;
  end;
  TValueList = TObjectList<TValue>;

  /// <summary>
  /// ѕарсер данных
  /// </summary>
  TParserData = class(TObject)
  private
    FData: TBytes;
    FSize: Integer;
    FPosition: Integer;
  public
    constructor Create(AData: TBytes; const ASize: LongWord); virtual;
    function Read(var Buffer; Count: Longint): Longint;
    property Size: Integer read FSize;
    property Position: Integer read FPosition write FPosition;
  end;

  TBlocks = class(TObject)
  private
    FValues: TValueList;
    FLenght: Integer;
    FColCount, FRowCount: Integer;
    function GetItems(Index: Integer): TValue;
    function GetCount: Integer;
  protected
    procedure Add(const AValue: TValue); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure AddValue(const AValue: Double); overload;
    procedure AddValue(const AValue: String); overload;
    procedure AddValue(const AValue: Boolean); overload;
    procedure AddValue(const AValue: Integer); overload;
    procedure AddErrorCode(const AErrorCode: Integer);
    procedure AddValue; overload;

    procedure CheckValues;

    procedure SetSize(const AColCount, ARowCount: Integer);
    property Items[Index: Integer]: TValue read GetItems;

    property Count: Integer read GetCount;
    property ColCount: Integer read FColCount;
    property RowCount: Integer read FRowCount;
  end;

procedure SetParserBlockData(AData: TBytes; ASize: LongWord; ABlocks: TBlocks);

implementation

{$IFDEF DEBUG}
uses
  Lb.Logger;
{$ENDIF}


function GetTypeBlock(const ATypeBlock: TTypeData): String;
begin
  case ATypeBlock of
    TTypeData.tdtTable  : Result := 'table';
    TTypeData.tdtFloat  : Result := 'flaot';
    TTypeData.tdtString : Result := 'string';
    TTypeData.tdtBool   : Result := 'bool';
    TTypeData.tdtError  : Result := 'error';
    TTypeData.tdtBlank  : Result := 'blank';
    TTypeData.tdtInteger: Result := 'integer';
    TTypeData.tdtSkip   : Result := 'skip';
  else
    Result := 'null';
  end;
end;

procedure SetParserBlockData(AData: TBytes; ASize: LongWord; ABlocks: TBlocks);

  procedure SetPaserFloat(ABlock: TBlock; AParserData: TParserData);
  var
    xValue: Double;
    xSizeBlock: Word;
  begin

    xSizeBlock := ABlock.SizeBlock;
    while xSizeBlock > 0 do
    begin
      AParserData.Read(xValue,8);
      ABlocks.AddValue(xValue);
      xSizeBlock := xSizeBlock - 8;
    end;
  end;

  procedure SetPaserBool(ABlock: TBlock; AParserData: TParserData);
  var
    xValue: Boolean;
    xSizeBlock: Word;
  begin
    xSizeBlock := ABlock.SizeBlock;
    while xSizeBlock > 0 do
    begin
      AParserData.Read(xValue,1);
      ABlocks.AddValue(xValue);
      xSizeBlock := xSizeBlock - 1;
    end;
  end;

  procedure SetPaserInt(ABlock: TBlock; AParserData: TParserData);
  var
    xValue: Integer;
    xSizeBlock: Word;
  begin
    xSizeBlock := ABlock.SizeBlock;
    while xSizeBlock > 0 do
    begin
      AParserData.Read(xValue,4);
      ABlocks.AddValue(xValue);
      xSizeBlock := xSizeBlock - 4;
    end;
  end;

  function GetStrToArr(AValue: TBytes): String;
  begin
    Result := TEncoding.ANSI.GetString(AValue);
  end;

  procedure SetPaserStr(ABlock: TBlock; AParserData: TParserData);
  var
    xValue: TBytes;
    xSizeBlock: Word;
    xLenght: Byte;
  begin
    xSizeBlock := ABlock.SizeBlock;
    while xSizeBlock > 0 do
    begin
      AParserData.Read(xLenght,1);
      xSizeBlock := xSizeBlock - 1;
      SetLength(xValue,xLenght);
      AParserData.Read(xValue[0],xLenght);
      ABlocks.AddValue(GetStrToArr(xValue));
      xSizeBlock := xSizeBlock - xLenght;
    end;
  end;

  procedure SetPaserError(ABlock: TBlock; AParserData: TParserData);
  var
    xSizeBlock: Word;
    xErrorCode: Word;
  begin
    xSizeBlock := ABlock.SizeBlock;
    while xSizeBlock > 0 do
    begin
      AParserData.Read(xErrorCode,2);
      ABlocks.AddErrorCode(xErrorCode);
      xSizeBlock := xSizeBlock - 2;
    end;
  end;

  procedure SetPaserBlankAndSkip(ABlock: TBlock; AParserData: TParserData);
  var
    i, Count: Word;
  begin
    AParserData.Read(Count,2);
    if Count > 0 then
      for i := 0 to Count - 1 do
        ABlocks.AddValue;
  end;

var
  xBlock: TBlock;
  xParserData: TParserData;
  xColCount, xRowCount: Word;
begin
  {$IFDEF LOG_QUIK_TABLE}
  TLogger.LogForm('table','SetParserBlockData');
  {$ENDIF}
  xParserData := TParserData.Create(AData,ASize);
  try
    while xParserData.Position < xParserData.Size do
    begin
      xParserData.Read(xBlock,4);
      case TTypeData(xBlock.TypeBlock) of
        TTypeData.tdtTable: begin
          xParserData.Read(xRowCount,2);
          xParserData.Read(xColCount,2);
          ABlocks.SetSize(xColCount,xRowCount);
          {$IFDEF LOG_QUIK_TABLE}
          TLogger.LogForm('table','–азмеры блока: количество строк ' + 
            IntToStr(xRowCount) + ' || количество колонок ' + 
            IntToStr(xColCount)
          );
          {$ENDIF}
        end;
        TTypeData.tdtFloat  : SetPaserFloat(xBlock,xParserData);
        TTypeData.tdtBool   : SetPaserBool(xBlock,xParserData);
        TTypeData.tdtInteger: SetPaserInt(xBlock,xParserData);
        TTypeData.tdtString : SetPaserStr(xBlock,xParserData);
        TTypeData.tdtError  : SetPaserError(xBlock,xParserData);
        TTypeData.tdtBlank  : SetPaserBlankAndSkip(xBlock,xParserData);
        TTypeData.tdtSkip   : SetPaserBlankAndSkip(xBlock,xParserData);
      else
        xParserData.Position := xParserData.Position + xBlock.SizeBlock;
      end;
    end;
  finally
    FreeAndNil(xParserData);
  end;
  {$IFDEF LOG_QUIK_TABLE}
  TLogger.Log('Blocks.Count := ' + IntToStr(ABlocks.Count));
  {$ENDIF}
end;

{ TValue }

procedure TValue.Clear;
begin
  TypeValue := TTypeData.tdtNull;
  FValue := Unassigned;
end;

constructor TValue.Create;
begin
  Clear;
end;

procedure TValue.Copy(const AValue: TValue);
begin
  TypeValue := AValue.TypeValue;
  Data := AValue.Data;
end;

constructor TValue.Create(ATypeData: TTypeData; AValue: Variant);
begin
  TypeValue := ATypeData;
  FValue := AValue;
end;

constructor TValue.Create(AValue: Double);
begin
  Create(TTypeData.tdtFloat,AValue);
end;

constructor TValue.Create(AValue: String);
begin
  Create(TTypeData.tdtString,AValue);
end;

constructor TValue.Create(AValue: Boolean);
begin
  Create(TTypeData.tdtBool,AValue);
end;

constructor TValue.Create(AValue: Integer);
begin
  Create(TTypeData.tdtInteger,AValue);
end;

function TValue.GetAsBoolean: Boolean;
begin
  try
    Result := Boolean(FValue);
  except
    Result := False;
  end;
end;

function TValue.GetAsDouble: Double;
begin
  try
    Result := Double(FValue);
  except
    Result := 0;
  end;
end;

function TValue.GetAsInteger: Integer;
begin
  try
    Result := Integer(FValue);
  except
    Result := 0;
  end;
end;

function TValue.GetAsString: String;
begin
  Result := VarToStrDef(FValue,'');
end;

{ TParserData }

constructor TParserData.Create(AData: TBytes; const ASize: LongWord);
begin
  FPosition := 0;
  FData := AData;
  FSize := ASize;
end;

function TParserData.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if (FPosition >= 0) and (Count >= 0) then
  begin
    if FSize - FPosition> 0 then
    begin
      if FSize > Count + FPosition then
        Result := Count
      else
        Result := FSize - FPosition;
      Move(FData[FPosition], Buffer, Result);
      Inc(FPosition, Result);
    end;
  end;
end;

{ TBlocks }

constructor TBlocks.Create;
begin
  FValues := TValueList.Create;
end;

destructor TBlocks.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TBlocks.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TBlocks.GetItems(Index: Integer): TValue;
begin
  Result := FValues[Index];
end;

procedure TBlocks.Clear;
begin
  FValues.Clear;
end;

procedure TBlocks.Add(const AValue: TValue);
begin
  {$IFDEF LOG_QUIK_TABLE}
  TLogger.Log('   >>. value_quik: table ' + AValue.AsString);
  {$ENDIF}
  FValues.Add(AValue);
end;

procedure TBlocks.AddValue(const AValue: Double);
begin
  Self.Add(TValue.Create(AValue));
end;

procedure TBlocks.AddValue(const AValue: String);
begin
  Self.Add(TValue.Create(AValue));
end;

procedure TBlocks.AddValue(const AValue: Boolean);
begin
  Self.Add(TValue.Create(AValue));
end;

procedure TBlocks.AddValue(const AValue: Integer);
begin
  Self.Add(TValue.Create(AValue));
end;

procedure TBlocks.AddErrorCode(const AErrorCode: Integer);
begin
  Self.Add(TValue.Create(TTypeData.tdtError,AErrorCode));
end;

procedure TBlocks.AddValue;
begin
  Self.Add(TValue.Create(TTypeData.tdtNull,Unassigned));
end;

procedure TBlocks.SetSize(const AColCount, ARowCount: Integer);
begin
  FLenght := AColCount * ARowCount;
  if FLenght > 0 then
    FValues.Capacity := FLenght;
  FColCount := AColCount;
  FRowCount := ARowCount;
end;

procedure TBlocks.CheckValues;
var
  xCountValue: Integer;
begin
  xCountValue := FColCount * FRowCount;
  if xCountValue <> FValues.Count then
    raise Exception.Create('Error Message: Ќе верна€ структура данных, разное количество €чеек');
end;

end.

