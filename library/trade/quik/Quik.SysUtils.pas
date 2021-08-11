unit Quik.SysUtils;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

const
  TDT_TABLE = 16;
  TDT_FLOAT = 1;
  TDT_STRING = 2;
  TDT_BOOL = 3;
  TDT_ERROR = 4;
  TDT_BLANK = 5;
  TDT_INTEGER = 6;
  TDT_SKIP = 7;
  TDT_NULL = -1;

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

  TValue = record
    TypeValue: Integer;
    Value: Variant;
  public
    constructor Create(ATypeValue: Integer; AValue: Variant);
  end;
  TValueList = TList<TValue>;
  TValues = TArray<TValue>;

  /// <summary>
  /// Парсер данных
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
    FValues: TValues;
    FCount: Integer;
    FLenght: Integer;
    FColCount, FRowCount: Integer;
    function GetItems(Index: Integer): TValue;
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
    procedure SetSize(const AColCount, ARowCount: Integer);
    property Items[Index: Integer]: TValue read GetItems;
    property Count: Integer read FCount;
    property ColCount: Integer read FColCount;
    property RowCount: Integer read FRowCount;
  end;

procedure SetParserBlockData(AData: TBytes; ASize: LongWord; ABlocks: TBlocks);

implementation


function GetTypeBlock(const TypeBlock: Integer): String;
begin
  case TypeBlock of
    TDT_TABLE: Result := 'table';
    TDT_FLOAT: Result := 'flaot';
    TDT_STRING: Result := 'string';
    TDT_BOOL: Result := 'bool';
    TDT_ERROR: Result := 'error';
    TDT_BLANK: Result := 'blank';
    TDT_INTEGER: Result := 'integer';
    TDT_SKIP: Result := 'skip';
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

  function GetStrToArr(AValue: array of AnsiChar): String;
  var
    tmpS: AnsiString;
    xC: AnsiChar;
  begin
    tmpS := '';
    for xC in AValue  do
      tmpS := tmpS + xC;
    Result := Trim(String(tmpS));
  end;

  procedure SetPaserStr(ABlock: TBlock; AParserData: TParserData);
  var
    xValue: array of AnsiChar;
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
  xParserData := TParserData.Create(AData,ASize);
  try
    while xParserData.Position < xParserData.Size do
    begin
      xParserData.Read(xBlock,4);
      case xBlock.TypeBlock of
        TDT_TABLE: begin
          xParserData.Read(xRowCount,2);
          xParserData.Read(xColCount,2);
          ABlocks.SetSize(xColCount,xRowCount);
        end;
        TDT_FLOAT: SetPaserFloat(xBlock,xParserData);
        TDT_BOOL: SetPaserBool(xBlock,xParserData);
        TDT_INTEGER: SetPaserInt(xBlock,xParserData);
        TDT_STRING: SetPaserStr(xBlock,xParserData);
        TDT_ERROR: SetPaserError(xBlock,xParserData);
        TDT_BLANK: SetPaserBlankAndSkip(xBlock,xParserData);
        TDT_SKIP: SetPaserBlankAndSkip(xBlock,xParserData);
      else
        xParserData.Position := xParserData.Position + xBlock.SizeBlock;
      end;
    end;
  finally
    FreeAndNil(xParserData);
  end;
end;

{ TValue }

constructor TValue.Create(ATypeValue: Integer; AValue: Variant);
begin
  TypeValue := ATypeValue;
  Value := AValue;
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
  Self.Clear;
end;

destructor TBlocks.Destroy;
begin
  inherited;
end;

function TBlocks.GetItems(Index: Integer): TValue;
begin
  Result := FValues[Index];
end;

procedure TBlocks.Clear;
begin
  FCount := 0;
  SetLength(FValues,FCount);
end;

procedure TBlocks.Add(const AValue: TValue);
begin
  FValues[FCount] := AValue;
  Inc(FCount);
end;

procedure TBlocks.AddValue(const AValue: Double);
begin
  Self.Add(TValue.Create(TDT_FLOAT,AValue));
end;

procedure TBlocks.AddValue(const AValue: String);
begin
  Self.Add(TValue.Create(TDT_STRING,AValue));
end;

procedure TBlocks.AddValue(const AValue: Boolean);
begin
  Self.Add(TValue.Create(TDT_BOOL,AValue));
end;

procedure TBlocks.AddValue(const AValue: Integer);
begin
  Self.Add(TValue.Create(TDT_INTEGER,AValue));
end;

procedure TBlocks.AddErrorCode(const AErrorCode: Integer);
begin
  Self.Add(TValue.Create(TDT_ERROR,AErrorCode));
end;

procedure TBlocks.AddValue;
begin
  Self.Add(TValue.Create(TDT_NULL,Unassigned));
end;

procedure TBlocks.SetSize(const AColCount, ARowCount: Integer);
begin
  FLenght := AColCount * ARowCount;
  if FLenght > 0 then
    SetLength(FValues,FLenght);
  FColCount := AColCount;
  FRowCount := ARowCount;
end;


end.

