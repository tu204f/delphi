unit Lb.Bits;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  ///<summary></summary>
  TBits = class(TObject)
  public type
    TBit = Byte;
    TBitList = TList<TBit>;
  private
    FBits: TBitList;
    function GetSize: Integer;
    function GetAsByte: Byte;
    function GetAsWord: Word;
    function GetAsLonWord: LongWord;
    function GetAsUInt64: UInt64;
    procedure SetAsByte(const Value: Byte);
    procedure SetAsWord(const Value: Word);
    procedure SetAsUInt64(const Value: UInt64);
    procedure SetAsLongWord(const Value: LongWord);
    function GetItems(Index: Integer): TBit;
  protected
    function GetBitTo: UInt64;
  public
    constructor Create;
    destructor Destroy; override;
    property Size: Integer read GetSize;
    property Items[Index: Integer]: TBit read GetItems;
    function AsValue(const AIndexBegin, AIndexEnd: Byte): Byte;
    function ToString: String; override;
  public
    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsWord: Word read GetAsWord write SetAsWord;
    property AsLongWord: LongWord read GetAsLonWord write SetAsLongWord;
    property AsUInt64: UInt64 read GetAsUInt64 write SetAsUInt64;
  end;

implementation

uses
  Math;

{ TBits }

constructor TBits.Create;
begin
  FBits := TBitList.Create;
end;

destructor TBits.Destroy;
begin
  FreeAndNil(FBits);
  inherited;
end;


function TBits.GetSize: Integer;
begin
  Result := FBits.Count;
end;

function TBits.GetItems(Index: Integer): TBit;
begin
  Result := FBits[Index];
end;

function TBits.ToString: String;
var
  xS: String;
  xB: TBit;
  i, iCount: Integer;
begin
  xS := '';
  iCount := FBits.Count;
  if iCount > 0 then
    for i := iCount - 1 downto 0 do
    begin
      xB := FBits[i];
      xS := xS + '[' + i.ToString + ']:' + xB.ToString;
    end;
  Result := xS;
end;

function TBits.GetBitTo: UInt64;
var
  xBit: TBit;
  xValue: UInt64;
begin
  {todo: Нужно перейти на обощение процедуру}
  xValue := 0;
  for var i := 0 to FBits.Count - 1 do
  begin
    xBit := FBits[i];
    if xBit = 1 then
      xValue := xValue + Round(IntPower(2,i));
  end;
  Result := xValue;
end;

function TBits.AsValue(const AIndexBegin, AIndexEnd: Byte): Byte;
var
  xBit: TBit;
  xValue: Byte;
begin
  {todo: Нужно поставить защиту от дурака}
  xValue := 0;
  for var i := AIndexBegin to AIndexEnd do
  begin
    xBit := FBits[i];
    if xBit = 1 then
      xValue := xValue + Round(IntPower(2,i));
  end;
  Result := xValue;
end;

procedure TBits.SetAsByte(const Value: Byte);

  function _IsBit(AValue: Byte; APosition: Integer): Boolean;
  begin
    Result := (AValue and (1 shl APosition)) > 0;
  end;

  function _IsBitSet(AValue: Byte; APosition: Integer): TBits.TBit;
  begin
    if _IsBit(AValue, APosition) then
      Result := 1
    else
      Result := 0;
  end;

var
  i, xSize: Integer;
begin
  FBits.Clear;
  xSize := SizeOf(Value) * 8;
  for i := 0 to xSize - 1 do
    FBits.Add(_IsBitSet(Value,i));
end;

function TBits.GetAsByte: Byte;
begin
  Result := GetBitTo;
end;

procedure TBits.SetAsWord(const Value: Word);

  function _IsBit(AValue: Word; APosition: Integer): Boolean;
  begin
    Result := (AValue and (1 shl APosition)) > 0;
  end;

  function _IsBitSet(AValue: Word; APosition: Integer): TBits.TBit;
  begin
    if _IsBit(AValue, APosition) then
      Result := 1
    else
      Result := 0;
  end;

var
  i, xSize: Integer;
begin
  FBits.Clear;
  xSize := SizeOf(Value) * 8;
  for i := 0 to xSize - 1 do
    FBits.Add(_IsBitSet(Value,i));
end;

function TBits.GetAsWord: Word;
begin
  Result := GetBitTo;
end;


procedure TBits.SetAsLongWord(const Value: LongWord);

  function _IsBit(AValue: LongWord; APosition: Integer): Boolean;
  begin
    Result := (AValue and (1 shl APosition)) > 0;
  end;

  function _IsBitSet(AValue: LongWord; APosition: Integer): TBits.TBit;
  begin
    if _IsBit(AValue, APosition) then
      Result := 1
    else
      Result := 0;
  end;

var
  i, xSize: Integer;
begin
  FBits.Clear;
  xSize := SizeOf(Value) * 8;
  for i := 0 to xSize - 1 do
    FBits.Add(_IsBitSet(Value,i));
end;

function TBits.GetAsLonWord: LongWord;
begin
  Result := GetBitTo;
end;

procedure TBits.SetAsUInt64(const Value: UInt64);

  function _IsBit(AValue: LongWord; APosition: Integer): Boolean;
  begin
    Result := (AValue and (1 shl APosition)) > 0;
  end;

  function _IsBitSet(AValue: LongWord; APosition: Integer): TBits.TBit;
  begin
    if _IsBit(AValue, APosition) then
      Result := 1
    else
      Result := 0;
  end;

var
  i, xSize: Integer;
begin
  FBits.Clear;
  xSize := SizeOf(Value) * 8;
  for i := 0 to xSize - 1 do
    FBits.Add(_IsBitSet(Value,i));
end;

function TBits.GetAsUInt64: UInt64;
begin
  Result := GetBitTo;
end;

end.
