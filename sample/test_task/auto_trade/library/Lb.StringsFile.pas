unit Lb.StringsFile;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Lb.SysUtils;

type
  TByteList = class(TObject)
  public type
    TBytes = TArray<Byte>;
  private
    FBytes: TBytes;
    FCount: Integer;
    FCapacity: Integer;
    function GetValue: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const AValue: Byte): Integer;
    property Bytes: TBytes read FBytes;
    property Count: Integer read FCount;
    property Value: String read GetValue;
  end;

  ///<summary>Читаем данные из потока</summary>
  TStringsFile = class(TObject)
  private
    FPosition: Int64;
    FStream: TFileStream;
    function GetEOF: Boolean;
    function GetMaxSize: Int64;
  protected
    function GetReadString: String;
  public
    constructor Create(const AFileName: String);
    destructor Destroy; override;
    function First: String;
    function Next: String;
    property EOF: Boolean read GetEOF;
    property Position: Int64 read FPosition;
    property MaxSize: Int64 read GetMaxSize;
  end;

implementation

uses
  Lb.Logger;

{ TByteList }

constructor TByteList.Create;
begin
  Clear;
end;

destructor TByteList.Destroy;
begin
  SetLength(FBytes,0);
  inherited;
end;

procedure TByteList.Clear;
begin
  FCount := 0;
  FCapacity := 256;
  SetLength(FBytes,FCapacity);
end;

function TByteList.Add(const AValue: Byte): Integer;
var
  xIndex: Integer;
begin
  xIndex := FCount;
  FBytes[xIndex] := AValue;
  Result := xIndex;
  Inc(FCount);
  if FCount >= FCapacity then
  begin
    Inc(FCapacity,256);
    SetLength(FBytes,FCapacity);
  end;
end;

function TByteList.GetValue: String;
var
  xS: String;
  xC: AnsiChar;
  i, Count: Integer;
begin
  xS := '';
  Count := Self.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
      xS := xS + AnsiChar(Self.Bytes[i]);
  Result := xS;
end;

{ TStringsFile }

constructor TStringsFile.Create(const AFileName: String);
begin
  FPosition := 0;
  FStream := TFileStream.Create(AFileName,fmOpenRead);
end;

destructor TStringsFile.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TStringsFile.GetEOF: Boolean;
begin
  Result := FPosition >= FStream.Size;
end;

function TStringsFile.GetMaxSize: Int64;
begin
  Result := FStream.Size;
end;

function TStringsFile.First: String;
begin
  FPosition := 0;
  FStream.Position := FPosition;
  Result := GetReadString;
end;

function TStringsFile.Next: String;
begin
  Result := GetReadString;
end;

function TStringsFile.GetReadString: String;
var
  xBuffer: TByteList;
  xBytes: array [0..1] of Byte;
begin
  xBuffer := TByteList.Create;
  try
    xBuffer.Clear;
    while True do
    begin
      FStream.Position := FPosition;
      FStream.Read(xBytes,2);
      Inc(FPosition);
      if (xBytes[0] = 10) or ((xBytes[0] = 13) and (xBytes[1] = 10)) then
      begin
        xBuffer.Add(xBytes[0]);
        Break;
      end
      else
        xBuffer.Add(xBytes[0]);

      if FPosition >= FStream.Size then
        Break;

    end;
    if xBuffer.Count > 0 then
      Result := xBuffer.Value
    else
      Result := '';
  finally
    FreeAndNil(xBuffer);
  end;
end;

end.
