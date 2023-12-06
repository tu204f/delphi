unit Lvb.WebSocket.Frame;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections;

type
  TWebSocketOpcode = (
    __default = 0,
    ContinuationFrame = $00,
    TextFrame = $01,
    BinaryFrame = $02,
    CloseFrame = $08,
    PingFrame = $09,
    PongFrame = $09
  );

  TWebSocketFrame = class(TObject)
  public
    constructor Create(ACode: TWebSocketOpcode; AData: AnsiString = ''); virtual;
    destructor Destroy; override;

    procedure SetType(ACode: TWebSocketOpcode);
    function Encode: AnsiString; virtual;
  public

    function getData: AnsiString; virtual; abstract;
    function getType: TWebSocketOpcode; virtual; abstract;
    function isReady: Boolean; virtual; abstract;
    class function decode(var raw: AnsiString; head: TWebSocketFrame = nil): TWebSocketFrame; virtual; abstract;
  public {first byte}
    FIN: Integer;
    RSV1: Integer;
    RSV2: Integer;
    RSV3: Integer;
    OpCode: TWebSocketOpcode;
  public {second byte}
    mask: Integer;
    payloadLength: Integer;
    maskingKey: AnsiString;
  public
    payloadData: AnsiString;
    actualLength: Integer;
  end;

  TWebSocketFrameEnumerator = class(TObject)
  public type
    TWebSocketFrameList = TObjectList<TWebSocketFrame>;
  private
    FWebSocketFrames: TWebSocketFrameList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AFrame: TWebSocketFrame);
    procedure Delete(AIndex: Integer);
    function Count: Integer;
    function Get(AIndex: Integer): TWebSocketFrame;
    property Frame[index: Integer]: TWebSocketFrame read Get;
  end;

  TWebSocketFrameHybie = class(TWebSocketFrame)
  protected
    // First Byte
    FIN: Integer;
    RSV1: Integer;
    RSV2: Integer;
    RSV3: Integer;
    opcode: TWebSocketOpcode;

    // Second Byte
    mask: Integer;
    payloadLength: Integer;
    maskingKey: AnsiString;

    payloadData: AnsiString;
    actualLength: Integer;

  public
    constructor Create(code: TWebSocketOpcode; data: AnsiString = ''); override;
    function encode: AnsiString; override;
  public
    function getData: AnsiString; override;
    function getType: TWebSocketOpcode; override;
    class function decode(var raw: AnsiString; head: TWebSocketFrame = nil): TWebSocketFrame; override;
    function isReady: Boolean; override;
    function isFinal: Boolean;
    function isMasked: Boolean;
  protected
    procedure setType(code: TWebSocketOpcode);
  end;

implementation

{ TWebSocketFrameEnumerator }

constructor TWebSocketFrameEnumerator.Create;
begin
  FWebSocketFrames := TWebSocketFrameList.Create;
end;

destructor TWebSocketFrameEnumerator.Destroy;
begin
  FreeAndNil(FWebSocketFrames);
  inherited;
end;

function TWebSocketFrameEnumerator.Count: Integer;
begin
  Result := FWebSocketFrames.Count;
end;

procedure TWebSocketFrameEnumerator.Add(AFrame: TWebSocketFrame);
begin
  FWebSocketFrames.Add(AFrame);
end;

procedure TWebSocketFrameEnumerator.Delete(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
    FWebSocketFrames.Delete(AIndex);
end;

function TWebSocketFrameEnumerator.Get(AIndex: Integer): TWebSocketFrame;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := FWebSocketFrames[AIndex]
  else
    raise Exception.Create('Error Message: Вышли переопределы массива');
end;

{ TWebSocketFrame }

constructor TWebSocketFrame.Create(ACode: TWebSocketOpcode; AData: AnsiString);
begin
  FIN := 1;
  RSV1 := 0;
  RSV2 := 0;
  RSV3 := 0;
  setType(code);

  mask := 0;
  payloadLength := Length(data);
  maskingKey := '';

  payloadData := data;

  actualLength := 0;
end;

destructor TWebSocketFrame.Destroy;
begin

  inherited;
end;

procedure TWebSocketFrame.SetType(ACode: TWebSocketOpcode);
begin
  opcode := ACode;
  if (ACode = CloseFrame) then
    mask := 1;
end;

function TWebSocketFrame.Encode: AnsiString;
var
  firstByte, secondByte: Byte;
  encoded, key: AnsiString;
begin
  payloadLength := Length(payloadData);

  firstByte := Integer(opcode);

  firstByte := firstByte + FIN * 128 + RSV1 * 64 + RSV2 * 32 + RSV3 * 16;

  encoded := AnsiChar(firstByte);

  if (payloadLength <= 125) then
  begin
    secondByte := payloadLength;
    secondByte := secondByte + mask * 128;
    encoded := encoded + AnsiChar(secondByte);
  end
  else
  if (payloadLength <= 255 * 255 - 1) then
  begin
    secondByte := 126;
    secondByte := secondByte + mask * 128;
    encoded := encoded + AnsiChar(secondByte) + packN16(payloadLength);
  end
  else
  begin
    secondByte := 127;
    secondByte := secondByte + mask * 128;
    encoded := encoded + AnsiChar(secondByte);
    encoded := encoded + packN64(payloadLength);
  end;

  key := '';
  if (mask = 1) then
  begin
    key := packN32(random(MaxInt));
    encoded := encoded + key;
  end;

  if (Length(payloadData) > 0) then
  begin
    if mask = 1 then
      encoded := encoded + rotMask(payloadData, key)
    else
      encoded := encoded + payloadData;
  end;

  Result := encoded;
end;

end.
