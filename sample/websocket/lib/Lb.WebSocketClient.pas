unit Lb.WebSocketClient;

interface

uses

  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.DateUtils,
  System.SyncObjs,
  System.Generics.Collections,

  FMX.Dialogs,

  Lb.Bits,

  IdURI,
  IdSSLOpenSSL,
  IdTCPClient,
  IdGlobal,
  IdCoderMIME,
  IdHash,
  IdHashSHA;

type
  ///<summary>
  /// Конструкция, подобная перечислению, содержащая все коды операций,
  /// определенные в протоколе WebSocket
  ///</summary>
  TWebSocketOpcode = (
    wsDefault = 0,
    wsContinuationFrame = $00,
    wsTextFrame = $01,
    wsBinaryFrame = $02,
    wsCloseFrame = $08,
    wsPingFrame = $09,
    wsPongFrame = $0A
  );

  ///<summary>Базовый протокол кадрирования</summary>
  TWSFrame = class(TObject)
  private
    FFIN : Byte;                // Хранить значение: 0 или 1
    FRSV1: Byte;                // Хранить значение: 0 или 1
    FRSV2: Byte;                // Хранить значение: 0 или 1
    FRSV3: Byte;                // Хранить значение: 0 или 1
    FOpCode: TWebSocketOpcode;  // 4 бита
    FMasked: Byte;              // Хранить значение: 0 или 1
    FPayloadLength: Cardinal;
    FMaskingKey: Cardinal;
    FPayloadData: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetStream(AStream: TStream);
    procedure GetStream(AStream: TStream);
  public
    property FIN : Byte read FFIN;
    property RSV1: Byte read FRSV1;
    property RSV2: Byte read FRSV2;
    property RSV3: Byte read FRSV3;
  end;

  ///<summary>Получено сообщение от сервера</summary>
  TOnEventMessage = procedure(Sender:TObject; AMessageValue: String) of object;

  ///<summary>Объект запроса</summary>
  TCustomWebSocketClient = class(TObject)
  public const
    RECIV_NULL   = 0;
    RECIV_HEADER = 1;
    RECIV_FRAME  = 2;
  private
    FResponseHeader: TStrings;
    FClientTCP: TIdTCPClient;
    FOpenSSL: TIdSSLIOHandlerSocketOpenSSL;
  private
    FOnRecivHeader: TNotifyEvent;
    function GetIsConnected: Boolean;
    procedure SetRecivThreading;
  protected
    FStatusRecivHeader: Integer;
    procedure RecivHeader(AStream: TStream);
    procedure RecivFrame(AStream: TStream);
    procedure DoReciv(AStream: TStream);
    property ClientTCP: TIdTCPClient read FClientTCP;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const AURL: String);
    procedure Close;

    procedure Send(const AText: String);

    property IsConnected: Boolean read GetIsConnected;
    ///<summary>Ответ сервера</summary>
    property ResponseHeader: TStrings read FResponseHeader;
    ///<summary>Получене заголовок сервера</summary>
    property OnRecivHeader: TNotifyEvent write FOnRecivHeader;
  end;

implementation

{ TWSFrame }

constructor TWSFrame.Create;
begin
(*
    ws-frame                = frame-fin           ; 1 bit in length
                              frame-rsv1          ; 1 bit in length
                              frame-rsv2          ; 1 bit in length
                              frame-rsv3          ; 1 bit in length
                              frame-opcode        ; 4 bits in length
                              frame-masked        ; 1 bit in length
                              frame-payload-length   ; either 7, 7+16,
                                                     ; or 7+64 bits in
                                                     ; length
                              [ frame-masking-key ]  ; 32 bits in length
                              frame-payload-data     ; n*8 bits in
                                                     ; length, where
                                                     ; n >= 0
*)
end;

destructor TWSFrame.Destroy;
begin



  inherited;
end;



procedure TWSFrame.SetStream(AStream: TStream);

  procedure _SetValueHeader(const AValueHeander: Word);
  var
    xBits: TBits;
    xOpCode: Byte;
  begin
    // Сразу парсим все 16 бит, из двух байтов
    xBits := TBits.Create;
    try
      xBits.AsWord := AValueHeander;
      FFIN  := xBits.Items[15];
      FRSV1 := xBits.Items[14];
      FRSV2 := xBits.Items[13];
      FRSV3 := xBits.Items[12];
      xOpCode := xBits.AsValue(8,4);
      FOpCode := TWebSocketOpcode(xOpCode);
      FMasked := xBits.Items[7];
      FPayloadLength := xBits.AsValue(0,6);
    finally
      FreeAndNil(xBits);
    end;
  end;

var
  xValueHeander: Word;
begin
  if AStream.Size = 2 then
  begin
    AStream.Read(xValueHeander,2);
    _SetValueHeader(xValueHeander);
  end
  else if AStream.Size > 2 then
  begin
    AStream.Read(xValueHeander,2);
    _SetValueHeader(xValueHeander);
  end;
end;

procedure TWSFrame.GetStream(AStream: TStream);
begin

end;


{ TCustomWebSocketClient }

constructor TCustomWebSocketClient.Create;
begin
  FClientTCP := TIdTCPClient.Create(nil);
  FOpenSSL := nil;
  FStatusRecivHeader := RECIV_NULL;
  FResponseHeader := TStringList.Create;
end;

destructor TCustomWebSocketClient.Destroy;
begin
  if Assigned(FOpenSSL) then
    FreeAndNil(FOpenSSL);
  FreeAndNil(FClientTCP);
  FreeAndNil(FResponseHeader);
  inherited;
end;

procedure TCustomWebSocketClient.Connect(const AURL: String);

  function _GenerateWebSocketKey: String;
  var rand:TidBytes;
    I: Integer;
  begin
    SetLength(rand, 16);
    for I := low(rand) to High(rand) do
      rand[i] := byte(random(255));
    Result := TIdEncoderMIME.EncodeBytes(Rand);  //generates a random Base64String
  end;

var
  xURI   : TIdURI;
  xSecure: Boolean;
begin
  if FClientTCP.Connected then
    raise Exception.Create('Already connected, verify');

  xURI := nil;
  try
    xURI            := TIdURI.Create(AURL);
    FClientTCP.Host := xURI.Host;
    //также заменяет wss на https, поскольку, по-видимому, indy пока не поддерживает ws(ы)
    xURI.Protocol   := ReplaceOnlyFirst(xURI.Protocol.ToLower, 'ws', 'http');
    // *
    if xURI.Path = '' then
      xURI.Path := '/';
    xSecure := xURI.Protocol = 'https';
    // *
    if xURI.Port.IsEmpty then
    begin
      if xSecure then
        FClientTCP.Port := 443
      else
        FClientTCP.Port := 80;
    end
    else
      FClientTCP.Port := StrToInt(xURI.Port);

    // ***********************************************************************
    // Установить соединение с сервером
    if xSecure and (not Assigned(FOpenSSL)) then
    begin
      FOpenSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
      FClientTCP.IOHandler := FOpenSSL;
      FOpenSSL.PassThrough := False;
      FOpenSSL.DefStringEncoding := IndyTextEncoding_UTF8;
      with FOpenSSL.SSLOptions do
      begin
        Mode := TIdSSLMode.sslmClient;
        SSLVersions := [
          TIdSSLVersion.sslvSSLv2,
          TIdSSLVersion.sslvSSLv23,
          TIdSSLVersion.sslvSSLv3,
          TIdSSLVersion.sslvTLSv1,
          TIdSSLVersion.sslvTLSv1_1,
          TIdSSLVersion.sslvTLSv1_2
        ];
      end;
    end;
    FClientTCP.Connect;
    // ************************************************************************
    // Порт соединение
    if not xURI.Port.IsEmpty then
      xURI.Host := xURI.Host + ':' + xURI.Port;
    // ************************************************************************
    // Формируем заголовок
    var xKey := _GenerateWebSocketKey;
    with FClientTCP.IOHandler do
    begin
      WriteLn('GET ' + xURI.Path + ' HTTP/1.1');
      WriteLn('Host: ' + xURI.Host);
      WriteLn('Sec-WebSocket-Version: 13');
      WriteLn('Origin: ' + xURI.Protocol + '://' + xURI.Host);
      WriteLn('Sec-WebSocket-Extensions: permessage-deflate');
      WriteLn('Sec-WebSocket-Key: ' + xKey);
      WriteLn('Connection: Upgrade');
      WriteLn('Upgrade: websocket');
      WriteLn('');
    end;
    SetRecivThreading;
  finally
    xURI.Free;
  end;
end;

procedure TCustomWebSocketClient.Close;
begin
  // **************************************************************************
  // Разорвать соединение
  FClientTCP.Disconnect;
end;

procedure TCustomWebSocketClient.RecivHeader(AStream: TStream);
var
  xB: Byte;
  xC: Char;
  xS: String;
begin
  FResponseHeader.Clear;
  xS := '';
  AStream.Position := 0;
  while AStream.Position < AStream.Size do
  begin
    AStream.Read(xB,1);
    xC := Char(xB);
    case xC of
      #10: begin
        if not xS.IsEmpty then
        begin
          FResponseHeader.Add(xS);
          xS := '';
        end;
      end;
    else
      if not CharInSet(xC,[#10,#13]) then
        xS := xS + xC;
    end;
  end;
  if not xS.IsEmpty then
  begin
    FResponseHeader.Add(xS);
    xS := '';
  end;
  if Assigned(FOnRecivHeader) then
    FOnRecivHeader(Self);
end;

procedure TCustomWebSocketClient.RecivFrame(AStream: TStream);
var
  xFrame: TWSFrame;
begin
  {Получаем структура данных}
  xFrame := TWSFrame.Create;
  try
    xFrame.SetStream(AStream);

  finally
    FreeAndNil(xFrame);
  end;
end;

procedure TCustomWebSocketClient.DoReciv(AStream: TStream);
begin
  // Полученое сообщение отсервера
  case FStatusRecivHeader of
    RECIV_HEADER: RecivHeader(AStream);
    RECIV_FRAME : RecivFrame(AStream);
  end;
  if FStatusRecivHeader = RECIV_HEADER then
    FStatusRecivHeader := RECIV_FRAME;
end;

function TCustomWebSocketClient.GetIsConnected: Boolean;
begin
  Result := FClientTCP.Connected;
end;

procedure TCustomWebSocketClient.SetRecivThreading;
var
  xTask: ITask;
begin
  // Запускам поток который читае в цикле полученные данные
  xTask := TTask.Create(
    procedure()
    var
      xStream: TMemoryStream;
    begin
      FStatusRecivHeader := RECIV_HEADER;
      repeat
        xStream := TMemoryStream.Create;
        try
          with FClientTCP.IOHandler do
          begin
            InputBuffer.SaveToStream(xStream);
            InputBuffer.Clear;
          end;
          if xStream.Size > 0 then
          begin
            TThread.Synchronize(
              nil,
              procedure()
              begin
                DoReciv(xStream);
              end
            );
          end;
        finally
          FreeAndNil(xStream);
        end;
      until not FClientTCP.Connected;
    end
  );
  xTask.Start;
end;

procedure TCustomWebSocketClient.Send(const AText: String);
begin
  if FClientTCP.Connected then
    FClientTCP.IOHandler.WriteLn(AText);
end;



end.
