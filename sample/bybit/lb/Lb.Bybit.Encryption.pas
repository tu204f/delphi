unit Lb.Bybit.Encryption;

interface

uses
  System.Classes,
  System.SysUtils,
  System.math,
  System.Threading,
  System.SyncObjs,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.JSON;

type
  ///<summary>Шифрование объекта</summary>
  TEncryption = class(TObject)
  private
    FApiKey: String;
    FApiSecret: String;
    FTimestamp: String;
    FRecvWindow: String;
    FQueryBody: String;
    function GetSignature: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
    property Timestamp: String read FTimestamp write FTimestamp;
    property RecvWindow: String read FRecvWindow write FRecvWindow;
    property QueryBody: String read FQueryBody write FQueryBody;
    property Signature: String read GetSignature;
  end;

implementation

uses
  System.Hash,
  System.DateUtils;

{ TEncryption }

constructor TEncryption.Create;
begin
  FApiKey    := '';
  FApiSecret := '';
  FTimestamp := (1000 * DateTimeToUnix(Now,True)).ToString;
  FRecvWindow:= '20000';
  FQueryBody := '';
end;

destructor TEncryption.Destroy;
begin

  inherited;
end;

function TEncryption.GetSignature: String;
var
  xValue: String;
begin
  xValue :=
    FTimestamp +    // Время
    FApiKey +       // Публичный ключ
    FRecvWindow +   //
    FQueryBody;     // Теля запроса

  Result := THashSHA2.GetHMAC(
    xValue,
    FApiSecret,
    THashSHA2.TSHA2Version.SHA256
  );
end;

end.
