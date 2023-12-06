unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,

  Lb.WebSocketClient;

type
  TFormMainForm = class(TForm)
    MemoLog: TMemo;
    ButtonConnect: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure ButtonConnectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  protected
    procedure Log(const S: String = '');
  private
    WebSocket: TCustomWebSocketClient;
    procedure WebSocketOnRecivHeader(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMainForm: TFormMainForm;

procedure Logger(S: String);

implementation

uses
  Lb.Bits;

{$R *.fmx}

procedure Logger(S: String);
begin
  FormMainForm.Log(S);
end;

procedure TFormMainForm.Log(const S: String);
begin
  MemoLog.Lines.Add(S);
end;

constructor TFormMainForm.Create(AOwner: TComponent);
begin
  inherited;
  WebSocket := TCustomWebSocketClient.Create;
  WebSocket.OnRecivHeader := WebSocketOnRecivHeader;
end;

destructor TFormMainForm.Destroy;
begin
  WebSocket.Close;
  FreeAndNil(WebSocket);
  inherited;
end;

procedure TFormMainForm.Button1Click(Sender: TObject);
begin
  WebSocket.Send('Hello Wold');
end;

procedure TFormMainForm.ButtonConnectClick(Sender: TObject);
begin
  // wss://stream-testnet.bybit.com/v5/public/spot
  // wss://socketsbay.com/wss/v2/1/demo/
  WebSocket.Connect('wss://socketsbay.com/wss/v2/1/demo/');
end;


procedure TFormMainForm.WebSocketOnRecivHeader(Sender: TObject);
begin
  Log(WebSocket.ResponseHeader.Text);
end;


procedure TFormMainForm.Button2Click(Sender: TObject);
var
  xV: Word;
  xBits: TBits;
begin
  xV := 122;
  xBits := TBits.Create;
  try
    Log('V. ' + xV.ToString);

    xBits.AsWord := xV;
    Log('1. ' + xBits.ToString);

    xV := xBits.AsWord;
    Log('R. ' + xBits.AsByte.ToString);


    xV := xBits.AsValue(0,3);
    Log('T. ' + xV.ToString);

  finally
    FreeAndNil(xBits);
  end;
end;


end.
