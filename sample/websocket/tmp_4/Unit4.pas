unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, sgcBase_Classes, sgcSocket_Classes,
  sgcTCP_Classes, sgcWebSocket_Classes, sgcWebSocket_Classes_Indy,
  sgcWebSocket_Client, sgcWebSocket, Vcl.StdCtrls, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TForm4 = class(TForm)
    sgcWebSocketClient: TsgcWebSocketClient;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Button3: TButton;
    Memo1: TMemo;
    ButtonHttp: TButton;
    Client: TNetHTTPClient;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure sgcWebSocketClientConnect(Connection: TsgcWSConnection);
    procedure sgcWebSocketClientDisconnect(Connection: TsgcWSConnection;
      Code: Integer);
    procedure sgcWebSocketClientMessage(Connection: TsgcWSConnection;
      const Text: string);
    procedure sgcWebSocketClientException(Connection: TsgcWSConnection;
      E: Exception);
    procedure sgcWebSocketClientError(Connection: TsgcWSConnection;
      const Error: string);
    procedure ButtonHttpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

uses
  System.Hash;

procedure TForm4.Button1Click(Sender: TObject);
begin
  sgcWebSocketClient.URL := 'wss://stream-testnet.bybit.com/v5/public/linear';
  //sgcWebSocketClient.URL := 'wss://socketsbay.com/wss/v2/1/demo/';
  sgcWebSocketClient.Active := True;
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  sgcWebSocketClient.Active := False;
end;

procedure TForm4.Button3Click(Sender: TObject);
var
  xS: String;
begin
  xS := '({"op": "subscribe", "args": ["orderbook.50.BTCUSDT"]})';
  sgcWebSocketClient.WriteData(xS);
end;

procedure TForm4.ButtonHttpClick(Sender: TObject);
var
  xURL: String;
  xResponse: IHTTPResponse;
//  xHeaders: TNetHeaders;
begin
//  SetLength(xHeaders,4);
//  xHeaders[0] := TNameValuePair.Create('X-BAPI-API-KEY','');
//  xHeaders[1] := TNameValuePair.Create('X-BAPI-TIMESTAMP','');
//  xHeaders[2] := TNameValuePair.Create('X-BAPI-SIGN','');
//  xHeaders[3] := TNameValuePair.Create('X-BAPI-RECV-WINDOW','');

{
    Testnet:
    https://api-testnet.bybit.com
    Mainnet (both endpoints are available):
    https://api.bybit.com
    https://api.bytick.com
}

  xURL := 'https://api-testnet.bybit.com/v5/announcements/index';
  xResponse := Client.Get(xURL);

  Memo1.Lines.Text := xResponse.ContentAsString;
end;

procedure TForm4.sgcWebSocketClientConnect(Connection: TsgcWSConnection);
begin
  Memo1.Lines.Add('#conneted');
end;

procedure TForm4.sgcWebSocketClientDisconnect(Connection: TsgcWSConnection;
  Code: Integer);
begin
  Memo1.Lines.Add('#disconneted');
end;

procedure TForm4.sgcWebSocketClientError(Connection: TsgcWSConnection;
  const Error: string);
begin
  Memo1.Lines.Add('#Error ' + Error);
end;

procedure TForm4.sgcWebSocketClientException(Connection: TsgcWSConnection;
  E: Exception);
begin
  Memo1.Lines.Add('#Exception ' + E.ToString);
end;

procedure TForm4.sgcWebSocketClientMessage(Connection: TsgcWSConnection;
  const Text: string);
begin
  Memo1.Lines.Add('#' + Text);
end;

end.
