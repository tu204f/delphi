program websocket;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form4},
  IdWebSocketSimpleClient in 'lib\IdWebSocketSimpleClient.pas',
  Lb.WebSocketClient in 'lib\Lb.WebSocketClient.pas',
  WS.Framing in 'lib\wsocket\WS.Framing.pas',
  WS.Functions in 'lib\wsocket\WS.Functions.pas',
  Lb.Base64 in 'lib\function\Lb.Base64.pas',
  Lb.Sha1 in 'lib\function\Lb.Sha1.pas',
  Lb.Url in 'lib\function\Lb.Url.pas',
  Lb.MD5 in 'lib\function\Lb.MD5.pas',
  Lb.Bits in 'lib\function\Lb.Bits.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainForm, FormMainForm);
  Application.Run;
end.
