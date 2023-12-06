program WebSocketServer;

{$APPTYPE CONSOLE}

{$I zLib.inc}

uses
  SysUtils
  ,Classes
  ,Net.CrossSocket.Base
  ,Net.CrossHttpServer
  ,Net.CrossWebSocketServer
  ,Net.CrossWebSocketParser
  ,Net.OpenSSL
  ,Utils.Utils
  ;

var
  __HttpServer: ICrossWebSocketServer;

procedure TestCrossHttpServer;
var
  LResponseStr: string;
begin
  __HttpServer := TCrossWebSocketServer.Create(2, True);
  if __HttpServer.Ssl then
  begin
    __HttpServer.SetCertificateFile('server.crt');
    __HttpServer.SetPrivateKeyFile('server.key');
  end;

  __HttpServer.Port := 8090;
  __HttpServer.Start(
      procedure(const AListen: ICrossListen; const ASuccess: Boolean)
      begin
        if ASuccess then
        begin
          if __HttpServer.Ssl then
            Writeln('WebSocket server(ssl: ' + TSSLTools.LibSSL + ' & ' + TSSLTools.LibCRYPTO + ') listen on [', AListen.LocalAddr, ':' , AListen.LocalPort, ']')
          else
            Writeln('WebSocket server listen on [', AListen.LocalAddr, ':' , AListen.LocalPort, ']');
        end;
      end);

  __HttpServer.Get('/',
    procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
    begin
      LResponseStr := TOSVersion.ToString + '<br>Hello World!';
      AResponse.Send(LResponseStr);
      AHandled := True;
    end);

  __HttpServer.OnOpen(
    procedure(const AConnection: ICrossWebSocketConnection)
    begin
      Writeln(Format('[%s : %d]Open', [AConnection.PeerAddr, AConnection.PeerPort]));
    end);

  __HttpServer.OnClose(
    procedure(const AConnection: ICrossWebSocketConnection)
    begin
      Writeln(Format('[%s : %d]Close', [AConnection.PeerAddr, AConnection.PeerPort]));
    end);

  __HttpServer.OnPing(
    procedure(const AConnection: ICrossWebSocketConnection)
    begin
      Writeln(Format('[%s : %d]Ping', [AConnection.PeerAddr, AConnection.PeerPort]));
    end);

  __HttpServer.OnPong(
    procedure(const AConnection: ICrossWebSocketConnection)
    begin
      Writeln(Format('[%s : %d]Pong', [AConnection.PeerAddr, AConnection.PeerPort]));
    end);

  __HttpServer.OnMessage(
    procedure(const AConnection: ICrossWebSocketConnection;
      const AType: TWsMessageType; const AData: TBytes)
    var
      LMessage: string;
    begin
      if (AType = wtText) then
        LMessage := TUtils.GetString(AData)
      else
        LMessage := TUtils.BytesToHex(AData);

      Writeln(Format('[message][%s : %d]', [AConnection.PeerAddr, AConnection.PeerPort]), LMessage);

      AConnection.WsSend('<response>' + LMessage,
        procedure(const AWsConnection: ICrossWebSocketConnection; const ASuccess: Boolean)
        begin
            //Writeln('ws server send response: ', ASuccess);
        end);
    end);
end;

begin
  // ��� openssl ���п�������Ĭ�����Ʋ�һ��, �����������´����޸�
// TSSLTools.LibSSL := 'libssl.so';
// TSSLTools.LibCRYPTO := 'libcrypto.so';

  TestCrossHttpServer;
  Readln;
end.

