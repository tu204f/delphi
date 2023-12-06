program HttpServer;

{$APPTYPE CONSOLE}

{$I zLib.inc}

uses
  SysUtils
  ,Classes
  ,Net.CrossSocket.Base
  ,Net.CrossHttpServer
  ,Net.CrossHttpParams
  ,Net.OpenSSL
  ,Utils.Utils
  ,Utils.Hash
  ;

var
  __HttpServer: ICrossHttpServer;

procedure TestCrossHttpServer;
var
  LResponseStr: string;
begin
  LResponseStr := TOSVersion.ToString + '<br>Hello World!';

  __HttpServer := TCrossHttpServer.Create(0, True);
  if __HttpServer.Ssl then
  begin
    __HttpServer.SetCertificateFile('server.crt');
    __HttpServer.SetPrivateKeyFile('server.key');
  end;

  __HttpServer.Port := 8080;
  __HttpServer.Start(
      procedure(const AListen: ICrossListen; const ASuccess: Boolean)
      begin
        if ASuccess then
        begin
          if __HttpServer.Ssl then
            Writeln('HTTP server(ssl: ' + TSSLTools.LibSSL + ' & ' + TSSLTools.LibCRYPTO + ') listen on [', AListen.LocalAddr, ':' , AListen.LocalPort, ']')
          else
            Writeln('HTTP server listen on [', AListen.LocalAddr, ':' , AListen.LocalPort, ']');
        end;
      end);

  __HttpServer.Get('/',
    procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
    begin
      AResponse.Send(LResponseStr);
      AHandled := True;
    end);

  __HttpServer.Post('/upload',
    procedure(const ARequest: ICrossHttpRequest; const AResponse: ICrossHttpResponse; var AHandled: Boolean)
    var
      LHashStr: string;
      LFormField: TFormField;
    begin
      LHashStr := '';
      if (ARequest.BodyType = btMultiPart) then
      begin
        for LFormField in (ARequest.Body as THttpMultiPartFormData) do
        begin
          if (LFormField.ContentType <> '') then
          begin
            if (LHashStr <> '') then
              LHashStr := LHashStr + sLineBreak;

            LHashStr := LHashStr + 'FileName: ' + LFormField.FileName + sLineBreak;
            LHashStr := LHashStr + 'MD5: ' + THashMD5.GetHashString(LFormField.Value) + sLineBreak;
            LHashStr := LHashStr + 'SHA1: ' + THashSHA1.GetHashString(LFormField.Value) + sLineBreak;
          end;
        end;
      end;

      AResponse.Send(LHashStr);
      AHandled := True;
    end);
end;

begin
  // ��� openssl ���п�������Ĭ�����Ʋ�һ��, �����������´����޸�
  // TSSLTools.LibSSL := 'libssl.so';
  // TSSLTools.LibCRYPTO := 'libcrypto.so';

  TestCrossHttpServer;
  Readln;
end.

