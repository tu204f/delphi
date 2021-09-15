unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdCustomTCPServer, IdTCPServer,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, Vcl.StdCtrls,
  IdContext;

type
  TForm2 = class(TForm)
    IdTCPClient1: TIdTCPClient;
    IdTCPServer1: TIdTCPServer;
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Button2: TButton;
    Edit2: TEdit;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure IdTCPClient1Connected(Sender: TObject);
    procedure IdTCPClient1Disconnected(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure IdTCPServer1Exception(AContext: TIdContext;
      AException: Exception);
    procedure IdTCPClient1Work(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

// ----------------------------------------------------------------------------
// сервер

procedure TForm2.Button1Click(Sender: TObject);
begin
  IdTCPClient1.Disconnect;
  IdTCPServer1.Active := True;
end;


procedure TForm2.Button5Click(Sender: TObject);
begin
  IdTCPServer1.Active := False;
end;

procedure TForm2.IdTCPServer1Connect(AContext: TIdContext);
begin
  Memo1.Lines.Add('Server - Connect');
end;

procedure TForm2.IdTCPServer1Disconnect(AContext: TIdContext);
begin
  Memo1.Lines.Add('Server - Disconnect');
end;

procedure TForm2.IdTCPServer1Exception(AContext: TIdContext;
  AException: Exception);
begin
  Memo1.Lines.Add('Server Error:' + AException.Message);
end;

procedure TForm2.IdTCPServer1Execute(AContext: TIdContext);
var
  xStr: String;
begin
  xStr := 'Cliente '+ AContext.Binding.PeerIP + ' :' + AContext.Connection.IOHandler.ReadLn;
  TThread.Queue(nil,
    procedure
    begin
      Memo1.Lines.Add(xStr);
    end
  );

end;

// ----------------------------------------------------------------------------
// Клиент

procedure TForm2.Button2Click(Sender: TObject);
begin
  IdTCPServer1.Active := False;
  IdTCPClient1.Connect;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  IdTCPClient1.Disconnect;
end;

procedure TForm2.IdTCPClient1Connected(Sender: TObject);
begin
  Memo1.Lines.Add('Client - Connect');
end;

procedure TForm2.IdTCPClient1Disconnected(Sender: TObject);
begin
  Memo1.Lines.Add('Client - Disconnect');
end;

procedure TForm2.IdTCPClient1Work(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  //
end;

end.
