unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Lb.DataModuleDB;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }

     DataBaseMQ: TDataModuleDB;
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
begin
  //
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  //
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  DataBaseMQ := TDataModuleDB.Create(nil);
  DataBaseMQ.DefaultConnection('mq.db');
end;

procedure TForm4.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(DataBaseMQ);
end;

end.
