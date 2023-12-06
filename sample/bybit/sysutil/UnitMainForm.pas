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
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    procedure BybitObjectEventMessage(ASender: TObject; AMessage: String);
    procedure BybitObjectEventStatus(ASender: TObject; Active: Boolean);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Bybit.SysUtils,
  Lb.Bybit.ServerTime;

var
  localBybitObject: TBybitServerTime = nil;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  localBybitObject := TBybitServerTime.Create;
  localBybitObject.OnEventMessage := BybitObjectEventMessage;
  localBybitObject.OnStatus := BybitObjectEventStatus;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(localBybitObject) then
    FreeAndNil(localBybitObject);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  localBybitObject.Host := 'https://api-testnet.bybit.com';
  localBybitObject.Start(True);
end;

procedure TMainForm.BybitObjectEventMessage(ASender: TObject; AMessage: String);
var
  xS: String;
begin
  xS := '' +
    localBybitObject.RetCode.ToString + ' ' +
    localBybitObject.RetMsg  + ' ' +
    localBybitObject.RetTime.ToString;

  Edit1.Text := xS;
end;

procedure TMainForm.BybitObjectEventStatus(ASender: TObject; Active: Boolean);
begin
  if Active then
    Memo1.Lines.Add('Старт')
  else
    Memo1.Lines.Add('Стоп');
end;

end.
