unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Quik.ValueTable,
  Quik.Manager.DDE, Vcl.ExtCtrls;

type
  TConnectQuikForm = class(TForm)
    LabelStatus: TLabel;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    ButtonSource1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonSource1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure EventAddValueBlock(Sender: TObject; APointCells: TPointCells; AQuikTable: TQuikTable);
  end;

var
  ConnectQuikForm: TConnectQuikForm;

implementation

{$R *.dfm}

uses
  Lb.Script.QPile,
  Lb.SysUtils.Table,
  Lb.Operation.V1, UnitSourceForm;

var
  OperationTrade: TOperationTrade = nil;

procedure TConnectQuikForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(OperationTrade);
  OperationTrade := nil;
end;

procedure TConnectQuikForm.FormShow(Sender: TObject);
begin
  QuikManagerTable.OnAddValueBlock := EventAddValueBlock;
  OperationTrade := TOperationTrade.Create;
end;

procedure TConnectQuikForm.Timer1Timer(Sender: TObject);
begin
  if GetIsQuikTable then
  begin
    // контролируем позицию  по бумагам
  end
  else
    SetInitializationTable;
end;

procedure TConnectQuikForm.Button1Click(Sender: TObject);
var
  xStr: TStrings;
begin
  try
    var xCaption := 'cap_sr';
    xStr := TStringList.Create;
    try
      var xS := GetResourceScritpQPL(xCaption,'SRU1','SPBFUT','MA_1',1,100);
      xStr.Text := xS;
      xStr.SaveToFile(xCaption + '.qpl')
    finally
      FreeAndNil(xStr);
    end;
  except on E: Exception do
    raise Exception.Create('Error Message: ' + sLineBreak);
  end;
end;

procedure TConnectQuikForm.Button2Click(Sender: TObject);
var
  xStr: TStrings;
begin
  var xCaption := 'cap_gz';
  xStr := TStringList.Create;
  try
    var xS := GetResourceScritpQPL(xCaption,'GZU1','SPBFUT','MA_2',1,100);
    xStr.Text := xS;
    xStr.SaveToFile(xCaption + '.qpl')
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TConnectQuikForm.ButtonSource1Click(Sender: TObject);
begin
  var xSourceForm := TSourceForm.Create(nil);
  xSourceForm.SourceCandel := SourceCandel;
  xSourceForm.Show;
end;

procedure TConnectQuikForm.EventAddValueBlock(Sender: TObject;
  APointCells: TPointCells; AQuikTable: TQuikTable);
begin
  LabelStatus.Caption := 'Состоние загрузка: ' + APointCells.ToString + ' :: ' + AQuikTable.Name;
end;

end.
