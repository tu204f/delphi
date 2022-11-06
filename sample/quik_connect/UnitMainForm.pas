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
  Vcl.ExtCtrls,
  Vcl.Grids;

type
  TConnectQuikForm = class(TForm)
    LabelStatus: TLabel;
    Timer: TTimer;
    StrGrid: TStringGrid;
    ComboBoxQuikTables: TComboBox;
    Label1: TLabel;
    ButtonUpData: TButton;
    StartTimer: TButton;
    procedure FormShow(Sender: TObject);
    procedure ButtonUpDataClick(Sender: TObject);
    procedure StartTimerClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConnectQuikForm: TConnectQuikForm;

implementation

{$R *.dfm}

uses
  Quik.Manager.DDE,
  Quik.ValueTable;

var
  QuikTable: TQuikTable = nil;

procedure TConnectQuikForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Работа транслирование скрипта';
end;

procedure TConnectQuikForm.ButtonUpDataClick(Sender: TObject);
begin
  QuikManagerTable.SetNameTables(ComboBoxQuikTables.Items);
  ComboBoxQuikTables.ItemIndex := 0;
end;

procedure TConnectQuikForm.StartTimerClick(Sender: TObject);
var
  xItemIndex: Integer;
begin
  Timer.Enabled := not Timer.Enabled;
  if Timer.Enabled then
  begin
    xItemIndex := ComboBoxQuikTables.ItemIndex;
    ButtonUpData.Caption := 'Стоп';
    QuikTable := QuikManagerTable.Tables[xItemIndex];
  end
  else
    ButtonUpData.Caption := 'Старт'
end;

procedure TConnectQuikForm.TimerTimer(Sender: TObject);
begin
  if Assigned(QuikTable) then
  begin
    StrGrid.RowCount := QuikTable.RowCount + 1;
    StrGrid.ColCount := QuikTable.ColCount + 1;
    for var xRow := 0 to QuikTable.RowCount - 1 do
      for var xCol := 0 to QuikTable.ColCount - 1 do
      begin
        var xC := QuikTable.Cells[xCol,xRow];
        StrGrid.Cells[xCol, xRow] := xC.AsString;
      end;
  end;
end;

end.
