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
  Vcl.Grids,
  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    StrGrid: TStringGrid;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    QuikTable : TQuikTable;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  QuikTable := nil;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Таблица QUIK';
end;

procedure TMainForm.TimerTimer(Sender: TObject);

  procedure _SetConnectTable;
  var
    xInd: Integer;
  begin
    xInd := QuikManagerTable.IndexOfTable('s_rsi');
    if xInd >= 0 then
      QuikTable := QuikManagerTable.Tables[xInd];
  end;

  procedure _SetShowTable;
  var
    iCount, jCount, i, j: Integer;
  begin
    iCount := QuikTable.ColCount;
    jCount := QuikTable.RowCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
        for j := 0 to jCount - 1 do
        begin
          if j = 0 then
            StrGrid.Cells[i,j] := '[' + i.ToString + ']:' + QuikTable.Cells[i,j].AsString
          else
            StrGrid.Cells[i,j] := QuikTable.Cells[i,j].AsString
        end;
  end;

begin
  try
  if Assigned(QuikTable) then
  begin
    _SetShowTable;
  end
  else
  begin
    _SetConnectTable;
    if Assigned(QuikTable) then
      _SetShowTable;
  end;
  except
    on E: Exception do
    begin
      Timer.Enabled := False;
      raise Exception.Create('Error Message: ' + E.Message);
    end;
  end;
end;

end.
