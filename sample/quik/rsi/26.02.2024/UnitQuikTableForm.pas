unit UnitQuikTableForm;

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
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable;

type
  TQuikTableForm = class(TForm)
    StrGrid: TStringGrid;
    ComboBoxTableQUIK: TComboBox;
    Label1: TLabel;
    Timer: TTimer;
    ButtonUpData: TButton;
    procedure FormShow(Sender: TObject);
    procedure ComboBoxTableQUIKChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonUpDataClick(Sender: TObject);
  private
    FQuikTable: TQuikTable;
  public
    property QuikTable: TQuikTable read FQuikTable;
  end;

var
  QuikTableForm: TQuikTableForm;

implementation

{$R *.dfm}

procedure TQuikTableForm.FormShow(Sender: TObject);
begin
  FQuikTable := nil;
  QuikManagerTable.SetNameTables(ComboBoxTableQUIK.Items);
end;

procedure TQuikTableForm.ButtonUpDataClick(Sender: TObject);
begin
  QuikManagerTable.SetNameTables(ComboBoxTableQUIK.Items);
end;

procedure TQuikTableForm.ComboBoxTableQUIKChange(Sender: TObject);
var
  xIndex: Integer;
begin
  xIndex := ComboBoxTableQUIK.ItemIndex;
  if xIndex >= 0 then
    FQuikTable := QuikManagerTable.Tables[xIndex]
  else
    FQuikTable := nil;
end;

procedure TQuikTableForm.TimerTimer(Sender: TObject);
var
  i, j, iCount, jCount: Integer;
begin
  if Assigned(FQuikTable) then
  begin
    StrGrid.ColCount := FQuikTable.ColCount;

    if FQuikTable.RowCount > 1 then
    begin
      StrGrid.RowCount := FQuikTable.RowCount;
    end else
    begin
      StrGrid.RowCount := 2;
      StrGrid.Rows[1].Clear;
    end;

    iCount := FQuikTable.ColCount;
    jCount := FQuikTable.RowCount;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
        for j := 0 to jCount - 1 do
        begin
          if j = 0 then
            StrGrid.Cells[i,j] := '[' + i.ToString + ']:' + FQuikTable.Cells[i,j].AsString
          else
            StrGrid.Cells[i,j] := FQuikTable.Cells[i,j].AsString
        end;
  end
  else
  begin
    StrGrid.ColCount := 2;
    StrGrid.RowCount := 2;
    for i := 0 to 1 do
      StrGrid.Rows[i].Clear;
  end;
end;

end.
