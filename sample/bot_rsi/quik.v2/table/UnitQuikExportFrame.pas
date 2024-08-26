unit UnitQuikExportFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,

  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable;

type
  TQuikExportFrame = class(TFrame)
    LayoutMenu: TLayout;
    TableGrid: TStringGrid;
    Text1: TText;
    ComboBoxTable: TComboBox;
    ButtonUpData: TButton;
    procedure ButtonUpDataClick(Sender: TObject);
    procedure ComboBoxTableChange(Sender: TObject);
  private
    FQuikTable: TQuikTable;
    procedure QuikTableOnChange(Sender: TObject);
    procedure ShowTableQUIK;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TQuikExportFrame }

constructor TQuikExportFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TQuikExportFrame.Destroy;
begin

  inherited;
end;

procedure TQuikExportFrame.ButtonUpDataClick(Sender: TObject);
var
  xQuikTable: TQuikTable;
  i, iCount: Integer;
begin
  // Обновление списка таблиы
  ComboBoxTable.Items.Clear;
  iCount := QuikManagerTable.Tables.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xQuikTable := QuikManagerTable.Tables.Items[i];
      ComboBoxTable.Items.Add(xQuikTable.Name);
    end;
end;

procedure TQuikExportFrame.ComboBoxTableChange(Sender: TObject);

  procedure _AddCol(const ATitle: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := TableGrid;
    xCol.Header := ATitle;
  end;

  procedure _Fields;
  var
    xS: String;
    xStr: TStrings;
    i, iCount: Integer;
  begin
    xStr := TStringList.Create;
    try
      FQuikTable.SetNameFields(xStr);
      iCount := xStr.Count;
      for i := 0 to iCount - 1 do
      begin
        xS := '[' + i.ToString + ']:' + xStr[i];
        _AddCol(xS);
      end;
    finally
      FreeAndNil(xStr);
    end;
  end;

var
  xIndex: Integer;
begin
  TableGrid.RowCount := 0;
  TableGrid.ClearColumns;
  FQuikTable := nil;
  xIndex := ComboBoxTable.ItemIndex;
  if Index >= 0 then
  begin
    FQuikTable := QuikManagerTable.Tables[xIndex];
    FQuikTable.OnChange := QuikTableOnChange;
    _Fields;
    ShowTableQUIK;
  end;
end;

procedure TQuikExportFrame.QuikTableOnChange(Sender: TObject);
begin
  ShowTableQUIK;
end;

procedure TQuikExportFrame.ShowTableQUIK;
var
  xRow, xRowCount: Integer;
  xCol, xColCount: Integer;
begin
  xRowCount := FQuikTable.RowCount;
  xColCount := FQuikTable.ColCount;
  for xRow := 1 to xRowCount - 1 do
  begin
    TableGrid.RowCount := xRowCount - 1;
    for xCol := 0 to xColCount - 1 do
      TableGrid.Cells[xCol,xRow - 1] := FQuikTable.Cells[xCol,xRow].AsString;
  end;
end;

end.
