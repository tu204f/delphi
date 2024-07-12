unit UnitQuikTableForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.StdCtrls,
  Quik.Manager.DDE,
  Quik.SysUtils,
  Quik.ValueTable;

type
  TQuikTableForm = class(TForm)
    StringGridTable: TStringGrid;
    ListBoxFields: TListBox;
    LayoutTop: TLayout;
    ComboBoxTable: TComboBox;
    TextTitle: TText;
    LayoutClient: TLayout;
    ButtonUpData: TButton;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure ButtonUpDataClick(Sender: TObject);
    procedure ComboBoxTableChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FQuikTable: TQuikTable;
  protected
    procedure UpData;
    procedure SetFieldsTable;
    procedure SetValuesTable;
    property QuikTable: TQuikTable read FQuikTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  QuikTableForm: TQuikTableForm;

implementation

{$R *.fmx}

{ TQuikTableForm }

constructor TQuikTableForm.Create(AOwner: TComponent);
begin
  inherited;
  FQuikTable := nil;
end;

destructor TQuikTableForm.Destroy;
begin
  inherited;
end;

procedure TQuikTableForm.FormShow(Sender: TObject);
begin
  UpData;
end;

procedure TQuikTableForm.UpData;
begin
  QuikManagerTable.SetNameTables(ComboBoxTable.Items);
end;

procedure TQuikTableForm.ButtonUpDataClick(Sender: TObject);
begin
  UpData;
end;

procedure TQuikTableForm.ComboBoxTableChange(Sender: TObject);
var
  xIndex: Integer;
begin
  xIndex := ComboBoxTable.ItemIndex;
  if xIndex >= 0 then
  begin
    FQuikTable := QuikManagerTable.Tables[xIndex];
    SetFieldsTable;
    SetValuesTable;
  end
  else
    FQuikTable := nil;
end;

procedure TQuikTableForm.SetFieldsTable;

  procedure _AddColumn(AGrid: TStringGrid; AHeader: String);
  var
    xColumn: TStringColumn;
  begin
    xColumn := TStringColumn.Create(nil);
    xColumn.Header := AHeader;
    xColumn.Parent := AGrid;
  end;

var
  xFields: TStrings;
  i, iCount: Integer;
begin
  if not Assigned(FQuikTable) then
    Exit;

  // Создаем колокий таблицы
  ListBoxFields.Items.Clear;
  FQuikTable.SetNameFields(ListBoxFields.Items);
  xFields := ListBoxFields.Items;
  StringGridTable.ClearColumns;
  iCount := xFields.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
      _AddColumn(StringGridTable,xFields[i]);
end;

procedure TQuikTableForm.SetValuesTable;
var
  xRow, xCol, xRowCount, xColCount: Integer;
begin
  if not Assigned(FQuikTable) then
    Exit;
  // Заполняем значениями таблицу
  xColCount := FQuikTable.ColCount;
  xRowCount := FQuikTable.RowCount;

  StringGridTable.RowCount := xRowCount;
  if (xRowCount > 0) and (xColCount > 0) then
    for xRow := 0 to xRowCount - 2 do
      for xCol := 0 to xColCount - 1 do
        StringGridTable.Cells[xCol,xRow] := QuikTable.Values[xCol,xRow + 1].AsString;
end;

procedure TQuikTableForm.TimerTimer(Sender: TObject);
begin
  SetValuesTable;
end;

end.
